# Publish Schemas

JSON Schema (Draft-07) contracts for every JSON the publishers emit to
`data/publish/`. Both `metill-is/sports` (producer) and
`metill-is/metill-platform` (consumer) validate against these schemas;
either side failing closed prevents broken pages on `fly.metill.is/ithrottir/`.

> Shipped 2026-05-26 (audit F4 closure).

## Layout

```
config/publish-schemas/
├── README.md                                    # this file
└── football/
    ├── meta.schema.json
    ├── next_games.schema.json
    ├── standings.schema.json
    ├── team_strengths.schema.json
    ├── final_positions.schema.json
    ├── team_strengths_history.schema.json
    ├── final_positions_history.schema.json
    ├── home_advantage.schema.json
    ├── points_distribution.schema.json
    ├── standings_history.schema.json
    └── tournament_placements.schema.json        # cup cells only
```

Schemas are **sport-namespaced**: a JSON at
`data/publish/<sport>/<...>/<file>.json` is validated against
`config/publish-schemas/<sport>/<file>.schema.json`, falling back to
`config/publish-schemas/<file>.schema.json` (sport-agnostic) if the
namespaced variant doesn't exist. JSONs without any matching schema are
classified as `unmatched` (informational, not errors).

Today only `football/` is populated. Basketball and handball legacy
publishers (`publish_basketball_iceland()`, `publish_handball_iceland()`)
still consume the fit RDS directly and emit a different schema; the
validator skips them via the `unmatched` path. They migrate into the
schema set as part of F6 at the autumn 2026 cutover.

## How the validators fire

### Producer side (R) — `R/validate-publish.R`

`validate_publish_dir(dir, schema_dir)` walks `dir` for `*.json` files,
runs `jsonvalidate::json_validate()` against the resolved schema, and
returns a list with `ok`, `n_files`, `n_passed`, `n_failed`, `errors`,
`unmatched`.

`publish_one()` calls it at the end of every successful publish; on
failure it aborts via `cli::cli_abort()` so the daily-driver
(`scripts/05_publish.R`) and `republish.yml` workflow both fail closed.
Set `validate = FALSE` from a synthetic-data test that emits a JSON the
schema would reject by design.

### Consumer side (Python) — `metill-platform/scripts/validate_publish.py`

Mirror implementation using `fastjsonschema`. Runs inside
`pull-sports-data.yml` between rsync and commit, against
`data/ithrottir/` (the synced JSONs) and `data/ithrottir-schemas/` (the
synced schemas). Exits non-zero on validation failure, which stops the
workflow before the commit + deploy-chain dispatch — production stays on
the last-known-good payload.

## How drift surfaces

If the producer evolves a JSON shape without updating the schema:

1. **Producer-side**: `validate_publish_dir()` rejects the new JSON
   immediately. `scripts/05_publish.R` exits non-zero. No commit, no
   push, no platform pull, no deploy.
2. **Consumer-side (belt-and-braces)**: if the producer schema was
   updated but the metill-platform rsync is somehow lagging the producer
   JSONs, `validate_publish.py` catches the mismatch and stops the pull
   before commit + deploy dispatch.

So a schema change requires updating both the schema and the producer
in the same commit. If you only update one, the other catches it.

## Strictness contract (v1)

| Constraint | Enforced? |
|---|---|
| Top-level required keys | ✅ |
| Field types (string / number / integer / boolean / array / null union) | ✅ |
| URL-safe slug patterns where used (division codes) | ✅ |
| Date string pattern `^\d{4}-\d{2}-\d{2}` and ISO datetime prefix | ✅ |
| Probability fields constrained to `[0, 1]` | ✅ |
| Cumulative-xG fields nullable (early-cell case before any fit) | ✅ |
| `additionalProperties: false` (typo-catching) | ❌ — left default-permissive |
| Cross-field constraints (`played == wins + draws + losses`) | ❌ — too brittle for schema; covered by tests |
| Numeric value ranges beyond probabilities | ❌ — handled by tests |

The deliberate omissions keep schemas additive: a future Stan model can
add a new field without breaking the contract, and tests catch the
semantic invariants without false-positive churn.

## Updating a schema

1. Edit the schema file under `config/publish-schemas/<sport>/`.
2. Update the producer (`R/publish-*.R` or `R/extract-*.R`) to emit the
   new shape — if the schema requires a new field, the test suite
   surfaces every test that doesn't add it yet.
3. Run `Rscript -e 'devtools::test(filter = "publish")'` — the
   `test-publish-schemas.R::validate_publish_dir() succeeds on the live
   publish tree` test catches mismatches against actual production JSONs.
4. Commit. The next hourly `pull-sports-data.yml` carries both the new
   producer output and the new schema to the platform side.

## Cross-reference

- [Sports/Knowledge/Publish Pipeline/data-contract.md](../../docs/) —
  Metill vault: per-JSON catalogue (the human-readable view of these
  schemas)
- `R/validate-publish.R` — R-side validator
- `metill-platform/scripts/validate_publish.py` — Python-side validator
- `tests/testthat/test-publish-schemas.R` — regression harness
