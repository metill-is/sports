# Publish Schemas

JSON Schema (Draft-07) contracts for every JSON the publishers emit to
`data/publish/`. Both `metill-is/sports` (producer) and
`metill-is/metill-platform` (consumer) validate against these schemas;
either side failing closed prevents broken pages on `fly.metill.is/ithrottir/`.

> Shipped 2026-05-26 (audit F4 closure). Generated from `_base` + `_delta`
> since 2026-09-04 (Plan B, WS11).

## Layout

```
config/publish-schemas/
+-- README.md                 # this file
+-- _base/                    # the shared contract: 11 *.json
+-- _delta/<sport>/           # per-sport RFC-7386 patches; SOURCE
+-- _draft/<sport>/           # rendered but NOT armed (resolves nowhere)
+-- football/                 # rendered + committed; ARMED
+-- basketball/               #   "
+-- handball/                 #   "
```

All three sports are armed as of 2026-09-04. Basketball and handball were
authored under `_draft/`, proved against eight fixture-published cells, and
moved into place by one `git mv` only after the 32 stale pre-division JSONs
were deleted -- in that order, because arming with non-conforming JSON on disk
freezes the site.

`_base/`, `_delta/` and `_draft/` are **sources and staging**, never resolved
by either validator. The per-sport directories are **generated output** and are
the only files either validator reads. Both are committed.

The base files are `<name>.json`, deliberately **not** `<name>.schema.json`.
Both resolvers try `<root>/<sport>/<name>.schema.json` then
`<root>/<name>.schema.json`, so a `_base/meta.schema.json` would resolve as if
`_base` were a sport. Nothing can reach it today -- a publish JSON's first path
segment is written by the publisher and is always a real sport -- but dropping
the `.schema` infix makes "the sources are inert" a fact rather than a
coincidence, and there is a test.

## Generating

```bash
Rscript tools/gen-publish-schemas.R
```

Renders `_base/<name>.json` merged with `_delta/<sport>/<name>.json`
into `<sport>/<name>.schema.json` for every sport listed in the generator's
`SCHEMA_ARMED_SPORTS`, and into `_draft/<sport>/` for every other sport that
has a `_delta/` directory.

Three rules the generator encodes, all of them load-bearing:

1. **The manifest is the directory.** `_delta/<sport>/<name>.json` *existing*
   is what declares that `<sport>` emits `<name>.json`. There is no separate
   list to drift. An empty `{}` delta is legal and means "identical to base".
   A rendered file with no delta behind it is deleted as a leftover.
2. **RFC 7386 replaces arrays wholesale.** Objects merge recursively and a
   `null` value deletes a key, but a delta touching `required` **replaces the
   whole array**. Forgetting an entry silently *relaxes* that sport's contract
   and the generator cannot detect it. Review every delta that mentions
   `required`.
3. **Everything under this directory is pure ASCII, enforced.**
   `jsonlite::toJSON()` renders a UTF-8 em-dash as the literal seven-character
   string `<U+2014>` even when `Encoding()` already reports `"UTF-8"`, so one
   non-ASCII character in a `_base` description would be silently corrupted
   into every rendered sport at once. The generator aborts on a non-ASCII byte
   in a source or in its own output; transliterate instead (`--` for an
   em-dash, `x` for a multiplication sign, plain ASCII for Icelandic letters).

`tests/testthat/test-publish-schema-generation.R` asserts that a fresh render
reproduces the committed output byte-for-byte, so a hand edit to a generated
file is caught rather than silently reverted by the next render.

## Arming a sport

A sport whose schemas exist but are not yet trusted lives in `_draft/`.
Neither resolver ever looks there: `R/validate-publish.R::.resolve_schema_path`
and `metill-platform/scripts/validate_publish.py::resolve_schema_path` each try
exactly `<root>/<sport>/<name>.schema.json` then `<root>/<name>.schema.json`,
and no publish JSON can have `_draft` as its first path segment. So drafts can
be committed, reviewed and rsynced to the platform while staying inert.

Arming is two steps, deliberately:

```bash
git mv config/publish-schemas/_draft/<sport> config/publish-schemas/<sport>
# then add <sport> to SCHEMA_ARMED_SPORTS in tools/gen-publish-schemas.R
```

Rollback is `git rm -r config/publish-schemas/<sport>` plus the reverse of that
one-line edit; no JSON is touched either way.

**Arming is immediate on both sides.** `pull-sports-data.yml` sparse-checks
`data/publish` and `config/publish-schemas` from ONE clone at ONE SHA and
rsyncs them to `data/ithrottir/` and `data/ithrottir-schemas/` with `--delete`,
7x/day. So the very next pull makes `validate_publish.py` fail closed for the
newly armed sport. Any non-conforming JSON already sitting in `data/publish/`
for that sport freezes the site on the last-known-good payload -- which is why
stale cells are deleted BEFORE the `git mv`, never after.

## No sport-agnostic root schema

There must be no `*.schema.json` at the root of `config/publish-schemas/`.
Both resolvers fall back to it for **every** sport, `world_cup` included, which
would arm a contract nobody wrote for it. There is a test.

## How the validators fire

### Producer side (R) -- `R/validate-publish.R`

`validate_publish_dir(dir, schema_dir, sport = NULL)` walks `dir` for `*.json`
files, runs `jsonvalidate::json_validate()` against the resolved schema, and
returns `ok`, `n_files`, `n_passed`, `n_failed`, `errors`, `unmatched`.

`sport = NULL` derives the sport from each file's first path segment relative
to `dir`, which is right when `dir` is the whole publish tree. A caller
narrowing `dir` to one sport's subtree **MUST** pass `sport` explicitly:
without it the derived sport becomes `"iceland"`, no schema resolves, every
file lands in `unmatched` and the function returns `ok = TRUE, n_files = 0` --
validation silently doing nothing.

`publish_one()` calls it at the end of every successful publish, against that
sport's subtree only, so arming one sport can never abort another's publish.
Set `validate = FALSE` from a synthetic-data test that emits a JSON the schema
would reject by design; `schema_dir` overrides the tree it validates against.

### Consumer side (Python) -- `metill-platform/scripts/validate_publish.py`

Mirror implementation using `fastjsonschema`. Runs inside
`pull-sports-data.yml` between rsync and commit, against `data/ithrottir/` and
`data/ithrottir-schemas/`. Exits non-zero on validation failure, which stops
the workflow before the commit + deploy-chain dispatch -- production stays on
the last-known-good payload.

Verify it locally against exactly what the rsync will deliver:

```bash
SCRATCH=$(mktemp -d)
mkdir -p "$SCRATCH/mirror"
rsync -a --delete ~/sports/data/publish/            "$SCRATCH/mirror/ithrottir/"
rsync -a --delete ~/sports/config/publish-schemas/  "$SCRATCH/mirror/ithrottir-schemas/"
cd ~/metill-platform && uv run --extra data python scripts/validate_publish.py \
  --tree "$SCRATCH/mirror/ithrottir" --schemas "$SCRATCH/mirror/ithrottir-schemas"
```

(`uv run` outside the project root drops `--extra` behind a warning that is
easy to skim past, so run it from `~/metill-platform`.)

`_base/`, `_delta/`, `_draft/` and this README ride that rsync as inert extra
files. If the noise ever matters the fix is an `--exclude '_*'` on the
platform's schema rsync.

## How drift surfaces

1. **Producer-side**: `validate_publish_dir()` rejects the new JSON
   immediately, that cell's publish target fails, and `scripts/05_publish.R`
   exits non-zero. No commit, no push, no platform pull, no deploy.
2. **Consumer-side (belt-and-braces)**: if the producer schema was updated but
   the rsync somehow lags the producer JSONs, `validate_publish.py` catches the
   mismatch and stops the pull before commit + deploy dispatch.

A sport with no schema directory at all is currently SKIPPED with an
informational note -- the fail-open end WS11 task 8 closes once every sport
that goes through `publish_one()` is armed.

## Strictness contract (v1)

| Constraint | Enforced? |
|---|---|
| Top-level required keys | yes |
| Field types (string / number / integer / boolean / array / null union) | yes |
| URL-safe slug patterns where used (division codes) | yes |
| Date string pattern and ISO datetime prefix | yes |
| Probability fields constrained to `[0, 1]` | yes |
| Cumulative-xG fields nullable (early-cell case before any fit) | yes |
| `additionalProperties: false` (typo-catching) | no -- left default-permissive |
| Cross-field constraints (`played == wins + draws + losses`) | no -- too brittle for schema; covered by tests |
| Numeric value ranges beyond probabilities | no -- handled by tests |

The deliberate omissions keep schemas additive: a future Stan model can add a
new field without breaking the contract, and tests catch the semantic
invariants without false-positive churn.

## Updating a schema

1. Edit `_base/<name>.schema.json` (all sports) or
   `_delta/<sport>/<name>.json` (one sport). **Never** edit a file under
   `<sport>/` -- the next render reverts it and the generation test goes red.
2. Run `Rscript tools/gen-publish-schemas.R`.
3. Update the producer (`R/publish-*.R` or `R/extract-*.R`) to emit the new
   shape in the same commit.
4. Run `Rscript -e 'devtools::test(filter = "publish")'`.
5. Commit source and rendered output together.

## Cross-reference

- `tools/gen-publish-schemas.R` -- the generator
- `R/validate-publish.R` -- R-side validator
- `metill-platform/scripts/validate_publish.py` -- Python-side validator
- `tests/testthat/test-publish-schema-generation.R` -- regenerability + ASCII
- `tests/testthat/test-publish-schema-arming.R` -- the subtree isolation proof
- `tests/testthat/test-publish-schemas.R` -- live-tree regression harness
