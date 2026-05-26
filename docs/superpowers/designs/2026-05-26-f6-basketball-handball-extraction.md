# F6 · Basketball + Handball Extraction Layer

**Status:** Phase 1 (extraction) shipping in this commit. Phase 2 (publisher refactor) deferred to autumn 2026 cutover.
**Closes:** F6 from the [2026-05-25 cross-project pipeline review](../../audits/2026-05-25-pipeline-cross-project-review.html).

## Background

Football iceland's publisher reads from per-fit Parquet *extracts* at `data/beliefs/extracts/sport=football/country=iceland/sex=Z/fit_date=D/`, decoupled from the in-memory fit RDS. This lets us re-publish for any historical date in seconds (no Stan re-fit) and gives us replay + cumulative-xPts analysis as a free byproduct.

Basketball and handball still load the fit RDS directly. That means:

- **No replay** — the RDS is ephemeral (gitignored, ~500 MB) and Stan's RNG would produce numerically different draws on re-fit.
- **No historical "what did we predict on date X" reconstruction** without keeping every fit RDS forever (untenable for storage).
- **No cumulative-trajectory analysis** equivalent to `cumulative_xpts_long()` for those sports.

The autumn 2026 cutover is when both sports come out of seasonal pause. Shipping the extraction layer **now** means the first autumn fit lands directly in `data/beliefs/extracts/` instead of the legacy fit-RDS-only path — so by the time we want to do post-season analysis we already have the data.

## Schema-alignment decision

Football's per-cell extracts shape:

| File | Shape | Sport-specific bits |
|------|-------|--------------------|
| `predicted_matches.parquet` | Per-match draws | `goal_diff_distribution` as integer-keyed long table |
| `team_strengths_quantiles.parquet` | 9-cell grid (component × location) | None — same parameter families |
| `round_strengths_quantiles.parquet` | Per-round projections | Football-specific (cumulative round simulation) |
| `home_advantage_quantiles.parquet` | Per-team home advantage | None |
| `final_positions.parquet` | Per-team placement probabilities | None |
| `points_distribution.parquet` | Per-team points-total distribution | Different draws (no-draws for basketball) |
| `tournament_placements.parquet` | Cup-only, P(reach round X) | Cup-only |

**Recommendation: hybrid alignment.**

| Element | Decision |
|---------|----------|
| `meta.json` outer envelope | **Identical** across sports — sport / sex / league / division / is_cup / season / generated_at / fit_date / round / n_draws |
| `team_strengths_quantiles` 9-cell grid | **Adopt verbatim** — Stan parameter families are identical (`cur_offense_home`, etc.) |
| `home_advantage_quantiles` | **Adopt verbatim** — same `home_advantage_off` / `home_advantage_def` / `home_advantage_tot` |
| `predicted_matches.parquet` | **Adopt with score-binning** — basketball goal_diff_distribution spans -50..+50 (binned in 5-point buckets); handball -20..+20 |
| `final_positions.parquet` | **Adopt verbatim** — same placement probability shape |
| `points_distribution.parquet` | **Adopt with per-sport scoring rules** — basketball is W/L only (`has_ties = FALSE`), handball is W/D/L like football. Both already in `publish-iceland-2dt-helpers.R` |
| `round_strengths_quantiles.parquet` | **Defer** — football-specific cumulative-round simulation. Basketball/handball can adopt later if needed |
| `tournament_placements.parquet` | **Skip** — basketball/handball don't have a modelled knockout cup currently |
| `xg_for` / `xg_against` / `xpts` in standings | **Null** — basketball/handball have no goals process (Student-t on signed goal diff). Already null in current publish output |

Net per-sport extract set: **5 Parquets** (vs football's 9). Same per-cell shape as football for the 5 that map cleanly; null/zero-row for the others.

## File layout (this session)

```
R/
├── publish-iceland-2dt-helpers.R       # existing; reused as-is
├── extract-iceland-2dt-shared.R        # NEW: shared extraction primitives
├── extract-basketball-iceland.R        # NEW: ~180 lines (entry point + sport glue)
├── extract-handball-iceland.R          # NEW: ~180 lines (entry point + sport glue)
├── publish-basketball-iceland.R        # unchanged this session
├── publish-handball-iceland.R          # unchanged this session
└── model-league.R                      # extended: extract on fit for these sports too

data/beliefs/extracts/
├── sport=football/country=iceland/sex=Z/fit_date=D/      # existing (9 parquets)
├── sport=basketball/country=iceland/sex=Z/fit_date=D/    # NEW (5 parquets)
└── sport=handball/country=iceland/sex=Z/fit_date=D/      # NEW (5 parquets)

tests/testthat/
├── test-extract-basketball-iceland.R   # NEW
└── test-extract-handball-iceland.R     # NEW
```

## Per-sport adaptations

### Basketball

- Top division: `BD` (Bonusdeild)
- No draws: `has_ties = FALSE`, no `p_draw` in next_games
- Score scale: 60-130 per side → goal_diff_distribution binned in 5-point buckets in [-50, +50]
- Posterior goals: `goals1_pred` / `goals2_pred` are continuous (Student-t draws), not integer
- N teams: 12 (male) / 10 (female) in current season

### Handball

- Top division: `OD` (Olís-deild — different storage-schema code than basketball/football)
- Has draws: `has_ties = TRUE` with `tie_threshold = 0` (close to football, but the Student-t makes draws probabilistic at the score-diff = 0 boundary; `tie_threshold` configured in `leagues.yml`)
- Score scale: 18-35 per side → goal_diff_distribution in 2-point buckets [-20, +20]
- Same Stan family as basketball

## Phase 2 — Publisher refactor (autumn 2026)

Defer the publisher rewrite until basketball/handball seasons restart. Then:

1. Add `read_extracted_basketball_iceland()` / `read_extracted_handball_iceland()` mirroring `read_extracted_football()`.
2. Refactor `publish_basketball_iceland(extracted, ...)` / `publish_handball_iceland(extracted, ...)` to consume the extracts tree.
3. Add `replay_basketball_iceland()` / `replay_handball_iceland()` to `R/replay.R`.
4. Wire schemas under `config/publish-schemas/{basketball,handball}/` matching the football set.
5. Update `scripts/0Nr_replay.R` argparse to drop the "football_iceland only" gate.
6. Backfill: re-fit at strategic past dates if a season-trajectory report is desired.

The extraction layer this session ships is the prerequisite; the autumn ramp is mechanical from here.

## Why not ship Phase 2 now?

Three reasons:

1. **Both sports are seasonally paused.** Regular seasons finished late April 2026. Refactoring publishers without a fresh fit to test against introduces drift risk.
2. **The publisher refactor is invasive.** Football's took 4 phases (extraction → publisher rewrite → CI flow → republish workflow). Basketball/handball don't need the full 4 — they're single-division — but the bytewise-regression baseline (so we know the new publisher matches the old) needs a known-good test fixture, which only fresh fits provide.
3. **Schema design wants real consumer feedback.** The metill-platform side's JS renderers for basketball/handball don't exist yet (per data-contract: "Basketball + handball are produced and rsynced, but not rendered until autumn 2026"). Better to design schemas alongside the rendering code, not before.

## Testing approach

- **Extraction round-trip**: `extract_basketball_iceland(fit, league, sex)` writes 5 parquets; `arrow::open_dataset()` reads them back; row counts and types match the in-memory tibbles.
- **Schema sanity**: each Parquet conforms to the schema columns documented in this design doc.
- **End-to-end smoke**: from the on-disk fit RDS at `data/beliefs/fits/sport=basketball/country=iceland/sex=male/fit.rds`, run the extraction into a tempdir and verify the 5 expected files are present with non-zero rows.

## Audit log entry

The companion entry lives in `docs/audits/2026-05-25-pipeline-cross-project-review.html` (Phase 1 shipped 2026-05-26). The Phase 2 work has its own design doc that future-self should add when the autumn 2026 ramp begins.
