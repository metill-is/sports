# Handball and basketball v2: experiment and report source

Branch `feat/hb-bb-v2`. Prototype work: nothing in this folder is called by the production pipeline, and no production path, config or ingest was changed.

## The report

- `report.md` is the report as Markdown and is tracked. `build_report.R` writes it in the same run as the HTML; each figure is reduced to its caption.
- The HTML, `docs/reports/2026-11-15-hb-bb-v2.html`, is **local-only**. `.gitignore` ignores `docs/reports/*.html`, so it is not in git.
- The source is `build_report.R` (header and assembly), which sources `report_helpers.R` and `report_s1.R` to `report_s4.R`. Every number is read from a result file; none is typed in.

Rebuild from the repository root:

```bash
LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/build_report.R")'
```

The build needs the result files, which were local-only: `$HBBB_OUT`, by default `/Users/brynjolfurjonsson/.claude-reviews/sports-hb-bb-v2/`, with `out/`, `kki/`, `repro/` and `logs/`. **They no longer exist** (next section). `report.md` needs `pandoc` on the `PATH`; without it only the HTML is written.

## The result files are gone (since 18 November 2026)

Everything under `/Users/brynjolfurjonsson/.claude-reviews/` was removed on 18 November 2026, with no backup. For this experiment that is the whole of `$HBBB_OUT`: the four states of the results in `out/` (with `run_identity*.rds` and `run_provenance.tsv`), the Baskethotel game cards and regulation-time scores in `kki/`, the repeat runs in `repro/`, `logs/`, the two option-off runs in `round3/`, the failing-test logs of the review rounds, and the frozen checkouts `tree-fits`, `tree-noopt`, `tree-r3` and `tree-r3b`.

What survives:

- `report.md` here, built on 17 November at d230adf85 from those files. It is now the only record of the measured numbers; it cannot be rebuilt and its numbers cannot be recomputed until the files are regenerated.
- The local HTML build, `docs/reports/2026-11-15-hb-bb-v2.html` in the main checkout (git-ignored).
- Every script that made the files, in this folder and in `R/`, and the data they were computed against, which is in git (below).

What fails without the files: `build_report.R` stops at its first line that reads `kki/periods_matched.rds` without a check; `check_headlines.R` and every scoring, comparison and reporting script stop on the first missing file. Nothing in the package or its tests reads `$HBBB_OUT`.

What would regenerate them, in this order (not done: it repeats about 2,200 R-INLA fits, the NUTS ladder and roughly 3,300 web requests):

1. **Frozen data.** `git worktree add --detach <dir> 0da2b3f79` (the last commit before main was first merged; its `data/` is the branch point's, 67fcb6baa), then set `HBBB_DATA=<dir>/data` for every script below.
2. **Regulation-time scores.** `kki_games.R`, `kki_fetch_periods.sh` (one Baskethotel game card per game, over the network), `kki_periods.R`. The source may have changed since November.
3. **Fixed-cutoff data and NUTS fits.** `prep.R` per cell (cutoff 2026-01-31, hold-out end 2026-05-31), then `run_fit.R` for each ladder rung and the production fits (`r0_prod`; tens of minutes each), `diag_prod.R`.
4. **R-INLA fits,** one heavy job at a time through the machine limiter: `wf_inla.R` for the eight walk-forwards (`hb_m none 2022`, `hb_f none 2022`, `bb_m none 2022 reg`, `bb_f none 2022 reg`, `bb_m ar1 2025 reg`, `bb_f ar1 2025 reg`, `bb_m none 2022`, `bb_f none 2022`), `inla_validate.R`, `descriptives.R`, `within_rho.R`.
5. **Scoring and comparisons.** `wf_score.R`, `wf_compare.R`, `ladder_summary.R`, `compare_validation.R`; for the reproducibility sections `repro_descriptives.sh` and `repro_descriptives.R`, `cold_start_check.R` (repeated in separate processes) and a second complete run of step 4 compared with `compare_runs.R`; then `wf_status.R`, `check_headlines.R` and `build_report.R`.

What cannot be regenerated as it was: the three earlier states (`out/at-84b9e6e5b/`, `out/first-run/`, `out/second-run/`). The first two runs were made with R-INLA's defaults, under which a fit does not repeat between processes, so a rerun gives one more draw of the same spread, not those numbers. The sections that set the final run against them (the corrections appendix, "The R-INLA fits, run four times", `review_corrections.R`, `headline_changes.R`, `report_table_changes.R`) would have to be dropped or rewritten. A regeneration with today's fitter goes through the per-call option (next section); whether it reproduces the stored final numbers bit for bit was not checked, because there is nothing left to compare with.

Recompute the headline numbers from the result files without going through the builder:

```bash
Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/check_headlines.R")'
```

## Scripts, in run order

Run everything from the repository root (`common.R` calls `devtools::load_all(".")` and reads `data/`). Launch scripts as `Rscript -e 'source("<script>")' <args>`: `Rscript <script>` reads the file while it runs, so editing a running script corrupts the run.

| Script | Does | Writes under `$HBBB_OUT` |
| --- | --- | --- |
| `common.R`, `score.R` | Shared paths, cells, data access; scoring functions | nothing |
| `prep.R <cell> <cutoff> <hold-out end>` | Training and hold-out data at the fixed cutoff | `out/data_<cell>_<cutoff>.rds` |
| `run_fit.R <cell> <rung> <cutoff>` | One NUTS fit of a ladder rung (`r0_prod` to `r5_gauss`, `v2*`) with the models in `stan/` | `out/res_<cell>_<rung>.rds`, `out/draws_<cell>_<rung>.rds` |
| `diag_prod.R` | Diagnostics of the production fits | `out/diag_prod.rds` |
| `descriptives.R` | One in-sample v2 fit per cell: residuals, dispersion, hyperparameters | `out/descriptives.rds` |
| `repro_descriptives.sh`, `repro_descriptives.R` | Repeat `descriptives.R` in separate processes and compare the runs | `repro/run_*/`, `out/repro_descriptives.rds` |
| `kki_games.R`, `kki_fetch_periods.sh`, `kki_periods.R` | Game ids, one Baskethotel game card per game, regulation-time scores | `kki/games.tsv`, `kki/raw/`, `kki/periods.tsv`, `kki/periods_matched.rds` |
| `inla_validate.R`, `compare_validation.R` | R-INLA at the fixed cutoff; R-INLA against the Stan reference | `out/inla_validate_<cell>.rds`, `out/compare_validation.rds` |
| `within_rho.R` | Sensitivity to the fixed weekly correlation | `out/within_rho.rds` |
| `ladder_summary.R` | Ladder tables from the fits | `out/ladder.rds` |
| `wf_inla.R <cell> <within> <first season> [reg]` | Weekly walk-forward with R-INLA; keeps the mode status of every fit and repeats a failed search from a cold start. With `WF_REFIT_FAILED=1` it applies that rule to a stored walk-forward instead of running one | `out/wf_<cell>_<tag>.rds` |
| `cold_start_check.R <run number> [noopt]` | One fit from a cold start, to be repeated in separate processes: with R-INLA's defaults, or with `noopt` with its online internal optimisation off, which is the fitter's own default | `out/cold_start_<n>.rds`, `out/cold_start_noopt_<n>.rds` |
| `compare_runs.R <results a> <results b> [name]` | Are two runs of the R-INLA fits identical? Every stored number, in the order stored, and which stored components differ | `out/<name>.rds` (default `run_identity`) |
| `wf_status.R` | Mode status and cold retries of every fit; the results in `out/` against an earlier run (`WFS_REFERENCE`, default `second-run`) | `out/wf_status.rds` |
| `review_corrections.R` | Old value beside new for every number the reviews changed, in four states of the results | `out/review_corrections.rds` |
| `wf_score.R`, `wf_compare.R` | Walk-forward scores and second-stage tests; comparison with production's stored forecasts and the Lengjan line | `out/wf_scores.rds`, `out/wf_compare.rds` |
| `check_headlines.R` | Independent recomputation of the headline numbers | prints only |
| `headline_changes.R` | The headline numbers of the second run against those of the results in `out/`, line by line | `out/headline_changes.rds` |
| `report_table_changes.R [earlier report.md]` | Every table row of the report, an earlier build against the current one (default: the second run's build, `out/second-run/report.md`). The report quotes its totals, so build, run this, and build once more | `out/report_table_changes.rds`, `logs/report_table_changes.md` |
| `build_report.R` | The report | `docs/reports/2026-11-15-hb-bb-v2.html` (ignored), `report.md` |

## Which data the results were computed against

The stored results were computed against the data and config of the branch point, commit 67fcb6baa. Main has since been merged into the branch twice: `data/` now holds newer results and a pruned forecast archive. The scripts that read data (`prep.R`, `wf_inla.R`, `descriptives.R`, `wf_compare.R`, `kki_periods.R`, `cold_start_check.R`) reproduce the stored results only from a checkout of the last commit before the merge (0da2b3f79), or with `HBBB_DATA` pointing at that checkout's `data/`. The scoring and reporting scripts read only `$HBBB_OUT`. The merge changed none of the package functions these scripts call (`model_training_results()`, `read_table()`, `.is_forfeit()`, `load_leagues()`, `normalise_lengjan_team_names()`), and the production comparison run from the merged checkout with `HBBB_DATA` set reproduces the pre-merge result exactly.

`fit_v2_inla()` asks R-INLA for each fit with its online internal optimisation off (`reproducible = TRUE`, its default), passing `control.compute = list(internal.opt = FALSE)` for that call only, as the football engine does since PR #107; for posterior draws it also seeds R's generator, which picks the hyperparameter configuration of each draw. The stored results were computed before the per-call form (465af14fa), when the fitter switched R-INLA's session-wide option off for the fit and put the caller's setting back. R-INLA 25.6.7 treats the two alike: only `inla.problem.section()` reads the session option, and only when the call's value is NULL. Every stored R-INLA result in `out/` was computed with the option off, in a run that was made twice and compared number by number (`compare_runs.R`, `out/run_identity.rds`). `out/run_provenance.tsv` names the commit each result file was computed from: the eight walk-forwards, the descriptive fits and the weekly-correlation fits from aabdeadd2, the fixed-cutoff fits from 14ed393e8, which differs from it in the seeding of the draws only. The earlier runs, made with R-INLA's defaults at commits before those changes, are kept under `out/first-run/` and `out/second-run/`.

`out/` holds four states of the results: `out/at-84b9e6e5b/` (the results the report was first built from), `out/first-run/` (the same predictions scored by the corrected scripts), `out/second-run/` (the second run, made with R-INLA's defaults, which kept the mode status of every fit) and `out/` itself (the run with the online optimisation off, which the report is built from). The repeat of that last run is in `round3/b/out/`; `round3/{a,b}/out/unseeded-draws/` holds the fixed-cutoff fits as they came out before the draws were seeded.

## Read the numbers with these in mind

- Every wall time, the R-INLA fit times included, was taken on a machine shared with other sessions: good for ratios, not benchmarks.
- With R-INLA's defaults a fit was not bit-reproducible between runs: six runs of the eight `descriptives.R` fits on identical inputs gave 46 of 48 bit-identical results, and the two others moved by a small fraction of a posterior sd. With the online optimisation off and the draws seeded, two complete runs of every R-INLA fit behind the report agree in every stored number (14 files, 3,717,938 numbers; timings, the machine record and row names left out).
- In the stored run, 7 of the 2,138 walk-forward fits were repeated from a cold start because the warm-started search failed or threw (all in basketball); every one then ended with status 0, so no prediction is left out. The 46 fits outside the walk-forward ended with status 0. In the second run, made with R-INLA's defaults, four walk-forward fits had failed and succeeded when refitted cold.
- A successful search is still not a unique answer. Between the two runs made with R-INLA's defaults, and again between the second of them and the stored run, single predictions moved (how a team's strength is split between club level and season level) while the mean scores stayed where they were. The option makes the answer repeatable; it does not make it the only one. See "The R-INLA fits, run four times" in the report.
- A basketball final margin cannot be level, so every score of one uses a distribution with no mass on zero (`v2_integer_pmf(ties = FALSE)`).
- The walk-forward scored margin and total as two separate whole-number distributions. The joint (home, away) sampler, `v2_sample_scores()` in `R/v2-margin-total-predict.R`, is unit-tested but its scores were not evaluated.

## Package files on this branch

`R/v2-margin-total-{data,inla,predict,adapter}.R` and `R/ingest-kki-periods.R`, with tests in `tests/testthat/`. `DESCRIPTION` gained five `Collate` entries for them; R-INLA is not declared in `Suggests`. The R-INLA tests reach it through `skip_if_no_inla()`: they skip in CI Tests and run, failing where INLA is missing, in main's INLA Engine workflow.
