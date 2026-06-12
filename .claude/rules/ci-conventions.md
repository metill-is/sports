---
paths:
  - ".github/workflows/**"
---

# CI / GitHub Actions Conventions

Two coordinated workarounds are applied across the R-package workflows
under `.github/workflows/`. Both exist to defend against upstream brittleness
in the GitHub-hosted Ubuntu runner image's R-package install pipeline.

## 1. `PKG_SYSREQS: "false"` on `setup-r-dependencies@v2`

Disables pak's automatic `add-apt-repository` install of system
requirements.

- **Rationale:** `chromote`'s `SystemRequirements` field maps to
  `ppa:xtradeb/apps` on Ubuntu. Registering that PPA queries
  `api.launchpad.net` for metadata, and Launchpad outages periodically
  time out (`TimeoutError: [Errno 110]`) for several hours at a time —
  silently breaking every R-package install on every workflow.
  Disabling pak's sysreqs handler removes the dependency on Launchpad
  entirely.
- **Substitute coverage:** the Ubuntu 24.04 GitHub-hosted runner image
  pre-installs Google Chrome and Chromium (so chromote works at
  runtime), and the standard `ubuntu-latest` libraries (libcurl,
  libssl, libxml2, libyaml) cover every other declared
  `SystemRequirements` in the project's package set. The
  `browser-actions/setup-chrome@v2` step in the scrape workflows
  further pins Chrome and exports `CHROMOTE_CHROME`.

## 2. Reinstall V8 from source with `DOWNLOAD_STATIC_LIBV8=1`

Runs **after** `setup-r-dependencies`:

```yaml
- name: Reinstall V8 from source with bundled static libv8
  env:
    DOWNLOAD_STATIC_LIBV8: "1"
  run: |
    Rscript -e 'install.packages("V8", type = "source", repos = c(CRAN = "https://cloud.r-project.org"))'
```

- **Rationale:** `jsonvalidate` (used in `R/config.R::validate_leagues`
  to check `leagues.yml` against `leagues.schema.json`) depends on
  `V8`. Posit Package Manager's pre-built `V8` binary for Ubuntu 24.04
  noble was linked against `libnode.so.109` (Node 18.x ABI), but noble
  ships Node 20.x, which provides `libnode.so.115`. With sysreqs
  disabled, no libnode is installed at all, so `dyn.load("V8.so")`
  fails immediately on every entry script that calls `load_leagues()`
  (every entry script does).
- **Fix:** build V8 from source on the runner with
  `DOWNLOAD_STATIC_LIBV8=1`. V8's configure script downloads a static
  libv8 binary from CRAN's V8 release page and links against it. The
  resulting V8 `.so` has zero system library dependencies. This step
  takes ~30 s but is robust against any libnode/libv8 ABI changes in
  the runner image.
- **Order matters:** this step runs **after** `setup-r-dependencies`
  (which has installed the broken PPM binary) and overwrites V8 in
  `R_LIBS_USER`.

## Detection if a future package adds an unmet sysreq

With `PKG_SYSREQS=false`, pak still _prints_ system requirements but
skips the apt install. If a new R package declares a sysreq the runner
image doesn't pre-install, the failure is a runtime
`dyn.load: cannot open shared object file` (loud, immediate). The
remedy is either a one-line `apt-get install` step before
`setup-r-dependencies`, or — if the issue is an ABI mismatch like the
V8 one — a from-source rebuild step like the V8 one above.

## Workflow name gotcha (`workflow_run` glob trap)

The `on.workflow_run.workflows` array uses **glob patterns**, not
literal strings. Special meta-characters (`+`, `*`, `?`, `[`, `]`, `!`)
in a workflow's `name:` field will silently break any `workflow_run`
trigger that references it
([github/docs#12572](https://github.com/github/docs/issues/12572)).

This bit us once: the upstream workflow was named
`"Scrape Federation Results + Schedules"`, so the downstream
`fit.yml`'s `workflows: ["Scrape Federation Results + Schedules"]`
never matched, and the chain silently produced zero fit runs from
2026-04-30 cutover until 2026-05-01 when the bug was found.

**Fix:** renamed to `"Scrape Federation Results and Schedules"` (no
meta-characters). Keep all workflow `name:` fields free of glob
meta-characters.

## Workflow inventory

| Workflow | Trigger | Purpose |
|---|---|---|
| `ci-tests.yml` | push, PR | `devtools::test()` |
| `scrape-results.yml` | cron 1×/day | Federation results + schedules |
| `scrape-odds.yml` | cron 3×/day | Lengjan odds snapshot |
| `fit.yml` | `workflow_run` from scrape-results | Stan fit |
| `decide-publish.yml` | `workflow_run` from fit AND scrape-odds | Recommendations + JSONs |
| `republish.yml` | `workflow_dispatch` only | Re-run publish from existing extraction archive (lever for fast publisher iteration) |
| `healthcheck.yml` | cron 2×/day + dispatch | Read-only `pipeline_health()` → `data/health/status.json`; commits if changed; fails the run on `overall == FAIL` so GitHub's failure email fires (the alert channel). `test-healthcheck-ci-isolation.R` proves it never writes the ledger. |
| `world-cup.yml` | cron 1×/day + dispatch | HM 2026 forecast: download martj42 internationals → ingest → fit → simulate → publish `data/publish/world_cup/karla/*.json`. Self-contained (own ingest, no `workflow_run` parent). See note below. |

The `decide-publish` chain reading `workflow_run` from *both* parents
is what keeps JSON outputs fresh on every odds scrape, not just on the
daily fit cycle.

### `world-cup.yml` is single-job and self-contained — by necessity

Unlike the league pipeline (scrape → fit → decide-publish, chained via
`workflow_run` across separate workflows), the World Cup forecast runs
download → ingest → fit → forecast in **one job**. The reason is
`data/wc/fit/` being gitignored (`.gitignore`: ~900 MB Stan fit,
regenerable): the fit artefacts never reach git, so a downstream job
would have nothing to consume. `scripts/wc/{ingest,fit,forecast}.R` must
therefore execute in sequence in the same runner.

Two guards keep it cheap and self-terminating:

- **Facts-diff skip-guard.** `scripts/wc/ingest.R` rewrites the
  `country=world` facts Parquets from a fresh martj42 download; the
  workflow refits only if `git status` shows those Parquets changed
  (i.e. new in-window results landed). The season-partitioned store
  makes this precise — only the touched `season=YYYY` partition flips,
  and arrow re-serialises unchanged partitions byte-identically.
  `workflow_dispatch` with `force: true` bypasses it.
- **Tournament-window gate.** A tiny `gate` job no-ops the run past
  `WC_END_DATE` (2026-07-20) so the daily cron auto-quiesces after the
  final instead of refitting a finished bracket off later friendlies.

Downstream is `metill-is/metill-platform`'s hourly `pull-sports-data.yml`,
which already rsyncs the whole `data/publish/` tree (incl. `world_cup/`),
so no metill-platform change was needed — same ≤1h freshness as the
Icelandic leagues.

The placer (`R/placer-*.R`) is **never** referenced from any workflow.
Enforcement: `tests/testthat/test-placer-ci-isolation.R` greps every
`.github/workflows/*.yml` and fails the build if any line references
`R/placer-`, `place_bets`, `preview_bets`, `placer_pipeline`, or
`LENGJAN_*`.
