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
| `scrape-odds.yml` | cron 4×/day (08,11,14,20 UTC) | Lengjan odds snapshot |
| `fit.yml` | `workflow_run` from scrape-results | Stan fit |
| `decide-publish.yml` | `workflow_run` from fit AND scrape-odds | Recommendations + JSONs |
| `republish.yml` | `workflow_dispatch` only | Re-run publish from existing extraction archive (lever for fast publisher iteration) |
| `healthcheck.yml` | cron 2×/day + dispatch | Read-only `pipeline_health()` → `data/health/status.json`; commits if changed; fails the run on `overall == FAIL` so GitHub's failure email fires (the alert channel). `test-healthcheck-ci-isolation.R` proves it never writes the ledger. |
| `world-cup.yml` | **dispatch only** (cron retired 2026-09-02) | HM 2026 forecast: download martj42 internationals → ingest → fit → simulate → publish `data/publish/world_cup/karla/*.json`. Self-contained (own ingest, no `workflow_run` parent). A cheap SHA pre-gate (`git ls-remote` martj42's tip vs the tracked `data/wc/martj42_pointer.txt`, read over raw.github — no clone of this ~11 GB repo) no-ops every poll where martj42 hasn't committed; the facts-diff is the precise second gate. See note below. |
| `discover-leagues.yml` | cron 2×/week (Mon+Thu) + dispatch | Read-only Lengjan competition-dropdown discovery → `data/discovery/proposals.json`; commits if changed. Surfaces via the `discovery` health WARN. References no placer token. |

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

**The cron was retired 2026-09-02** — the workflow is now `workflow_dispatch`
only. It had been hourly (`17 * * * *`) as deliberate WC26-era cadence, because
during the live tournament martj42 posts results across the whole UTC day (2026
WC games kick off in North-American afternoons/evenings → late-UTC commits).
Once the final was played on 2026-07-19 the tournament-window gate below made
every subsequent firing a **provably dead branch**: ~720 no-op runs a month, each
still doing a courtesy `git ls-remote` against martj42 for a result it could not
act on. Last real output was 2026-07-19T23:57Z.

To re-arm for the next cycle: bump `WC_END_DATE` and restore the `schedule:` key
with `- cron: '17 * * * *'`.

Three guards keep it cheap and self-terminating:

- **SHA pre-gate.** The `gate` job reads martj42/international_results'
  `master` tip via `git ls-remote` (no clone) and the SHA it last acted
  on from the tracked pointer `data/wc/martj42_pointer.txt` (over
  raw.github, since this repo is public — a full checkout of this ~11 GB
  repo would defeat the point). If they match, the entire forecast job is
  skipped, so a poll is a sub-minute no-op whenever
  martj42 hasn't committed — it never pays R/CmdStan setup. The forecast
  job advances the pointer on **every** proceeding run (even a no-refit
  one), so a stale pointer can't make later polls re-ingest forever. A
  failed pointer read fails *safe* (proceeds); `git ls-remote` failure
  retries 3× then errors loudly. `force: true` bypasses it.
- **Facts-diff skip-guard.** `scripts/wc/ingest.R` rewrites the
  `country=world` facts Parquets from a fresh martj42 download; the
  workflow refits only if `git status` shows those Parquets changed
  (i.e. new in-window results landed). This is the *precise* gate — the
  SHA pre-gate only filters polls where martj42 didn't commit at all, so a
  real-but-WC-irrelevant martj42 commit passes the SHA gate, ingests, and
  no-ops here. The season-partitioned store makes this precise — only the
  touched `season=YYYY` partition flips, and arrow re-serialises unchanged
  partitions byte-identically. `workflow_dispatch` with `force: true`
  bypasses both gates.
- **Tournament-window gate.** The same `gate` job no-ops the run past
  `WC_END_DATE` (2026-07-20) so the run auto-quiesces after the final
  instead of refitting a finished bracket off later friendlies. This gate is
  what made the retired cron provably dead rather than merely idle.

Downstream is `metill-is/metill-platform`'s `pull-sports-data.yml` (7×/day
since 2026-09-02, clustered 07–12 + 19 UTC), which already rsyncs the whole
`data/publish/` tree (incl. `world_cup/`), so no metill-platform change was
needed — freshness now ≤~4h in-window rather than ≤1h.

The placer (`R/placer-*.R`) is **never** referenced from any workflow.
Enforcement: `tests/testthat/test-placer-ci-isolation.R` greps every
`.github/workflows/*.yml` and fails the build if any line references
`R/placer-`, `place_bets`, `preview_bets`, `placer_pipeline`, or
`LENGJAN_*`.

## Pushing to main: always via `.github/scripts/push-with-retry.sh`

Eight workflows commit generated data to main (fit, decide-publish,
scrape-odds, scrape-results, healthcheck, discover-leagues, republish,
world-cup). Each declares its **own** `concurrency` group, which serialises
a workflow against itself but does nothing across workflows — so several are
routinely in flight at once. On 2026-08-25 a stan fit pushed at 08:39, an
odds scrape at 08:40 and a decide+publish at 08:44.

**Do not "fix" this with a shared concurrency group.** That is what
metill-platform does (`group: main-data-push`), and it works there because
every data cron finishes in minutes. Here `fit` has a *median* runtime of
~2 h (max ~4 h), so a shared group would queue a 4-minute healthcheck behind
a 4-hour fit — and because GitHub keeps only one *pending* run per group,
the scheduled runs behind it would be silently cancelled. The race is a
seconds-long window at the end of a long job; serialising whole jobs to
protect it is the wrong shape.

The historic `git pull --rebase origin main && git push` had no retry and
lost two ways:

1. **Ref-lock race** — the rebase succeeds, then origin advances in the
   milliseconds before the push:
   `! [remote rejected] main -> main (cannot lock ref 'refs/heads/main': is
   at d2f50f8 but expected 4f9776b)`. This killed a ~2 h stan fit
   (run 32698702043, 2026-08-24).
2. **Content conflict** — two writers regenerate the same paths and the
   rebase stops with conflicts (97 in run 32827816691 on 2026-08-25; same
   signature on 2026-08-10 and 2026-08-18).

Both discard the entire run's output even though the work itself was fine.

```yaml
git commit -m "data: ..."
.github/scripts/push-with-retry.sh                 # disjoint paths
.github/scripts/push-with-retry.sh --prefer-ours   # regenerated output
```

**Which flag.** Plain retry is the default: it fixes the ref-lock race, and
a genuine content conflict fails loudly because for a disjoint-path job that
means something unexpected happened. `--prefer-ours` (which passes
`-X theirs` — correct but backwards-reading, since rebase replays *our*
commit, so "theirs" is our side) is only for jobs whose every written path
is fully regenerated from the freshest inputs: `decide-publish`, `republish`
and `world-cup`, all of which rewrite `data/publish/` wholesale.

**Caveat before adding a new `--prefer-ours` caller.** A file that
*accumulates* rather than being recomputed can lose a sibling's appended
record for the one round where two runs overlapped —
`data/beliefs/round_predictions_history/**/round_predictions_history.json`
is an accumulator (see the roxygen on `publish_football_iceland()`), as is
the WC `prediction_log.json`. That is a strictly smaller loss than the old
behaviour, which threw away the whole run, and the next run re-accumulates.

**Enforcement:** `tests/testthat/test-workflow-push-retry.R` fails the build
if any workflow that creates a commit does not route its push through the
script, if a bare `git push` / `git pull --rebase` reappears, or if
`--prefer-ours` is added to a workflow outside the reviewed set.
