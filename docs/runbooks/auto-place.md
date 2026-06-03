# Runbook: unattended auto-placement

## Enable / disable
- Enable: `bash tools/install-autoplace.sh install`
- Disable now (no unload): `touch data/AUTO_PLACE_DISABLED`
- Remove the agent: `bash tools/install-autoplace.sh uninstall`

## Health says placement_health WARN/FAIL
1. `Rscript -e 'print(sports::read_placement_status("data"))'` -- last run.
2. `failed:*` -> read `~/Library/Logs/sports-autoplace.log`; common causes:
   Lengjan login (`LENGJAN_*` in `.Renviron`), Chromote launch, parser
   disagreement (Lengjan UI change -> see `.claude/rules/sports-betting.md`).
3. `sync_failed` -> resolve the git state manually (`/sync-main`), then let the
   next cycle run.
4. Stale (no recent healthy run) while pending -> check the Mac was awake in the
   daytime window and the agent is loaded (`launchctl print ...`).

## Confirm it is not on CI
`Rscript -e 'devtools::test_file("tests/testthat/test-placer-ci-isolation.R")'`

## Known limitation: CI alert lag
`data/health/placement_status.json` is written locally each run but is NOT
auto-committed by `scripts/auto_place.R`. So `/pipeline-doctor` and the
SessionStart banner reflect it immediately (they read the local file), but the
twice-daily `healthcheck.yml` -- and its failure-email alert -- only see it once
it's committed (e.g. by the next fit/decide cron that touches `data/health/`).
A `failed:*` that self-resolves between healthchecks can therefore be missed by
the email path. Auto-committing the status file from `auto_place.R` is a noted
follow-up; the local surfaces are the timely ones.
