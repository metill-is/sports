# Runbook: stale odds

**Symptom.** `pipeline_health()` reports `odds_freshness` FAIL (latest Lengjan
odds older than 24h) for football_iceland, and/or fly.metill.is shows stale
recommendations.

## Diagnose

1. **Check the seasonal pause first.** Only `football_iceland` is active right
   now; `basketball_iceland` / `handball_iceland` are off-season until autumn
   2026 and show `PAUSED`, which is correct, not a fault. If the only stale cell
   is a paused sport, stop here.
2. Did the odds scrape run and fail? `gh run list --repo metill-is/sports --workflow scrape-odds.yml --limit 6`.
   Since 2026-05-30 the scrape **fails loudly** (red workflow -> failure email)
   when in-season leagues yield 0 odds rows, so a recent red run is the signal.
3. Inspect the failed run's logs for timeouts ("could not parse", chromote
   errors) vs a Lengjan markup change.

## Fix

- Transient (timeout / Lengjan briefly down): re-run
  `gh workflow run scrape-odds.yml --repo metill-is/sports`.
- Lengjan markup changed: the DOM odds parser
  (`parse_actual_odds_from_dom()` in `R/placer-place.R`) or
  `R/ingest-lengjan-odds.R` selectors need updating; fixtures live in
  `tests/testthat/test-placer-place.R`. Local-only — never wire the placer into CI.

## Verify

`Rscript scripts/07_healthcheck.R` -> `odds_freshness` is `OK`. The next
`decide-publish.yml` run republishes fresh JSONs.
