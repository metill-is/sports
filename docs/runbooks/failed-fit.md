# Runbook: failed or degrading fit

**Symptom.** `fit_freshness` FAIL/WARN (latest fit too old), or
`divergence_drift` / `rhat_drift` WARN (a fit is degrading while still under the
abort gate), or `diagnostics_store` WARN (the `fit_diagnostics` store cannot be
read). `fit.yml` may be red.

## Diagnose

1. `gh run list --repo metill-is/sports --workflow fit.yml --limit 8` — was the
   fit aborting? The Stan gate (`check_stan_diagnostics`) stops a fit on
   divergences > 1%, R-hat > 1.05, bulk/tail ESS < 100, treedepth saturation, or
   E-BFMI too low, and prints which one.
2. Read the persisted trend:

   ```r
   Rscript -e 'suppressMessages(devtools::load_all()); print(as.data.frame(
     sports::read_table("fit_diagnostics",
       filter = list(sport="football", country="iceland", sex="male"))))'
   ```

   A creep in `div_frac` toward 1% or `max_rhat` toward 1.05 (the WARN band) is
   the early signal the gate has not yet tripped on.
3. If `fit_freshness` is FAIL but the fit is *green*, the upstream results
   scrape may not be moving — check `scrape-results.yml`.
4. `diagnostics_store` WARN: drift is unknown, and the cell whose partition
   holds the bad file loses `needs_refit()`'s row-count rule (its read is
   filtered to that cell, so other cells keep it), so a game dated on that
   cell's fit date goes unfitted until a later one arrives. The value carries Arrow's error naming the bad file. The store
   is tracked: restore that file from the last commit that wrote it intact
   (`git log -- <path>`). Do not delete it to clear the WARN, because its
   `n_obs` is what the row-count rule compares with.

## Fix

- Divergences / funnel geometry: raise `adapt_delta`
  (`gh workflow run fit.yml --repo metill-is/sports -f force=true`, or set
  `SPORTS_FIT_ADAPT_DELTA=0.99`), or reparameterise the model — see the `stan`
  skill and `.claude/rules/stan-conventions.md`.
- Low ESS: raise `iter_sampling`.
- After a fix, force a refit: `gh workflow run fit.yml -f force=true`.

## Verify

`scripts/07_healthcheck.R` -> `fit_freshness` `OK` and the new
`fit_diagnostics` row is back inside the band.
