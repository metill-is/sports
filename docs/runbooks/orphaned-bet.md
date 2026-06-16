# Runbook: orphaned bet

**Symptom.** `orphaned_bets` WARN — an unsettled ledger row whose `match_date`
is more than 10 days in the past. (`bankroll` FAIL is the related, more severe
case: realised PnL ran `current_pool` to/under zero.)

## NEVER do this

**Do not edit the ledger row's `match_date` (or any frozen field).**
`data/decisions/ledger/` is the canonical money record: L3 freezes bet
parameters at write time; L4 allows settlement to change only
`settled` / `win` / `pnl`. Mutating `match_date` to "make it settle" corrupts
the record. Reference incident: commit 121710d (ledger rows lost in a git reset).

## Diagnose

```r
Rscript -e 'suppressMessages(devtools::load_all()); led <- sports::read_table("ledger");
  print(led[!is.na(led$settled) & !led$settled, c("match_date","sport","sex","home_team","away_team","market","bet_amount")])'
```

Why it did not settle:
- **Team-name drift** — the federation result was scraped under a different
  rendering than the bet's frozen team name.
- **Rescheduled fixture** — the settle layer's reschedule window
  (`match_date_window_days = 3`) auto-settles small shifts; a larger gap leaves
  it orphaned.
- **Postponed fixture** — the federation lists the match but records no score
  (KSÍ shows `LEIK FRESTAÐ`, no replay date). Confirm against KSÍ live for the
  league's 2026 competition id (`KSI_IDS` in `R/ingest-ksi-football.R`) with
  `fetch_ksi_page(id, toggle = "results")` + `extract_ksi_matches()`. If it is
  replayed later the bet should settle against the new result (the gap usually
  exceeds the 3-day window, so a manual settle or a wider `match_date_window_days`
  is needed); if Lengjan voids it for non-completion, treat it as a void (below).
- **Voided / abandoned match**, or the result was simply never scraped.

## Fix

- If the federation result exists in `data/facts/results/` under the matching
  `(sport, country, sex, home_team, away_team)`, run `Rscript scripts/06_settle.R`
  (locally) — it joins and flips `settled`/`win`/`pnl` only.
- If a team-name rendering changed, add the mapping to
  `leagues.yml::*.lengjan.team_names[[sex]]` (load-time injectivity guard
  enforces it) so the result joins.
- **A genuinely voided bet** (e.g. a postponed fixture Lengjan refunded —
  confirm the void/refund in your Lengjan bet history) has **no settle-path
  resolution**: `compute_settlement()` only resolves rows that join to a
  scraped result, so `settle_ledger()` (and `scripts/06_settle.R`) will never
  touch a match that was never played. Record the void with `void_bet()`
  (`R/settle.R`), which flips **only** `settled` / `win` / `pnl` to
  `TRUE` / `NA` / `0` (a void is a stake refund, not a loss → `pnl = 0`,
  and `win = NA` so it never pollutes the calibration win-rate), leaves every
  frozen field alone, and re-writes the whole `(sport, country)` partition for
  you (`write_table` is partition-replace — `void_bet` handles that internally,
  so you never hand-edit the tibble). It `stop()`s rather than guess if the key
  matches zero rows, more than one row, or an already-settled row (L4).

  Pass the **exact** frozen team-name renderings printed by the Diagnose step
  above (`void_bet` matches with `==`, not a fuzzy grep):

  ```r
  Rscript -e 'suppressMessages(devtools::load_all())
    void_bet(
      sport = "football", country = "iceland", sex = "male",
      match_date = as.Date("2026-06-06"),
      home_team = "Grótta", away_team = "Grindavík",
      market = "moneyline"
    )
    commit_ledger_changes(here::here(), "data(ledger): void <fixture> -- <reason>")'
  ```
  If the Diagnose step shows more than one unsettled bet on that fixture+market
  (e.g. two `total` lines), add `line = ...`, `outcome = ...`, or
  `placed_at = ...` to pick exactly one — `void_bet` aborts on an ambiguous key.
  Then `git push origin main` — nothing auto-pushes the ledger.

## Verify

`scripts/07_healthcheck.R` (or `pipeline_health()`) -> `orphaned_bets` `OK`.
For a result-based settle, run `scripts/06_settle.R` first; a manual void
already flips the row, so only the healthcheck is needed.
