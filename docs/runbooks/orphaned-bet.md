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
  touch a match that was never played. Record the void with a manual,
  L4-respecting flip of **only** `settled` / `win` / `pnl` (a void is a stake
  refund, not a loss → `pnl = 0`; never edit `match_date` or any frozen field).
  `write_table` is partition-replace, so pass **every** row of the
  `(sport, country)` partition — not just the voided one — or the rest are
  wiped:

  ```r
  Rscript -e 'suppressMessages(devtools::load_all()); led <- read_table("ledger")
    key <- led$sport=="football" & led$country=="iceland" & led$sex=="male" &
      led$market=="moneyline" & grepl("Gr.tta", led$home_team) &
      grepl("Grindav", led$away_team) & led$match_date==as.Date("2026-06-06") &
      (is.na(led$settled) | !led$settled)
    stopifnot(sum(key) == 1L); i <- which(key)
    led$settled[i] <- TRUE; led$win[i] <- NA; led$pnl[i] <- 0
    write_table(led[led$sport=="football" & led$country=="iceland", ], "ledger")
    commit_ledger_changes(here::here(), "data(ledger): void <fixture> -- <reason>")'
  ```
  Then `git push origin main` — nothing auto-pushes the ledger.

## Verify

`scripts/07_healthcheck.R` (or `pipeline_health()`) -> `orphaned_bets` `OK`.
For a result-based settle, run `scripts/06_settle.R` first; a manual void
already flips the row, so only the healthcheck is needed.
