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
- **Voided / abandoned match**, or the result was simply never scraped.

## Fix

- If the federation result exists in `data/facts/results/` under the matching
  `(sport, country, sex, home_team, away_team)`, run `Rscript scripts/06_settle.R`
  (locally) — it joins and flips `settled`/`win`/`pnl` only.
- If a team-name rendering changed, add the mapping to
  `leagues.yml::*.lengjan.team_names[[sex]]` (load-time injectivity guard
  enforces it) so the result joins.
- A genuinely voided bet is settled **locally** through the settle path
  (`win`/`pnl` per the void rule), never by hand-editing `match_date`.

## Verify

`scripts/06_settle.R` then `scripts/07_healthcheck.R` -> `orphaned_bets` `OK`.
