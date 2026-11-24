# Runbook: moving a league's Lengjan odds from the DOM scraper to the JSON API

Football still scrapes Lengjan with the Chromote DOM scraper
(`R/ingest-lengjan-odds.R`); handball and basketball use the JSON API
(`R/lengjan-api.R`, `lengjan.source: api`). This runbook covers the shadow
comparison that has to pass before football switches (spec
`docs/superpowers/specs/2026-09-23-hb-bb-lengjan-odds-staged-betting-design.md`,
WS3), the proposed judging rule, and the cutover itself. Results so far:
`docs/reports/2026-11-24-football-api-shadow.md`.

## The shadow comparison

```bash
# configured league (in season): every configured competition
Rscript scripts/0Nc_compare_odds_sources.R --league football_iceland --out <dir>
# throwaway league (off season): one Lengjan competition id
Rscript scripts/0Nc_compare_odds_sources.R --competition 296 --country england --max-detail 10 --out <dir>
```

- **Local only.** It stops when `CI` or `GITHUB_ACTIONS` is set, and no workflow
  may name it (`tests/testthat/test-lengjan-compare.R`).
- **Writes nothing to `data/`.** `--out` takes a report folder anywhere else
  (an RDS with every table, plus summary and cause CSVs).
- **Order:** API snapshot A, then the DOM scrape, then API snapshot B. A
  reuses one `current-program` for both snapshots, so a run makes one program
  request, two rounds of `markets` requests (20 events each) and one browser
  page per competition plus one per detail page opened.
- **Key:** `(match_date, home_team, away_team, market, outcome, line)`, names as
  each scraper writes them (no `team_names` mapping), lines to two decimals,
  prices to the cent.
- Run it with `ml run --`: it drives Chrome.

## Proposed judging rule (for the owner to approve)

The spec's criterion, "every DOM row has an identical API row", is not
reachable with live prices: the DOM run takes minutes and prices can move in
that time. The proposed rule:

1. **A DOM row passes** when an API row with the same key has the same price in
   snapshot A or snapshot B.
2. **A run passes** when every DOM row passes, after the one known
   systematic difference, stray whitespace in names, is fixed in the API
   parser (see below).
3. **API-only rows are allowed** and listed with a cause. Expected causes:
   `extra_line_on_api` (the API lists handicap or total lines the DOM detail
   page does not render), `changed_during_run`, `dom_detail_not_visited`
   (beyond `--max-detail`), `dom_detail_failed`. Any `match_not_on_dom`,
   `market_not_on_dom` or `outcome_not_on_dom` row is read by hand.
4. **Cutover** needs at least 3 passing runs on at least 2 matchdays of
   Icelandic football, in configured competitions (the Lengjubikar, the cup or
   Besta deild round 1), with a clean team-name audit.

Failure causes, in the order tested: `dom_detail_mispaired`, `odds_mismatch`,
`suspended_on_api`, `line_not_on_api`, `market_not_on_api`, `date_differs`,
`name_whitespace`, `match_not_on_api` (definitions in
`?compare_odds_sources`).

## The team-name audit

```bash
Rscript scripts/0Nc_audit_team_names.R --league football_iceland [--live] --out <dir>
```

It checks every participant name of an event in a configured competition (the
API test fixture, a live program with `--live`, and archived programs when a
local archive copy is configured; see `docs/runbooks/odds-archive.md`)
against the competition's sex in `team_names`, checks every name the DOM
scraper wrote against both sexes, lists configured renderings no source has
shown, and lists every API participant name with stray whitespace.

## The cutover change set

Prepared, not applied: branch `claude/football-lengjan-api-cutover`. It
carries:

- `source: api` under `football_iceland.lengjan` in `config/leagues.yml`;
- Chrome (`browser-actions/setup-chrome`, `CHROMOTE_CHROME`) and the
  `chromote` extra package out of `scrape-odds.yml`;
- squished participant names in `.lengjan_event_row()`, so API names key as
  the DOM shows them (handball's `Víkingur` mapping already lists both
  `Víkingur` and the stored `"Víkingur "`, so no config change is needed);
- `test-betting-interlock.R` pinning football to `source: api`.

`R/ingest-lengjan-odds.R` stays for one release as the fallback. **Rolling
back** means reverting the whole cutover commit, not only the config line:
with Chrome gone from `scrape-odds.yml`, `source: dom` cannot run on CI.
