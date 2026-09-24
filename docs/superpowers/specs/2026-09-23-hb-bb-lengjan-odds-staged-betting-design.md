# Icelandic handball + basketball: Lengjan odds via the JSON API, and a staged betting ladder

**Status:** design drafted 2026-09-23, pending user review of this document.
**Partly supersedes:** D2 ("publish only, no bets this season") of
`docs/superpowers/specs/2026-09-02-basketball-handball-metill-parity-design.md`. For handball, D2 is
lifted in stages (below). For basketball, D2 still holds for *betting*, but odds scraping resumes.

## Scope and fixed decisions

The user took these on 2026-09-23. They are requirements, not options:

- **U1: basketball is scraped now and bet on later.** Lengjan settles basketball 1X2 *and* totals on
  regulation time (L6), but the model and the results table use final scores including overtime
  (C7). Basketball odds are scraped from the season start, so their history (which cannot be
  recovered later) accrues. Betting waits for Phase C (regulation-time scores and pricing), which gets
  its own spec.
- **U2: handball rolls out in stages.** scrape → paper recommendations → manual placement →
  autoplace. Each step is a config change, and the user decides each promotion.
- **U3: sizing is decided after the paper phase.** Handball `kelly_frac` stays at 0.05 until then.
  At 0.05 with a 14,909 kr pool, a stake reaches the 200 kr minimum only when `kelly_raw·λ ≥ 0.27`,
  so few or no live bets would clear. The paper phase therefore evaluates candidates *before* the
  `min_bet` filter (WS8).

One more decision is taken in this design, and the user may override it:

- **U4: one API scraper for all three sports.** It replaces the Chromote DOM scraper. Handball and
  basketball go straight to the API. Football cuts over only after a shadow comparison (WS3), because
  it is the live operation.

## Verified findings

Every row was confirmed on 2026-09-23 against the live site (L\*) or by reading source (C\*).

| id | finding | evidence |
|----|---------|----------|
| L1 | **Lengjan has a public, unauthenticated JSON API.** `GET https://games.lotto.is/api/proxy/lengjan/current-program` returns every listed event (753 in `events`, 186 in `liveSoon`; about 1.05 MB in 0.3 s). `GET /api/proxy/lengjan/markets?eventIds[0]=<id>&…&live=false` returns all markets per event. Both answer plain `curl` with HTTP 200. | Found via `performance.getEntriesByType('resource')` in the browser pane, then reproduced with `curl`. |
| L2 | **`markets` takes at most 21 ids per call.** Batches of 5, 10, 15 and 20 return 200. A batch of 25 returns 400 `ZodError: eventIds Expected array, received object`: the server's `qs` parser stops treating the parameter as an array past index 20. | Bisected with `urllib`. |
| L3 | **Odds are integers ×100.** A value of `139` means 1.39. Each selection is `{nr, bet, name, odds, teamId}`. Participants carry `homeOrAway` 1 or 2 and `name`. `datePlayed` is an ISO UTC timestamp. | Program and markets JSON. |
| L4 | **Icelandic events have `countryName: "IS"` and an empty `countryCode`.** `filters.countries` is keyed by `countryCode`, so Iceland is missing from the site's country dropdown for every sport. The data is still present. | Program JSON. The handball dropdown lists only DE, FR, DK and ES. |
| L5 | **Which markets each sport offers** (all current events, fetched in batches of ≤20). Handball, 29 events: `3WAY` "Úrslit leiksins" 29, `DC` "Tveir möguleikar" 27, `OU_FT` "Yfir/undir" 14, half-time markets 14. Basketball, 27 events: `3WAY` "Úrslit leiksins" 27 (draw priced around 12), `OU_FT` 26. Football, 60 events: `3WAY` "Úrslit" 58, `OU_FT` 57, `HC_FT` "Forgjöf" 57. **Neither handball nor basketball has a handicap market.** | Batched `markets` survey. This matches legacy, which never captured a HB or BB handicap row. |
| L6 | **Basketball settles on regulation time.** The *Spilareglur Lengjunnar* page says: "Körfuknattleikur: Úrslit leikja miðast við venjulegan leiktíma. Framlenging gildir ekki." Its market-code table separates `OU_FT` from `OU_OT` ("Yfir/Undir – með framlengingu"), so `OU_FT` totals exclude overtime too. The general rule is regulation time "nema annað sé tekið fram" (unless stated otherwise). | `_next/data/…/reglur-og-leidbeiningar/spilareglur-lengjunnar.json`. |
| L7 | **Handball comp `1269` "Olísdeild karla" is live.** FH – Haukar (event `4602104`, 2026-09-24T19:30Z) still had `marketCount: 0` at about 16:30Z on 2026-09-23. Icelandic events with no open market appear only in `liveSoon`, not in `events`. No women's handball and no Grill 66 competition was listed. | Program JSON. |
| L8 | **The placer's DOM labels already fit handball.** The section leaves read `Úrslit leiksins`, `Yfir/Undir`, `Úrslit  - Fyrri hálfleikur` and so on. Totals rows render `th` as `56.5`, which matches `as.character(56.5)`. | Handball match `4616742` in the browser pane. |
| L9 | **The HC line maps one-to-one.** `HC_FT` "Forgjöf 1-0" has `specialValue: "1"` and `params: "1-0"`; "0-1" has `"-1"`. `parse_handicap("1-0") = 1` (`R/decide-odds.R:11-20`). | Markets JSON plus source. |
| C1 | Discovery only visits leagues that already have Lengjan competitions (`has_lengjan = TRUE`), so it cannot see HB or BB. | `R/discover-lengjan.R:254` |
| C2 | Odds ingest refuses any league whose betting is disabled (decision D2), so a scrape-only state cannot be expressed today. | `R/ingest.R:398` |
| C3 | Autoplace calls `place_fn(...)` without a `leagues` filter. Enabling betting on a league therefore puts it straight into unattended placement, and the daily cap is shared across sports. | `R/auto-place.R:216`, `R/auto-place.R:84-96` |
| C4 | The odds schema has no `sex`, event id, competition id or kickoff column. The upsert natural key is `(sport, country, scraped_at, match_date, home_team, away_team, market, outcome, line)`. | `R/storage-schemas.R:41-52`, `R/storage.R:237-240` |
| C5 | The DOM scraper pairs each detail page with its match by position (`comp_rows[1+(i-1)*3]`), so one skipped match mislabels every later page. | `R/ingest-lengjan-odds.R:299-300` |
| C6 | Decide contains no sport-specific code. Beliefs have the same shape in all three sports: 4,000 continuous score draws per match. The only per-sport input is `scoring.tie_threshold`. | `R/decide-kelly.R`, `R/decide-pipeline.R:200` |
| C7 | Settle grades on final scores. The KKÍ ingest has no quarter or overtime data (grepping for `overtime\|framleng\|leikhlut\|quarter\|period` finds nothing). | `R/settle.R:104-141`, `R/ingest-kki-basketball.R` |
| C8 | `capture_rate` is one global row computed over every recommendation. | `R/health.R:381` |
| C9 | `test-betting-interlock.R` pins the D2 state: HB and BB disabled with zero competitions. | `tests/testthat/test-betting-interlock.R:26-39` |
| C10 | Chrome is set up in `scrape-odds.yml`, `discover-leagues.yml` and `scrape-results.yml`. Results scraping (KSÍ/HSÍ/KKÍ) is out of scope here. | `.github/workflows/*.yml` |
| C11 | Handball `team_names.male` has 9 entries, but Olísdeild karla has 12 teams. "Haukar", which the API shows, is unmapped and would pass through unchanged. | `config/leagues.yml` handball block |

## Architecture

### WS1: the betting ladder (`betting.mode`)

A single ordinal replaces the boolean `betting.enabled`:

```
off < scrape < paper < manual < auto
```

| layer | runs when `mode` is at least | today's gate |
|---|---|---|
| odds scrape (`ingest_one_lengjan`) | `scrape` | `betting_enabled()` |
| decide (`decide_league`), which writes candidates and recommendations | `paper` | `betting_enabled()` |
| placer loader and pre-flight (`place_bets.R`, `preview_bets.R`) | `manual` | `betting_enabled()` |
| autoplace (`run_auto_place` passes a league filter to `place_fn`) | `auto` | none (C3) |
| `capture_rate` health (which recommendations count) | `manual` | all |
| `odds_freshness` can FAIL | `manual` (a lower mode is capped at WARN) | enabled only |

- **New predicates** in `R/config.R`: `betting_mode(league)` and `betting_mode_at_least(league, stage)`.
  Every call site states the stage it needs, so no call site keeps an implicit meaning.
- **Backward compatibility:**
  - An absent `mode` with no `enabled` key means `auto`, so football is unchanged.
  - `enabled: false` without `mode` means `off`.
  - The schema rejects a league that sets both keys.
  - `betting_enabled()` stays as a documented alias for `betting_mode_at_least(league, "paper")`, so
    existing readers keep their meaning.
- **Paper recommendations need no schema change.** The placer drops every league below `manual` at
  load time. `/bet` labels rows from leagues below `manual` as `paper`, working that out from the
  config.
- **The D2 interlock survives in spirit.** A league at `off` still does nothing at any layer, and
  every layer's gate is tested in one table-driven test (a mode × layer grid).

### WS2: the Lengjan JSON API client (`R/lengjan-api.R`)

- `lengjan_program()` fetches `current-program` and returns one tibble of events from the union of
  `events`, `liveSoon` and `popular`, de-duplicated on `id`. Columns:
  - `event_id`, `sport_id`, `competition_id`, `competition_name`, `country_name`
  - `kickoff_at` (POSIXct UTC), `home_team`, `away_team` (from `participants[].homeOrAway`)
  - `market_count`
- `lengjan_markets(event_ids)` fetches in batches of **≤20** (L2) and returns long rows:
  - `event_id`, `group`, `type`, `type_name`, `market_name`, `special_value`, `primary`, `status`
  - `selection`, `odds` (already divided by 100)
- **Transport:** `httr2` with a user agent, a timeout and `req_retry` with backoff. At most about 1
  request per second (a run makes about 3–6 requests). A fetch failure raises the existing
  `lengjan_fetch_error` class, so the soft-fail semantics of `ingest_one_lengjan` stay as they are.
- **Mapping to canonical odds rows.** Only markets with `status == "open"` are used; everything else
  is ignored.

  | Lengjan market | canonical `market` | `outcome` | `line` |
  |---|---|---|---|
  | the event's full-time `3WAY` (`type == "1"`, the primary market) | `moneyline` | `1/X/2` → `home/draw/away` | `NA` |
  | `OU_FT` group, `type_name == "OU"` | `total` | `Yfir/Undir` → `over/under` | `as.numeric(special_value)` |
  | `HC_FT` group, `type_name == "HC"` | `spread` | `1/X/2` → `home/draw/away` | `as.numeric(special_value)` (L9) |

  Half-time markets, double chance (`DC`), both-teams-to-score and the rest are ignored for now.
  Handball `DC` is a possible later market, since it can be derived from the same draws.
- **Choosing events.** An event is scraped when its `competition_id` is among the league's configured
  `lengjan.competitions`. The row's `sex` comes from that competition entry, and `match_date` is
  `as.Date(kickoff_at)` (Iceland is UTC all year).
- **Schema evolution** (the same pattern as `kickoff_time` on schedules):
  - `odds` gains nullable `sex`, `event_id`, `competition_id` and `kickoff_at`.
  - `sex` joins the upsert natural key, so a men's and a women's fixture between the same two clubs
    on the same day cannot collide.
  - Historical rows keep `NA` in the new columns, and `read_table` already unifies schemas.
- **Decide change:** `prepare_odds` filters on `sex` when the column is present and not `NA`. Rows
  with `NA` pass through as they do today, which keeps the backtest and walk-forward readers working.

### WS3: football cutover with a shadow comparison

- A per-league `lengjan.source: api | dom`, defaulting to `dom`. Handball and basketball are `api`
  from day one. Football stays `dom` until cutover.
- `scripts/0Nc_compare_odds_sources.R` runs both scrapers in memory for football at the same moment
  and writes nothing. It reports rows missing on either side, odds mismatches and line mismatches.
  It runs locally, not on CI.
- **Cutover criterion:** at least 3 comparison runs across at least 2 matchdays, where every DOM row
  has an identical API row. After that:
  - football flips to `source: api`, a one-line change that can be reverted;
  - Chrome and `chromote` are removed from `scrape-odds.yml`.
- The DOM parser is deleted one release later, once the API has run cleanly. It stays until then as
  the fallback `source: dom`.

### WS4: discovery via the API

- `discover_new_competitions()` visits every active league at mode `scrape` or above, whether or not
  it has competitions yet (fixes C1).
- It lists competitions from `lengjan_program()` where `sport_id` matches and `country_name == "IS"`.
  Team-name proposals come from `participants`. There is no Chromote session.
- `classify_competition()` gets sport-aware patterns:
  - handball: `olís` → OD, `grill ?66` → G66
  - basketball: `bónus` → BD, `1\. ?deild` → 1D
  - sex: `kvenna | kv\.` → female, `karla | ka\.` → male
- Chrome is removed from `discover-leagues.yml`.

### WS5: config

- **handball_iceland**
  - `lengjan.source: api`
  - `competitions: [{ id: "1269", name: "Olísdeild karla", sex: male }]` (L7)
  - `team_names.male` re-checked against API participant names for all 12 OD teams (C11)
  - `betting.mode: paper`
  - `markets.spread: false`, because the market doesn't exist (L5). This makes the absence explicit.
- **basketball_iceland**
  - `lengjan.source: api`, `betting.mode: scrape`
  - Competitions are wired once discovery finds the 2026-27 Bónus deild comps. They are not listed
    yet; the season starts 29 Sep / 1 Oct. The `/wire-league` flow is unchanged.
- **Schema:** `betting.mode` enum, `lengjan.source` enum, and `enabled` kept as deprecated and
  mutually exclusive with `mode`.
- **`test-betting-interlock.R`:** the D2 assertions (C9) are replaced with the new pinned state:
  football `auto`, handball `paper`, basketball `scrape`.

### WS6: the placer (needed before stage 2)

- **Direct match URLs.** When a recommendation's latest odds row carries an `event_id`, the placer
  builds `match_url(event_id, sport_id)` directly and skips scraping the listing page and matching
  names. The name-matching path stays as the fallback for rows without an `event_id` (football before
  cutover).
- **Sex-keyed fallback map.** The fallback match-id map in `resolve_match_ids_new` is keyed by sex.
- **Stage-2 check:** before stage 2, a dry run of `place_bets.R --league handball_iceland` against a
  live handball match confirms the 1X2 and totals click path (the labels already match, L8).
- **Unchanged:** the P1–P4 rules and the 200 kr P3 literal (Lengjan's minimum stake).

### WS7: health

- `capture_rate` counts only recommendations from leagues at mode `manual` or above, so paper
  recommendations can never read as missed captures.
- `odds_freshness`:
  - covers leagues at mode `scrape` or above;
  - caps severity at WARN below `manual`;
  - limits its expectations to (sex, division) pairs that have a configured competition. To support
    this, competition entries gain an optional `division` field, so Grill 66 and women's fixtures
    that Lengjan never prices do not raise false WARNs.

### WS8: paper report (the gate from stage 1 to stage 2)

- `scripts/0Np_paper_report.R` is read-only and local.
- **Input:** for a paper league, the candidates store rows with `stage ∈ {kept, dropped_min_bet}`,
  i.e. positive EV at the model `p` before sizing (U3).
- **Two metrics:**
  - hypothetical flat-stake PnL, graded with the settle outcome logic;
  - CLV = `log(odds_at_decision / closing_odds)`, where the closing price is the last scrape before
    `kickoff_at` for the same `(event_id, market, outcome, line)`.
- **Output:** median CLV and PnL with bootstrap CIs, per market.
- **No automatic promotion.** The user reads the report and decides stage 2, and sizing at the same
  time (U3).

### WS9: scrape cadence

- A job that used to spend minutes on Chrome setup and page loads now makes a handful of HTTP
  requests. Its runtime becomes dominated by R setup.
- The cron moves to an offset minute (`17` instead of `00`, the most congested minute) and gains a
  late-afternoon slot for closing lines. The new schedule is
  `17 8,11,14,17,20 * * *`.
- Once `event_id` and `kickoff_at` exist, the odds table itself records when each market first
  appears (the first `scraped_at` per `event_id`). One short query then shows whether Icelandic
  handball odds post on matchday (L7 suggests they do). The cadence is re-tuned from that evidence,
  not guessed.

## Phase C (basketball betting): its own spec, outline only

1. Ingest regulation-time scores from KKÍ, as quarter scores or a `went_to_ot` flag.
2. Price the regulation-time 1X2 and totals. Two options: model regulation scores directly, or
   derive them from final-score draws with an overtime adjustment and a ±0.5 regulation-tie band.
3. Settle basketball bets on regulation scores.
4. Only then move basketball through `paper → manual → auto` as for handball.

## Testing

- **API client:** fixtures captured on 2026-09-23 under `tests/testthat/fixtures/lengjan-api/`. They
  contain a program subset plus markets for one handball, one basketball and one football event.
  Tests cover the parse, the ×100 conversion, the market mapping, the `special_value` line, the
  batch size of 20 and `status` filtering. No test touches the network. Fixture dates are held
  constant via a fixed `now` (the time-bomb fixture gotcha).
- **Ladder:** one table-driven test over the mode × layer grid (WS1).
- **Decide:** a `prepare_odds` sex-filter test with a men's and a women's fixture between the same
  two clubs on the same date.
- **Storage:** an upsert with the new natural key, plus reading a partition that mixes old rows
  (without `sex`) and new rows.
- **Health:** `capture_rate` excludes paper leagues. `odds_freshness` is capped at WARN below
  `manual` and scoped by `division`.
- **Existing guards:** `test-placer-ci-isolation.R` still holds, since the placer is untouched on CI.
  `test-betting-interlock.R` is rewritten as described in WS5.

## Rollout order

1. **Milestone A** (WS1, WS2, WS4, WS5, WS7, WS9). Merge; handball starts scraping and producing
   paper recommendations, basketball starts scraping, football is unchanged.
   **Time-sensitive:** the handball season is under way (first fixture 2026-09-05), and odds rows
   cannot be backfilled.
2. **Milestone B** (WS3): shadow comparisons, then the football cutover and removing Chrome from
   `scrape-odds.yml` and `discover-leagues.yml`.
3. **Milestone C** (WS8, WS6): the paper report is ready and the placer can place handball bets.
   The user decides whether handball moves to `manual`, and what sizing to use.
4. **Stage 3:** the user moves handball from `manual` to `auto`.
5. **Basketball:** comps are wired at mode `scrape` once discovery finds them. Phase C gets its own
   spec.

## Risks

- **The API is unofficial.** The site uses it itself and it is unauthenticated, but it could change
  shape or start rate-limiting. Mitigations:
  - request volume stays low (about 3–6 requests per run);
  - fetch failures raise the typed soft-fail error;
  - a parse or shape mismatch fails loudly, as DOM parse errors do today;
  - `odds_freshness` escalates staleness;
  - the DOM path stays as the `source: dom` fallback through Milestone B.
- **Shared daily cap.** The autoplace cap (`max(0.10 × pool, 1000)`) is shared across sports, so
  handball at `auto` competes with football for the same daily room. This is accepted, and noted
  here so it isn't a surprise.
- **Out of scope, noted.** Football comp `27524` "Besta deild kv. neðri hluti" is live on Lengjan
  (Valur kv – Víkingur Rvk kv, 2026-09-25) but is not in `leagues.yml`. The Thursday discovery run
  should propose it. Wiring it is a football change, outside this spec.
