# Runbook: a season restarts

**Symptom.** A league that has been dormant for months should be ingesting
again — or you want to confirm it will, before the first fixture.

## What is supposed to happen, unattended

Nothing. Since the WS6 change (spec §7) the federation ingest is gated on
*fetch state*, not on fetched data, so a dormant league resumes by itself:

1. `data/health/ingest_log.json` holds, per `(league, sex)`,
   `last_attempt_at` / `last_rows` / `zero_streak` / `last_nonzero_at`.
2. A cell is skipped only when its last attempt was inside 24 h **and** its
   last three attempts all returned zero rows.
3. So at most one attempt per cell per day happens all off-season, and the
   first attempt after the fixtures appear returns rows, clears the streak,
   and the cell is fully awake again.

There is no flag to flip and no `--force` to remember. That is the point:
the previous gate read `config/active_competitions.json`, which is derived
from `data/facts/schedules` rows that only ingest can write — a closed loop
that no dormant league could ever escape.

## Checks, in order

```bash
Rscript -e 'devtools::load_all("."); print(read_ingest_log())'
```

- **A cell absent from the log** → it has never been attempted. Fine on a new
  install; suspicious otherwise.
- **`zero_streak` climbing, `last_nonzero_at` months ago** → ordinary
  off-season.
- **`zero_streak` climbing but the season HAS started** → the scraper is
  returning nothing without erroring. This is the ambiguous case the first
  zero warns loudly about. Go to "silently-empty scraper" below.

Confirm the season ids resolve before blaming the scraper:

```bash
Rscript -e 'devtools::load_all("."); print(sports:::kki_unresolved_seasons(sports:::kki_current_season()))'
Rscript -e 'devtools::load_all("."); print(hsi_unresolved_seasons(sports:::hsi_current_season()))'
```

Any row returned is a cell the ingest will skip. Both federations rotate
their ids each July:

- **KKÍ** — `league_id` is stable (`KKI_LEAGUE_IDS`); `season_id` rotates.
  Re-read it from the mótayfirlit season selector with
  `parse_kki_season_options()`. The parser cross-validates: on seasons already
  in `KKI_SEASON_IDS` it must reproduce the committed value.
- **HSÍ** — every page is `/tournament/<id>` keyed by season
  (`HSI_TOURNAMENT_IDS`); rediscover with `hsi_discover_tournaments()`.

## Silently-empty scraper

The federation changed its page shape and the parser now matches nothing.
Distinguishing it from an off-season is the whole reason the first zero is
loud. Fetch one cell directly and look at the row count; if the fetch
succeeds and the season is live, the parser is broken, not the season.

**A wrong-season page is a different failure and is already fatal.**
`.assert_season_stamp()` aborts when more than 5 % of parsed `match_date`
years fall outside `{season - 1, season}`, so an id that quietly still serves
last season cannot be written into this season's hive partition.

## What must NOT be done

- Do not hand-edit `config/active_competitions.json`. `scrape-results.yml`
  runs `00_active_competitions.R` immediately before the ingest and
  overwrites it, and since WS6 the federation ingest does not read it anyway.
- Do not enable betting to "wake a league up". Odds and results are separate
  paths: `betting.enabled: false` stops odds and placement, and has no effect
  on results ingest, fitting or publishing.
