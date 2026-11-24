# Runbook: reading the raw Lengjan odds archive

**What it is.** A capture job in a separate private repository archives
Lengjan's raw API payloads (the `current-program` body and market detail) as
gzipped JSON with a manifest, far more often than the CI scrape, which keeps
only parsed rows of wired competitions: a median of 4 snapshots per football
match and 2 per handball match (Icelandic matches since 2026-08-01, counted
from `data/facts/odds/` on 2026-11-16). The archive is the odds history for
testing prediction quality. This repo only *reads* a local copy of it
(`R/lengjan-archive.R`).

**Setup.** Put the path of your local copy in `.Renviron` (template:
`.Renviron.example`) as `LENGJAN_ARCHIVE_DIR=<path>`, or set
`options(sports.lengjan_archive_dir = "<path>")`, which wins over the
environment variable. Both readers also take `dir` directly.

```r
devtools::load_all()
ix <- lengjan_archive_index()   # one row per capture: ts, status, files
odds <- read_lengjan_archive(from = "2026-11-19", to = "2026-11-20")
main <- read_lengjan_archive(from = "2026-11-19", what = "program")
parse_lengjan_program_markets(prog)   # the same, for one parsed program body
every <- read_lengjan_archive(from = "2026-11-22", table = "odds_markets")
```

- `what = "detail"` (default): moneyline, total and spread rows for the events
  a capture fetched market detail for; cost grows with the markets fetched.
- `what = "program"`: moneyline rows from each listed event's embedded main
  market, where that is an open, pre-match primary 3WAY (no row for two-way
  sports, outrights or in-play markets). Slow: bound it with `from`/`to`.
- `table = "odds"` (default) gives the odds table's shape: only moneyline,
  total and spread. `table = "odds_markets"` gives the shape of
  `data/facts/odds_markets/`, which the CI scrape has filled since 2026-11-24:
  one row per open selection priced above 1 of every market (1X2, double
  chance, half-time markets, totals, handicaps, ...), keyed by Lengjan's
  `market_id` and `selection`, with the stable market-type code
  `market_code` (the payload's `externalId`; the primary 1X2 carries 1 or 0)
  and `market_version`, which changes when the line moves. With
  `what = "program"` it is each event's embedded main market whatever its
  type, in-play ones kept with `is_live` TRUE.
- Rows carry the chosen table's columns (`scraped_at` is the capture time)
  plus `sport_id`, `sport_name`, `country_code`, `competition_name`. Unwired competitions come
  back too, with `sex` `NA`; team names stay as Lengjan renders them.
- A capture whose file is missing or cut off in the local copy, or whose body
  has an unexpected shape, is skipped; one warning lists the skipped captures.

**What it never does.** It never writes to `data/facts/odds/`,
`data/facts/odds_markets/` or anywhere else, does not validate against
`schemas()`, and makes no network request.
The on-disk format (version 1) is the capture job's contract and is documented
with it; the reader skips manifest lines of any other version.
