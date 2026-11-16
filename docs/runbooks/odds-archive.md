# Runbook: reading the raw Lengjan odds archive

**What it is.** A capture job in a separate private repository archives
Lengjan's raw API payloads (the `current-program` body and market detail) as
gzipped JSON with a manifest, far more often than the CI scrape, which keeps
about two parsed snapshots per match and only for wired competitions. The
archive is the odds history for testing prediction quality. This repo only
*reads* a local copy of it (`R/lengjan-archive.R`).

**Setup.** Put the path of your local copy in `.Renviron` (template:
`.Renviron.example`) as `LENGJAN_ARCHIVE_DIR=<path>`, or set
`options(sports.lengjan_archive_dir = "<path>")`, which wins over the
environment variable. Every function also takes `dir` directly.

```r
devtools::load_all()
ix <- lengjan_archive_index()   # one row per capture: ts, status, files
odds <- read_lengjan_archive(from = "2026-11-19", to = "2026-11-20")
main <- read_lengjan_archive(from = "2026-11-19", what = "program")
parse_lengjan_program_markets(prog)   # the same, for one parsed program body
```

- `what = "detail"` (default): moneyline, total and spread rows for the events
  a capture fetched market detail for. About 0.1 s per capture.
- `what = "program"`: a moneyline row per selection of *every* listed event's
  embedded main market. About 4 s per capture on a 1,000-event programme
  (both timings measured 2026-11-16 on a synthetic programme), so bound the
  read with `from`/`to`.
- Rows are odds-shaped (`scraped_at` is the capture time) plus `sport_id`,
  `sport_name`, `country_code`, `competition_name`. Every captured event comes
  back; `sex` is `NA` unless `config/leagues.yml` lists the competition, and
  team names stay as Lengjan renders them.
- A capture whose file is missing or cut off in the local copy is skipped with
  a warning naming the file; re-pull and read again.

**What it never does.** It never writes to `data/facts/odds/` or anywhere else,
does not validate against `schemas()$odds`, and makes no network request.
The on-disk format (version 1) is the capture job's contract and is documented
with it; the reader skips manifest lines of any other version.
