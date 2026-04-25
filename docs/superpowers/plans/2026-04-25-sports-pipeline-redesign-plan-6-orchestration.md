# Sports Pipeline Redesign — Plan 6: Orchestration + CI + Cutover

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Wire the pieces shipped in Plans 1–5 into a `{targets}` DAG, add four GitHub Actions workflows that run scrape → fit → publish on a schedule, port the missing Lengjan odds scraper from `_legacy/lengjan-odds/`, add the `pull-sports-data.yml` workflow on the metill-platform side, and complete the cutover from the legacy four-repo topology.

**Architecture:** A single `_targets.R` at repo root defines per-league dynamic targets across five layers: `schedule_active` (filter generator) → `ingest_*` (federation results/schedule + Lengjan odds) → `fit_*` (Stan posteriors) → `decide_*` (recommendations) → `publish_*` (JSON snapshots). `run.R` is a thin CLI wrapper around `tar_make()`. CI runs three cron workflows (odds 3×/day, results 1×/day, fit-and-publish daily + `workflow_dispatch`) plus an on-push `ci-tests.yml`. The placer keeps zero CI presence — the `test-placer-ci-isolation.R` gate from Plan 5 enforces this. Cutover drops the placer's CSV dual-write (Parquet becomes canonical) and deprecates the legacy `scripts/{fit,decide,publish}_all.R` runners in favour of `Rscript run.R`.

**Tech Stack:** R (≥ 4.0), `{targets}` (DAG orchestration), `{cmdstanr}` (Stan fits — CI cached via `actions/cache`), GitHub Actions (`r-lib/actions/setup-r@v2`, `r-lib/actions/setup-r-dependencies@v2`, `browser-actions/setup-chrome@v1` for chromote), Plan 1–5 stack (`{arrow}`, `{dplyr}`, `{rvest}`, `{chromote}`, `{cli}`, `{here}`, `{readr}`, `{yaml}`, `{jsonlite}`), `{testthat}` ed 3.

**Scope (Plan 6):**

- `R/ingest-lengjan-odds.R` — port `_legacy/lengjan-odds/R/{parse,scrape,pipeline}.R` (~700 lines) onto Parquet output via `write_table("odds", ...)`
- `R/schedule-active.R` — generate `config/active_competitions.json` from `data/facts/schedules/` (lookahead-window filter)
- `_targets.R` — DAG definition: schedule_active → ingest → fit → decide → publish with per-league dynamic targets
- `run.R` — thin CLI wrapper mapping `--league X --sex Y --step fit` to `tar_make(names = …)`
- `.github/workflows/{ci-tests,scrape-odds,scrape-results,fit-and-publish}.yml` — 4 workflows
- `metill-platform/.github/workflows/pull-sports-data.yml` — hourly pull from sports → `data/ithrottir/`
- Cutover: drop placer CSV dual-write (set `dual_write_csv = FALSE` default in `R/placer-ledger.R`), deprecate `scripts/{fit,decide,publish}_all.R` (replaced by `Rscript run.R`), document `_legacy/` archival
- CLAUDE.md update — Plan 6 → ✅ Complete, status table all six plans done; conventions describe `tar_make()` as the daily driver

**Out of scope:**

- Reviving paused non-Icelandic leagues (still phase 2)
- Reviving `livesport-data/` (still deferred — non-Icelandic league trigger)
- Migrating handball/other 12-country structure (still deferred)
- Walk-forward backtester / `R/research/*` (spec §3.4 names it but tags it "no production code touched"; this plan does not add it)
- New Stan models, new placement policies, ledger schema changes — all outside the migration mandate per spec §5
- Removing `_legacy/` from the repo (kept on disk; archival is GitHub-side admin only)

---

## File structure created by this plan

```
sports/
├── _targets.R                        # NEW — DAG entry point
├── run.R                             # NEW — thin CLI wrapper around tar_make()
├── R/
│   ├── ingest-lengjan-odds.R         # NEW — port of _legacy/lengjan-odds/R/{parse,scrape,pipeline}.R
│   ├── schedule-active.R             # NEW — generate config/active_competitions.json
│   └── placer-ledger.R               # MODIFIED — dual_write_csv = FALSE default
├── config/
│   └── active_competitions.json      # GENERATED at runtime; tracked when CI commits it
├── tests/testthat/
│   ├── fixtures/
│   │   └── lengjan/
│   │       ├── competition_page.html # Captured Lengjan competition page snippet
│   │       └── match_detail.html     # Captured Lengjan match detail snippet
│   ├── test-ingest-lengjan-odds.R    # Parser unit tests + skip-friendly chromote integration
│   ├── test-schedule-active.R        # Lookahead-window logic
│   └── test-targets-dag.R            # tar_validate() + per-target structure assertions
├── .github/workflows/
│   ├── ci-tests.yml                  # On push/PR: devtools::test()
│   ├── scrape-odds.yml               # cron 3×/day → tar_make(starts_with("odds_"))
│   ├── scrape-results.yml            # cron 1×/day → tar_make(starts_with("ingest_"))
│   └── fit-and-publish.yml           # cron daily + workflow_dispatch → fit/decide/publish
└── docs/superpowers/plans/
    └── 2026-04-25-sports-pipeline-redesign-plan-6-orchestration.md  # this file

metill-platform/                       # SEPARATE REPO
└── .github/workflows/
    └── pull-sports-data.yml          # NEW — hourly pull
```

The legacy `scripts/{fit,decide,publish}_all.R` and `scripts/backfill_ingest.R` files are **kept** as one-shot ad-hoc tools but flagged deprecated in CLAUDE.md (Plan 6 cutover note). The `{targets}` DAG is the supported orchestration path; the scripts remain as escape hatches.

---

## Task 1: `R/ingest-lengjan-odds.R` — port Lengjan odds scraper to Parquet (TDD)

**Files:**

- Create: `R/ingest-lengjan-odds.R`
- Create: `tests/testthat/fixtures/lengjan/competition_page.html`
- Create: `tests/testthat/fixtures/lengjan/match_detail.html`
- Create: `tests/testthat/test-ingest-lengjan-odds.R`
- Modify: `DESCRIPTION` — add `rvest` to Imports if not already there

**Purpose:** Port `_legacy/lengjan-odds/R/{parse,scrape,pipeline}.R` (~700 lines, three files) into a single `R/ingest-lengjan-odds.R` that writes to `data/facts/odds/` Parquet via `write_table("odds", ...)` instead of CSV. Reuses the legacy two-stage Chromote scraper (competition page → 1x2 odds + match-detail URLs → handicap + totals odds) but normalises the long-form output to the `odds` schema (sport, country, scraped_at, match_date, home_team, away_team, market, outcome, line, odds).

The legacy code reads competitions from `_legacy/lengjan-odds/config/competitions.yml`; the new version reads `config/leagues.yml::*.lengjan.competitions` (already established in Plan 1). The legacy code's CSS-selector struggle stays verbatim — it's a real risk surface the spec acknowledges (§3.6 "scraper URLs and team-name mappings") but not one we widen here.

**Signatures:**

```r
ingest_lengjan_odds(leagues, scraped_at = Sys.time(), root = here::here("data"),
                   chromote_session = NULL) -> integer (rows written)
parse_competition_page(html, sport, country) -> tibble (1x2 odds, long form)
parse_match_detail(html, sport, country, match_meta) -> tibble (handicap+totals, long form)
```

`leagues` is the filtered output of `load_leagues() |> filter_leagues(active = TRUE)` — only leagues with a non-empty `lengjan.competitions` block are scraped. `chromote_session = NULL` means create a fresh session; tests pass an existing one to keep mocks tractable.

- [ ] **Step 1: Capture Lengjan HTML fixtures**

The legacy scraper has no fixtures, so we create two small ones from a live page (one-time copy). Run this once locally to refresh; commit the HTML files.

```bash
mkdir -p tests/testthat/fixtures/lengjan
# Grab a small representative HTML chunk — competition page (1x2 + match links)
# and a single match detail page (handicap + totals tables).
# Use any current Lengjan competition; we strip down to the relevant DOM.
# (Run interactively via chromote::ChromoteSession$new()$Page$navigate(...) and
# capture $get_outer_html for the two DOM subtrees.)
```

For the plan, here are minimal valid fixtures we can hand-craft from the legacy scraper output to drive the parser tests without a live page:

`tests/testthat/fixtures/lengjan/competition_page.html`:

```html
<div class="lj1n6v0">
  <a class="lj1n6v1" href="/?sport=1&competition=14&match=42">
    <div class="lj1n6v9">
      <span>KR</span>
      <span>vs</span>
      <span>Valur</span>
    </div>
    <div class="lj1n6vb">
      <ol class="lj1n6vd">
        <li><button class="uazl1c1" aria-label="1, studull: 2.10"><div class="h7cub57"><p>2.10</p></div></button></li>
        <li><button class="uazl1c1" aria-label="X, studull: 3.50"><div class="h7cub57"><p>3.50</p></div></button></li>
        <li><button class="uazl1c1" aria-label="2, studull: 3.20"><div class="h7cub57"><p>3.20</p></div></button></li>
      </ol>
    </div>
    <span class="match-date">25. apr 19:15</span>
  </a>
</div>
```

`tests/testthat/fixtures/lengjan/match_detail.html`:

```html
<section class="zh0raz0">
  <button aria-controls="row-HC_FT">Forgjof</button>
  <table>
    <tr><th>-1.0</th><td><button class="uazl1c1"><div class="h7cub57"><p>2.65</p></div></button></td><td><button class="uazl1c1"><div class="h7cub57"><p>1.45</p></div></button></td></tr>
  </table>
</section>
<section class="zh0raz0">
  <button aria-controls="row-OU_FT">Yfir eda undir</button>
  <table>
    <tr><th>2.5</th><td><button class="uazl1c1"><div class="h7cub57"><p>1.80</p></div></button></td><td><button class="uazl1c1"><div class="h7cub57"><p>2.00</p></div></button></td></tr>
  </table>
</section>
```

- [ ] **Step 2: Write failing parser tests**

```r
# tests/testthat/test-ingest-lengjan-odds.R
test_that("parse_competition_page extracts 1x2 odds in long form", {
  html_path <- testthat::test_path("fixtures/lengjan/competition_page.html")
  html <- rvest::read_html(html_path)

  out <- parse_competition_page(html, sport = "football", country = "iceland")

  # Three rows: home, draw, away
  expect_equal(nrow(out), 3L)
  expect_setequal(out$outcome, c("home", "draw", "away"))
  expect_equal(out$market, rep("moneyline", 3L))
  expect_equal(out$home_team, rep("KR", 3L))
  expect_equal(out$away_team, rep("Valur", 3L))
  expect_equal(
    out$odds[match(c("home", "draw", "away"), out$outcome)],
    c(2.10, 3.50, 3.20)
  )
  expect_true(all(is.na(out$line)))
  # Date parsing: "25. apr <year>" -> 25 April current year (or +/- 1 if past)
  expect_equal(format(out$match_date[[1]], "%m-%d"), "04-25")
})

test_that("parse_match_detail extracts handicap + totals", {
  html_path <- testthat::test_path("fixtures/lengjan/match_detail.html")
  html <- rvest::read_html(html_path)

  match_meta <- list(
    sport = "football", country = "iceland",
    home_team = "KR", away_team = "Valur",
    match_date = as.Date("2026-04-25")
  )
  out <- parse_match_detail(html, match_meta)

  spread_rows <- out[out$market == "spread", ]
  expect_equal(nrow(spread_rows), 2L)
  expect_setequal(spread_rows$outcome, c("home", "away"))
  expect_equal(unique(spread_rows$line), -1.0)
  expect_equal(
    spread_rows$odds[match(c("home", "away"), spread_rows$outcome)],
    c(2.65, 1.45)
  )

  total_rows <- out[out$market == "total", ]
  expect_equal(nrow(total_rows), 2L)
  expect_setequal(total_rows$outcome, c("over", "under"))
  expect_equal(unique(total_rows$line), 2.5)
  expect_equal(
    total_rows$odds[match(c("over", "under"), total_rows$outcome)],
    c(1.80, 2.00)
  )
})

test_that("ingest_lengjan_odds skips chromote bits with skip_if", {
  testthat::skip_if_not(
    nzchar(Sys.getenv("LENGJAN_LIVE_TEST")),
    "set LENGJAN_LIVE_TEST=1 to run live scraper integration"
  )
  leagues <- load_leagues()
  active <- filter_leagues(leagues, active = TRUE, has_lengjan = TRUE)
  expect_gt(length(active), 0L)
})
```

- [ ] **Step 3: Run tests to confirm they fail**

```bash
Rscript -e 'devtools::test(filter = "ingest-lengjan-odds")'
```

Expected: ERROR — `parse_competition_page` not found.

- [ ] **Step 4: Implement `R/ingest-lengjan-odds.R` parser core**

```r
#' @include storage.R config.R
NULL

# --- CSS selectors (matches legacy _legacy/lengjan-odds/R/scrape.R) ----------

.lengjan_selectors <- list(
  match_container = "div.lj1n6v0",
  match_link      = "a.lj1n6v1",
  teams           = ".lj1n6v9",
  odds_list       = "ol.lj1n6vd",
  odds_button     = "button.uazl1c1",
  odds_value      = ".h7cub57 p",
  market_section  = "section.zh0raz0",
  date_text       = ".match-date"
)

#' Parse a Lengjan competition page (1x2 odds + match links).
#'
#' Returns a long-form tibble matching `schemas()$odds` minus `scraped_at`,
#' which is filled in by [ingest_lengjan_odds()].
#'
#' @param html An rvest-parsed HTML document.
#' @param sport,country League dimensions, passed through.
#' @return Tibble with 3 rows per match (home/draw/away).
#' @noRd
parse_competition_page <- function(html, sport, country) {
  matches <- rvest::html_elements(html, .lengjan_selectors$match_container)

  date_strs <- vapply(matches, function(m) {
    rvest::html_text2(rvest::html_element(m, .lengjan_selectors$date_text))
  }, character(1))
  match_dates <- parse_lengjan_dates(date_strs)

  rows <- purrr::map2(matches, match_dates, function(m, match_date) {
    teams_el <- rvest::html_elements(m, .lengjan_selectors$teams)
    teams <- rvest::html_text2(rvest::html_elements(teams_el, "span"))
    teams <- teams[teams != "vs" & nzchar(teams)]
    if (length(teams) < 2L) {
      return(NULL)
    }

    odds_btns <- rvest::html_elements(m, .lengjan_selectors$odds_button)
    odds_vals <- as.numeric(rvest::html_text2(
      rvest::html_elements(odds_btns, .lengjan_selectors$odds_value)
    ))
    if (length(odds_vals) < 3L || any(is.na(odds_vals[1:3]))) {
      return(NULL)
    }

    tibble::tibble(
      sport = sport, country = country,
      match_date = match_date,
      home_team = teams[[1]], away_team = teams[[2]],
      market = "moneyline",
      outcome = c("home", "draw", "away"),
      line = NA_real_,
      odds = odds_vals[1:3]
    )
  })

  dplyr::bind_rows(rows)
}

#' Parse a Lengjan match-detail page (handicap + totals).
#'
#' @param html An rvest-parsed HTML document.
#' @param match_meta List with sport, country, home_team, away_team, match_date.
#' @return Tibble with handicap (market="spread") and totals (market="total")
#'   rows in long form.
#' @noRd
parse_match_detail <- function(html, match_meta) {
  sections <- rvest::html_elements(html, .lengjan_selectors$market_section)

  out <- list()
  for (sec in sections) {
    btn <- rvest::html_element(sec, "button[aria-controls]")
    aria <- rvest::html_attr(btn, "aria-controls")

    if (identical(aria, "row-HC_FT")) {
      market <- "spread"
      outcomes <- c("home", "away")
    } else if (identical(aria, "row-OU_FT")) {
      market <- "total"
      outcomes <- c("over", "under")
    } else {
      next
    }

    rows <- rvest::html_elements(sec, "tr")
    for (r in rows) {
      line_str <- rvest::html_text2(rvest::html_element(r, "th"))
      line_val <- suppressWarnings(as.numeric(line_str))
      if (is.na(line_val)) next

      btns <- rvest::html_elements(r, .lengjan_selectors$odds_button)
      odds_vals <- as.numeric(rvest::html_text2(
        rvest::html_elements(btns, .lengjan_selectors$odds_value)
      ))
      if (length(odds_vals) < 2L) next

      out[[length(out) + 1L]] <- tibble::tibble(
        sport = match_meta$sport, country = match_meta$country,
        match_date = match_meta$match_date,
        home_team = match_meta$home_team, away_team = match_meta$away_team,
        market = market,
        outcome = outcomes,
        line = line_val,
        odds = odds_vals[1:2]
      )
    }
  }

  dplyr::bind_rows(out)
}

# Verbatim from _legacy/lengjan-odds/R/parse.R. Lengjan format:
# "3. mar 19:45" (day. month abbrev hour:min); year inferred dynamically.
icelandic_months <- c(
  "jan" = "01", "feb" = "02", "mar" = "03", "apr" = "04",
  "maí" = "05", "jún" = "06", "júl" = "07", "ágú" = "08",
  "sep" = "09", "okt" = "10", "nóv" = "11", "des" = "12"
)

parse_lengjan_dates <- function(date_strings) {
  cleaned <- stringr::str_extract(
    date_strings,
    "\\d+\\.\\s*[a-záéíóúðþæ]+"
  )
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  for (abbr in names(icelandic_months)) {
    cleaned <- stringr::str_replace(cleaned, abbr, icelandic_months[[abbr]])
  }
  with_year <- stringr::str_c(cleaned, " ", current_year)
  parsed <- lubridate::dmy(with_year, quiet = TRUE)
  # Year boundary: if a parsed date is >6 months in the future,
  # it's likely from the previous year (e.g., scraping in Jan, seeing "des")
  future_cutoff <- Sys.Date() + 180
  past_shift <- !is.na(parsed) & parsed > future_cutoff
  if (any(past_shift)) {
    with_year_prev <- stringr::str_c(cleaned[past_shift], " ", current_year - 1L)
    parsed[past_shift] <- lubridate::dmy(with_year_prev, quiet = TRUE)
  }
  parsed
}
```

Update `parse_competition_page` to call the vector form: replace the per-match `match_date <- parse_lengjan_date(date_str)` with one batch call collecting `date_str` across matches and calling `parse_lengjan_dates(date_strs)` once. (The fixture in Step 1 uses "25. apr 19:15" — single Icelandic abbreviation — so the test exercises the legacy code path.)

- [ ] **Step 5: Run parser tests — they should pass**

```bash
Rscript -e 'devtools::test(filter = "ingest-lengjan-odds")'
```

Expected: 2 PASS (parsers), 1 SKIP (chromote integration without `LENGJAN_LIVE_TEST=1`).

- [ ] **Step 6: Implement the chromote-driven scraper layer**

Append to `R/ingest-lengjan-odds.R`:

```r
#' Scrape live Lengjan odds for active leagues and write to facts/odds Parquet.
#'
#' Two-stage: (1) competition page yields 1x2 + match URLs; (2) match-detail
#' pages yield handicap + totals. All odds rows go into one long-form tibble
#' and a single `write_table("odds", ...)` append.
#'
#' @param leagues Filtered list (output of `filter_leagues(active = TRUE,
#'   has_lengjan = TRUE)`).
#' @param scraped_at Single timestamp for the entire run; recorded on every
#'   row so a re-scrape is identifiable as one event.
#' @param root Storage root.
#' @param chromote_session Existing session (for tests/parallel runs); NULL
#'   means create + close one.
#' @return Number of rows written (invisible).
#' @export
ingest_lengjan_odds <- function(leagues, scraped_at = Sys.time(),
                                root = here::here("data"),
                                chromote_session = NULL) {
  stopifnot(is.list(leagues), length(leagues) > 0L)

  owns_session <- is.null(chromote_session)
  if (owns_session) {
    chromote_session <- chromote::ChromoteSession$new()
  }
  on.exit(if (owns_session) chromote_session$close(), add = TRUE)

  rows <- list()
  for (key in names(leagues)) {
    league <- leagues[[key]]
    comps <- league$lengjan$competitions %||% list()
    if (length(comps) == 0L) next

    for (comp in comps) {
      cli::cli_alert_info("Scraping {key}: {comp$name} (id={comp$id})")
      url <- sprintf(
        "https://games.lotto.is/?sport=%d&competition=%s",
        .lengjan_sport_id(league$sport),
        comp$id
      )
      html <- .lengjan_fetch(chromote_session, url)
      comp_rows <- parse_competition_page(
        html, sport = league$sport, country = league$country
      )

      # Stage 2 — match detail pages
      match_links <- rvest::html_attr(
        rvest::html_elements(html, .lengjan_selectors$match_link), "href"
      )
      for (i in seq_along(match_links)) {
        if (i > nrow(comp_rows) / 3L) break
        match_meta <- list(
          sport = league$sport, country = league$country,
          home_team = comp_rows$home_team[1L + (i - 1L) * 3L],
          away_team = comp_rows$away_team[1L + (i - 1L) * 3L],
          match_date = comp_rows$match_date[1L + (i - 1L) * 3L]
        )
        match_url <- sprintf("https://games.lotto.is%s", match_links[[i]])
        detail_html <- tryCatch(
          .lengjan_fetch(chromote_session, match_url),
          error = function(e) {
            cli::cli_alert_warning("Match detail fetch failed: {conditionMessage(e)}")
            NULL
          }
        )
        if (!is.null(detail_html)) {
          comp_rows <- dplyr::bind_rows(
            comp_rows,
            parse_match_detail(detail_html, match_meta)
          )
        }
        Sys.sleep(stats::runif(1, 2, 5)) # Rate limit
      }

      rows[[key]] <- dplyr::bind_rows(rows[[key]], comp_rows)
    }
  }

  all_rows <- dplyr::bind_rows(rows) |>
    dplyr::mutate(scraped_at = !!scraped_at) |>
    dplyr::select(
      sport, country, scraped_at, match_date,
      home_team, away_team, market, outcome, line, odds
    )

  if (nrow(all_rows) == 0L) {
    cli::cli_alert_info("No odds rows scraped")
    return(invisible(0L))
  }

  write_table(all_rows, "odds", root = root)
  cli::cli_alert_success("Wrote {nrow(all_rows)} odds rows to facts/odds/")
  invisible(nrow(all_rows))
}

# Lengjan sport-id mapping (verbatim from _legacy/lengjan-odds/config).
.lengjan_sport_id <- function(sport) {
  c(football = 1L, basketball = 2L, handball = 6L)[[sport]]
}

# Fetch + parse a Lengjan page via chromote.
.lengjan_fetch <- function(session, url) {
  session$Page$navigate(url)
  session$Page$loadEventFired(timeout_ = 30000)
  Sys.sleep(2) # JS render settle
  rvest::read_html(session$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value)
}

# %||% helper
`%||%` <- function(x, y) if (is.null(x)) y else x
```

- [ ] **Step 7: Add `has_lengjan` to `filter_leagues()`**

In `R/config.R`, extend `filter_leagues()` to accept `has_lengjan = FALSE` (default off; on returns only leagues with non-empty `lengjan$competitions`).

```r
filter_leagues <- function(leagues, active = NULL, sport = NULL, country = NULL,
                           has_lengjan = FALSE) {
  out <- leagues
  if (!is.null(active)) {
    out <- out[vapply(out, function(l) isTRUE(l$active) == active, logical(1))]
  }
  if (!is.null(sport)) {
    out <- out[vapply(out, function(l) identical(l$sport, sport), logical(1))]
  }
  if (!is.null(country)) {
    out <- out[vapply(out, function(l) identical(l$country, country), logical(1))]
  }
  if (isTRUE(has_lengjan)) {
    out <- out[vapply(out, function(l) {
      length(l$lengjan$competitions %||% list()) > 0L
    }, logical(1))]
  }
  out
}
```

- [ ] **Step 8: Add a smoke test for filter_leagues(has_lengjan = TRUE)**

In `tests/testthat/test-config.R`, add:

```r
test_that("filter_leagues(has_lengjan = TRUE) keeps Icelandic leagues", {
  leagues <- load_leagues()
  active <- filter_leagues(leagues, active = TRUE, has_lengjan = TRUE)
  expect_gt(length(active), 0L)
  for (l in active) {
    expect_true(length(l$lengjan$competitions %||% list()) > 0L)
  }
})
```

- [ ] **Step 9: Run full test suite**

```bash
Rscript -e 'devtools::test()'
```

Expected: all previous tests still pass; the new ingest-lengjan-odds parser tests pass; chromote integration test skips.

- [ ] **Step 10: Commit**

```bash
git add R/ingest-lengjan-odds.R R/config.R tests/testthat/test-ingest-lengjan-odds.R \
        tests/testthat/test-config.R tests/testthat/fixtures/lengjan/
git commit -m "feat: R/ingest-lengjan-odds.R — port Lengjan odds scraper to Parquet

Ports _legacy/lengjan-odds/R/{parse,scrape,pipeline}.R into a single file
that writes data/facts/odds/ via write_table() instead of CSV.

Two-stage: competition page (1x2) -> match detail (handicap + totals).
Long-form output matches the odds schema. CSS selectors held verbatim
from legacy.

Adds filter_leagues(has_lengjan = TRUE) to drop leagues without a
configured Lengjan competitions block."
```

---

## Task 2: `R/schedule-active.R` — generate active_competitions.json (TDD)

**Files:**

- Create: `R/schedule-active.R`
- Create: `tests/testthat/test-schedule-active.R`

**Purpose:** Replicate the legacy `livesport-data/R/check_schedules.R` and `Sports/check_schedules.R` filter generators, but reading the new `data/facts/schedules/` Parquet instead of per-league CSVs. Writes `config/active_competitions.json` with shape `{generated_at, lookahead_days, active: {league_key: bool}}`. The DAG and scrape workflows consume this file.

A league is "active" if `data/facts/schedules/` contains at least one fixture in `[today, today + lookahead_days]`. Cold-start safe: if no schedules Parquet exists yet, returns `active = TRUE` for all leagues.

**Signature:**

```r
generate_active_competitions(leagues, lookahead_days = 7L,
                             root = here::here("data"),
                             out_path = here::here("config", "active_competitions.json")
                            ) -> path (invisible)
```

- [ ] **Step 1: Write failing tests**

```r
# tests/testthat/test-schedule-active.R
test_that("generate_active_competitions marks leagues with future fixtures active", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "data"), recursive = TRUE)

  fake_schedules <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    match_date = Sys.Date() + 3L,
    home_team = "KR", away_team = "Valur",
    round = NA_integer_
  )
  write_table(fake_schedules, "schedules", root = file.path(tmp, "data"))

  leagues <- list(
    football_iceland = list(sport = "football", country = "iceland",
                            sexes = c("male", "female"), active = TRUE),
    basketball_iceland = list(sport = "basketball", country = "iceland",
                              sexes = c("male", "female"), active = TRUE)
  )

  out_path <- file.path(tmp, "config", "active_competitions.json")
  dir.create(dirname(out_path), recursive = TRUE)
  generate_active_competitions(leagues, lookahead_days = 7L,
                               root = file.path(tmp, "data"),
                               out_path = out_path)

  res <- jsonlite::fromJSON(out_path)
  expect_true(res$active$football_iceland)
  expect_false(res$active$basketball_iceland)
  expect_equal(res$lookahead_days, 7L)
})

test_that("generate_active_competitions defaults to all-active on missing data", {
  tmp <- withr::local_tempdir()
  out_path <- file.path(tmp, "config", "active_competitions.json")
  dir.create(dirname(out_path), recursive = TRUE)

  leagues <- list(
    football_iceland = list(sport = "football", country = "iceland",
                            sexes = c("male", "female"), active = TRUE)
  )
  generate_active_competitions(leagues, lookahead_days = 7L,
                               root = file.path(tmp, "data-missing"),
                               out_path = out_path)

  res <- jsonlite::fromJSON(out_path)
  expect_true(res$active$football_iceland)
})

test_that("generate_active_competitions ignores past fixtures", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "data"), recursive = TRUE)

  fake <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    match_date = Sys.Date() - 7L,
    home_team = "KR", away_team = "Valur",
    round = NA_integer_
  )
  write_table(fake, "schedules", root = file.path(tmp, "data"))

  leagues <- list(
    football_iceland = list(sport = "football", country = "iceland",
                            sexes = c("male", "female"), active = TRUE)
  )
  out_path <- file.path(tmp, "config", "active_competitions.json")
  dir.create(dirname(out_path), recursive = TRUE)
  generate_active_competitions(leagues, lookahead_days = 7L,
                               root = file.path(tmp, "data"),
                               out_path = out_path)

  res <- jsonlite::fromJSON(out_path)
  expect_false(res$active$football_iceland)
})
```

- [ ] **Step 2: Run tests — confirm fail**

```bash
Rscript -e 'devtools::test(filter = "schedule-active")'
```

Expected: ERROR — `generate_active_competitions` not found.

- [ ] **Step 3: Implement `R/schedule-active.R`**

```r
#' @include storage.R
NULL

#' Generate `config/active_competitions.json` from fixture data.
#'
#' Reads `data/facts/schedules/` (Parquet, partitioned by sport/country/sex/
#' season). A league is "active" if at least one fixture falls in
#' `[today, today + lookahead_days]`. Cold-start safe: missing schedule data
#' marks every league active to avoid silently skipping all scrapes.
#'
#' @param leagues Output of `load_leagues()` (or filtered subset).
#' @param lookahead_days Window size in days.
#' @param root Storage root.
#' @param out_path Path to write JSON.
#' @return invisible(out_path).
#' @export
generate_active_competitions <- function(leagues, lookahead_days = 7L,
                                         root = here::here("data"),
                                         out_path = here::here(
                                           "config", "active_competitions.json"
                                         )) {
  stopifnot(is.list(leagues), length(leagues) > 0L)

  schedule_root <- file.path(root, "facts", "schedules")
  if (!dir.exists(schedule_root)) {
    cli::cli_alert_warning(
      "No schedules at {schedule_root} — defaulting all leagues to active"
    )
    active <- as.list(rep(TRUE, length(leagues)))
    names(active) <- names(leagues)
  } else {
    today <- Sys.Date()
    horizon <- today + as.integer(lookahead_days)
    schedules <- tryCatch(
      read_table("schedules", root = root),
      error = function(e) {
        cli::cli_alert_warning("Read schedules failed: {conditionMessage(e)}")
        tibble::tibble(sport = character(0), country = character(0),
                      match_date = as.Date(character(0)))
      }
    )
    active <- vapply(leagues, function(l) {
      hits <- schedules$sport == l$sport &
        schedules$country == l$country &
        schedules$match_date >= today &
        schedules$match_date <= horizon
      isTRUE(any(hits))
    }, logical(1))
    active <- as.list(active)
  }

  payload <- list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    lookahead_days = as.integer(lookahead_days),
    active = active
  )
  jsonlite::write_json(payload, out_path, pretty = TRUE, auto_unbox = TRUE)
  cli::cli_alert_success(
    "Wrote {out_path} ({sum(unlist(active))}/{length(active)} active)"
  )
  invisible(out_path)
}
```

- [ ] **Step 4: Run tests — they should pass**

```bash
Rscript -e 'devtools::test(filter = "schedule-active")'
```

Expected: 3 PASS.

- [ ] **Step 5: Run `devtools::document()` and full test suite**

```bash
Rscript -e 'devtools::document(); devtools::test()'
```

Expected: all green. NAMESPACE picks up the new export.

- [ ] **Step 6: Commit**

```bash
git add R/schedule-active.R tests/testthat/test-schedule-active.R NAMESPACE
git commit -m "feat: R/schedule-active.R — write active_competitions.json from schedules

Reads data/facts/schedules/ (Parquet) and computes which leagues have
fixtures in the [today, today+lookahead_days] window. Replaces legacy
livesport-data/R/check_schedules.R + Sports/check_schedules.R.

Cold-start safe: missing schedules Parquet defaults all leagues to
active so a fresh clone doesn't silently skip every scrape."
```

---

## Task 3: `_targets.R` — DAG ingest layer (config + schedule + scrapers)

**Files:**

- Create: `_targets.R`
- Create: `tests/testthat/test-targets-dag.R`
- Modify: `DESCRIPTION` — add `targets` to Imports

**Purpose:** Lay down `_targets.R` with the ingest layer: config (file-tracked) → schedule_active filter → per-league `ingest_<key>` (federation results+schedules) and `odds_<key>` (Lengjan odds). Targets are dynamic — generated by iterating over `names(leagues)` at definition time. Fit/decide/publish layers come in Tasks 4–5.

The DAG follows the legacy `_legacy/lengjan-odds/_targets.R` pattern: `tar_target_raw()` + `substitute()` to materialise per-league names. `cue = tar_cue(mode = "always")` on scrape targets (data changes, code doesn't) and `format = "file"` on the active-competitions JSON so dependents re-run when it changes.

- [ ] **Step 1: Write failing tests**

```r
# tests/testthat/test-targets-dag.R
test_that("_targets.R parses and validates", {
  testthat::skip_if_not_installed("targets")
  manifest <- targets::tar_manifest(
    script = here::here("_targets.R"),
    fields = "name",
    callr_function = NULL
  )
  expect_true(nrow(manifest) > 0L)
  expect_true("config" %in% manifest$name)
  expect_true("active_competitions" %in% manifest$name)
})

test_that("ingest targets exist for every active league", {
  testthat::skip_if_not_installed("targets")
  manifest <- targets::tar_manifest(
    script = here::here("_targets.R"),
    fields = "name",
    callr_function = NULL
  )
  leagues <- load_leagues()
  active <- filter_leagues(leagues, active = TRUE)
  for (key in names(active)) {
    expect_true(
      paste0("ingest_", key) %in% manifest$name,
      info = paste("missing ingest target for", key)
    )
  }
})

test_that("odds targets exist for every Lengjan-configured active league", {
  testthat::skip_if_not_installed("targets")
  manifest <- targets::tar_manifest(
    script = here::here("_targets.R"),
    fields = "name",
    callr_function = NULL
  )
  leagues <- load_leagues()
  active_lengjan <- filter_leagues(leagues, active = TRUE, has_lengjan = TRUE)
  for (key in names(active_lengjan)) {
    expect_true(
      paste0("odds_", key) %in% manifest$name,
      info = paste("missing odds target for", key)
    )
  }
})
```

- [ ] **Step 2: Run tests — confirm fail**

```bash
Rscript -e 'devtools::test(filter = "targets-dag")'
```

Expected: ERROR — `_targets.R` does not exist.

- [ ] **Step 3: Create `_targets.R` with config + schedule + ingest layer**

```r
# _targets.R — Sports pipeline DAG.
#
# Layers (in dependency order):
#   1. config + active_competitions filter
#   2. ingest_<league> (federation results + schedules)
#   3. odds_<league>   (Lengjan odds, schedule-aware)
#   4. fit_<league>_<sex>          (Stan posteriors)
#   5. decide_<league>_<sex>       (recommendations)
#   6. publish_<league>_<sex>      (JSONs)
#
# Run: Rscript run.R --league football_iceland --sex male --step fit
# Or: Rscript -e 'targets::tar_make()'                       (everything)
# Or: Rscript -e 'targets::tar_make(starts_with("odds_"))'   (just odds)

library(targets)

tar_option_set(
  packages = c(
    "arrow", "dplyr", "tibble", "purrr", "readr", "stringr", "lubridate",
    "rvest", "yaml", "jsonlite", "here", "cli", "fs"
  ),
  format = "rds"
)

# Source all R/ files so package functions are available to targets.
# Equivalent to devtools::load_all() inside the targets process.
tar_source("R/")

# Read leagues at DAG definition time so we can generate per-league targets.
leagues_definition <- load_leagues()
active_keys <- names(filter_leagues(leagues_definition, active = TRUE))
lengjan_keys <- names(filter_leagues(
  leagues_definition, active = TRUE, has_lengjan = TRUE
))

# Static targets (config, active filter)
static_targets <- list(
  tar_target(
    leagues_file,
    here::here("config", "leagues.yml"),
    format = "file"
  ),
  tar_target(
    leagues_config,
    load_leagues(leagues_file)
  ),
  tar_target(
    bankroll_file,
    here::here("config", "bankroll.yml"),
    format = "file"
  ),
  tar_target(
    bankroll,
    load_bankroll(bankroll_file)
  ),
  tar_target(
    active_competitions,
    {
      out <- generate_active_competitions(
        leagues_config, lookahead_days = 14L
      )
      out
    },
    format = "file",
    cue = tar_cue(mode = "always")
  )
)

# Per-league ingest targets — federation results + schedules.
ingest_targets <- lapply(active_keys, function(key) {
  tar_target_raw(
    name = paste0("ingest_", key),
    command = substitute(
      ingest_one_league(leagues_config, k, active_competitions),
      list(k = key)
    ),
    cue = tar_cue(mode = "always")
  )
})

# Per-league odds targets — Lengjan, schedule-aware.
odds_targets <- lapply(lengjan_keys, function(key) {
  tar_target_raw(
    name = paste0("odds_", key),
    command = substitute(
      ingest_one_lengjan(leagues_config, k, active_competitions),
      list(k = key)
    ),
    cue = tar_cue(mode = "always")
  )
})

c(static_targets, ingest_targets, odds_targets)
```

Then add the per-league wrappers used by the targets to `R/ingest.R`:

```r
#' Run federation ingest for a single league across all configured sexes.
#' @export
ingest_one_league <- function(leagues, key, active_path) {
  active <- jsonlite::fromJSON(active_path)
  if (isFALSE(active$active[[key]])) {
    cli::cli_alert_info("{key}: skipped (no active fixtures)")
    return(0L)
  }
  league <- leagues[[key]]
  total <- 0L
  for (sex in league$sexes) {
    total <- total + ingest_league(league, sex, seasons = NULL)
  }
  total
}

#' Run Lengjan odds ingest for a single league.
#' @export
ingest_one_lengjan <- function(leagues, key, active_path) {
  active <- jsonlite::fromJSON(active_path)
  if (isFALSE(active$active[[key]])) {
    cli::cli_alert_info("{key}: skipped (no active fixtures)")
    return(0L)
  }
  ingest_lengjan_odds(leagues[key])
}
```

(`ingest_league()` already exists from Plan 2; verify by grep before adding the wrappers — if signatures differ, adjust the call in `ingest_one_league()`.)

- [ ] **Step 4: Run tests — they should pass**

```bash
Rscript -e 'devtools::test(filter = "targets-dag")'
```

Expected: 3 PASS.

- [ ] **Step 5: Sanity-check the DAG renders**

```bash
Rscript -e 'targets::tar_manifest(fields = c("name", "command"))' | head -40
```

Expected: a manifest listing `leagues_file`, `leagues_config`, `bankroll_file`, `bankroll`, `active_competitions`, `ingest_basketball_iceland`, `ingest_handball_iceland`, `ingest_football_iceland`, `odds_basketball_iceland`, `odds_handball_iceland`, `odds_football_iceland`.

- [ ] **Step 6: Commit**

```bash
git add _targets.R R/ingest.R tests/testthat/test-targets-dag.R DESCRIPTION
git commit -m "feat: _targets.R — DAG ingest layer

Adds the DAG entry point with config -> active_competitions filter ->
per-league ingest_<key> + odds_<key> targets. Schedule-aware filtering
via active_competitions.json (cue=always so it regenerates each run).

Per-league wrappers ingest_one_league / ingest_one_lengjan adapt
existing Plan 2 + Plan 6 ingest functions to a (leagues, key, active_path)
signature suitable for tar_target_raw + substitute()."
```

---

## Task 4: `_targets.R` — DAG model layer

**Files:**

- Modify: `_targets.R`
- Modify: `tests/testthat/test-targets-dag.R`

**Purpose:** Add per-(league × sex) `fit_<key>_<sex>` targets that depend on `ingest_<key>` and call `fit_league(league, sex)`. The fit targets return the row count of beliefs written (cheap; lets downstream targets invalidate when beliefs change). Use `cue = tar_cue(mode = "thorough")` (default) so a fit only re-runs when a dependency changes.

- [ ] **Step 1: Add failing test**

In `tests/testthat/test-targets-dag.R`, append:

```r
test_that("fit targets exist for every active (league x sex) combo", {
  testthat::skip_if_not_installed("targets")
  manifest <- targets::tar_manifest(
    script = here::here("_targets.R"),
    fields = "name",
    callr_function = NULL
  )
  leagues <- load_leagues()
  active <- filter_leagues(leagues, active = TRUE)
  for (key in names(active)) {
    for (sex in active[[key]]$sexes) {
      expect_true(
        paste0("fit_", key, "_", sex) %in% manifest$name,
        info = paste("missing fit target for", key, sex)
      )
    }
  }
})
```

- [ ] **Step 2: Run test — confirm fail**

```bash
Rscript -e 'devtools::test(filter = "targets-dag")'
```

Expected: FAIL — `fit_basketball_iceland_male` not in manifest.

- [ ] **Step 3: Add fit_targets to `_targets.R`**

Insert before the final `c(...)` line:

```r
# Per-(league x sex) fit targets — Stan posteriors.
fit_targets <- list()
for (key in active_keys) {
  league_def <- leagues_definition[[key]]
  for (sex in league_def$sexes) {
    target_name <- paste0("fit_", key, "_", sex)
    ingest_dep <- as.symbol(paste0("ingest_", key))
    fit_targets[[length(fit_targets) + 1L]] <- tar_target_raw(
      name = target_name,
      command = substitute(
        fit_one(leagues_config, k, s, ingest_dep),
        list(k = key, s = sex, ingest_dep = ingest_dep)
      )
    )
  }
}
```

Update the final return line:

```r
c(static_targets, ingest_targets, odds_targets, fit_targets)
```

Add the wrapper to `R/model-league.R`:

```r
#' tar_target wrapper: fit a single (league x sex) and return belief row count.
#'
#' Takes the leagues config + key + sex (rather than a league object) so it
#' fits the (config, k, …) signature pattern used elsewhere in the DAG. Final
#' arg `_ingest_dep` is unused at runtime — its only purpose is to declare the
#' DAG dependency on the ingest target without coupling fit_league() to one.
#'
#' @export
fit_one <- function(leagues, key, sex, ingest_dep = NULL) {
  league <- leagues[[key]]
  beliefs <- fit_league(league, sex)
  nrow(beliefs)
}
```

- [ ] **Step 4: Run test — should pass**

```bash
Rscript -e 'devtools::test(filter = "targets-dag")'
```

Expected: PASS.

- [ ] **Step 5: Manifest sanity**

```bash
Rscript -e 'm <- targets::tar_manifest(fields = "name"); print(m[grepl("^fit_", m$name), ])'
```

Expected: 6 rows (3 leagues × 2 sexes).

- [ ] **Step 6: Commit**

```bash
git add _targets.R R/model-league.R tests/testthat/test-targets-dag.R
git commit -m "feat: _targets.R — DAG model layer (fit per league x sex)

Per-(league, sex) fit_<key>_<sex> targets that depend on ingest_<key>
and call fit_league(). Returns belief row count so downstream targets
re-run when the fit changes.

fit_one() wrapper adapts fit_league() to the (config, k, s, ingest_dep)
signature pattern used by tar_target_raw + substitute()."
```

---

## Task 5: `_targets.R` — DAG decide & publish layer

**Files:**

- Modify: `_targets.R`
- Modify: `tests/testthat/test-targets-dag.R`

**Purpose:** Per-(league × sex) `decide_<key>_<sex>` and `publish_<key>_<sex>` targets. `decide` depends on `fit` (beliefs) and `odds_<key>` (Lengjan odds); `publish` depends on `fit` (and implicitly `decide` via league config). Decide returns recommendation row count; publish returns invisible NULL.

- [ ] **Step 1: Add failing tests**

In `tests/testthat/test-targets-dag.R`, append:

```r
test_that("decide targets exist for every active (league x sex)", {
  testthat::skip_if_not_installed("targets")
  manifest <- targets::tar_manifest(
    script = here::here("_targets.R"),
    fields = "name",
    callr_function = NULL
  )
  leagues <- load_leagues()
  active <- filter_leagues(leagues, active = TRUE)
  for (key in names(active)) {
    for (sex in active[[key]]$sexes) {
      expect_true(
        paste0("decide_", key, "_", sex) %in% manifest$name,
        info = paste("missing decide target for", key, sex)
      )
    }
  }
})

test_that("publish targets exist for every active (league x sex)", {
  testthat::skip_if_not_installed("targets")
  manifest <- targets::tar_manifest(
    script = here::here("_targets.R"),
    fields = "name",
    callr_function = NULL
  )
  leagues <- load_leagues()
  active <- filter_leagues(leagues, active = TRUE)
  for (key in names(active)) {
    for (sex in active[[key]]$sexes) {
      expect_true(
        paste0("publish_", key, "_", sex) %in% manifest$name,
        info = paste("missing publish target for", key, sex)
      )
    }
  }
})
```

- [ ] **Step 2: Run — confirm fail**

```bash
Rscript -e 'devtools::test(filter = "targets-dag")'
```

Expected: FAIL on missing decide/publish targets.

- [ ] **Step 3: Add decide & publish targets**

Append to `_targets.R` (before the final `c(...)`):

```r
# Per-(league x sex) decide targets — Kelly + portfolio + calibration.
decide_targets <- list()
for (key in active_keys) {
  league_def <- leagues_definition[[key]]
  for (sex in league_def$sexes) {
    target_name <- paste0("decide_", key, "_", sex)
    fit_dep <- as.symbol(paste0("fit_", key, "_", sex))
    odds_dep_name <- paste0("odds_", key)
    if (key %in% lengjan_keys) {
      odds_dep <- as.symbol(odds_dep_name)
      cmd <- substitute(
        decide_one(leagues_config, k, s, bankroll, fit_dep, odds_dep),
        list(k = key, s = sex, fit_dep = fit_dep, odds_dep = odds_dep)
      )
    } else {
      cmd <- substitute(
        decide_one(leagues_config, k, s, bankroll, fit_dep),
        list(k = key, s = sex, fit_dep = fit_dep)
      )
    }
    decide_targets[[length(decide_targets) + 1L]] <- tar_target_raw(
      name = target_name, command = cmd
    )
  }
}

# Per-(league x sex) publish targets — JSONs.
publish_targets <- list()
for (key in active_keys) {
  league_def <- leagues_definition[[key]]
  for (sex in league_def$sexes) {
    target_name <- paste0("publish_", key, "_", sex)
    fit_dep <- as.symbol(paste0("fit_", key, "_", sex))
    decide_dep <- as.symbol(paste0("decide_", key, "_", sex))
    publish_targets[[length(publish_targets) + 1L]] <- tar_target_raw(
      name = target_name,
      command = substitute(
        publish_one(leagues_config, k, s, fit_dep, decide_dep),
        list(k = key, s = sex, fit_dep = fit_dep, decide_dep = decide_dep)
      )
    )
  }
}
```

Update the return:

```r
c(static_targets, ingest_targets, odds_targets, fit_targets,
  decide_targets, publish_targets)
```

Add the wrappers to `R/decide-pipeline.R`:

```r
#' tar_target wrapper: decide for a single (league x sex).
#'
#' fit_dep and odds_dep are pure dependency declarations — their values are
#' ignored. decide_league() reads its inputs from data/ Parquet, but listing
#' the deps here keeps the DAG honest about staleness.
#'
#' @export
decide_one <- function(leagues, key, sex, bankroll,
                       fit_dep = NULL, odds_dep = NULL) {
  league <- leagues[[key]]
  recs <- decide_league(league, sex, bankroll = bankroll)
  nrow(recs)
}
```

And to a new file `R/publish-pipeline.R` (small dispatcher; lives next to other publish files):

```r
#' @include publish-football-iceland.R publish-basketball-iceland.R publish-handball-iceland.R
NULL

#' tar_target wrapper: publish JSONs for a single (league x sex).
#'
#' Dispatches to the appropriate publish_<sport>_iceland() based on the
#' league key. Reads the latest fit from `data/beliefs/latest/` instead of
#' an .rds backup (Plan 6 cutover from SPORTS_BACKUP_ROOT).
#'
#' @export
publish_one <- function(leagues, key, sex, fit_dep = NULL, decide_dep = NULL) {
  league <- leagues[[key]]
  dispatch <- list(
    football_iceland   = publish_football_iceland,
    basketball_iceland = publish_basketball_iceland,
    handball_iceland   = publish_handball_iceland
  )
  pub_fn <- dispatch[[key]]
  if (is.null(pub_fn)) {
    cli::cli_alert_info("No publish dispatcher for {key} — skipping")
    return(invisible(NULL))
  }
  pub_fn(fit = NULL, league = league, sex = sex, end_date = Sys.Date())
}
```

**Publisher signature evolution.** `publish_*_iceland(fit, league, sex, end_date, root, output_root)` currently requires a fit object (it calls `posterior::as_draws_df(fit)` internally). Plan 6 cuts over to reading `data/beliefs/latest/` Parquet so the publishers stop needing the fit's `.rds` backup. Add a fit=NULL branch at the top of each publisher:

```r
publish_football_iceland <- function(fit = NULL, league, sex,
                                     end_date = Sys.Date(),
                                     root = here::here("data"),
                                     output_root = here::here("data", "publish")) {
  # Plan 6 cutover: when fit is NULL, read draws from beliefs/latest/.
  # Existing fit-based code path stays for ad-hoc reruns from a saved .rds.
  if (is.null(fit)) {
    draws <- read_table(
      "beliefs_latest", root = root,
      filter = list(sport = league$sport, country = league$country, sex = sex)
    )
    if (nrow(draws) == 0L) {
      cli::cli_alert_warning(
        "No beliefs at {root}/beliefs/latest/ for {league$sport}/{league$country}/{sex}"
      )
      return(invisible(NULL))
    }
  } else {
    draws <- posterior::as_draws_df(fit$draws())
  }
  # ... existing publisher body, but use `draws` instead of fit-derived draws
}
```

The same one-branch addition goes in `publish_basketball_iceland` and `publish_handball_iceland`. Where the existing code references the fit object beyond the draws (e.g. `fit$summary()` for diagnostics), gate that with `if (!is.null(fit))`.

Add a regression test in `tests/testthat/test-publish-fit-null.R`:

```r
test_that("publish_football_iceland reads beliefs/latest when fit is NULL", {
  testthat::skip_if_no_beliefs()  # gate on data presence
  league <- load_leagues()$football_iceland
  out_dir <- withr::local_tempdir()
  res <- publish_football_iceland(
    fit = NULL, league = league, sex = "male",
    output_root = out_dir
  )
  expect_true(file.exists(file.path(out_dir, "football", "iceland", "karla", "meta.json")))
})
```

`skip_if_no_beliefs()` is a small helper that skips when `data/beliefs/latest/` is empty (cold-start CI). Define it once in `tests/testthat/helper-skips.R` and reuse.

- [ ] **Step 4: Run tests — should pass**

```bash
Rscript -e 'devtools::test(filter = "targets-dag")'
```

Expected: 5 PASS.

- [ ] **Step 5: DAG dry-run — visualise dependencies**

```bash
Rscript -e 'targets::tar_outdated()'
```

Expected: lists all outdated targets (everything on first run). No errors about undefined nodes.

- [ ] **Step 6: Run `devtools::document()` and full test suite**

```bash
Rscript -e 'devtools::document(); devtools::test()'
```

Expected: all green. NAMESPACE picks up `decide_one`, `publish_one`, `fit_one`, `ingest_one_league`, `ingest_one_lengjan`.

- [ ] **Step 7: Commit**

```bash
git add _targets.R R/decide-pipeline.R R/publish-pipeline.R \
        R/publish-football-iceland.R R/publish-basketball-iceland.R \
        R/publish-handball-iceland.R \
        tests/testthat/test-targets-dag.R NAMESPACE
git commit -m "feat: _targets.R — DAG decide + publish layer

decide_<key>_<sex> depends on fit + odds; publish_<key>_<sex> depends
on fit + decide. Per-target wrappers (decide_one, publish_one) adapt
existing pipelines to the (config, k, s, ...deps) signature.

publish_one reads beliefs from data/beliefs/latest/ via read_table()
instead of the SPORTS_BACKUP_ROOT .rds backup — cutover from Plan 4."
```

---

## Task 6: `run.R` — thin CLI wrapper around `tar_make()`

**Files:**

- Create: `run.R`
- Create: `tests/testthat/test-run-cli.R`

**Purpose:** Replace the per-step `scripts/{fit,decide,publish}_all.R` runners with a single `Rscript run.R` that dispatches to `targets::tar_make(names = …)`. The CLI accepts `--league`, `--sex`, `--step` (data | odds | fit | decide | publish | all), and `--all` (run everything).

- [ ] **Step 1: Write failing tests**

```r
# tests/testthat/test-run-cli.R
test_that("run.R --help prints usage", {
  out <- system2(
    "Rscript", c(here::here("run.R"), "--help"),
    stdout = TRUE, stderr = TRUE
  )
  expect_true(any(grepl("--league", out)))
  expect_true(any(grepl("--sex", out)))
  expect_true(any(grepl("--step", out)))
})

test_that("run.R --dry-run --step fit prints the planned target names", {
  out <- system2(
    "Rscript",
    c(here::here("run.R"),
      "--league", "football_iceland",
      "--sex", "male",
      "--step", "fit",
      "--dry-run"),
    stdout = TRUE, stderr = TRUE
  )
  expect_true(any(grepl("fit_football_iceland_male", out)))
})
```

- [ ] **Step 2: Run — confirm fail**

```bash
Rscript -e 'devtools::test(filter = "run-cli")'
```

Expected: FAIL — `run.R` does not exist.

- [ ] **Step 3: Implement `run.R`**

```r
#!/usr/bin/env Rscript
# run.R — Thin CLI wrapper around targets::tar_make().
#
# Usage:
#   Rscript run.R --league football_iceland --sex male --step fit
#   Rscript run.R --all --step odds
#   Rscript run.R --step publish                   # all active leagues, both sexes
#   Rscript run.R --dry-run --step fit             # print plan, don't execute
#   Rscript run.R --help

suppressPackageStartupMessages({
  if (!requireNamespace("targets", quietly = TRUE)) {
    stop("Install {targets} to use run.R: install.packages('targets')")
  }
  devtools::load_all(here::here(), quiet = TRUE)
})

args <- commandArgs(trailingOnly = TRUE)

print_help <- function() {
  cat(
    "Usage: Rscript run.R [options]\n",
    "\n",
    "Options:\n",
    "  --league KEY     Run for a single league (e.g. football_iceland)\n",
    "  --sex SEX        Restrict to one sex (male | female | all)\n",
    "  --step STEP      One of: data | odds | fit | decide | publish | all\n",
    "  --all            Run all active leagues for the chosen step\n",
    "  --dry-run        Print the targets that would run; don't execute\n",
    "  --help           Show this message\n",
    "\n",
    "Examples:\n",
    "  Rscript run.R --all --step odds\n",
    "  Rscript run.R --league handball_iceland --step fit\n",
    "  Rscript run.R --league football_iceland --sex female --step decide\n",
    sep = ""
  )
}

if ("--help" %in% args || length(args) == 0L) {
  print_help()
  quit(save = "no", status = 0L)
}

get_flag <- function(name, default = NULL) {
  i <- which(args == paste0("--", name))
  if (length(i) == 0L) default else args[[i + 1L]]
}
has_flag <- function(name) paste0("--", name) %in% args

league <- get_flag("league")
sex    <- get_flag("sex")
step   <- get_flag("step", "all")
all_flag <- has_flag("all")
dry_run <- has_flag("dry-run")

# Resolve target names
leagues <- load_leagues()
active <- filter_leagues(leagues, active = TRUE)
keys <- if (all_flag || is.null(league)) names(active) else league

target_names <- character(0)
prefix_for_step <- list(
  data    = "ingest_",
  odds    = "odds_",
  fit     = "fit_",
  decide  = "decide_",
  publish = "publish_"
)

steps_to_run <- if (step == "all") {
  c("data", "odds", "fit", "decide", "publish")
} else {
  step
}

for (s in steps_to_run) {
  prefix <- prefix_for_step[[s]]
  if (is.null(prefix)) {
    stop("Unknown --step: ", s, ". Use one of ",
         paste(c(names(prefix_for_step), "all"), collapse = ", "))
  }
  for (k in keys) {
    if (s %in% c("data", "odds")) {
      target_names <- c(target_names, paste0(prefix, k))
    } else {
      sexes <- if (is.null(sex) || sex == "all") {
        active[[k]]$sexes
      } else {
        sex
      }
      target_names <- c(target_names, paste0(prefix, k, "_", sexes))
    }
  }
}

target_names <- unique(target_names)

if (dry_run) {
  cli::cli_h2("Planned targets")
  for (t in target_names) cat(" -", t, "\n")
  quit(save = "no", status = 0L)
}

cli::cli_h1("tar_make({length(target_names)} targets)")
targets::tar_make(names = tidyselect::all_of(target_names))
```

- [ ] **Step 4: Make `run.R` executable and run tests**

```bash
chmod +x run.R
Rscript -e 'devtools::test(filter = "run-cli")'
```

Expected: 2 PASS.

- [ ] **Step 5: Smoke-check the CLI**

```bash
Rscript run.R --help
Rscript run.R --league football_iceland --sex male --step fit --dry-run
```

Expected: help text on first call; on second, a single line `fit_football_iceland_male` printed.

- [ ] **Step 6: Commit**

```bash
git add run.R tests/testthat/test-run-cli.R
git commit -m "feat: run.R — thin CLI wrapper around tar_make()

Maps --league X --sex Y --step fit to tar_make(names = c('fit_X_Y')).
Supports --all, --dry-run, --help.

Replaces scripts/{fit,decide,publish}_all.R as the supported daily
driver. Old scripts kept as ad-hoc escape hatches for now (deprecated
in CLAUDE.md cutover note)."
```

---

## Task 7: `.github/workflows/ci-tests.yml` — devtools::test() on push

**Files:**

- Create: `.github/workflows/ci-tests.yml`

**Purpose:** Run the test suite on every push and pull request. cmdstanr-bound tests skip on CI (no Stan toolchain installed). Chromote tests skip unless `LENGJAN_LIVE_TEST=1` is set, which CI never sets. The placer-isolation gate (test-placer-ci-isolation.R) runs every commit and protects spec §6's local-only invariant.

- [ ] **Step 1: Create the workflow file**

```yaml
# .github/workflows/ci-tests.yml
name: CI Tests

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]
  workflow_dispatch: {}

permissions:
  contents: read

jobs:
  test:
    runs-on: ubuntu-latest

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
      R_KEEP_PKG_SOURCE: yes

    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: 'release'
          use-public-rspm: true

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: |
            any::devtools
            any::testthat
            any::targets
            any::tidyselect

      - name: Run tests
        run: |
          Rscript -e 'devtools::test(stop_on_failure = TRUE)'
```

- [ ] **Step 2: Verify the placer-isolation test passes against this file**

The test from Plan 5 grep's `.github/workflows/*.yml` for placer references. Run it now — `ci-tests.yml` does not mention the placer, so the test should still pass (no longer skipping).

```bash
Rscript -e 'devtools::test(filter = "placer-ci-isolation")'
```

Expected: 1 PASS (no skip — workflow files now exist).

- [ ] **Step 3: Local lint of YAML (if `yamllint` available)**

```bash
yamllint .github/workflows/ci-tests.yml || echo "yamllint not installed — visual review"
```

Expected: clean output, or visual review confirms structure matches the legacy patterns in `_legacy/lengjan-odds/.github/workflows/scrape.yml`.

- [ ] **Step 4: Commit**

```bash
git add .github/workflows/ci-tests.yml
git commit -m "ci: ci-tests.yml — run devtools::test() on push and PR

Triggers on push to main, pull requests, and manual dispatch.
Stan-bound and chromote-bound tests skip on CI. The placer-isolation
gate (test-placer-ci-isolation.R) is no longer skipped — workflow
files exist now and the test enforces local-only on every commit."
```

- [ ] **Step 5: Push and verify the run goes green**

```bash
git push
gh run watch
```

Expected: workflow run reports green; failed runs require investigation before proceeding to Task 8.

---

## Task 8: `.github/workflows/scrape-odds.yml` — cron 3×/day

**Files:**

- Create: `.github/workflows/scrape-odds.yml`

**Purpose:** Scheduled Lengjan-odds scrape. Runs `tar_make(names = starts_with("odds_"))` on `ubuntu-latest` with chromote (Chrome installed via `browser-actions/setup-chrome@v1`). Commits `data/facts/odds/` and `config/active_competitions.json` if anything changed.

Schedule: 08:00, 14:00, 20:00 UTC (matches legacy `_legacy/lengjan-odds/.github/workflows/scrape.yml`).

- [ ] **Step 1: Create the workflow file**

```yaml
# .github/workflows/scrape-odds.yml
name: Scrape Lengjan Odds

on:
  schedule:
    - cron: '0 8,14,20 * * *'   # 3x daily UTC
  workflow_dispatch: {}

permissions:
  contents: write

jobs:
  scrape:
    runs-on: ubuntu-latest
    timeout-minutes: 45

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}

    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: 'release'
          use-public-rspm: true

      - uses: browser-actions/setup-chrome@v1
        id: setup-chrome
        with:
          chrome-version: stable

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: |
            any::devtools
            any::targets
            any::tidyselect
            any::chromote

      - name: Restore targets cache
        uses: actions/cache@v4
        with:
          path: _targets
          key: targets-${{ hashFiles('_targets.R', 'R/**', 'config/**') }}
          restore-keys: targets-

      - name: Refresh active_competitions.json
        run: |
          Rscript -e 'devtools::load_all(); generate_active_competitions(load_leagues(), lookahead_days = 14L)'

      - name: Scrape odds
        env:
          CHROMOTE_CHROME: ${{ steps.setup-chrome.outputs.chrome-path }}
        run: |
          Rscript run.R --all --step odds

      - name: Commit if data changed
        run: |
          git config user.name  "github-actions[bot]"
          git config user.email "github-actions[bot]@users.noreply.github.com"
          git add data/facts/odds/ config/active_competitions.json
          git diff --cached --quiet || git commit -m "data: odds scrape $(date -u +%Y-%m-%dT%H:%MZ)"
          git pull --rebase origin main
          git push
```

- [ ] **Step 2: Re-run placer-isolation test**

```bash
Rscript -e 'devtools::test(filter = "placer-ci-isolation")'
```

Expected: PASS (still no placer references in workflows).

- [ ] **Step 3: Commit**

```bash
git add .github/workflows/scrape-odds.yml
git commit -m "ci: scrape-odds.yml — Lengjan odds 3x/day cron

Runs Rscript run.R --all --step odds, which expands to
tar_make(names = c('odds_basketball_iceland', 'odds_handball_iceland',
                   'odds_football_iceland')).

Refreshes config/active_competitions.json before scraping so leagues
without near-term fixtures are skipped.

Chrome installed via browser-actions/setup-chrome@v1 (CHROMOTE_CHROME
env var). Targets cache restored on best-effort hash of _targets.R +
R/ + config/."
```

- [ ] **Step 4: Push and watch the next scheduled run**

```bash
git push
# Trigger immediately to verify
gh workflow run scrape-odds.yml
gh run watch
```

Expected: run completes within ~15-30 min and either commits a data update or exits cleanly (no diff). If it fails, inspect logs and adjust before proceeding.

---

## Task 9: `.github/workflows/scrape-results.yml` — cron 1×/day

**Files:**

- Create: `.github/workflows/scrape-results.yml`

**Purpose:** Daily federation scrape (KSÍ / KKÍ / HSÍ). Runs `tar_make(names = starts_with("ingest_"))`. Commits `data/facts/{results,schedules}/`.

Schedule: 06:00 UTC (matches legacy livesport-data cadence).

- [ ] **Step 1: Create the workflow file**

```yaml
# .github/workflows/scrape-results.yml
name: Scrape Federation Results + Schedules

on:
  schedule:
    - cron: '0 6 * * *'   # 1x daily UTC
  workflow_dispatch: {}

permissions:
  contents: write

jobs:
  scrape:
    runs-on: ubuntu-latest
    timeout-minutes: 60

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}

    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: 'release'
          use-public-rspm: true

      - uses: browser-actions/setup-chrome@v1
        id: setup-chrome
        with:
          chrome-version: stable

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: |
            any::devtools
            any::targets
            any::tidyselect
            any::chromote

      - name: Restore targets cache
        uses: actions/cache@v4
        with:
          path: _targets
          key: targets-${{ hashFiles('_targets.R', 'R/**', 'config/**') }}
          restore-keys: targets-

      - name: Scrape results + schedules
        env:
          CHROMOTE_CHROME: ${{ steps.setup-chrome.outputs.chrome-path }}
        run: |
          Rscript run.R --all --step data

      - name: Commit if data changed
        run: |
          git config user.name  "github-actions[bot]"
          git config user.email "github-actions[bot]@users.noreply.github.com"
          git add data/facts/results/ data/facts/schedules/
          git diff --cached --quiet || git commit -m "data: federation scrape $(date -u +%Y-%m-%dT%H:%MZ)"
          git pull --rebase origin main
          git push
```

- [ ] **Step 2: Run the placer-isolation test**

```bash
Rscript -e 'devtools::test(filter = "placer-ci-isolation")'
```

Expected: PASS.

- [ ] **Step 3: Commit + push**

```bash
git add .github/workflows/scrape-results.yml
git commit -m "ci: scrape-results.yml — federation results + schedules 1x/day

Runs Rscript run.R --all --step data, which expands to
tar_make(names = c('ingest_basketball_iceland',
                   'ingest_handball_iceland',
                   'ingest_football_iceland')).

Commits data/facts/{results,schedules}/. HSI handball uses chromote;
KKI XLSX and KSI HTML do not, but Chrome is installed unconditionally
to keep the workflow uniform with scrape-odds.yml."

git push
gh workflow run scrape-results.yml
gh run watch
```

Expected: green run.

---

## Task 10: `.github/workflows/fit-and-publish.yml` — fit/decide/publish daily + workflow_dispatch

**Files:**

- Create: `.github/workflows/fit-and-publish.yml`

**Purpose:** Daily fit + decide + publish. Cmdstan installation is cached (`actions/cache` keyed by R + cmdstan version). Runs `Rscript run.R --all --step fit` then `--step decide` then `--step publish`. Commits `data/{beliefs/{latest,archive},decisions/{candidates,recommendations},publish}/`.

Wall-clock: ~30-90 min depending on whether cmdstan needs to compile fresh. The first run will hit ~90 min; subsequent runs are 30-50 min from cache.

- [ ] **Step 1: Create the workflow file**

```yaml
# .github/workflows/fit-and-publish.yml
name: Fit + Decide + Publish

on:
  schedule:
    - cron: '0 7 * * *'   # 1x daily UTC, after results scrape
  workflow_dispatch:
    inputs:
      league:
        description: 'Single league key (e.g. football_iceland) or "all"'
        required: false
        default: 'all'
      sex:
        description: 'male | female | all'
        required: false
        default: 'all'

permissions:
  contents: write

jobs:
  fit:
    runs-on: ubuntu-latest
    timeout-minutes: 180

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
      CMDSTAN_VERSION: '2.34.1'

    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: 'release'
          use-public-rspm: true

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: |
            any::devtools
            any::targets
            any::tidyselect
            any::cmdstanr

      - name: Cache CmdStan installation
        id: cache-cmdstan
        uses: actions/cache@v4
        with:
          path: ~/.cmdstan
          key: cmdstan-${{ env.CMDSTAN_VERSION }}-ubuntu-latest

      - name: Install CmdStan if not cached
        if: steps.cache-cmdstan.outputs.cache-hit != 'true'
        run: |
          Rscript -e 'cmdstanr::install_cmdstan(version = Sys.getenv("CMDSTAN_VERSION"), cores = 2)'

      - name: Restore targets cache
        uses: actions/cache@v4
        with:
          path: _targets
          key: targets-${{ hashFiles('_targets.R', 'R/**', 'config/**', 'Stan/**') }}
          restore-keys: targets-

      - name: Fit + decide + publish
        run: |
          LEAGUE="${{ github.event.inputs.league || 'all' }}"
          SEX="${{ github.event.inputs.sex || 'all' }}"
          if [ "$LEAGUE" = "all" ]; then
            Rscript run.R --all --step fit
            Rscript run.R --all --step decide
            Rscript run.R --all --step publish
          else
            Rscript run.R --league "$LEAGUE" --sex "$SEX" --step fit
            Rscript run.R --league "$LEAGUE" --sex "$SEX" --step decide
            Rscript run.R --league "$LEAGUE" --sex "$SEX" --step publish
          fi

      - name: Commit if data changed
        run: |
          git config user.name  "github-actions[bot]"
          git config user.email "github-actions[bot]@users.noreply.github.com"
          git add data/beliefs/ data/decisions/candidates/ \
                  data/decisions/recommendations/ data/publish/
          git diff --cached --quiet || git commit -m "data: fit/decide/publish $(date -u +%Y-%m-%dT%H:%MZ)"
          git pull --rebase origin main
          git push
```

- [ ] **Step 2: Re-run placer-isolation test**

```bash
Rscript -e 'devtools::test(filter = "placer-ci-isolation")'
```

Expected: PASS.

- [ ] **Step 3: Commit + push + manually dispatch the first run**

```bash
git add .github/workflows/fit-and-publish.yml
git commit -m "ci: fit-and-publish.yml — daily Stan fit + decide + publish

cron 07:00 UTC (after the 06:00 results scrape) + workflow_dispatch.
CmdStan cached via actions/cache keyed by version + OS; first run
~90 min compiling, subsequent runs 30-50 min.

run.R --step fit/decide/publish runs each step in sequence over the
selected scope (default --all). Commits data/beliefs/, data/decisions/
{candidates,recommendations}/, and data/publish/."

git push
gh workflow run fit-and-publish.yml
gh run watch
```

Expected: first run takes ~90 min; the resulting commit should advance the daily fit cycle. Inspect output JSONs visually to confirm parity with previous SPORTS_BACKUP_ROOT-based publishes.

---

## Task 11: metill-platform `pull-sports-data.yml` — hourly pull

**Files:**

- Create (in **metill-platform repo**, not sports): `.github/workflows/pull-sports-data.yml`
- Modify (metill-platform): `data/ithrottir/` — directory must exist (track with `.gitkeep`)

**Purpose:** Hourly cron in the **metill-platform** repo that clones `metill-is/sports`, copies `data/publish/**` into `data/ithrottir/`, commits, and pushes — which triggers Fly.io auto-deploy.

This task changes a **different** repo. The implementer subagent should clone metill-platform locally (or use `gh repo clone metill-is/metill-platform` in a temp dir), commit there, and push. The sports repo has zero changes from this task.

- [ ] **Step 1: Verify access and clone metill-platform**

```bash
gh repo clone metill-is/metill-platform /tmp/metill-platform-pull-task
cd /tmp/metill-platform-pull-task
ls .github/workflows/ 2>/dev/null || mkdir -p .github/workflows
mkdir -p data/ithrottir
```

- [ ] **Step 2: Create the workflow file**

```yaml
# metill-platform/.github/workflows/pull-sports-data.yml
name: Pull Sports Data

on:
  schedule:
    - cron: '0 * * * *'   # hourly
  workflow_dispatch: {}

permissions:
  contents: write

jobs:
  pull:
    runs-on: ubuntu-latest

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}

    steps:
      - uses: actions/checkout@v4

      - name: Clone metill-is/sports
        run: |
          git clone --depth 1 https://github.com/metill-is/sports.git /tmp/sports

      - name: Copy publish JSONs into data/ithrottir/
        run: |
          mkdir -p data/ithrottir
          rsync -a --delete /tmp/sports/data/publish/ data/ithrottir/

      - name: Commit if data changed
        run: |
          git config user.name  "github-actions[bot]"
          git config user.email "github-actions[bot]@users.noreply.github.com"
          git add data/ithrottir/
          git diff --cached --quiet || git commit -m "data: pull sports JSONs $(date -u +%Y-%m-%dT%H:%MZ)"
          git push
```

- [ ] **Step 3: Add `.gitkeep` to ensure directory exists**

```bash
touch data/ithrottir/.gitkeep
```

- [ ] **Step 4: Commit + push to metill-platform**

```bash
git add .github/workflows/pull-sports-data.yml data/ithrottir/.gitkeep
git commit -m "ci: pull-sports-data.yml — hourly pull of sports JSONs

Hourly cron clones metill-is/sports and rsyncs data/publish/ into
data/ithrottir/. A push to main triggers Fly.io auto-deploy.

Replaces the previous manual file-copy workflow."

git push
gh workflow run pull-sports-data.yml
gh run watch
```

Expected: green run; first commit replicates current `data/ithrottir/` snapshot exactly (rsync `--delete` reconciles anything that drifted).

- [ ] **Step 5: Return to the sports repo and document the integration**

The sports repo has no code changes from Task 11; only document the integration in CLAUDE.md (handled in Task 13). Switch back:

```bash
cd /Users/brynjolfurjonsson/sports
```

**Note:** Task 11 does **not** create a commit on `metill-is/sports`. The sports-side acceptance is "the metill-platform PR is green and data/ithrottir/ stays in sync." Skip immediately to Task 12.

---

## Task 12: Cutover — drop placer CSV dual-write

**Files:**

- Modify: `R/placer-ledger.R` (change `dual_write_csv` default to `FALSE`)
- Modify: `tests/testthat/test-placer-ledger.R` (default-path test)

**Purpose:** Per spec §4.3 step 7 and Plan 5's cutover plan: after a week of production runs where Parquet and CSV agree, the CSV dual-write goes away. Plan 6 flips the default to `FALSE` while keeping the parameter for opt-in regression-testing.

Since this is a default change rather than a feature removal, the cutover is reversible — call sites passing `dual_write_csv = TRUE` keep working.

- [ ] **Step 1: Update test to assert new default**

In `tests/testthat/test-placer-ledger.R`, modify the existing default-path test:

```r
test_that("append_to_ledger writes Parquet only by default (no CSV)", {
  tmp <- withr::local_tempdir()
  bet_row <- ledger_fixture_one_row()  # existing helper

  append_to_ledger(bet_row, root = file.path(tmp, "data"))

  expect_true(dir.exists(file.path(tmp, "data", "decisions", "ledger")))
  expect_false(file.exists(file.path(
    tmp, "_legacy", "sports",
    bet_row$sport, bet_row$country, "history", "bets_log.csv"
  )))
})

test_that("append_to_ledger writes CSV when dual_write_csv = TRUE", {
  tmp <- withr::local_tempdir()
  bet_row <- ledger_fixture_one_row()

  append_to_ledger(bet_row, root = file.path(tmp, "data"),
                   dual_write_csv = TRUE,
                   legacy_root = file.path(tmp, "_legacy", "sports"))

  expect_true(file.exists(file.path(
    tmp, "_legacy", "sports",
    bet_row$sport, bet_row$country, "history", "bets_log.csv"
  )))
})
```

- [ ] **Step 2: Run tests — confirm fail (default test now expects no CSV)**

```bash
Rscript -e 'devtools::test(filter = "placer-ledger")'
```

Expected: FAIL on "writes Parquet only by default" — current default still writes CSV.

- [ ] **Step 3: Flip the default in `R/placer-ledger.R`**

```r
append_to_ledger <- function(bet_row, root,
                             dual_write_csv = FALSE,
                             legacy_root = normalizePath(
                               file.path(root, "..", "_legacy", "sports"),
                               mustWork = FALSE
                             )) {
```

(Other than the default, no behavioural change.)

- [ ] **Step 4: Re-run tests**

```bash
Rscript -e 'devtools::test(filter = "placer-ledger")'
```

Expected: 2 PASS (default-no-CSV and opt-in-CSV).

- [ ] **Step 5: Update the docstring**

```r
#' @param dual_write_csv Default `FALSE` (Plan 6 cutover from Plan 5's
#'   dual-write). Set `TRUE` only for opt-in regression-testing — Parquet
#'   is the canonical store from Plan 6 onward.
```

- [ ] **Step 6: Run full test suite**

```bash
Rscript -e 'devtools::test()'
```

Expected: all green. Some Plan 5 placer tests may need a one-line update if they relied on the old default; fix as found.

- [ ] **Step 7: Commit**

```bash
git add R/placer-ledger.R tests/testthat/test-placer-ledger.R
git commit -m "feat: placer-ledger — drop CSV dual-write default (Plan 6 cutover)

dual_write_csv = FALSE by default. Parquet at data/decisions/ledger/
is canonical. dual_write_csv = TRUE remains as an opt-in for
regression-testing while the legacy CSV files exist on disk."
```

---

## Task 13: CLAUDE.md final update + plan-6 acceptance

**Files:**

- Modify: `CLAUDE.md` — Plan 6 → ✅ Complete; deprecate `scripts/{fit,decide,publish}_all.R`; add `_targets.R` + `run.R` to directory tree; document metill-platform integration; archive note for `_legacy/`
- Modify: `~/.claude/projects/-Users-brynjolfurjonsson-sports/memory/MEMORY.md` — note the Plan 6 cutover

**Purpose:** Lock in Plan 6 as ✅ Complete and update conventions to reflect `Rscript run.R` (not `scripts/*_all.R`) as the daily driver.

- [ ] **Step 1: Update the status table in CLAUDE.md**

In `CLAUDE.md`, change the status row for Plan 6:

```markdown
| **6: Orchestration + CI + cutover** | `_targets.R` DAG + `run.R` CLI, 4 GitHub workflows (ci-tests, scrape-odds, scrape-results, fit-and-publish), metill-platform `pull-sports-data.yml`, placer dual-write dropped | ✅ Complete |
```

- [ ] **Step 2: Update the directory-structure section**

Add to the directory tree:

```
sports/
├── _targets.R                      # NEW — DAG definition (5 layers)
├── run.R                           # NEW — thin CLI: --league/--sex/--step
├── R/
│   ├── ingest-lengjan-odds.R       # Port of _legacy/lengjan-odds/R/{parse,scrape,pipeline}.R
│   ├── schedule-active.R           # Generate config/active_competitions.json
│   ├── publish-pipeline.R          # publish_one() dispatcher used by _targets.R
│   └── ...
├── config/
│   └── active_competitions.json    # GENERATED at each scrape run
└── .github/workflows/
    ├── ci-tests.yml                # devtools::test() on push + PR
    ├── scrape-odds.yml             # cron 3x/day
    ├── scrape-results.yml          # cron 1x/day
    └── fit-and-publish.yml         # cron daily + workflow_dispatch
```

- [ ] **Step 3: Replace the "Quick reference" section**

```markdown
## Quick reference

```bash
# Development
Rscript -e 'devtools::load_all()'
Rscript -e 'devtools::test()'

# Run the pipeline (daily driver — uses {targets})
Rscript run.R --help
Rscript run.R --all --step odds                       # 3 leagues, scrape odds
Rscript run.R --league football_iceland --sex male --step fit
Rscript run.R --all --step fit                        # backfill all
Rscript run.R --all --step decide                     # decide layer
Rscript run.R --all --step publish                    # publish JSONs

# Targets directly (advanced)
Rscript -e 'targets::tar_make()'
Rscript -e 'targets::tar_make(names = c("fit_handball_iceland_male"))'
Rscript -e 'targets::tar_visnetwork()'                # DAG visualisation

# Local placer (NEVER on CI)
Rscript scripts/place_bets.R --dry-run
Rscript scripts/place_bets.R --live
Rscript scripts/preview_bets.R                        # no browser

# Rebuild sports.duckdb after fresh Parquet writes
Rscript -e 'sports::rebuild_duckdb()'
```
```

- [ ] **Step 4: Add a "Deprecated runners" note**

```markdown
## Deprecated runners

The `scripts/{fit,decide,publish}_all.R` and `scripts/backfill_ingest.R` scripts
are kept as ad-hoc escape hatches but are **deprecated** in favour of
`Rscript run.R`. They bypass the {targets} DAG and won't pick up cached
results — use them only for one-shot reruns where freshness matters more
than caching.
```

- [ ] **Step 5: Add a "metill-platform integration" note**

```markdown
## metill-platform integration

The `metill-is/metill-platform` repo runs `pull-sports-data.yml` hourly:
clones `metill-is/sports`, rsyncs `data/publish/` into `data/ithrottir/`,
commits if changed. A push to metill-platform triggers Fly.io auto-deploy.

Sports-side workflow:
1. fit-and-publish.yml writes data/publish/{...}/*.json
2. The push to main triggers metill-platform's pull-sports-data.yml
   within the next hour
3. metill-platform's commit deploys to fly.metill.is

To force a refresh without waiting for cron:
```bash
gh workflow run pull-sports-data.yml --repo metill-is/metill-platform
```
```

- [ ] **Step 6: Add a "Legacy archival" note**

```markdown
## Legacy archival

`_legacy/{sports,lengjan-odds,livesport-data,lengjan-bets}/` is kept on disk
for `git log --follow` history access. The corresponding GitHub repos are
archived (read-only) post-cutover:

```bash
gh repo archive metill-is/sports          # the OLD sports repo, not this one
gh repo archive metill-is/lengjan-odds
gh repo archive metill-is/livesport-data
gh repo archive metill-is/lengjan-bets
```

This is one-time post-cutover admin; the new monorepo is `metill-is/sports`
(replacing the archived predecessor).
```

- [ ] **Step 7: Run full test suite a final time**

```bash
Rscript -e 'devtools::test()'
```

Expected: all green.

- [ ] **Step 8: Commit**

```bash
git add CLAUDE.md
git commit -m "docs: CLAUDE.md — Plan 6 (orchestration + CI + cutover) complete

Status table all six plans -> Complete. New directory entries for
_targets.R, run.R, R/ingest-lengjan-odds.R, R/schedule-active.R,
R/publish-pipeline.R, .github/workflows/.

Quick-reference now leads with Rscript run.R (replaces scripts/*_all.R
which are deprecated as ad-hoc escape hatches).

metill-platform integration documented; legacy GitHub repos slated for
archival via gh repo archive (manual one-time admin)."
```

- [ ] **Step 9: Update workspace memory**

Append to `~/.claude/projects/-Users-brynjolfurjonsson-sports/memory/MEMORY.md` under "Pipeline Gotchas":

```markdown
- **Plan 6 cutover (2026-04-25)**: `Rscript run.R --step …` is the daily driver via `{targets}`. `scripts/{fit,decide,publish}_all.R` deprecated but kept for one-shot reruns. Placer CSV dual-write default flipped to FALSE; Parquet at `data/decisions/ledger/` is canonical. CI lives in `.github/workflows/{ci-tests,scrape-odds,scrape-results,fit-and-publish}.yml`. metill-platform pulls hourly via `pull-sports-data.yml`.
```

(The memory file is in the user's home, not the repo; no commit.)

- [ ] **Step 10: Final check — placer-isolation gate runs against all four workflows**

```bash
Rscript -e 'devtools::test(filter = "placer-ci-isolation")'
```

Expected: PASS — all four workflow files exist and none reference the placer.

- [ ] **Step 11: Push**

```bash
git push
```

Expected: green CI, the migration is complete.

---

## Acceptance criteria (Plan 6)

- [ ] `_targets.R` validates with `targets::tar_validate()` and the manifest contains the expected per-league × per-sex targets
- [ ] `Rscript run.R --help` prints usage; `--dry-run --step fit` lists target names
- [ ] All four GitHub workflows have completed at least one green run
- [ ] `metill-is/metill-platform` has `pull-sports-data.yml` and a green run
- [ ] `R/placer-ledger.R::append_to_ledger()` defaults to Parquet-only (`dual_write_csv = FALSE`)
- [ ] `tests/testthat/test-placer-ci-isolation.R` passes (no skip — workflows exist)
- [ ] `devtools::test()` is green locally and on CI
- [ ] CLAUDE.md status table shows Plans 1–6 all ✅

---

## Risks specific to Plan 6

- **CI fit wall-clock.** Football-iceland BVP fit on `ubuntu-latest` may run 60-80 minutes; the timeout is 180 min so there's headroom, but a model regression could blow this. Mitigation: cache CmdStan, cache `_targets/`. Fallback: schedule fit-and-publish weekly (cron `'0 7 * * 0'`) and rely on local nohup runs for daily refreshes.
- **CmdStan installation cost.** Cache miss = ~10-15 min compile. Cache hits when version + OS unchanged; bump `CMDSTAN_VERSION` deliberately and accept the compile cost.
- **Lengjan CSS-selector drift.** The legacy CSS classes (`lj1n6v0`, `uazl1c1`, etc.) were verbatim-ported. A Lengjan UI deploy will silently break the scraper before it's caught by stale-data alerts. Mitigation: monitor `data/facts/odds/` row counts and add an alert if a 3×/day scrape produces zero rows two cycles in a row. (Out-of-plan; file as a follow-up if it bites.)
- **active_competitions.json race.** The scrape-odds and scrape-results workflows both regenerate the file. If they run concurrently they could write inconsistent state; in practice cron times don't overlap (08/14/20 vs 06). Mitigation: only `scrape-odds.yml` runs `generate_active_competitions()` explicitly; `scrape-results.yml` accepts whatever's on disk.
- **CI-fit and ci-tests are different beasts.** `ci-tests.yml` runs `devtools::test()` on every push; `fit-and-publish.yml` runs Stan. Failure modes are unrelated — a broken Stan model fails fit-and-publish but not ci-tests; a broken R helper fails both.
- **Subtree merge cleanup.** `_legacy/` directories aren't deleted; spec §4.3 step 11 says "archive old repos" not "delete legacy/". Plan 6 only flags the GitHub-side archive command. Disk cleanup is a future Plan 7+ if ever.

---

## Why Plan 6 is the last plan

Plans 1–5 built the static substrate (storage, ingest, model, decide+publish, placer). Plan 6 wires them together into a self-driving system: cron-triggered scrape → fit → decide → publish, with the placer running locally on a human's laptop. Every component covered in the spec §3 is now production-instantiated. Phase 2 (paused non-Icelandic leagues, livesport-data plugin, walk-forward backtester) is deferred per spec §2 — those are post-migration projects, not migration steps.
