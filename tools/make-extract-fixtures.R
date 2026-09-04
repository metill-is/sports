# Regenerate every committed test fixture for the bb/hb metill-parity harness.
#
#   Rscript tools/make-extract-fixtures.R            # facts + 2DT extracts
#   Rscript tools/make-extract-fixtures.R --golden   # football golden hashes
#
# WHY the odd self-execution guard: in a git worktree `here::here()` resolves to
# the MAIN checkout, so a top-level `devtools::load_all(here::here())` would load
# a different package than the one being edited and silently regenerate fixtures
# against it. The package root is derived from THIS file's own path instead, and
# the load happens only when the file is run as a script. Nothing at top level
# touches the package, so a test can `sys.source()` this file to inspect it.

FIXTURE_SEASONS <- c(2099L, 2100L)
FIXTURE_END_DATE <- as.Date("2100-01-15")
FIXTURE_FIT_DATE <- as.Date("2100-01-01")
FIXTURE_N_DRAWS <- 50L

# Division -> team count. Football BD carries the split-season group sizes from
# config/leagues.yml (male 6/6, female 6/4), so it needs 12 / 10 teams.
# MUST stay identical to tests/testthat/helper-fixture-facts.R.
FIXTURE_DIVISIONS <- list(
  # BD / OD are 4 teams, not 6: team_strengths_quantiles is 9 cells x 99
  # quantiles per team, and at 6 teams the committed extracts tree came to
  # 318 KB -- past the 250 KB budget. Every non-top division stays at 6.
  basketball = list(male = c(BD = 4L, `1D` = 6L), female = c(BD = 4L, `1D` = 6L)),
  handball   = list(male = c(OD = 4L, G66 = 6L), female = c(OD = 4L, G66 = 6L)),
  football   = list(
    male   = c(BD = 12L, LD1 = 6L, LD2 = 6L, LD3 = 6L, CUP = 4L),
    female = c(BD = 10L, LD1 = 6L, LD2 = 6L, CUP = 4L)
  )
)

# Deterministic fixture team names for one (sport, sex, division) cell.
fixture_division_teams <- function(sport, sex, division) {
  n <- FIXTURE_DIVISIONS[[sport]][[sex]][[division]]
  stopifnot(!is.null(n))
  sprintf(
    "%s%s %s %02d",
    toupper(substr(sport, 1L, 2L)), toupper(substr(sex, 1L, 1L)),
    division, seq_len(n)
  )
}

# Resolve the package root from this script's own --file= argument. Returns NULL
# when the file was source()d rather than run, which disables self-execution.
.fixture_gen_pkg_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (length(hit) != 1L) return(NULL)
  script <- sub("^--file=", "", hit)
  if (basename(script) != "make-extract-fixtures.R") return(NULL)
  normalizePath(file.path(dirname(script), ".."), mustWork = FALSE)
}

# Single round-robin per (sport, sex, division, season). The home team of each
# pair is indexed lower than the away team and wins by a margin that also orders
# goal difference, so the realised table equals the team order -- a
# deterministic standings target.
#
# Matches are packed at most 10 match-days deep (`per_day` below) so that even
# football's 12-team BD round-robin (66 matches) finishes well before
# FIXTURE_END_DATE. If it spilled past that date prepare_data() would fold the
# overflow into pred_d via its upcoming-from-results union, making N_pred depend
# on the division size.
.fixture_results_one <- function(sport, sex, division, season, start_date) {
  teams <- fixture_division_teams(sport, sex, division)
  grid <- utils::combn(seq_along(teams), 2L)
  n <- ncol(grid)
  hi <- grid[1L, ]
  ai <- grid[2L, ]
  per_day <- ceiling(n / 10L)
  day <- ceiling(seq_len(n) / per_day)
  base <- switch(sport, basketball = 80L, handball = 24L, football = 1L)
  tibble::tibble(
    sport      = sport,
    country    = "iceland",
    sex        = sex,
    season     = as.integer(season),
    match_date = start_date + day,
    home_team  = teams[hi],
    away_team  = teams[ai],
    home_score = as.integer(base + length(teams) - hi + 1L),
    away_score = as.integer(base + length(teams) - ai),
    division   = division,
    round      = as.integer(day)
  )
}

# All committed synthetic results rows.
.fixture_results <- function() {
  out <- list()
  for (sport in names(FIXTURE_DIVISIONS)) {
    for (sex in names(FIXTURE_DIVISIONS[[sport]])) {
      for (division in names(FIXTURE_DIVISIONS[[sport]][[sex]])) {
        # 2099 gives prepare_data a second season (season_first / N_seasons);
        # 2100 is the current season the publishers summarise.
        out[[length(out) + 1L]] <- .fixture_results_one(
          sport, sex, division, 2099L, as.Date("2099-11-02")
        )
        out[[length(out) + 1L]] <- .fixture_results_one(
          sport, sex, division, 2100L, as.Date("2100-01-02")
        )
      }
    }
  }
  dplyr::bind_rows(out)
}

# Three upcoming matches per (sport, sex, division), all inside
# [FIXTURE_END_DATE, FIXTURE_END_DATE + 14] so prepare_data()'s DEFAULT
# schedule_horizon_days = 14L picks them up -- the publishers call
# prepare_data() internally at the default and take no prep= argument.
.fixture_schedules <- function() {
  out <- list()
  for (sport in names(FIXTURE_DIVISIONS)) {
    for (sex in names(FIXTURE_DIVISIONS[[sport]])) {
      for (division in names(FIXTURE_DIVISIONS[[sport]][[sex]])) {
        teams <- fixture_division_teams(sport, sex, division)
        out[[length(out) + 1L]] <- tibble::tibble(
          sport        = sport,
          country      = "iceland",
          sex          = sex,
          season       = 2100L,
          match_date   = as.Date(c("2100-01-16", "2100-01-18", "2100-01-20")),
          home_team    = teams[c(1L, 3L, 2L)],
          away_team    = teams[c(2L, 4L, 3L)],
          division     = division,
          round        = c(90L, 91L, 92L),
          kickoff_time = c("19:15", "19:15", "17:00")
        )
      }
    }
  }
  dplyr::bind_rows(out)
}

# Generate the committed 2DT extracts tree by running the REAL extractors
# against a stub fit -- so the fixture's schema is the extractor's own output,
# not a hand-written guess that can drift from it.
.write_2dt_extract_fixtures <- function(dest, facts_root, stub_env) {
  extracts_root <- file.path(dest, "extracts")
  unlink(file.path(extracts_root, "sport=basketball"), recursive = TRUE)
  unlink(file.path(extracts_root, "sport=handball"), recursive = TRUE)

  cfg <- list(
    basketball = list(key = "basketball_iceland", fn = extract_basketball_iceland),
    handball   = list(key = "handball_iceland", fn = extract_handball_iceland)
  )
  leagues <- load_leagues()
  for (sport in names(cfg)) {
    for (sex in c("male", "female")) {
      league <- leagues[[cfg[[sport]]$key]]
      prep <- prepare_data(league, sex, end_date = FIXTURE_END_DATE, root = facts_root)
      # n_rounds, like n_pred, is sized from THIS prepare_data() call: the
      # round-strength trajectory indexes offense[global_round, k] with an index
      # derived from the same results set.
      fit <- stub_env$stub_fit(stub_env$stub_2dt_draws(
        prep$teams$team, nrow(prep$pred_d), n_draws = FIXTURE_N_DRAWS,
        n_rounds = prep$stan_data$N_rounds
      ))
      cfg[[sport]]$fn(
        fit = fit, league = league, sex = sex,
        fit_date = FIXTURE_FIT_DATE,
        end_date = FIXTURE_END_DATE,
        root = facts_root,
        extracts_root = extracts_root,
        prep = prep
      )
    }
  }
  list.files(extracts_root, recursive = TRUE, full.names = TRUE)
}

# stub_fit() / stub_2dt_draws() live in the test helper, not the package, so
# the generator sources it into a private env rather than duplicating it.
.fixture_stub_env <- function(dest) {
  env <- new.env(parent = globalenv())
  helper <- file.path(dirname(dest), "helper-stub-fit.R")
  stopifnot(file.exists(helper))
  sys.source(helper, envir = env)
  env
}

# Regenerate tests/testthat/fixtures/golden/football-publish-hashes.csv.
#
# Football's extracts partition is NOT committed -- see
# build_football_extracts_fixture() in helper-extract-fixtures.R for why -- so
# only the hash manifest is stored, and both this generator and the test
# rebuild the partition from the same deterministic function.
make_football_golden_hashes <- function(dest = NULL) {
  root <- .fixture_gen_pkg_root()
  stopifnot(!is.null(root))
  if (is.null(dest)) {
    dest <- file.path(root, "tests", "testthat", "fixtures", "golden")
  }
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  helpers <- file.path(root, "tests", "testthat")
  env <- new.env(parent = environment())
  for (h in c("helper-fixture-facts.R", "helper-stub-fit.R",
              "helper-extract-fixtures.R")) {
    sys.source(file.path(helpers, h), envir = env)
  }

  facts_root <- file.path(tempdir(), paste0("golden-facts-", Sys.getpid()))
  unlink(facts_root, recursive = TRUE)
  write_table(.fixture_results(), "results", root = facts_root)
  write_table(.fixture_schedules(), "schedules", root = facts_root)

  extracts_root <- file.path(tempdir(), paste0("golden-extracts-", Sys.getpid()))
  out <- file.path(tempdir(), paste0("golden-out-", Sys.getpid()))
  unlink(c(extracts_root, out), recursive = TRUE)
  league <- load_leagues()[["football_iceland"]]
  for (sex in c("male", "female")) {
    env$build_football_extracts_fixture(facts_root, extracts_root, sex)
    extracted <- read_extracted_iceland(
      league, sex = sex, fit_date = FIXTURE_FIT_DATE, extracts_root = extracts_root
    )
    suppressWarnings(publish_football_iceland(
      extracted = extracted, league = league, sex = sex,
      end_date = FIXTURE_END_DATE, root = facts_root,
      output_root = out, extracts_root = extracts_root,
      archive_root = file.path(tempdir(), paste0("golden-archive-", Sys.getpid()))
    ))
  }
  produced <- list.files(
    file.path(out, "football"), pattern = "\\.json$",
    recursive = TRUE, full.names = TRUE
  )
  manifest <- data.frame(
    file = sub(paste0("^", out, "/"), "", produced),
    sha256 = vapply(produced, env$publish_json_digest, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  manifest <- manifest[order(manifest$file), , drop = FALSE]
  path <- file.path(dest, "football-publish-hashes.csv")
  utils::write.csv(manifest, path, row.names = FALSE)
  message("make_football_golden_hashes: ", nrow(manifest), " payloads -> ", path)
  invisible(path)
}

# The three basketball cells whose post-season is embedded in the league
# division, plus the irregular fourth. KKI packages urslitakeppni as extra
# rounds inside the SAME season_id (R/ingest-kki-basketball.R:23-24), so
# `division == "BD"` carries the regular season AND the playoffs; without a
# regular-season cut the published league table is simulated on post-season
# points -- a silently wrong table, not a visible error.
#
# Measured 2026-09-04 against data/facts/results, season 2026:
#   male   BD  162 rows, 12 teams, 132 regular (22 rounds) + 30 post-season
#   male   1D  159 rows, 12 teams, 132 regular (22 rounds) + 27 post-season
#   female BD  137 rows, 10 teams,  90 regular (18 rounds) + 47 post-season
#   female 1D   98 rows, 11 teams -- the deliberately irregular cell: no
#              configured expected_meetings, so n_rounds resolves off the
#              schedule (24) and the round floor is 6
#
# Real team names are kept on purpose. The regular-season boundary is a
# property of the real federation calendar; an anonymised copy would prove
# nothing about it.
# `root` is explicit so the generator also runs when this file is source()d
# rather than executed: `.fixture_gen_pkg_root()` derives the package root from
# the script's own --file= argument and is NULL under source().
make_playoff_overhang_fixture <- function(dest = NULL,
                                          root = .fixture_gen_pkg_root()) {
  stopifnot(!is.null(root))
  if (is.null(dest)) {
    dest <- file.path(root, "tests", "testthat", "fixtures", "facts")
  }
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  rows <- arrow::open_dataset(file.path(root, "data", "facts", "results")) |>
    dplyr::filter(
      country == "iceland",
      sport == "basketball",
      season == 2026L,
      division %in% c("BD", "1D")
    ) |>
    dplyr::collect() |>
    dplyr::arrange(sex, division, match_date, home_team, away_team)

  stopifnot(nrow(rows) == 556L)
  path <- file.path(dest, "playoff-overhang.parquet")
  arrow::write_parquet(rows, path)
  message(sprintf(
    "make_playoff_overhang_fixture: %d rows, %s KB -> %s",
    nrow(rows), format(round(file.info(path)$size / 1024, 1)), path
  ))
  invisible(path)
}

# Regenerate all committed fixtures.
make_extract_fixtures <- function(dest = NULL, quiet = FALSE) {
  if (is.null(dest)) {
    root <- .fixture_gen_pkg_root()
    stopifnot(!is.null(root))
    dest <- file.path(root, "tests", "testthat", "fixtures")
  }
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  facts_dir <- file.path(dest, "facts")
  dir.create(facts_dir, recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(.fixture_results(), file.path(facts_dir, "results.parquet"))
  arrow::write_parquet(.fixture_schedules(), file.path(facts_dir, "schedules.parquet"))
  files <- c(
    file.path(facts_dir, "results.parquet"),
    file.path(facts_dir, "schedules.parquet"),
    make_playoff_overhang_fixture(facts_dir)
  )

  facts_root <- file.path(tempdir(), paste0("fixture-facts-", Sys.getpid()))
  unlink(facts_root, recursive = TRUE)
  dir.create(facts_root, recursive = TRUE, showWarnings = FALSE)
  write_table(.fixture_results(), "results", root = facts_root)
  write_table(.fixture_schedules(), "schedules", root = facts_root)

  extract_files <- .write_2dt_extract_fixtures(
    dest, facts_root, .fixture_stub_env(dest)
  )
  files <- c(files, extract_files)
  bytes <- sum(file.info(files)$size)

  if (!quiet) {
    message(sprintf(
      "make_extract_fixtures: %d files, %s KB (extracts tree: %s KB / 250 KB budget)",
      length(files), format(round(bytes / 1024, 1)),
      format(round(sum(file.info(extract_files)$size) / 1024, 1))
    ))
  }
  invisible(list(bytes = bytes, files = files))
}

if (sys.nframe() == 0L && !is.null(.fixture_gen_pkg_root())) {
  devtools::load_all(.fixture_gen_pkg_root(), quiet = TRUE)
  if ("--golden" %in% commandArgs(trailingOnly = TRUE)) {
    make_football_golden_hashes()
  } else {
    make_extract_fixtures()
  }
}
