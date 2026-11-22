# The forecast archive: every pre-match forecast the site published, kept
# forever.
#
# WHY: the extracts tree is the only place a past fit's forecasts live, and
# prune_extracts() deletes old partitions on every fit (keep 14 days, three per
# cell and one per season). After that nothing records what the site told
# people before a match, so a published model can never be scored against what
# it actually published -- the v2.0 re-examination due July 2027 needs exactly
# that. The archive is append-only: a forecast, once stored, is never changed
# or removed, every write is a new file, and nothing prunes it.

.fa_row <- function(...,
                    sport = "football", country = "iceland", sex = "male",
                    fit_date = as.Date("2100-01-15"), fit_id = "fit-a",
                    published_to = "football/iceland/karla-bd/next_games.json",
                    division = "BD", match_date = as.Date("2100-01-16"),
                    home_team = "Valur", away_team = "KR",
                    kickoff_time = "19:15",
                    p_home_win = 0.5, p_draw = 0.25, p_away_win = 0.25,
                    xg_home = 1.6, xg_away = 1.1,
                    engine = "stan", stan_model = "football_iceland/m.stan",
                    model_version = NA_character_,
                    published_at = as.POSIXct("2100-01-15 08:00:00", tz = "UTC"),
                    code_commit = "abc123") {
  tibble::tibble(
    sport = sport, country = country, sex = sex, fit_date = fit_date, fit_id = fit_id,
    published_to = published_to, division = division, match_date = match_date,
    home_team = home_team, away_team = away_team, kickoff_time = kickoff_time,
    p_home_win = p_home_win, p_draw = p_draw, p_away_win = p_away_win,
    xg_home = xg_home, xg_away = xg_away,
    engine = engine, stan_model = stan_model, model_version = model_version,
    published_at = published_at, code_commit = code_commit
  )
}

.fa_partition_files <- function(root) {
  list.files(file.path(root, "archive", "forecasts"),
    pattern = "[.]parquet$", recursive = TRUE, full.names = TRUE
  )
}

.fa_md5 <- function(root) {
  f <- .fa_partition_files(root)
  stats::setNames(unname(tools::md5sum(f)), sub(paste0("^", root, "/"), "", f))
}

test_that("the forecast archive is a registered store under data/archive/forecasts, one partition per write", {
  expect_true("forecast_archive" %in% names(schemas()))
  expect_identical(table_subdir("forecast_archive"), c("archive", "forecasts"))
  expect_identical(
    table_partitions()[["forecast_archive"]],
    c("sport", "country", "sex", "fit_date", "batch")
  )
  expect_true("forecast_archive" %in% names(append_only_tables()))
  expect_setequal(
    schemas()$forecast_archive$names,
    names(.fa_row())
  )
})

test_that("a first archive write stores every new forecast", {
  root <- withr::local_tempdir()
  rows <- dplyr::bind_rows(
    .fa_row(),
    .fa_row(home_team = "FH", away_team = "Fram", p_home_win = 0.4, p_draw = 0.3, p_away_win = 0.3)
  )
  res <- suppressMessages(archive_forecasts(rows, root = root))

  expect_equal(nrow(res$appended), 2L)
  expect_equal(res$unchanged, 0L)
  expect_equal(nrow(res$refused), 0L)

  stored <- read_forecast_archive(root)
  expect_equal(nrow(stored), 2L)
  expect_setequal(stored$home_team, c("Valur", "FH"))
  expect_equal(stored$p_home_win[stored$home_team == "Valur"], 0.5)
  expect_equal(stored$fit_date, rep(as.Date("2100-01-15"), 2))
  expect_s3_class(stored$published_at, "POSIXct")
})

test_that("republishing the same forecasts writes nothing", {
  root <- withr::local_tempdir()
  rows <- dplyr::bind_rows(.fa_row(), .fa_row(home_team = "FH", away_team = "Fram"))
  suppressMessages(archive_forecasts(rows, root = root))
  before <- .fa_md5(root)

  # The same fit republished hours later: decide-publish runs on every odds
  # scrape, so this is the common case. Only the publication time differs.
  again <- rows
  again$published_at <- again$published_at + 3600
  again$code_commit <- "def456"
  res <- suppressMessages(archive_forecasts(again, root = root))

  expect_equal(nrow(res$appended), 0L)
  expect_equal(res$unchanged, 2L)
  expect_equal(nrow(res$refused), 0L)
  expect_identical(.fa_md5(root), before)
  stored <- read_forecast_archive(root)
  expect_equal(unique(stored$code_commit), "abc123")
})

test_that("a different forecast from the same fit, file and match is refused and the stored row is kept", {
  root <- withr::local_tempdir()
  suppressMessages(archive_forecasts(.fa_row(), root = root))
  before <- .fa_md5(root)

  # Same fit (same extract partition), different published numbers: only a
  # change to the publisher's own output does that.
  changed <- .fa_row(p_home_win = 0.55, p_away_win = 0.2)
  expect_warning(
    res <- suppressMessages(archive_forecasts(changed, root = root)),
    "refused"
  )
  expect_equal(nrow(res$refused), 1L)
  expect_equal(res$refused$home_team, "Valur")
  expect_equal(nrow(res$appended), 0L)
  expect_identical(.fa_md5(root), before)
  expect_equal(read_forecast_archive(root)$p_home_win, 0.5)
})

test_that("new matches from the same fit are appended and stored rows are left as they were", {
  root <- withr::local_tempdir()
  first <- .fa_row()
  suppressMessages(archive_forecasts(first, root = root))
  stored_before <- read_forecast_archive(root)

  # The publish window moves forward a day while the fit stays the same, so a
  # fixture the fit predicted enters next_games.json for the first time. A
  # changed row in the same write is refused without stopping the new one.
  later <- dplyr::bind_rows(
    .fa_row(p_home_win = 0.6, p_away_win = 0.15),
    .fa_row(
      home_team = "Stjarnan", away_team = "KA", match_date = as.Date("2100-01-29"),
      published_at = as.POSIXct("2100-01-16 08:00:00", tz = "UTC")
    )
  )
  files_before <- .fa_md5(root)
  res <- suppressWarnings(suppressMessages(archive_forecasts(later, root = root)))
  # The first write's file is untouched; the new row is in a new one.
  expect_identical(.fa_md5(root)[names(files_before)], files_before)
  expect_length(.fa_md5(root), length(files_before) + 1L)
  expect_equal(nrow(res$appended), 1L)
  expect_equal(res$appended$home_team, "Stjarnan")
  expect_equal(nrow(res$refused), 1L)

  stored <- read_forecast_archive(root)
  expect_equal(nrow(stored), 2L)
  kept <- stored[stored$home_team == "Valur", names(stored_before)]
  expect_equal(as.data.frame(kept), as.data.frame(stored_before))
  expect_equal(
    stored$published_at[stored$home_team == "Stjarnan"],
    as.POSIXct("2100-01-16 08:00:00", tz = "UTC")
  )
})

test_that("a fit's id is its forecasts, not its file's bytes", {
  fit_dir <- withr::local_tempdir()
  forecasts <- tibble::tibble(
    home_team = c("Valur", "FH"), away_team = c("KR", "Fram"),
    match_date = as.Date("2100-01-16"), home_goals = c(1, 2), away_goals = c(0, 2), count = c(10L, 7L)
  )
  write <- function(d, compression) arrow::write_parquet(d, file.path(fit_dir, "predicted_matches.parquet"), compression = compression)
  write(forecasts, "snappy")
  id <- forecast_archive_fit_id(fit_dir)
  expect_false(is.na(id))
  # The same forecasts rewritten (rows reordered, another codec): the same fit.
  write(forecasts[2:1, ], "gzip")
  expect_identical(forecast_archive_fit_id(fit_dir), id)
  # Another forecast: another fit.
  forecasts$count[1] <- 11L
  write(forecasts, "snappy")
  expect_false(identical(forecast_archive_fit_id(fit_dir), id))
  expect_identical(forecast_archive_fit_id(withr::local_tempdir()), NA_character_)
})

test_that("a same-day refit is archived beside the first fit, told apart by fit_id", {
  root <- withr::local_tempdir()
  suppressMessages(archive_forecasts(.fa_row(), root = root))
  # A second fit on the same day overwrites the fit_date's extract partition;
  # what the site shows from then on is another fit's forecast.
  refit <- .fa_row(fit_id = "fit-b", p_home_win = 0.55, p_away_win = 0.2,
    published_at = as.POSIXct("2100-01-15 18:00:00", tz = "UTC"))
  res <- suppressMessages(archive_forecasts(refit, root = root))
  expect_equal(nrow(res$appended), 1L)
  expect_equal(nrow(res$refused), 0L)
  stored <- read_forecast_archive(root)
  expect_equal(nrow(stored), 2L)
  expect_setequal(stored$fit_id, c("fit-a", "fit-b"))
  expect_equal(stored$p_home_win[stored$fit_id == "fit-a"], 0.5)
})

test_that("write_table refuses a forecast-archive write into a stored partition or against a stored row", {
  root <- withr::local_tempdir()
  rows <- dplyr::bind_rows(.fa_row(), .fa_row(home_team = "FH", away_team = "Fram"))
  suppressMessages(archive_forecasts(rows, root = root, batch = "b1"))
  before <- .fa_md5(root)

  # Into the stored partition, even with the same rows.
  expect_error(write_table(transform(rows, batch = "b1"), "forecast_archive", root = root), "refused")
  # A new partition that contradicts a stored row under the same key.
  changed <- rows[1, ]
  changed$p_home_win <- 0.9
  expect_error(write_table(transform(changed, batch = "b2"), "forecast_archive", root = root), "refused")
  # Two values for one key in one write.
  expect_error(write_table(transform(dplyr::bind_rows(rows[1, ], changed), batch = "b3"), "forecast_archive", root = root), "refused")
  expect_identical(.fa_md5(root), before)

  # New keys in a new partition are fine; so is a stored forecast repeated
  # unchanged by a writer that could not see it (readers keep the earliest).
  more <- .fa_row(home_team = "\u00cdA", away_team = "Vestri")
  again <- rows[1, ]
  again$published_at <- again$published_at + 60
  expect_no_error(write_table(transform(dplyr::bind_rows(more, again), batch = "b4"), "forecast_archive", root = root))
  expect_equal(nrow(read_forecast_archive(root, distinct = FALSE)), 4L)
  stored <- read_forecast_archive(root)
  expect_equal(nrow(stored), 3L)
  expect_equal(stored$published_at[stored$home_team == "Valur"], rows$published_at[1])
})

test_that("two runs that could not see each other's rows both keep theirs", {
  # decide-publish's queued run checks out the commit of its trigger, without
  # the rows the run before it pushed; republish runs beside decide-publish.
  # Each writes new files only, so git keeps both; nothing is lost and the
  # earliest publication of a shared forecast is the one read back.
  root_a <- withr::local_tempdir()
  root_b <- withr::local_tempdir()
  shared <- .fa_row()
  only_a <- .fa_row(home_team = "FH", away_team = "Fram")
  only_b <- .fa_row(home_team = "KA", away_team = "Fylkir")
  suppressMessages(archive_forecasts(dplyr::bind_rows(shared, only_a), root = root_a))
  late <- function(r) transform(r, published_at = r$published_at + 600)
  suppressMessages(archive_forecasts(dplyr::bind_rows(late(shared), late(only_b)), root = root_b))
  # What git does with two commits that add different files: the union.
  merged <- withr::local_tempdir()
  for (r in c(root_a, root_b)) file.copy(file.path(r, "archive"), merged, recursive = TRUE)
  expect_length(.fa_partition_files(merged), 2L)
  stored <- read_forecast_archive(merged)
  expect_setequal(stored$home_team, c("Valur", "FH", "KA"))
  expect_equal(stored$published_at[stored$home_team == "Valur"], shared$published_at)
})

test_that("only forecasts published before kick-off are archived", {
  publish_root <- withr::local_tempdir()
  cell <- file.path(publish_root, "handball", "iceland", "karla-od")
  dir.create(cell, recursive = TRUE)
  match <- function(home, away, date, p) {
    list(
      date = date, venue = NULL, division = "OD", division_code = "OD",
      home = home, away = away, mean_home_goals = 30.5, mean_away_goals = 28.25,
      mean_goal_diff = 2.25, p_home_win = p, p_draw = 0.05, p_away_win = 0.95 - p,
      goal_diff_distribution = list()
    )
  }
  jsonlite::write_json(
    list(generated_at = "2100-01-16T19:00:00+0000", matches = list(
      match("Valur", "FH", "2100-01-16", 0.5), # kick-off 18:00: already playing
      match("KA", "Fram", "2100-01-16", 0.4), # kick-off 19:30: still to come
      match("Haukar", "HK", "2100-01-16", 0.45), # kick-off unknown, today
      match("Selfoss", "ÍR", "2100-01-17", 0.6) # kick-off unknown, tomorrow
    )),
    file.path(cell, "next_games.json"),
    auto_unbox = TRUE, null = "null", digits = 5
  )
  kickoffs <- tibble::tibble(
    match_date = as.Date("2100-01-16"),
    home_team = c("Valur", "KA"), away_team = c("FH", "Fram"),
    kickoff_at = as.POSIXct(c("2100-01-16 18:00:00", "2100-01-16 19:30:00"), tz = "UTC")
  )
  rows <- forecast_archive_rows(
    publish_root = publish_root, sport = "handball", country = "iceland",
    sex = "male", cells = "karla-od", fit_date = as.Date("2100-01-16"), fit_id = "fit-a",
    fit_meta = tibble::tibble(engine = "stan", stan_model = "m.stan", model_version = NA_character_),
    kickoffs = kickoffs,
    published_at = as.POSIXct("2100-01-16 19:00:00", tz = "UTC")
  )
  # A same-day forecast with no known kick-off may have been published after
  # the game began, so it is not archived; tomorrow's is.
  expect_setequal(rows$home_team, c("KA", "Selfoss"))
  expect_equal(rows$kickoff_time[rows$home_team == "KA"], "19:30")
  expect_equal(rows$kickoff_time[rows$home_team == "Selfoss"], NA_character_)
  expect_equal(rows$p_home_win[rows$home_team == "KA"], 0.4)
  expect_equal(rows$xg_home, c(30.5, 30.5))
  expect_equal(unique(rows$fit_id), "fit-a")
  expect_equal(unique(rows$published_to), "handball/iceland/karla-od/next_games.json")
})

test_that("kick-offs come from the schedule, else from the newest odds scrape under federation names", {
  root <- withr::local_tempdir()
  write_table(tibble::tibble(
    sport = "handball", country = "iceland", sex = "male", season = 2100L,
    match_date = as.Date("2100-01-16"), home_team = c("Valur", "KA"), away_team = c("FH", "Fram"),
    division = "OD", round = 1L, kickoff_time = c("19:30", NA)
  ), "schedules", root = root)
  scrape <- function(at, kickoff) tibble::tibble(
    sport = "handball", country = "iceland", scraped_at = as.POSIXct(at, tz = "UTC"),
    match_date = as.Date("2100-01-16"), home_team = c("KA Akureyri", "Valur"), away_team = c("Fram", "FH"),
    market = "moneyline", outcome = "home", line = NA_real_, odds = 2,
    sex = "male", kickoff_at = as.POSIXct(kickoff, tz = "UTC")
  )
  write_table(scrape("2100-01-15 08:17:00", c("2100-01-16 18:00:00", "2100-01-16 20:00:00")), "odds", root = root)
  write_table(scrape("2100-01-16 08:17:00", c("2100-01-16 18:15:00", "2100-01-16 20:00:00")), "odds", root = root)
  league <- list(
    sport = "handball", country = "iceland",
    lengjan = list(team_names = list(male = list(KA = "KA Akureyri", Valur = "Valur", Fram = "Fram", FH = "FH")))
  )
  k <- forecast_archive_kickoffs(league, "male", root, from = as.Date("2100-01-16"))
  at <- function(home) format(k$kickoff_at[k$home_team == home], "%H:%M", tz = "UTC")
  expect_equal(at("Valur"), "19:30") # the schedule wins
  expect_equal(at("KA"), "18:15") # the newest scrape, its Lengjan name mapped
  expect_equal(nrow(k), 2L)
})

test_that("publish_one archives every published football fixture with the numbers it published", {
  root <- fixture_facts_root()
  extracts_root <- file.path(root, "beliefs", "extracts")
  league <- load_leagues()[["football_iceland"]]
  static <- league[c("sport", "country", "sexes", "active", "stan_model", "data_source")]

  for (sex in c("male", "female")) {
    build_football_extracts_fixture(root, extracts_root, sex)
    suppressMessages(suppressWarnings(publish_one(
      static, league$betting, "football_iceland", sex,
      root = root, validate = FALSE, end_date = FIXTURE_END_DATE,
      archive_forecasts = TRUE
    )))
  }

  out <- file.path(root, "publish")
  files <- list.files(file.path(out, "football"), pattern = "^next_games[.]json$", recursive = TRUE, full.names = TRUE)
  published <- dplyr::bind_rows(lapply(files, function(f) {
    m <- jsonlite::fromJSON(f, simplifyVector = TRUE)$matches
    if (!length(m) || !NROW(m)) {
      return(NULL)
    }
    meta <- jsonlite::fromJSON(file.path(dirname(f), "meta.json"))
    tibble::tibble(
      published_to = sub(paste0("^", out, "/"), "", f),
      fit_date = as.Date(meta$fit_date),
      match_date = as.Date(m$date), home_team = m$home, away_team = m$away,
      p_home_win = m$p_home_win, p_draw = m$p_draw, p_away_win = m$p_away_win,
      xg_home = m$mean_home_goals, xg_away = m$mean_away_goals
    )
  }))
  expect_gt(nrow(published), 0L)

  stored <- read_forecast_archive(root)
  expect_equal(nrow(stored), nrow(published))
  cols <- names(published)
  key <- c("published_to", "match_date", "home_team", "away_team")
  joined <- dplyr::inner_join(published, stored[cols], by = key, suffix = c("", ".stored"))
  expect_equal(nrow(joined), nrow(published))
  for (col in setdiff(cols, key)) {
    expect_equal(joined[[paste0(col, ".stored")]], joined[[col]], info = col)
  }
  expect_setequal(unique(stored$sex), c("male", "female"))
  # The fit's id is the one of the partition each cell was published from.
  for (sx in c("male", "female")) {
    part <- list.dirs(file.path(extracts_root, "sport=football", "country=iceland", paste0("sex=", sx)), recursive = FALSE)
    expect_length(part, 1L)
    expect_identical(unique(stored$fit_id[stored$sex == sx]), forecast_archive_fit_id(part))
  }
  # Kick-offs from the schedule, which posts one for every fixture here.
  sched <- read_table("schedules", root = root, filter = list(sport = "football"))
  k <- sched$kickoff_time[match(paste(stored$match_date, stored$home_team, stored$away_team), paste(sched$match_date, sched$home_team, sched$away_team))]
  expect_false(anyNA(k))
  expect_identical(stored$kickoff_time, k)
  expect_true(all(stored$engine == "stan"))
  expect_true(all(!is.na(stored$stan_model)))
  expect_true(all(stored$published_at <= Sys.time()))
})

test_that("basketball and handball publishes are archived too", {
  root <- fixture_facts_root()
  extracts_root <- file.path(root, "beliefs", "extracts")
  dir.create(extracts_root, recursive = TRUE, showWarnings = FALSE)
  for (sport in c("basketball", "handball")) {
    file.copy(testthat::test_path("fixtures", "extracts", paste0("sport=", sport)), extracts_root, recursive = TRUE)
  }
  leagues <- load_leagues()
  for (key in c("basketball_iceland", "handball_iceland")) {
    league <- leagues[[key]]
    static <- league[c("sport", "country", "sexes", "active", "stan_model", "data_source")]
    for (sex in c("male", "female")) {
      suppressMessages(suppressWarnings(publish_one(
        static, league$betting, key, sex,
        root = root, validate = FALSE, end_date = FIXTURE_END_DATE,
        archive_forecasts = TRUE
      )))
    }
  }
  out <- file.path(root, "publish")
  n_published <- function(sport) {
    files <- list.files(file.path(out, sport), pattern = "^next_games[.]json$", recursive = TRUE, full.names = TRUE)
    sum(vapply(files, function(f) NROW(jsonlite::fromJSON(f)$matches), integer(1)))
  }
  stored <- read_forecast_archive(root)
  for (sport in c("basketball", "handball")) {
    expect_gt(n_published(sport), 0L)
    expect_equal(sum(stored$sport == sport), n_published(sport), info = sport)
  }
})

test_that("publish_one archives nothing unless asked", {
  root <- fixture_facts_root()
  extracts_root <- file.path(root, "beliefs", "extracts")
  league <- load_leagues()[["football_iceland"]]
  static <- league[c("sport", "country", "sexes", "active", "stan_model", "data_source")]
  build_football_extracts_fixture(root, extracts_root, "male")
  suppressMessages(suppressWarnings(publish_one(
    static, league$betting, "football_iceland", "male",
    root = root, validate = FALSE, end_date = FIXTURE_END_DATE
  )))
  expect_true(dir.exists(file.path(root, "publish", "football")))
  expect_false(dir.exists(file.path(root, "archive", "forecasts")))
})

test_that("the publish script archives on GitHub Actions or when asked, and nowhere else", {
  # A local publish is not a publication: the site serves what CI commits to
  # main. A local write would also put a binary file that CI rewrites into the
  # local tree, which the stash/pull/pop sync can only resolve by picking one
  # side's rows.
  expect_true(publish_archives_forecasts(flag = FALSE, github_actions = "true"))
  expect_true(publish_archives_forecasts(flag = TRUE, github_actions = ""))
  expect_false(publish_archives_forecasts(flag = FALSE, github_actions = ""))
  expect_false(publish_archives_forecasts(flag = FALSE, github_actions = "false"))

  src <- readLines(testthat::test_path("..", "..", "scripts", "05_publish.R"), warn = FALSE)
  body <- src[!grepl("^\\s*#", src)]
  expect_true(any(grepl("publish_archives_forecasts(", body, fixed = TRUE)))
  expect_true(any(grepl("archive_forecasts =", body, fixed = TRUE)))
})

test_that("decide-publish and republish commit the forecast archive, and still commit before it exists", {
  skip_if(Sys.which("git") == "" || Sys.which("bash") == "", "git and bash needed")
  for (wf in c("decide-publish.yml", "republish.yml")) {
    yml <- yaml::read_yaml(testthat::test_path("..", "..", ".github", "workflows", wf))
    steps <- yml$jobs[[1]]$steps
    commit <- steps[[which(vapply(steps, function(s) grepl("git commit", s$run %||% "", fixed = TRUE), logical(1)))]]
    adds <- trimws(grep("git add ", strsplit(commit$run, "\n")[[1]], value = TRUE))
    expect_true(any(grepl("data/archive/forecasts/", adds, fixed = TRUE)), info = wf)

    # The step's own `git add` lines, under `bash -e` as Actions runs them, in
    # a repository with publish output and with and without an archive.
    for (with_archive in c(FALSE, TRUE)) {
      repo <- withr::local_tempdir()
      git <- function(...) system2("git", c("-C", repo, ...), stdout = TRUE, stderr = TRUE)
      git("init", "-q")
      # Every other path the step stages exists on main; give each a file.
      others <- unlist(strsplit(trimws(sub("^git add", "", adds[!grepl("archive/forecasts", adds)])), "\\s+"))
      for (d in others) {
        dir.create(file.path(repo, d), recursive = TRUE, showWarnings = FALSE)
        writeLines("{}", file.path(repo, d, "x.json"))
      }
      if (with_archive) {
        dir.create(file.path(repo, "data", "archive", "forecasts"), recursive = TRUE)
        writeLines("x", file.path(repo, "data", "archive", "forecasts", "part-0.parquet"))
      }
      script <- file.path(repo, "add.sh")
      writeLines(c("set -e", paste("cd", shQuote(repo)), trimws(adds)), script)
      status <- system2("bash", script, stdout = FALSE, stderr = FALSE)
      expect_identical(status, 0L, info = paste(wf, "archive present:", with_archive))
      staged <- git("diff", "--cached", "--name-only")
      expect_true(all(file.path(sub("/$", "", others), "x.json") %in% staged), info = paste(wf, with_archive))
      expect_identical("data/archive/forecasts/part-0.parquet" %in% staged, with_archive, info = paste(wf, with_archive))
    }
  }
})

test_that("nothing in the pipeline deletes from the forecast archive", {
  # Functional: retention on the data root's extracts leaves the archive as it
  # was, however old the archive's fit dates are.
  root <- withr::local_tempdir()
  suppressMessages(archive_forecasts(
    dplyr::bind_rows(
      .fa_row(fit_date = as.Date("2020-04-01"), match_date = as.Date("2020-04-03")),
      .fa_row(fit_date = as.Date("2021-05-01"), match_date = as.Date("2021-05-02"))
    ),
    root = root
  ))
  before <- .fa_md5(root)
  for (d in c("2020-04-01", "2021-05-01", "2026-11-01")) {
    p <- file.path(root, "beliefs", "extracts", "sport=football", "country=iceland", "sex=male", paste0("fit_date=", d))
    dir.create(p, recursive = TRUE)
    writeLines("x", file.path(p, "predicted_matches.parquet"))
  }
  pruned <- prune_extracts(
    root = file.path(root, "beliefs", "extracts"), keep_days = 0L, keep_min = 1L,
    keep_season_anchor = FALSE, now = as.Date("2030-01-01"), dry_run = FALSE
  )
  expect_equal(nrow(pruned), 2L)
  expect_identical(.fa_md5(root), before)
  # The archive is laid out sport=/country=/sex=/fit_date= like the extracts.
  # Pointed at the data root, retention still leaves both archives alone.
  draws <- file.path(root, "beliefs", "archive", "sport=football", "country=iceland", "sex=male", "fit_date=2020-04-01")
  dir.create(draws, recursive = TRUE)
  writeLines("x", file.path(draws, "part-0.parquet"))
  pruned <- prune_extracts(
    root = root, keep_days = 0L, keep_min = 1L,
    keep_season_anchor = FALSE, now = as.Date("2030-01-01"), dry_run = TRUE
  )
  expect_false(any(grepl("archive", pruned$path)))
  expect_identical(.fa_md5(root), before)

  # Static: no line of package code, script or workflow that names the archive
  # deletes anything. The archive writer itself only stages and renames
  # through write_table().
  repo <- testthat::test_path("..", "..")
  files <- c(
    list.files(file.path(repo, "R"), pattern = "[.]R$", full.names = TRUE),
    list.files(file.path(repo, "scripts"), pattern = "[.](R|sh)$", recursive = TRUE, full.names = TRUE),
    list.files(file.path(repo, ".github"), pattern = "[.](ya?ml|sh)$", recursive = TRUE, full.names = TRUE)
  )
  deleting <- "unlink|file[.]remove|dir_delete|file_delete|git rm|\\brm -|prune"
  hits <- character()
  for (f in files) {
    l <- readLines(f, warn = FALSE)
    l <- l[!grepl("^\\s*#", l)]
    bad <- grepl("archive/forecasts|forecast_archive", l) & grepl(deleting, l)
    if (any(bad)) hits <- c(hits, paste0(basename(f), ": ", l[bad]))
  }
  expect_identical(hits, character())
})
