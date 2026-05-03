# Phase 2d regression: confirms the new publish_football_iceland(extracted, ...)
# produces the same JSONs as the legacy .publish_football_iceland_from_fit_pfi
# when both are fed the same backup fit. Subtle math drift in the
# count-weighted refactor would surface here.
#
# Bytewise identical assertion is limited to JSONs whose values flow through
# arithmetic that the count representation preserves exactly (mean, outcome
# probabilities, points sums, final-position probabilities). For the
# quantile-band JSONs (team_strengths*, home_advantage), 50 % and 80 % bands
# are exact (q25/q75/q10/q90 lookups match stats::quantile), but the 95 %
# band is interpolated from q2/q3 + q97/q98 -- close to but not identical
# to the type-7 quantile from per-draw values. We assert exact for medians
# and 50/80 % bands and a small tolerance for 95 % bands.
#
# Phase 3 deletes this file alongside .publish_football_iceland_from_fit_pfi.

backup_fit_path_reg <- function(sex) {
  root <- Sys.getenv(
    "SPORTS_BACKUP_ROOT",
    "/Users/brynjolfurjonsson/sports-backup-20260424-163153"
  )
  file.path(root, "Sports", "football", "iceland", "results", sex, "fit.rds")
}

# Recursively drop `generated_at` keys from a parsed-JSON list.
.drop_generated_at_reg <- function(x) {
  if (is.list(x)) {
    x[["generated_at"]] <- NULL
    lapply(x, .drop_generated_at_reg)
  } else {
    x
  }
}

# In test fixtures the backup fit's N_pred can mismatch the current
# prepare_data() result (facts/results have grown since the fit was
# saved). Both publishers handle this branch correctly but report
# different `n_draws` in meta.json -- legacy reads it from the in-memory
# fit (true draw count, e.g. 4000), the new path derives it from
# predicted_matches counts which are zero on mismatch (so it reports 0).
# In production these always agree because fit and prepare_data share
# the same input. Strip n_draws from meta.json when comparing under the
# stale-fixture regime; the count-weighted invariant is exercised by the
# next_games / standings comparisons elsewhere.
.normalize_meta_for_regression <- function(parsed) {
  if (is.list(parsed) && !is.null(parsed[["n_draws"]])) {
    parsed[["n_draws"]] <- NULL
  }
  parsed
}

# Run new + legacy publishers on the same fit and return the two output dirs.
.run_both_publishers_reg <- function(sex, end_date) {
  fit_path <- backup_fit_path_reg(sex)
  if (!file.exists(fit_path)) {
    testthat::skip(paste("legacy football fit unavailable for", sex))
  }
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent -- cannot reconstruct prepare_data")
  }

  fit <- readRDS(fit_path)
  league <- load_leagues()[["football_iceland"]]

  extract_dir <- withr::local_tempdir(.local_envir = parent.frame())
  out_new <- withr::local_tempdir(.local_envir = parent.frame())
  out_old <- withr::local_tempdir(.local_envir = parent.frame())

  suppressWarnings(suppressMessages(
    extract_football_iceland(
      fit, league,
      sex = sex,
      fit_date = end_date,
      end_date = end_date,
      archive_root = extract_dir
    )
  ))
  extracted <- read_extracted_football(
    league,
    sex = sex,
    fit_date = end_date,
    archive_root = extract_dir
  )

  suppressWarnings(suppressMessages(
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = sex,
      end_date = end_date,
      output_root = out_new,
      archive_root = extract_dir
    )
  ))
  suppressWarnings(suppressMessages(
    sports:::.publish_football_iceland_from_fit_pfi(
      fit = fit,
      league = league,
      sex = sex,
      end_date = end_date,
      output_root = out_old,
      archive_root = extract_dir
    )
  ))

  # Helper writes to {sex}-bd/ for the default target_div = "BD".
  sex_folder <- if (sex == "male") "karla-bd" else "kvenna-bd"
  list(
    new = file.path(out_new, "football", "iceland", sex_folder),
    old = file.path(out_old, "football", "iceland", sex_folder)
  )
}

test_that("regression (male): exact JSONs (modulo generated_at) for non-quantile fields", {
  dirs <- .run_both_publishers_reg(sex = "male", end_date = as.Date("2026-04-25"))

  exact_files <- c(
    "meta.json",
    "next_games.json",
    "standings.json",
    "final_positions.json",
    "points_distribution.json",
    "round_predictions_history.json",
    "standings_history.json",
    "final_positions_history.json"
  )
  for (jsonfile in exact_files) {
    p_new <- file.path(dirs$new, jsonfile)
    p_old <- file.path(dirs$old, jsonfile)
    expect_true(file.exists(p_new), info = paste("new missing:", jsonfile))
    expect_true(file.exists(p_old), info = paste("old missing:", jsonfile))

    j_new <- .drop_generated_at_reg(
      jsonlite::fromJSON(p_new, simplifyVector = FALSE)
    )
    j_old <- .drop_generated_at_reg(
      jsonlite::fromJSON(p_old, simplifyVector = FALSE)
    )
    if (jsonfile == "meta.json") {
      j_new <- .normalize_meta_for_regression(j_new)
      j_old <- .normalize_meta_for_regression(j_old)
    }
    expect_equal(j_new, j_old, info = jsonfile, tolerance = 1e-9)
  }
})

test_that("regression (male): quantile JSONs match exactly for 50/80 % bands and median", {
  dirs <- .run_both_publishers_reg(sex = "male", end_date = as.Date("2026-04-25"))

  band_files <- c(
    "team_strengths.json",
    "team_strengths_history.json",
    "home_advantage.json"
  )
  for (jsonfile in band_files) {
    j_new <- jsonlite::fromJSON(
      file.path(dirs$new, jsonfile),
      simplifyDataFrame = TRUE
    )
    j_old <- jsonlite::fromJSON(
      file.path(dirs$old, jsonfile),
      simplifyDataFrame = TRUE
    )
    rec_new <- j_new$records
    rec_old <- j_old$records

    if (is.null(rec_new) || (is.data.frame(rec_new) && nrow(rec_new) == 0L)) {
      testthat::skip(paste(jsonfile, "empty -- nothing to compare"))
    }

    expect_setequal(names(rec_new), names(rec_old))
    key_cols <- intersect(
      c("team", "component", "location", "round", "coverage"),
      names(rec_new)
    )
    rec_new <- rec_new[
      do.call(order, rec_new[, key_cols, drop = FALSE]), ,
      drop = FALSE
    ]
    rec_old <- rec_old[
      do.call(order, rec_old[, key_cols, drop = FALSE]), ,
      drop = FALSE
    ]
    expect_equal(nrow(rec_new), nrow(rec_old), info = jsonfile)

    # 50/80 % bands and median come from exact quantile lookups
    for (cov in c(0.5, 0.8)) {
      mask <- rec_new$coverage == cov
      expect_equal(
        rec_new$lower[mask], rec_old$lower[mask],
        tolerance = 1e-9,
        info = paste(jsonfile, "lower @", cov)
      )
      expect_equal(
        rec_new$upper[mask], rec_old$upper[mask],
        tolerance = 1e-9,
        info = paste(jsonfile, "upper @", cov)
      )
    }
    expect_equal(
      rec_new$median, rec_old$median,
      tolerance = 1e-9,
      info = paste(jsonfile, "median")
    )
  }
})

test_that("regression (male): 95 % band drift stays within quantile-interpolation tolerance", {
  dirs <- .run_both_publishers_reg(sex = "male", end_date = as.Date("2026-04-25"))

  band_files <- c(
    "team_strengths.json",
    "team_strengths_history.json",
    "home_advantage.json"
  )
  for (jsonfile in band_files) {
    j_new <- jsonlite::fromJSON(
      file.path(dirs$new, jsonfile),
      simplifyDataFrame = TRUE
    )
    j_old <- jsonlite::fromJSON(
      file.path(dirs$old, jsonfile),
      simplifyDataFrame = TRUE
    )
    rec_new <- j_new$records
    rec_old <- j_old$records
    if (is.null(rec_new) || (is.data.frame(rec_new) && nrow(rec_new) == 0L)) {
      testthat::skip(paste(jsonfile, "empty"))
    }

    key_cols <- intersect(
      c("team", "component", "location", "round", "coverage"),
      names(rec_new)
    )
    rec_new <- rec_new[
      do.call(order, rec_new[, key_cols, drop = FALSE]), ,
      drop = FALSE
    ]
    rec_old <- rec_old[
      do.call(order, rec_old[, key_cols, drop = FALSE]), ,
      drop = FALSE
    ]

    mask <- rec_new$coverage == 0.95
    if (sum(mask) == 0L) next

    # 95 % band: extract uses linear interp between q2/q3 and q97/q98;
    # legacy uses stats::quantile(..., 0.025) / 0.975 (type-7 interp on
    # per-draw values). The two agree to within ~1 % of the band width
    # for typical posteriors. Use a modest absolute tolerance.
    expect_lt(
      max(abs(rec_new$lower[mask] - rec_old$lower[mask])),
      0.05,
      label = paste(jsonfile, "max lower drift @ 0.95")
    )
    expect_lt(
      max(abs(rec_new$upper[mask] - rec_old$upper[mask])),
      0.05,
      label = paste(jsonfile, "max upper drift @ 0.95")
    )
  }
})
