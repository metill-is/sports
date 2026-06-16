# R/dashboard-export.R
#' Assemble + export the model-quality dashboard data contract.
#'
#' Computes every backtest diagnostic (P1 joint-distribution, P2 stratified
#' calibration/skill, P3 line-softness) into a tidy JSON contract under
#' `data/dashboard/`, consumed by `docs/dashboard/experiment.qmd`. Read-only on
#' the money path -- never touches the ledger.
#' @importFrom rlang .data
NULL

#' Assemble the dashboard contract (pure: data in, named list of tidy tibbles out).
#'
#' @param predicted [bt_load_predicted()] output (both sexes).
#' @param results Results store.
#' @param odds Odds store.
#' @param wf_list Named-by-sex list of division-attached walk-forward bets.
#' @param now Generation timestamp (a `POSIXct`).
#' @param season Season year stamped into `meta`.
#' @return Named list of tibbles: `meta, pit_total, pit_diff, pit_uniformity,
#'   draw_rate, scoreline, model_calibration, skill, brier_decomp,
#'   market_calibration, market_bias, disagreement, line_stability`.
#' @export
dashboard_assemble <- function(predicted, results, odds, wf_list, now, season = 2026L) {
  sexes <- names(wf_list)
  mkt_list <- stats::setNames(lapply(wf_list, function(wf) {
    if (is.null(wf) || nrow(wf) == 0L) NULL else bt_devig(wf)
  }), sexes)

  per_sex <- function(fn) {
    dplyr::bind_rows(lapply(sexes, function(s) {
      r <- fn(wf_list[[s]], mkt_list[[s]], s)
      if (is.null(r) || nrow(r) == 0L) {
        return(NULL)
      }
      r$sex <- s
      r
    }))
  }

  pit_total <- bt_pit_values(predicted, results, "total")
  pit_diff <- bt_pit_values(predicted, results, "diff")
  pit_uniformity <- dplyr::bind_rows(
    dplyr::mutate(bt_pit_uniformity(pit_total, by = "sex"), marginal = "total"),
    dplyr::mutate(bt_pit_uniformity(pit_diff, by = "sex"), marginal = "diff")
  )
  draw_rate <- dplyr::bind_rows(
    dplyr::mutate(bt_draw_rate(predicted, results, by = "sex"), scope = "sex"),
    dplyr::mutate(bt_draw_rate(predicted, results, by = c("sex", "division")), scope = "division")
  )
  scoreline <- bt_scoreline_residuals(predicted, results, by = "sex")

  model_calibration <- per_sex(function(wf, mkt, s) {
    if (is.null(wf) || nrow(wf) == 0L) {
      return(NULL)
    }
    bt_calibration_bands(wf, n_bins = 10)
  })
  skill <- per_sex(function(wf, mkt, s) {
    if (is.null(mkt) || nrow(mkt) == 0L) {
      return(NULL)
    }
    dplyr::left_join(bt_skill(mkt, by = "market"),
      bt_skill_ci(mkt, by = "market", R = 1000),
      by = "market"
    )
  })
  brier_decomp <- per_sex(function(wf, mkt, s) {
    if (is.null(mkt) || nrow(mkt) == 0L) {
      return(NULL)
    }
    bt_brier_decomp(mkt, by = "market", n_bins = 5)
  })
  market_calibration <- per_sex(function(wf, mkt, s) {
    if (is.null(mkt) || nrow(mkt) == 0L) {
      return(NULL)
    }
    bt_market_calibration(mkt, n_bins = 8)
  })
  market_bias <- per_sex(function(wf, mkt, s) {
    if (is.null(mkt) || nrow(mkt) == 0L) {
      return(NULL)
    }
    bt_market_bias(mkt, by = "market")
  })
  disagreement <- per_sex(function(wf, mkt, s) {
    if (is.null(mkt) || nrow(mkt) == 0L) {
      return(NULL)
    }
    d <- bt_disagreement(mkt, by = "market")
    d$band <- as.character(d$band)
    d
  })
  line_stability <- bt_line_stability(odds)

  meta <- tibble::tibble(
    generated_at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    season = season,
    sexes = paste(sexes, collapse = ","),
    n_pit_total = nrow(pit_total),
    n_devig = sum(vapply(mkt_list, function(m) if (is.null(m)) 0L else nrow(m), integer(1)))
  )

  list(
    meta = meta, pit_total = pit_total, pit_diff = pit_diff,
    pit_uniformity = pit_uniformity, draw_rate = draw_rate, scoreline = scoreline,
    model_calibration = model_calibration, skill = skill, brier_decomp = brier_decomp,
    market_calibration = market_calibration, market_bias = market_bias,
    disagreement = disagreement, line_stability = line_stability
  )
}

#' Write a dashboard contract to `<out_dir>/<name>.json` (one file per element).
#' @param contract Output of [dashboard_assemble()].
#' @param out_dir Target directory (created if absent).
#' @return `out_dir`, invisibly.
#' @export
dashboard_write_json <- function(contract, out_dir) {
  fs::dir_create(out_dir, recurse = TRUE)
  for (nm in names(contract)) {
    jsonlite::write_json(
      contract[[nm]], file.path(out_dir, paste0(nm, ".json")),
      dataframe = "rows", auto_unbox = TRUE, na = "null", pretty = TRUE
    )
  }
  invisible(out_dir)
}

#' Run the diagnostics for real and write the dashboard JSON contract.
#'
#' Loads `results`/`odds`/predicted extracts, runs the REUSE-mode walk-forward
#' per sex (`wf_fn`, injectable for tests), attaches division, assembles the
#' contract, and writes it. Strictly read-only on the ledger.
#' @param root Data root.
#' @param out_dir Output directory for the JSON contract.
#' @param season Season year.
#' @param now Generation timestamp.
#' @param wf_fn Walk-forward function `(sex) -> list(bets=...)` or NULL.
#' @return The assembled contract, invisibly.
#' @export
dashboard_export <- function(root = here::here("data"),
                             out_dir = here::here("data", "dashboard"),
                             season = 2026L, now = Sys.time(),
                             wf_fn = function(s) bt_walkforward_reuse(s, season = season, root = root)) {
  results <- read_table("results", root = root, filter = list(sport = "football", country = "iceland"))
  odds <- read_table("odds", root = root, filter = list(sport = "football", country = "iceland"))
  predicted <- bt_load_predicted(root = root, sex = c("male", "female"), season = season)
  wf_list <- list()
  for (s in c("male", "female")) {
    r <- tryCatch(wf_fn(s), error = function(e) NULL)
    if (!is.null(r) && !is.null(r$bets) && nrow(r$bets) > 0L) {
      wf_list[[s]] <- bt_attach_division(r$bets, results)
    }
  }
  contract <- dashboard_assemble(predicted, results, odds, wf_list, now, season = season)
  dashboard_write_json(contract, out_dir)
  invisible(contract)
}
