# Monthly scorecard of the handball / basketball v2 switch path. LOCAL ONLY:
# no workflow may run it (tests/testthat/test-hbbb-v2-scorecard.R checks).
#
#   ml run -- Rscript scripts/hbbb_v2_scorecard.R [--from 2026-11-17]
#       [--to YYYY-MM-DD] [--draws 4000] [--cache DIR] [--out DIR] [--root DIR]
#
# --root reads another data tree instead of the checkout's data/ (for example
# a newer origin/main's, extracted with `git archive`); it is only read.
#
# Every handball and basketball match played from --from to --to (default
# yesterday) that production forecast is scored twice, as the report scored
# them (docs/reports/2026-11-15-hb-bb-v2/report.md, "Against production's
# stored forecasts" and "Against the Lengjan line"): by production's newest
# stored forecast dated before the match, and by v2 fitted on the same
# information (the results dated up to the day before production's fit), with
# the switch path's frozen specification. Margin and total log scores and the
# result log likelihood, against production and against the de-vigged
# Lengjan line where one exists; standard errors clustered by team-season.
# The gate per sport (both sexes pooled): at least 150 scored matches, pooled
# margin and total log scores not worse than production's by more than one
# standard error, the mean expected home margin within one standard error of
# the observed, and every v2 fit through the production gate. Writes
# scorecard.md and its CSV tables to docs/reports/<to>-hb-bb-v2-scorecard/.
#
# One v2 fit per cell and information date (about half a minute to two
# minutes each here). Fits are cached under --cache (default: the user cache
# directory, or SPORTS_SCORECARD_CACHE), keyed on their exact inputs and
# code; scores are always computed from the current results, so a late or
# corrected result counts on the next run. A failed fit is not cached.
options(width = 150, warn = 1)
suppressMessages(devtools::load_all(quiet = TRUE))

arg <- function(name, default) {
  a <- commandArgs(trailingOnly = TRUE)
  i <- match(paste0("--", name), a)
  if (is.na(i)) default else a[i + 1L]
}
from <- as.Date(arg("from", format(HBBB_SCORECARD_START)))
to <- as.Date(arg("to", format(Sys.Date() - 1L)))
n_draws <- as.integer(arg("draws", "4000"))
default_cache <- Sys.getenv("SPORTS_SCORECARD_CACHE", file.path(tools::R_user_dir("sports", "cache"), "hbbb-v2-scorecard"))
cache_dir <- arg("cache", default_cache)
out_dir <- arg("out", here::here("docs", "reports", sprintf("%s-hb-bb-v2-scorecard", format(to))))
root <- normalizePath(arg("root", here::here("data")))
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
seed <- 1L

leagues <- load_leagues()
scored <- list()
fits_log <- list()
markets <- list()
n_unpaired <- 0L
t_start <- Sys.time()

for (key in c("handball_iceland", "basketball_iceland")) {
  league <- leagues[[key]]
  for (sex in league$sexes) {
    sport <- league$sport
    res <- as.data.frame(read_table("results", root = root, filter = list(sport = sport, country = league$country, sex = sex)))
    played <- res[!is.na(res$home_score) & !is.na(res$away_score) & res$match_date >= from & res$match_date <= to, , drop = FALSE]
    played <- played[!.is_forfeit(played, sport), , drop = FALSE]
    cli::cli_h2("{sport} {sex}: {nrow(played)} played match{?es}")
    if (!nrow(played)) next
    prod <- hbbb_production_forecasts(sport, sex, played, root = root)
    if (is.null(prod) || !nrow(prod)) {
      n_unpaired <- n_unpaired + nrow(played)
      next
    }
    n_unpaired <- n_unpaired + nrow(played) - nrow(prod)
    plan <- hbbb_scorecard_plan(prod)
    previous_sd <- NULL
    cell <- list()
    for (i in seq_along(plan$info_date)) {
      info <- plan$info_date[i]
      inp <- hbbb_v2_info_inputs(league, sex, info, plan$fixtures[[i]], root = root)
      if (!length(inp$j)) next
      ck <- hbbb_v2_fit_key(inp, n_draws = n_draws, seed = seed, previous_sd = previous_sd)
      cache_file <- file.path(cache_dir, sprintf("%s-%s-%s-%s.rds", sport, sex, format(info), substr(ck, 1, 16)))
      t0 <- Sys.time()
      fit <- if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        f <- tryCatch(
          suppressMessages(hbbb_v2_fit_info(inp, sport, n_draws = n_draws, seed = seed, previous_sd = previous_sd)),
          error = function(e) structure(list(message = conditionMessage(e)), class = "fit_error")
        )
        # WHY: a failed fit is not cached, so a passing problem (memory, a
        # stopped run) is retried next time.
        if (!inherits(f, "fit_error")) saveRDS(f, cache_file)
        f
      }
      if (inherits(fit, "fit_error")) {
        fits_log[[length(fits_log) + 1L]] <- data.frame(
          sport = sport, sex = sex, info_date = info, fixtures = length(inp$j),
          gate = NA, breaches = paste("fit failed:", fit$message), seconds = NA_real_
        )
        next
      }
      fits_log[[length(fits_log) + 1L]] <- data.frame(
        sport = sport, sex = sex, info_date = info, fixtures = length(inp$j),
        gate = isTRUE(fit$gate$passed), breaches = paste(fit$gate$breaches, collapse = "; "),
        seconds = as.numeric(difftime(Sys.time(), t0, units = "secs"))
      )
      # As on the daily path: the next fit's jump check follows the last fit
      # that passed, and a fit that did not pass publishes nothing.
      if (!isTRUE(fit$gate$passed)) next
      previous_sd <- fit$hyper_sd
      s <- hbbb_v2_score_fit(fit, res)
      if (!is.null(s)) cell[[length(cell) + 1L]] <- s
    }
    if (!length(cell)) next
    v2 <- do.call(rbind, cell)
    v2$sport <- sport
    v2$sex <- sex
    names(v2)[match(c("ls_margin", "ls_total", "ll_result", "mean_m", "mean_t", "p_home", "p_draw", "p_away"), names(v2))] <-
      paste0("v2_", c("ls_margin", "ls_total", "ll_result", "mean_m", "mean_t", "p_home", "p_draw", "p_away"))
    p <- prod[, c(
      "match_date", "home_team", "away_team", "fit_date", "lead_days", "ls_margin", "ls_total", "ll_result",
      "mean_m", "mean_t", "p_home", "p_draw", "p_away", "p_home_band", "p_draw_band"
    )]
    names(p)[-(1:3)] <- paste0("prod_", names(p)[-(1:3)])
    scored[[length(scored) + 1L]] <- merge(v2, p, by = c("match_date", "home_team", "away_team"))
    mk <- hbbb_market_line(league, sex, root = root)
    if (!is.null(mk)) {
      m <- merge(scored[[length(scored)]], mk, by = c("match_date", "home_team", "away_team"))
      m <- m[!is.na(m$margin_reg), , drop = FALSE]
      if (nrow(m)) {
        pick <- function(k, h, d, a) log(ifelse(k > 0, h, ifelse(k < 0, a, d)))
        ll_mk <- pick(m$margin_reg, m$q_home, m$q_draw, m$q_away)
        ll_prod <- pick(m$margin_reg, m$prod_p_home_band, m$prod_p_draw_band, 1 - m$prod_p_home_band - m$prod_p_draw_band)
        d <- m$ll_result_reg - ll_mk
        markets[[length(markets) + 1L]] <- data.frame(
          group = paste(sport, sex), n = nrow(m),
          period = paste(format(min(m$match_date)), "to", format(max(m$match_date))),
          market = mean(ll_mk), production = mean(ll_prod), v2 = mean(m$ll_result_reg), diff = mean(d),
          se = cluster_se_team_season(d, paste(m$home_team, m$season), paste(m$away_team, m$season))
        )
      }
    }
  }
}

paired <- do.call(rbind, scored)
fits <- do.call(rbind, fits_log)
if (is.null(fits)) fits <- data.frame(sport = character(), sex = character(), gate = logical(), breaches = character())
market <- do.call(rbind, markets)
if (is.null(paired) || !nrow(paired)) stop("no match scored between ", from, " and ", to)
cmp <- hbbb_scorecard_compare(paired)
accuracy <- hbbb_scorecard_accuracy(paired)
gate <- hbbb_scorecard_gate(cmp, paired, fits)

utils::write.csv(paired, file.path(out_dir, "paired_scores.csv"), row.names = FALSE)
utils::write.csv(cmp, file.path(out_dir, "comparison.csv"), row.names = FALSE)
utils::write.csv(accuracy, file.path(out_dir, "accuracy.csv"), row.names = FALSE)
utils::write.csv(gate, file.path(out_dir, "gate.csv"), row.names = FALSE)
utils::write.csv(fits, file.path(out_dir, "fits.csv"), row.names = FALSE)
if (!is.null(market)) utils::write.csv(market, file.path(out_dir, "market.csv"), row.names = FALSE)

lines <- hbbb_scorecard_note(
  meta = list(
    from = from, to = to, n_draws = n_draws,
    commit = system2("git", c("-C", shQuote(here::here()), "rev-parse", "--short", "HEAD"), stdout = TRUE),
    minutes = as.numeric(difftime(Sys.time(), t_start, units = "mins"))
  ),
  gate = gate, cmp = cmp, accuracy = accuracy, market = market, fits = fits, paired = paired
)
lines <- c(lines, sprintf("%d played matches in the window had no stored production forecast and are not scored.", n_unpaired), "",
  sprintf("Data: %s.", Sys.getenv("SCORECARD_DATA_NOTE", if (identical(root, normalizePath(here::here("data")))) "the checkout's data/" else root)), "")
writeLines(lines, file.path(out_dir, "scorecard.md"))
cat(lines, sep = "\n")
