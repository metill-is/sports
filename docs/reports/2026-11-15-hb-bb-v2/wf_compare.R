# The same matches, three forecasters: production's stored pre-match forecast (the newest
# archived fit dated strictly before the match, however old that is: `lead_days`), the v2
# walk-forward (weekly cutoff, one to seven days before the match), and the de-vigged
# Lengjan line. Neither forecaster always has the newer information.
#
# Normalisation of the margin scores. Handball: the rounded predictive over every whole
# number, for both forecasters. Basketball final margin: a final score cannot be level, so
# production's draws within half a point of level are set aside and the rest renormalised
# (`ls_margin`; the plain rounded score is kept as `ls_margin_rounded`); v2 reaches the
# final margin through overtime and has no level mass to begin with.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/wf_compare.R")'
source("docs/reports/2026-11-15-hb-bb-v2/common.R")
source(file.path(EXP_DIR, "score.R"))
suppressMessages(library(dplyr))
wf <- readRDS(file.path(OUT, "out", "wf_scores.rds"))
key3 <- function(x) paste(x$match_date, x$home_team, x$away_team)

# Production's archived draws -> per-match summaries, scored against the played result.
prod_archive <- function(cell) {
  ci <- CELLS[[cell]]
  dir <- file.path("data/beliefs/archive", paste0("sport=", ci$sport), "country=iceland", paste0("sex=", ci$sex))
  played <- played_only(cell_results(cell), ci$sport)
  played <- played[, c("match_date", "home_team", "away_team", "home_score", "away_score", "division", "season")]
  band <- if (ci$sport == "handball") 0.5 else 0
  parts <- list.files(dir, pattern = "^fit_date=", full.names = TRUE)
  res <- lapply(parts, function(p) {
    fd <- as.Date(sub("fit_date=", "", basename(p)))
    a <- as.data.frame(arrow::open_dataset(p) |> dplyr::collect())
    a <- a[a$match_date > fd, , drop = FALSE]
    if (nrow(a) == 0L) {
      return(NULL)
    }
    a <- dplyr::inner_join(a, played, by = c("match_date", "home_team", "away_team"))
    if (nrow(a) == 0L) {
      return(NULL)
    }
    a$diff <- a$home_goals - a$away_goals
    a$tot <- a$home_goals + a$away_goals
    a$m_obs <- a$home_score - a$away_score
    a$t_obs <- a$home_score + a$away_score
    s <- a |>
      dplyr::group_by(match_date, home_team, away_team, division, season, home_score, away_score) |>
      dplyr::summarise(
        n_draws = dplyr::n(), mean_m = mean(diff), sd_m = stats::sd(diff), mean_t = mean(tot), sd_t = stats::sd(tot),
        p_home = mean(diff > band), p_draw = mean(abs(diff) <= band), p_away = mean(diff < -band),
        p_draw_band = mean(abs(diff) <= 0.5), p_home_band = mean(diff > 0.5),
        # WHY +0.5: 4,000 draws cannot resolve a probability below 1/4000; without the
        # half count a margin no draw happened to hit would score minus infinity.
        pm_obs = (sum(abs(diff - m_obs) < 0.5) + 0.5) / (dplyr::n() + 1),
        # The same, among the draws that are not within half a point of a level margin.
        pm_obs_nolevel = (sum(abs(diff - m_obs) < 0.5 & abs(diff) >= 0.5) + 0.5) / (sum(abs(diff) >= 0.5) + 1),
        pt_obs = (sum(abs(tot - t_obs) < 0.5) + 0.5) / (dplyr::n() + 1),
        .groups = "drop"
      )
    s$fit_date <- fd
    as.data.frame(s)
  })
  res <- do.call(rbind, res)
  res <- res[order(res$match_date, res$home_team, res$fit_date), ]
  res <- res[!duplicated(key3(res), fromLast = TRUE), ]
  m <- res$home_score - res$away_score
  res$ll_wdl <- log(ifelse(m > 0, res$p_home, ifelse(m < 0, res$p_away, res$p_draw)))
  res$ls_margin_rounded <- log(res$pm_obs)
  res$ls_margin <- if (ci$sport == "basketball") log(res$pm_obs_nolevel) else res$ls_margin_rounded
  res$ls_total <- log(res$pt_obs)
  res$lead_days <- as.numeric(res$match_date - res$fit_date)
  res
}

# Lengjan 1X2: last scrape on or before the match day, proportional de-vig.
market_line <- function(cell) {
  ci <- CELLS[[cell]]
  league <- load_leagues()[[ci$league]]
  o <- as.data.frame(read_table("odds", root = DATA_ROOT, filter = list(sport = ci$sport, country = "iceland")))
  o <- o[o$market == "moneyline" & !is.na(o$odds), , drop = FALSE]
  kv <- grepl(" kv$", o$home_team) | grepl(" kv$", o$away_team)
  keep <- if (ci$sex == "female") (is.na(o$sex) & kv) | o$sex %in% "female" else (is.na(o$sex) & !kv) | o$sex %in% "male"
  o <- o[keep, , drop = FALSE]
  if (nrow(o) == 0L) {
    return(NULL)
  }
  o <- suppressMessages(normalise_lengjan_team_names(o, league, ci$sex))
  o <- o[as.Date(o$scraped_at) <= o$match_date, , drop = FALSE]
  o$k <- key3(o)
  last <- ave(as.numeric(o$scraped_at), o$k, FUN = max)
  o <- o[as.numeric(o$scraped_at) == last, , drop = FALSE]
  w <- stats::reshape(o[, c("k", "match_date", "home_team", "away_team", "outcome", "odds")],
                      idvar = c("k", "match_date", "home_team", "away_team"), timevar = "outcome", direction = "wide")
  need <- c("odds.home", "odds.draw", "odds.away")
  if (!all(need %in% names(w))) {
    return(NULL)
  }
  w <- w[stats::complete.cases(w[, need]), , drop = FALSE]
  q <- v2_devig(w[, need])
  data.frame(match_date = w$match_date, home_team = w$home_team, away_team = w$away_team,
             q_home = q[, 1], q_draw = q[, 2], q_away = q[, 3], overround = attr(q, "overround"))
}

show_pair <- function(label, a, b) {
  p <- paired(a, b)
  cat(sprintf("    %-46s %+.4f (se %.4f, z %+.1f)\n", label, p[["diff"]], p[["se"]], p[["diff"]] / p[["se"]]))
}

out <- list()
for (cell in names(CELLS)) {
  ci <- CELLS[[cell]]
  v2 <- wf[[paste(cell, "none")]]$scores
  v2r <- wf[[paste(cell, "none_reg")]]$scores
  if (is.null(v2) && is.null(v2r)) next
  v2_label <- "v2 walk-forward (final scores)"
  if (!is.null(v2r)) {
    # Basketball: the recommended model is trained on regulation time and carried to the
    # final result through its tie and overtime model; that is what is compared.
    v2 <- v2r
    variant <- REG_SHIFT[[cell]]
    v2$lsm_raw <- v2r[[paste0("lsm_final_", variant)]]
    v2$lst_raw <- v2r[[paste0("lst_final_", variant)]]
    v2$ll_wdl_raw <- v2r[[paste0("ll_final_", variant)]]
    v2$p_home <- v2r[[paste0("p_home_final_", variant)]]
    v2$ll_wdl_shift <- NULL
    v2_label <- sprintf("v2 regulation-time model + ties (%s) + overtime, scored on the final result", variant)
  }
  pa <- prod_archive(cell)
  cat(sprintf("\n================ %s: %d matches with a stored production forecast (fit dates %s to %s; median lead %.0f days)\n",
              cell, nrow(pa), min(pa$fit_date), max(pa$fit_date), stats::median(pa$lead_days)))
  cmp <- NULL
  if (!is.null(v2)) {
    j <- merge(pa, v2, by = c("match_date", "home_team", "away_team"), suffixes = c(".prod", ".v2"))
    j$period <- ifelse(j$match_date < as.Date("2026-07-01"), "Apr-May 2026 (season end)", "Sep-Nov 2026 (new season)")
    cat(sprintf("  matched with the %s: n = %d\n", v2_label, nrow(j)))
    for (g in c("all", unique(j$period))) {
      x <- if (g == "all") j else j[j$period == g, ]
      cat(sprintf("  [%s] n = %d | production: margin LS %.4f, total LS %.4f, W/D/L LL %.4f | v2: %.4f, %.4f, %.4f\n", g, nrow(x),
                  mean(x$ls_margin), mean(x$ls_total), mean(x$ll_wdl), mean(x$lsm_raw), mean(x$lst_raw), mean(x$ll_wdl_raw)))
      show_pair("v2 - production, margin log score", x$lsm_raw, x$ls_margin)
      show_pair("v2 - production, total log score", x$lst_raw, x$ls_total)
      show_pair("v2 - production, W/D/L log likelihood", x$ll_wdl_raw, x$ll_wdl)
      if ("ll_wdl_shift" %in% names(x)) show_pair("v2 with level-score shift - production, W/D/L", x$ll_wdl_shift, x$ll_wdl)
      cat(sprintf("    mean p(home): production %.3f, v2 %.3f, observed %.3f | mean expected margin: production %.2f, v2 %.2f, observed %.2f | total: %.1f, %.1f, %.1f\n",
                  mean(x$p_home.prod), mean(x$p_home.v2), mean(x$margin > 0), mean(x$mean_m.prod), mean(x$mean_m.v2), mean(x$margin),
                  mean(x$mean_t.prod), mean(x$mean_t.v2), mean(x$total)))
      cat(sprintf("    predictive sd of the margin: production %.2f, v2 %.2f; of the total: %.2f, %.2f | rmse margin: %.2f, %.2f; total: %.2f, %.2f\n",
                  mean(x$sd_m), mean(x$s_m), mean(x$sd_t), mean(x$s_t), sqrt(mean((x$margin - x$mean_m.prod)^2)),
                  sqrt(mean((x$margin - x$mean_m.v2)^2)), sqrt(mean((x$total - x$mean_t.prod)^2)), sqrt(mean((x$total - x$mean_t.v2)^2))))
    }
    cmp <- j
  }
  mk <- market_line(cell)
  mk_cmp <- NULL
  if (!is.null(mk) && nrow(mk) > 0L) {
    base <- if (ci$sport == "basketball" && !is.null(v2r)) v2r else v2
    jm <- merge(mk, base, by = c("match_date", "home_team", "away_team"))
    jm <- merge(jm, pa[, c("match_date", "home_team", "away_team", "p_home_band", "p_draw_band")], by = c("match_date", "home_team", "away_team"), all.x = TRUE)
    if (nrow(jm) > 0L) {
      m <- jm$margin_fit
      pick <- function(h, dr, aw) log(ifelse(m > 0, h, ifelse(m < 0, aw, dr)))
      jm$ll_market <- pick(jm$q_home, jm$q_draw, jm$q_away)
      # Handball: the plain rounded margin, as proposed. Basketball (regulation 1X2): the
      # proposed tie treatment for the cell, the one every other "v2" table uses.
      jm$ll_v2_plain <- jm$ll_wdl_raw
      jm$ll_v2 <- if (ci$sport == "basketball") jm[[paste0("ll_wdl_", REG_SHIFT[[cell]])]] else jm$ll_wdl_raw
      jm$ll_v2_shift <- if ("ll_wdl_shift" %in% names(jm)) jm$ll_wdl_shift else jm$ll_wdl_raw
      jm$ll_prod <- pick(jm$p_home_band, jm$p_draw_band, 1 - jm$p_home_band - jm$p_draw_band)
      cat(sprintf("  Lengjan 1X2 (%s): %d priced matches matched, mean overround %.3f, %s to %s\n",
                  if (ci$sport == "basketball") "regulation time" else "full time", nrow(jm), mean(jm$overround), min(jm$match_date), max(jm$match_date)))
      cat(sprintf("    1X2 log likelihood: market %.4f | v2 %.4f | v2 with level-score shift %.4f\n", mean(jm$ll_market), mean(jm$ll_v2), mean(jm$ll_v2_shift)))
      show_pair("v2 (shift) - market", jm$ll_v2_shift, jm$ll_market)
      has_prod <- !is.na(jm$ll_prod) & is.finite(jm$ll_prod)
      if (sum(has_prod) >= 5L) {
        cat(sprintf("    on the %d of them with a stored production forecast: market %.4f | production (+/-0.5 band) %.4f | v2 (shift) %.4f\n",
                    sum(has_prod), mean(jm$ll_market[has_prod]), mean(jm$ll_prod[has_prod]), mean(jm$ll_v2_shift[has_prod])))
        show_pair("production - market", jm$ll_prod[has_prod], jm$ll_market[has_prod])
        show_pair("v2 (shift) - production", jm$ll_v2_shift[has_prod], jm$ll_prod[has_prod])
      }
      cat(sprintf("    mean p(home): market %.3f, v2 %.3f, observed %.3f | mean p(draw): market %.3f, v2 %.3f, observed %.3f\n",
                  mean(jm$q_home), mean(jm$p_home), mean(m > 0), mean(jm$q_draw),
                  mean(if ("p_draw_shift" %in% names(jm)) jm$p_draw_shift else jm$p_draw), mean(m == 0)))
      mk_cmp <- jm
    }
  }
  out[[cell]] <- list(prod = pa, cmp = cmp, market = mk_cmp)
}
saveRDS(out, file.path(OUT, "out", "wf_compare.rds"))
