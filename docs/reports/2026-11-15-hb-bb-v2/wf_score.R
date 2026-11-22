# Score the walk-forward predictions and test second-stage refinements out of sample.
# Every refinement (variance shape, level-score shift, overtime) is fitted on the
# walk-forward predictions of EARLIER seasons only and scored on the next season, so no
# number here has seen the match it scores.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/wf_score.R")'
source("docs/reports/2026-11-15-hb-bb-v2/common.R")
source(file.path(EXP_DIR, "score.R"))

# A play-off match: handball's PO division; in basketball (no play-off label) a pair that
# meets at least three times within three weeks, which only a play-off series does.
is_playoff <- function(d, sport) {
  if (sport == "handball") {
    return(d$division == "PO")
  }
  pair <- paste(d$season, pmin(d$home_team, d$away_team), pmax(d$home_team, d$away_team))
  day <- as.numeric(d$match_date)
  vapply(seq_len(nrow(d)), function(i) sum(pair == pair[i] & abs(day - day[i]) <= 21) >= 3, logical(1))
}

# The last regular-season week of each row's (season, division). A finished season shows
# it; the season still being played does not. When the newest season stops more than six
# weeks short of the earlier seasons of its division, it is given their median.
# WHY not the schedules table: it lists the fixtures announced so far, and a split or
# play-off phase is announced later (women's Bonusdeild 2027 ends on 3 March there, the
# earlier seasons ran into April), so it would understate the season's length.
last_regular_week <- function(d) {
  seen <- tapply(d$week, list(d$season, d$division), max)
  newest <- as.character(max(d$season))
  for (dv in colnames(seen)) {
    earlier <- seen[rownames(seen) != newest, dv]
    earlier <- earlier[!is.na(earlier)]
    if (is.na(seen[newest, dv]) || length(earlier) == 0L) next
    if (seen[newest, dv] < stats::median(earlier) - 6) seen[newest, dv] <- stats::median(earlier)
  }
  unname(seen[cbind(as.character(d$season), as.character(d$division))])
}

load_wf <- function(cell, tag) {
  f <- file.path(OUT, "out", sprintf("wf_%s_%s.rds", cell, tag))
  if (!file.exists(f)) {
    return(NULL)
  }
  stored <- readRDS(f)
  # WHY: a prediction from a fit whose mode search failed is not a forecast of the model.
  # Such rows are left out and counted; a walk-forward stored before the status was kept
  # (the first run) cannot be filtered and is passed whole.
  d <- v2_successful_predictions(stored$pred, stored$cut)
  # A cutoff whose fit threw on its warm and its cold attempt has no rows at all; its
  # matches are counted as left out too (stored since 22 November 2026, see wf_inla.R).
  dropped <- attr(d, "dropped") + if (is.null(stored$lost)) 0L else sum(stored$lost$n_pred)
  if (isTRUE(dropped > 0L)) cat(sprintf("%s [%s]: %d predictions left out, from cutoffs with a failed mode search\n", cell, tag, dropped))
  attr(d, "dropped") <- NULL
  sport <- CELLS[[cell]]$sport
  d$cell <- cell
  d$s_m <- sqrt(d$sd_m^2 + d$sig2_m)
  d$s_t <- sqrt(d$sd_t^2 + d$sig2_t)
  d$margin <- d$home_score - d$away_score
  d$total <- d$home_score + d$away_score
  d$playoff <- is_playoff(d, sport)
  last_week <- rep(NA_real_, nrow(d))
  last_week[!d$playoff] <- last_regular_week(d[!d$playoff, , drop = FALSE])
  d$phase <- ifelse(d$playoff, "play-off", ifelse(d$week <= 6, "first 6 weeks",
    ifelse(d$week > last_week - 6, "last 6 weeks", "middle")
  ))
  d$phase <- factor(d$phase, levels = c("first 6 weeks", "middle", "last 6 weeks", "play-off"))
  attr(d, "dropped") <- dropped
  d
}

norm_ll <- function(y, mu, s) stats::dnorm(y, mu, s, log = TRUE)

# Variance-shape candidates. Each returns function(par, d, ctx) -> list(s_m, s_t) and a
# start. WHY ctx: whatever a shape centres on is fixed from the training seasons and
# carried to the scored season with the fitted parameters. Recomputing it on the scored
# season would apply the rule around a different centre than it was fitted around, and
# would use that season's own predictions, later matches included.
shapes <- list(
  constant = list(start = c(0, 0), f = function(par, d, ctx) list(s_m = d$s_m * exp(par[1]), s_t = d$s_t * exp(par[2]))),
  by_mismatch = list(start = c(0, 0, 0, 0), f = function(par, d, ctx) {
    list(
      s_m = d$s_m * exp(par[1] + par[2] * abs(d$mean_m) / d$s_m),
      s_t = d$s_t * exp(par[3] + par[4] * (d$mean_t - ctx$mean_t) / d$s_t)
    )
  }),
  by_division = list(start = NULL, f = function(par, d, ctx) {
    k <- nlevels(d$div_f)
    list(s_m = d$s_m * exp(par[seq_len(k)][d$div_f]), s_t = d$s_t * exp(par[k + seq_len(k)][d$div_f]))
  }),
  # Possessions: both sides have N attacks, each scoring with its own efficiency. Then
  # var(side) = mu (1 - mu / n) given N, plus the share of var(N) that both sides carry.
  possession = list(start = c(log(45), log(16)), f = function(par, d, ctx) {
    n <- exp(par[1])
    v_n <- exp(par[2])
    mu_h <- (d$mean_t + d$mean_m) / 2
    mu_a <- (d$mean_t - d$mean_m) / 2
    # WHY row by row: a bound over every row would make one match's variance depend on
    # the other matches' predictions, later cutoffs included.
    n <- pmax(n, pmax(mu_h, mu_a) + 1)
    binom <- mu_h * (1 - mu_h / n) + mu_a * (1 - mu_a / n)
    list(
      s_m = sqrt(d$sd_m^2 + binom + ((mu_h - mu_a) / n)^2 * v_n),
      s_t = sqrt(d$sd_t^2 + binom + ((mu_h + mu_a) / n)^2 * v_n)
    )
  })
)

fit_shape <- function(shape, train) {
  start <- shape$start
  if (is.null(start)) start <- rep(0, 2 * nlevels(train$div_f))
  ctx <- list(mean_t = mean(train$mean_t))
  nll <- function(par) {
    s <- shape$f(par, train, ctx)
    -sum(norm_ll(train$margin_fit, train$mean_m, s$s_m)) - sum(norm_ll(train$total_fit, train$mean_t, s$s_t))
  }
  list(par = stats::optim(start, nll, method = "BFGS", control = list(maxit = 500))$par, ctx = ctx)
}

# Level-score shift: constant, or constant + slope in the expected margin measured in sds.
fit_shift <- function(train, slope = FALSE) {
  p0 <- v2_integer_pmf(rep(0, nrow(train)), train$mean_m, train$s_m)
  tie <- as.integer(train$margin_fit == 0)
  if (sum(tie) < 5L) {
    return(c(0, 0))
  }
  x <- abs(train$mean_m) / train$s_m
  if (slope) {
    co <- stats::coef(stats::glm(tie ~ x, offset = stats::qlogis(p0), family = stats::binomial()))
    return(unname(co))
  }
  c(unname(stats::coef(stats::glm(tie ~ 1, offset = stats::qlogis(p0), family = stats::binomial()))), 0)
}

fit_shift_by <- function(train, group) {
  p0 <- v2_integer_pmf(rep(0, nrow(train)), train$mean_m, train$s_m)
  tie <- train$margin_fit == 0
  vapply(split(seq_len(nrow(train)), group, drop = TRUE), function(ix) {
    if (sum(tie[ix]) < 5L) {
      return(0)
    }
    v2_fit_zero_shift(p0[ix], tie[ix])[["shift"]]
  }, numeric(1))
}

# Degrees of freedom of a unit-variance Student-t for the standardised margin and total.
fit_student <- function(train) {
  one <- function(z) {
    nll <- function(lnu) {
      nu <- 2.05 + exp(lnu)
      sc <- sqrt((nu - 2) / nu)
      -sum(stats::dt(z / sc, df = nu, log = TRUE) - log(sc))
    }
    2.05 + exp(stats::optimize(nll, c(-2, 6))$minimum)
  }
  c(one((train$margin_fit - train$mean_m) / train$s_m), one((train$total_fit - train$mean_t) / train$s_t))
}

ot_pmf <- function(k, mean_m, s_m, share = 5 / 40) {
  v2_integer_pmf(k, mean_m * share, s_m * sqrt(share), ties = FALSE)
}

# Out-of-sample scores for one cell. `reg` = the predictions are of regulation-time scores
# (basketball), where a level score exists and the final result goes through overtime.
# Normalisation of the margin scores: where the scored margin can be level (handball,
# basketball regulation time) the pmf is the rounded predictive over every whole number;
# where it cannot (a basketball final score from a model trained on final scores) the
# rounded mass on zero is removed and the rest renormalised (`ties = FALSE`), so no
# model is charged for an outcome that cannot happen.
score_cell <- function(d, sport, reg = FALSE, first_scored = 2023L) {
  d$div_f <- factor(d$division)
  d$margin_fit <- if (reg) d$home_reg - d$away_reg else d$margin
  d$total_fit <- if (reg) d$home_reg + d$away_reg else d$total
  if (reg) d <- d[!is.na(d$margin_fit), , drop = FALSE]
  ties <- sport == "handball" || reg
  seasons <- sort(unique(d$season))
  seasons <- seasons[seasons >= first_scored]
  rows <- list()
  pars <- list()
  for (s in seasons) {
    tr <- d[d$season < s, , drop = FALSE]
    te <- d[d$season == s, , drop = FALSE]
    if (nrow(tr) < 100L || nrow(te) == 0L) next
    out <- te[, c(
      "cell", "match_date", "cutoff", "season", "division", "home_team", "away_team", "phase", "playoff", "week",
      "home_score", "away_score", "margin", "total", "margin_fit", "total_fit", "mean_m", "mean_t", "s_m", "s_t"
    )]
    for (nm in names(shapes)) {
      if (nm == "possession" && sport != "handball") next
      fs <- tryCatch(fit_shape(shapes[[nm]], tr), error = function(e) NULL)
      if (is.null(fs)) next
      par <- fs$par
      sc <- shapes[[nm]]$f(par, te, fs$ctx)
      out[[paste0("lsm_", nm)]] <- log(v2_integer_pmf(te$margin_fit, te$mean_m, sc$s_m, ties = ties))
      out[[paste0("lst_", nm)]] <- log(v2_integer_pmf(te$total_fit, te$mean_t, sc$s_t))
      out[[paste0("sm_", nm)]] <- sc$s_m
      out[[paste0("st_", nm)]] <- sc$s_t
      pars[[paste(s, nm)]] <- par
    }
    out$lsm_raw <- log(v2_integer_pmf(te$margin_fit, te$mean_m, te$s_m, ties = ties))
    out$lst_raw <- log(v2_integer_pmf(te$total_fit, te$mean_t, te$s_t))
    wdl <- function(shift) {
      o <- v2_outcome_probs(te$mean_m, te$s_m, ties = ties, zero_shift = shift)
      m <- te$margin_fit
      cbind(o, ll = log(ifelse(m > 0, o$p_home, ifelse(m < 0, o$p_away, o$p_draw))))
    }
    base <- wdl(0)
    out$p_home <- base$p_home
    out$p_draw <- base$p_draw
    out$p_away <- base$p_away
    out$ll_wdl_raw <- base$ll
    if (ties) {
      a1 <- fit_shift(tr, slope = FALSE)
      a2 <- fit_shift(tr, slope = TRUE)
      sh1 <- rep(a1[1], nrow(te))
      sh2 <- a2[1] + a2[2] * abs(te$mean_m) / te$s_m
      w1 <- wdl(sh1)
      w2 <- wdl(sh2)
      out$ll_wdl_shift <- w1$ll
      out$ll_wdl_shift_slope <- w2$ll
      out$p_draw_shift <- w1$p_draw
      out$p_home_shift <- w1$p_home
      out$p_draw_shift_slope <- w2$p_draw
      out$p_home_shift_slope <- w2$p_home
      out$lsm_shift <- log(v2_integer_pmf(te$margin_fit, te$mean_m, te$s_m, zero_shift = sh1))
      out$lsm_shift_slope <- log(v2_integer_pmf(te$margin_fit, te$mean_m, te$s_m, zero_shift = sh2))
      out$shift_used <- sh1
      out$shift_slope_used <- sh2
      pars[[paste(s, "shift")]] <- a1
      pars[[paste(s, "shift_slope")]] <- a2
      # One shift per division: the level-score rate differs between divisions.
      a3 <- fit_shift_by(tr, tr$div_f)
      sh3 <- unname(a3[as.character(te$div_f)])
      sh3[is.na(sh3)] <- a1[1]
      w3 <- wdl(sh3)
      out$ll_wdl_shift_div <- w3$ll
      out$p_draw_shift_div <- w3$p_draw
      out$p_home_shift_div <- w3$p_home
      out$lsm_shift_div <- log(v2_integer_pmf(te$margin_fit, te$mean_m, te$s_m, zero_shift = sh3))
      out$shift_div_used <- sh3
      pars[[paste(s, "shift_div")]] <- a3
      if (reg) {
        # Final result through overtime, from the regulation-time predictive.
        home_won <- te$margin > 0
        for (v in c("none", "shift", "shift_div")) {
          sh <- switch(v, none = rep(0, nrow(te)), shift = sh1, shift_div = sh3)
          r <- v2_basketball_result_probs(te$mean_m, te$s_m, zero_shift = sh)
          out[[paste0("ll_final_", v)]] <- log(ifelse(home_won, r$p_home, r$p_away))
          out[[paste0("p_home_final_", v)]] <- r$p_home
          out[[paste0("p_home_ot_", v)]] <- r$p_home_ot
          reg_part <- v2_integer_pmf(te$margin, te$mean_m, te$s_m, zero_shift = sh)
          out[[paste0("lsm_final_", v)]] <- log(reg_part + r$p_draw_reg * ot_pmf(te$margin, te$mean_m, te$s_m))
          # Final total: the regulation total, plus the overtime points when regulation is
          # level. Overtime points are summarised from the earlier seasons' overtime games.
          ot_pts <- (tr$total - tr$total_fit)[tr$margin_fit == 0]
          ot_mean <- if (length(ot_pts) >= 10L) mean(ot_pts) else 25
          ot_sd <- if (length(ot_pts) >= 10L) stats::sd(ot_pts) else 10
          out[[paste0("lst_final_", v)]] <- log(
            (1 - r$p_draw_reg) * v2_integer_pmf(te$total, te$mean_t, te$s_t) +
              r$p_draw_reg * v2_integer_pmf(te$total, te$mean_t + ot_mean, sqrt(te$s_t^2 + ot_sd^2))
          )
          # Standardised residuals of the final score against the final-score predictive
          # itself: the regulation margin with its level mass moved through overtime, and
          # the two-part total. Their mean and sd come from those distributions.
          # WHY 250: the most lopsided women's game has an expected margin near 84 points.
          grid <- -250:250
          pm_grid <- vapply(grid, function(k) {
            kk <- rep(k, nrow(te))
            (k != 0) * v2_integer_pmf(kk, te$mean_m, te$s_m, zero_shift = sh) + r$p_draw_reg * ot_pmf(kk, te$mean_m, te$s_m)
          }, numeric(nrow(te)))
          stopifnot(max(abs(rowSums(pm_grid) - 1)) < 1e-6)
          em <- as.numeric(pm_grid %*% grid)
          sdm <- sqrt(as.numeric(pm_grid %*% grid^2) - em^2)
          out[[paste0("zm_final_", v)]] <- (te$margin - em) / sdm
          et <- te$mean_t + r$p_draw_reg * ot_mean
          sdt <- sqrt(te$s_t^2 + r$p_draw_reg * ot_sd^2 + r$p_draw_reg * (1 - r$p_draw_reg) * ot_mean^2)
          out[[paste0("zt_final_", v)]] <- (te$total - et) / sdt
        }
      }
    }
    # Heavy tails: a Student-t predictive with the same sd, degrees of freedom fitted on
    # the earlier seasons. Is the Gaussian leaving anything on the table?
    nu <- fit_student(tr)
    out$lsm_student <- log(v2_integer_pmf(te$margin_fit, te$mean_m, te$s_m * sqrt((nu[1] - 2) / nu[1]), df = nu[1], ties = ties))
    out$lst_student <- log(v2_integer_pmf(te$total_fit, te$mean_t, te$s_t * sqrt((nu[2] - 2) / nu[2]), df = nu[2]))
    pars[[paste(s, "student")]] <- nu
    # The same heavier tail carried to the result probabilities (with the per-division
    # level-score shift where the result can be level).
    sc_t <- te$s_m * sqrt((nu[1] - 2) / nu[1])
    sh_t <- if (ties) out$shift_div_used else rep(0, nrow(te))
    o_t <- v2_outcome_probs(te$mean_m, sc_t, df = nu[1], ties = ties, zero_shift = sh_t)
    out$ll_wdl_student <- log(ifelse(te$margin_fit > 0, o_t$p_home, ifelse(te$margin_fit < 0, o_t$p_away, o_t$p_draw)))
    out$lsm_student_shift <- log(v2_integer_pmf(te$margin_fit, te$mean_m, sc_t, df = nu[1], zero_shift = sh_t, ties = ties))
    if (reg) {
      r_t <- v2_basketball_result_probs(te$mean_m, sc_t, df = nu[1], zero_shift = sh_t)
      out$ll_final_student <- log(ifelse(te$margin > 0, r_t$p_home, r_t$p_away))
    }
    rows[[as.character(s)]] <- out
  }
  list(scores = do.call(rbind, rows), pars = pars)
}

summarise_scores <- function(sc, by = NULL) {
  cols <- grep("^(lsm_|lst_|ll_wdl)", names(sc), value = TRUE)
  f <- function(x) {
    data.frame(n = nrow(x), t(vapply(cols, function(cn) mean(x[[cn]]), numeric(1))), check.names = FALSE)
  }
  if (is.null(by)) {
    return(f(sc))
  }
  do.call(rbind, lapply(split(sc, sc[[by]], drop = TRUE), f))
}

paired_table <- function(sc, base, others) {
  do.call(rbind, lapply(others, function(o) {
    if (!o %in% names(sc)) {
      return(NULL)
    }
    p <- paired(sc[[o]], sc[[base]])
    data.frame(variant = o, vs = base, n = nrow(sc), diff = p[["diff"]], se = p[["se"]])
  }))
}

all_scores <- list()
for (cell in names(CELLS)) {
  sport <- CELLS[[cell]]$sport
  for (tag in c("none", "none_reg", "ar1", "ar1_reg")) {
    d <- load_wf(cell, tag)
    if (is.null(d)) next
    reg <- grepl("_reg$", tag)
    ties <- sport == "handball" || reg
    r <- score_cell(d, sport, reg = reg)
    sc <- r$scores
    all_scores[[paste(cell, tag)]] <- list(scores = sc, pars = r$pars, wf = d, failed_fit_rows_left_out = attr(d, "dropped"))
    cat(sprintf(
      "\n================ %s [%s]: %d walk-forward predictions, %d scored (seasons %s)\n", cell, tag, nrow(d), nrow(sc),
      paste(range(sc$season), collapse = "-")
    ))
    cat("mean out-of-sample scores (integer-scale log score of margin and total; W/D/L log likelihood):\n")
    print(round(summarise_scores(sc), 4), row.names = FALSE)
    cat("paired differences (positive = better than the comparison), match-clustered se:\n")
    pt <- rbind(
      paired_table(sc, "lsm_raw", c(
        "lsm_constant", "lsm_by_mismatch", "lsm_by_division", "lsm_possession", "lsm_student",
        "lsm_shift", "lsm_shift_slope", "lsm_shift_div"
      )),
      paired_table(sc, "lst_raw", c("lst_constant", "lst_by_mismatch", "lst_by_division", "lst_possession", "lst_student")),
      paired_table(sc, "ll_wdl_raw", c("ll_wdl_shift", "ll_wdl_shift_slope", "ll_wdl_shift_div")),
      if (ties) paired_table(sc, "ll_wdl_shift_div", "ll_wdl_student") else paired_table(sc, "ll_wdl_raw", "ll_wdl_student"),
      if (ties) paired_table(sc, "lsm_shift_div", "lsm_student_shift"),
      if (reg) paired_table(sc, "ll_final_shift_div", "ll_final_student")
    )
    pt$z <- pt$diff / pt$se
    print(pt, digits = 3, row.names = FALSE)
    cat("by season phase:\n")
    print(round(summarise_scores(sc, "phase")[, c("n", "lsm_raw", "lst_raw", "ll_wdl_raw")], 4))
    cat("by division:\n")
    print(round(summarise_scores(sc, "division")[, c("n", "lsm_raw", "lst_raw", "ll_wdl_raw")], 4))
    cat("by season:\n")
    print(round(summarise_scores(sc, "season")[, c("n", "lsm_raw", "lst_raw", "ll_wdl_raw")], 4))
    # Calibration of the predictive: standardised residuals by phase and by mismatch.
    z_m <- (sc$margin_fit - sc$mean_m) / sc$s_m
    z_t <- (sc$total_fit - sc$mean_t) / sc$s_t
    cal <- function(g) {
      do.call(rbind, lapply(split(seq_len(nrow(sc)), g, drop = TRUE), function(ix) {
        data.frame(
          n = length(ix), mean_z_m = mean(z_m[ix]), sd_z_m = stats::sd(z_m[ix]), mean_z_t = mean(z_t[ix]), sd_z_t = stats::sd(z_t[ix]),
          fav_resid = mean(sign(sc$mean_m[ix]) * (sc$margin_fit[ix] - sc$mean_m[ix])),
          cover80_m = mean(abs(z_m[ix]) < stats::qnorm(0.9)), cover80_t = mean(abs(z_t[ix]) < stats::qnorm(0.9))
        )
      }))
    }
    cat("standardised residuals (sd 1 = calibrated spread) by phase:\n")
    print(cal(sc$phase), digits = 3)
    cat("... by |expected margin| quintile:\n")
    print(cal(cut(abs(sc$mean_m), unique(stats::quantile(abs(sc$mean_m), 0:5 / 5)), include.lowest = TRUE)), digits = 3)
    cat("... by expected-total quintile:\n")
    print(cal(cut(sc$mean_t, unique(stats::quantile(sc$mean_t, 0:5 / 5)), include.lowest = TRUE)), digits = 3)
    cat("... by division:\n")
    print(cal(sc$division), digits = 3)
    nu_last <- r$pars[[paste(max(sc$season), "student")]]
    cat(sprintf("Student-t degrees of freedom fitted on all seasons before %d: margin %.1f, total %.1f\n", max(sc$season), nu_last[1], nu_last[2]))
    cat(sprintf(
      "outcomes: home win %.3f predicted %.3f | residual excess kurtosis margin %.2f total %.2f | cor(z_m, z_t) %.3f\n",
      mean(sc$margin_fit > 0), mean(sc$p_home), mean(z_m^4) / mean(z_m^2)^2 - 3, mean(z_t^4) / mean(z_t^2)^2 - 3, stats::cor(z_m, z_t)
    ))
    if ("p_draw_shift" %in% names(sc)) {
      tie <- sc$margin_fit == 0
      cb <- cut(abs(sc$mean_m) / sc$s_m, c(0, 0.2, 0.4, 0.8, 1.5, Inf), include.lowest = TRUE)
      cat(sprintf(
        "level scores: observed %.4f | rounded Gaussian %.4f | with shift %.4f | with shift + slope %.4f | shift by division %.4f\n",
        mean(tie), mean(sc$p_draw), mean(sc$p_draw_shift), mean(sc$p_draw_shift_slope), mean(sc$p_draw_shift_div)
      ))
      print(do.call(rbind, lapply(split(seq_len(nrow(sc)), cb, drop = TRUE), function(ix) {
        data.frame(
          n = length(ix), observed = mean(tie[ix]), rounded = mean(sc$p_draw[ix]), shift = mean(sc$p_draw_shift[ix]),
          shift_slope = mean(sc$p_draw_shift_slope[ix])
        )
      })), digits = 3)
      cat("level scores by division: observed, rounded, shift\n")
      print(do.call(rbind, lapply(split(seq_len(nrow(sc)), sc$division, drop = TRUE), function(ix) {
        data.frame(
          n = length(ix), observed = mean(tie[ix]), rounded = mean(sc$p_draw[ix]), shift = mean(sc$p_draw_shift[ix]),
          shift_div = mean(sc$p_draw_shift_div[ix])
        )
      })), digits = 3)
      cat("shift by division, fitted on all seasons before the last: ")
      print(round(r$pars[[paste(max(sc$season), "shift_div")]], 3))
      last <- r$pars[[paste(max(sc$season), "shift")]]
      last2 <- r$pars[[paste(max(sc$season), "shift_slope")]]
      cat(sprintf(
        "shift fitted on all seasons before %d: %.3f; with slope: %.3f %+.3f per sd of expected margin\n",
        max(sc$season), last[1], last2[1], last2[2]
      ))
    }
  }
}

# Basketball: a regulation-time model with explicit ties and overtime against the model
# trained on final scores, on the same matches.
for (cell in c("bb_m", "bb_f")) {
  reg <- all_scores[[paste(cell, "none_reg")]]$scores
  if (is.null(reg)) next
  ot <- reg$margin_fit == 0
  home_won <- reg$margin > 0
  cat(sprintf("\n================ %s: regulation-time model + overtime, n = %d (overtime games %d)\n", cell, nrow(reg), sum(ot)))
  cat(sprintf("final-result log likelihood: no shift %.4f | one shift %.4f | shift by division %.4f\n",
              mean(reg$ll_final_none), mean(reg$ll_final_shift), mean(reg$ll_final_shift_div)))
  cat(sprintf("final-margin log score:      no shift %.4f | one shift %.4f | shift by division %.4f\n",
              mean(reg$lsm_final_none), mean(reg$lsm_final_shift), mean(reg$lsm_final_shift_div)))
  cat(sprintf("regulation 1X2 log lik.:     no shift %.4f | one shift %.4f | shift by division %.4f\n",
              mean(reg$ll_wdl_raw), mean(reg$ll_wdl_shift), mean(reg$ll_wdl_shift_div)))
  if (sum(ot) > 0) {
    p_ot <- reg$p_home_ot_shift_div[ot]
    fav_won <- ifelse(reg$mean_m[ot] > 0, home_won[ot], !home_won[ot])
    cat(sprintf("overtime games: home won %d of %d (%.3f), model mean %.3f; favourite won %d (%.3f), model mean %.3f; log likelihood %.4f against %.4f for a coin flip\n",
                sum(home_won[ot]), sum(ot), mean(home_won[ot]), mean(p_ot), sum(fav_won), mean(fav_won), mean(pmax(p_ot, 1 - p_ot)),
                mean(log(ifelse(home_won[ot], p_ot, 1 - p_ot))), log(0.5)))
  }
  fin <- all_scores[[paste(cell, "none")]]$scores
  if (is.null(fin)) next
  j <- merge(reg, fin[, c("match_date", "home_team", "away_team", "ll_wdl_raw", "lsm_raw", "lst_raw")],
             by = c("match_date", "home_team", "away_team"), suffixes = c("", ".final"))
  cat(sprintf("against the model trained on final scores, same %d matches:\n", nrow(j)))
  cat(sprintf("  final-result log likelihood: final-score model %.4f | regulation + overtime (shift by division) %.4f\n",
              mean(j$ll_wdl_raw.final), mean(j$ll_final_shift_div)))
  print(paired_table(j, "ll_wdl_raw.final", c("ll_final_none", "ll_final_shift", "ll_final_shift_div")), digits = 3, row.names = FALSE)
  cat(sprintf("  final-margin log score: final-score model %.4f | regulation + overtime %.4f\n", mean(j$lsm_raw.final), mean(j$lsm_final_shift_div)))
  print(paired_table(j, "lsm_raw.final", c("lsm_final_none", "lsm_final_shift", "lsm_final_shift_div")), digits = 3, row.names = FALSE)
  all_scores[[paste(cell, "reg_vs_final")]] <- j
}

# Within-season term: static against AR(1), on every match both walk-forwards predicted.
# These are the plain model scores (no second stage), so no season has to be set aside.
raw_scores <- function(d, sport, reg) {
  m <- if (reg) d$home_reg - d$away_reg else d$margin
  tt <- if (reg) d$home_reg + d$away_reg else d$total
  keep <- !is.na(m)
  d <- d[keep, , drop = FALSE]
  m <- m[keep]
  tt <- tt[keep]
  o <- v2_outcome_probs(d$mean_m, d$s_m, ties = sport == "handball" || reg)
  data.frame(
    match_date = d$match_date, home_team = d$home_team, away_team = d$away_team, phase = d$phase, division = d$division,
    season = d$season, s_m = d$s_m, s_t = d$s_t,
    lsm_raw = log(v2_integer_pmf(m, d$mean_m, d$s_m, ties = sport == "handball" || reg)),
    lst_raw = log(v2_integer_pmf(tt, d$mean_t, d$s_t)),
    ll_wdl_raw = log(ifelse(m > 0, o$p_home, ifelse(m < 0, o$p_away, o$p_draw)))
  )
}
within_cmp <- list()
within_j <- list()
for (cell in names(CELLS)) {
  for (suffix in c("", "_reg")) {
    st <- all_scores[[paste0(cell, " none", suffix)]]$wf
    ar <- all_scores[[paste0(cell, " ar1", suffix)]]$wf
    if (is.null(st) || is.null(ar)) next
    sport <- CELLS[[cell]]$sport
    st <- raw_scores(st, sport, suffix == "_reg")
    ar <- raw_scores(ar, sport, suffix == "_reg")
    j <- merge(st, ar[, c("match_date", "home_team", "away_team", "lsm_raw", "lst_raw", "ll_wdl_raw", "s_m", "s_t")],
               by = c("match_date", "home_team", "away_team"), suffixes = c(".static", ".ar1"))
    cat(sprintf("\n================ %s%s: within-season AR(1) minus static, n = %d (seasons %s)\n", cell, suffix, nrow(j),
                paste(range(j$season), collapse = "-")))
    tab <- do.call(rbind, lapply(c("all", levels(j$phase)), function(g) {
      x <- if (g == "all") j else j[j$phase == g, ]
      if (nrow(x) < 5L) {
        return(NULL)
      }
      pm <- paired(x$lsm_raw.ar1, x$lsm_raw.static)
      pt <- paired(x$lst_raw.ar1, x$lst_raw.static)
      pw <- paired(x$ll_wdl_raw.ar1, x$ll_wdl_raw.static)
      data.frame(phase = g, n = nrow(x), margin = pm[["diff"]], margin_se = pm[["se"]], total = pt[["diff"]], total_se = pt[["se"]],
                 wdl = pw[["diff"]], wdl_se = pw[["se"]], sd_m_static = mean(x$s_m.static), sd_m_ar1 = mean(x$s_m.ar1))
    }))
    print(tab, digits = 3, row.names = FALSE)
    within_cmp[[paste0(cell, suffix)]] <- tab
    within_j[[paste0(cell, suffix)]] <- j
  }
}
all_scores[["within_cmp"]] <- within_cmp
all_scores[["within_j"]] <- within_j
saveRDS(all_scores, file.path(OUT, "out", "wf_scores.rds"))
