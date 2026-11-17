# Report sections: handball (draws, variance), basketball (regulation time), walk-forward.

pd <- function(sc, a, b) {
  p <- paired(sc[[a]], sc[[b]])
  pm(p[["diff"]], p[["se"]], 4)
}
tie_rows <- function(cl, sc) {
  tie <- sc$margin_fit == 0
  g <- c(list(all = seq_len(nrow(sc))), split(seq_len(nrow(sc)), sc$division, drop = TRUE))
  do.call(rbind, lapply(names(g), function(nm) {
    ix <- g[[nm]]
    data.frame(
      cell = LAB[[cl]], division = if (nm == "all") "all" else nm, n = int(length(ix)), ties = int(sum(tie[ix])),
      observed = pct(mean(tie[ix]), 2), rounded = pct(mean(sc$p_draw[ix]), 2), shift = pct(mean(sc$p_draw_shift[ix]), 2),
      shift_div = pct(mean(sc$p_draw_shift_div[ix]), 2)
    )
  }))
}
last_par <- function(cl, tag, what) {
  p <- wf[[paste(cl, tag)]]$pars
  s <- max(wf[[paste(cl, tag)]]$scores$season)
  p[[paste(s, what)]]
}

# ---------------------------------------------------------------- handball
cur <- "handball"
add("<h2 id=\"handball\">Handball: draws and the variance question</h2>")
add("<h3>A principled draw probability</h3>")
add("<p>Scores are whole numbers and the model's margin is continuous. Rounding the continuous margin to the nearest whole number gives P(draw) = P(|margin| &lt; 0.5), which is exactly production's &plusmn;0.5 band. So the band is not wrong in itself. What it cannot do is say that level scores are more or less common than a smooth curve implies. The v2 prediction layer adds one number for that: a shift of the draw probability on the logit scale, with every other margin keeping its relative share. It is estimated from the data per sex, and can be estimated per division.</p>")
if (have(hbm) && have(hbf)) {
  tt <- rbind(tie_rows("hb_m", hbm), tie_rows("hb_f", hbf))
  add(tbl(tt,
    right = names(tt)[-(1:2)],
    head = c("Cell", "Division", "Matches", "Level scores", "Observed", "Rounded Gaussian (the band)", "With one shift per sex", "With a shift per division"),
    note = "Out of sample: seasons 2023 to 2027 of the weekly walk-forward; each season's shift was estimated on the earlier seasons only."
  ))
  in_shift <- function(cl) {
    d <- desc[[cl]]$d
    v2_fit_zero_shift(v2_integer_pmf(rep(0, nrow(d)), d$mu_m, d$sd_m), d$margin == 0)
  }
  zm <- in_shift("hb_m")
  zf <- in_shift("hb_f")
  # A logit shift is an odds ratio. The ratio of rates it implies depends on the rates.
  rate_ratio <- function(p0, shift) mean(stats::plogis(stats::qlogis(p0) + shift)) / mean(p0)
  rr_hbm <- with(desc[["hb_m"]]$d, rate_ratio(v2_integer_pmf(rep(0, length(mu_m)), mu_m, sd_m), zm[["shift"]]))
  sdm <- last_par("hb_m", "none", "shift_div")
  add(sprintf(
    paste0(
      "<p><strong>Men.</strong> Level scores are more common than the band predicts: %s observed against %s out of sample, and on all data the shift is %s (se %s) on the logit scale: an odds ratio of %s, which at these rates is about %s %% more draws. ",
      "Broken down by division, the surplus is in Ol&iacute;sdeild (shift %s on the seasons before 2027) and absent in Grill 66 (%s). ",
      "It is not stable enough to pay yet: the share of level scores in Ol&iacute;sdeild swings from season to season, the shift estimated on earlier seasons overshoots the later ones, and the out-of-sample W/D/L log likelihood changes by %s with one shift and %s with a shift per division. ",
      "<strong>Women.</strong> The band is calibrated as it stands: %s observed against %s, shift %s (se %s).</p>"
    ),
    pct(mean(hbm$margin_fit == 0), 1), pct(mean(hbm$p_draw), 1), num(zm[["shift"]], 2), num(zm[["se"]], 2), num(exp(zm[["shift"]]), 2), num(100 * (rr_hbm - 1), 0),
    num(sdm[["OD"]], 2), num(sdm[["G66"]], 2), pd(hbm, "ll_wdl_shift", "ll_wdl_raw"), pd(hbm, "ll_wdl_shift_div", "ll_wdl_raw"),
    pct(mean(hbf$margin_fit == 0), 1), pct(mean(hbf$p_draw), 1), num(zf[["shift"]], 2), num(zf[["se"]], 2)
  ))
  cb <- function(sc) {
    g <- cut(abs(sc$mean_m) / sc$s_m, c(0, 0.2, 0.4, 0.8, 1.5, Inf),
      include.lowest = TRUE,
      labels = c("under 0.2 sd", "0.2 to 0.4", "0.4 to 0.8", "0.8 to 1.5", "over 1.5 sd")
    )
    tie <- sc$margin_fit == 0
    do.call(rbind, lapply(levels(g), function(l) {
      ix <- g == l
      data.frame(bin = l, n = int(sum(ix)), observed = pct(mean(tie[ix]), 1), rounded = pct(mean(sc$p_draw[ix]), 1), shift = pct(mean(sc$p_draw_shift[ix]), 1))
    }))
  }
  cbm <- cb(hbm)
  cbf <- cb(hbf)
  both <- cbind(cbm, cbf[, -1])
  names(both) <- c("bin", "n_m", "obs_m", "rnd_m", "sh_m", "n_f", "obs_f", "rnd_f", "sh_f")
  add(tbl(both,
    right = names(both)[-1],
    head = c("Expected margin, in predictive sds", "Men: matches", "observed", "band", "with shift", "Women: matches", "observed", "band", "with shift"),
    note = "Draw calibration by closeness of the match, out of sample."
  ))
  # Production's own draw probability, where it can be read off.
  pdraw <- do.call(rbind, lapply(hb, function(cl) {
    rows <- list()
    r0 <- if (have(ladder)) ladder$scores[[paste(cl, "r0_prod")]] else NULL
    if (have(r0)) {
      rows[[1]] <- data.frame(cell = LAB[[cl]], what = "Production fit at the fixed cutoff, held-out matches", n = int(nrow(r0)),
                              pred = pct(mean(r0$p_draw), 1), obs = pct(mean(r0$margin == 0), 1))
    }
    j <- if (have(cmp)) cmp[[cl]]$cmp else NULL
    if (have(j)) {
      rows[[2]] <- data.frame(cell = LAB[[cl]], what = "Production's stored forecasts", n = int(nrow(j)),
                              pred = pct(mean(j$p_draw.prod), 1), obs = pct(mean(j$margin == 0), 1))
      rows[[3]] <- data.frame(cell = LAB[[cl]], what = "v2 walk-forward, same matches (no shift)", n = int(nrow(j)),
                              pred = pct(mean(j$p_draw.v2), 1), obs = pct(mean(j$margin == 0), 1))
    }
    do.call(rbind, rows)
  }))
  if (have(pdraw)) {
    add(tbl(pdraw, right = c("n", "pred", "obs"), head = c("Cell", "Forecast", "Matches", "Mean predicted draw probability", "Level scores observed"),
            note = "Production's draw probability is the share of its posterior draws with |margin| at most 0.5. These samples are small; the September audit's 5.5 % predicted against 4.6 % observed was on a different sample."))
  }
  add("<h3>Possession-informed variance against a free variance</h3>")
  add("<p>The literature describes handball goals as binomial counts: about 50 to 60 attacks per side at about 50 % efficiency, with both sides having the same number of attacks. That gives a variance for the margin and the total that follows from the expected scores, with two parameters (attacks per side and the variance of that number) in place of two free sds. The test: fit each variance form on the walk-forward predictions of earlier seasons, score the next season, compare with the free constant variance.</p>")
  vr <- function(sc, cols) vapply(cols, function(cn) if (cn %in% names(sc)) pd(sc, cn, sub("_.*$", "_raw", cn)) else "–", "")
  forms <- c(
    constant = "Free variance, rescaled on past errors", by_mismatch = "Variance grows with the mismatch (margin) / the expected total",
    by_division = "One variance per division", possession = "Possession-informed (binomial with shared attacks)",
    student = "Student-t with fitted degrees of freedom"
  )
  vt <- data.frame(
    form = unname(forms),
    mm = vr(hbm, paste0("lsm_", names(forms))), mt = vr(hbm, paste0("lst_", names(forms))),
    fm = vr(hbf, paste0("lsm_", names(forms))), ft = vr(hbf, paste0("lst_", names(forms)))
  )
  add(tbl(vt,
    right = names(vt)[-1],
    head = c("Variance form", "Men: margin", "Men: total", "Women: margin", "Women: total"),
    note = sprintf(
      "Change in the out-of-sample integer log score per match against the plain model (one free sd for the margin, one for the total), with its match-clustered standard error. Positive is better. Men n = %s, women n = %s.",
      int(nrow(hbm)), int(nrow(hbf))
    )
  ))
  # Variance of a side's score around its fitted mean, relative to a binomial with 55 attacks.
  binom55 <- function(cl, div) {
    d <- desc[[cl]]$d
    d <- d[d$division == div, ]
    mu <- c((d$mu_t + d$mu_m) / 2, (d$mu_t - d$mu_m) / 2)
    y <- c(d$home_score, d$away_score)
    num(mean((y - mu)^2 / (mu * (1 - mu / 55))), 2)
  }
  nu_m <- last_par("hb_m", "none", "student")
  nu_f <- last_par("hb_f", "none", "student")
  add(sprintf(
    paste0(
      "<p>Nothing beats the free variance. The possession-informed form is no better for men and clearly worse for the women's margin (%s), the cells the literature disagrees about. ",
      "The scores do look binomial in one respect: each side's variance is close to that of a binomial with 55 attacks in Ol&iacute;sdeild (ratio %s men, %s women; %s and %s in Grill 66). But the possession form ties the margin variance and the total variance together through its two parameters, and two free sds fit both better. ",
      "A Student-t with fitted degrees of freedom (%s for the men's margin, %s for the women's) changes the score by less than its standard error. <strong>Handball keeps a Gaussian with one free sd for the margin and one for the total.</strong></p>"
    ),
    pd(hbf, "lsm_possession", "lsm_raw"), binom55("hb_m", "OD"), binom55("hb_f", "OD"), binom55("hb_m", "G66"), binom55("hb_f", "G66"),
    num(nu_m[1], 0), num(nu_f[1], 0)
  ))
} else {
  add(missing_note("the handball walk-forward"))
}

# ---------------------------------------------------------------- basketball
cur <- "basketball"
add("<h2 id=\"basketball\">Basketball: regulation time</h2>")
add("<h3>The data exist, in the source already in use</h3>")
cov <- do.call(rbind, lapply(split(per, paste(per$sex, per$division)), function(x) {
  data.frame(
    cell = sprintf("%s, %s", if (x$sex[1] == "male") "Men" else "Women", if (x$division[1] == "BD") "B&oacute;nusdeild" else "1. deild"),
    n = int(nrow(x)), ok = int(sum(x$ok)), share = pct(mean(x$ok), 1), ot = int(sum(x$ok & x$overtime %in% TRUE)),
    ot_share = pct(mean(x$overtime[x$ok] %in% TRUE), 1)
  )
}))
add(sprintf(
  paste0(
    "<p>The Baskethotel results export that the production ingest reads has a game id in its first column, which the parser drops. Baskethotel's game-card widget (number 400 in its <code>api.js</code>, the same widget kki.is embeds) returns that game's four quarter scores. One request per game: %s cards were fetched once, 0.35 s apart, into the scratch folder; the production ingest was not touched. ",
    "The card always lists four quarters, also for overtime games, so regulation time is the quarter sum and an overtime game is a level quarter sum with a final score above it. Genius Sports LiveStats was not needed and was not tried.</p>"
  ),
  int(3286)
))
add(tbl(cov,
  right = names(cov)[-1], head = c("League", "Stored played games", "With a regulation-time score", "Share", "Went to overtime", "Share of those"),
  note = sprintf(
    "Seasons 2021 to 2027. All %s stored games matched a card. The %s without a usable regulation score are %s walkovers, %s cards with no quarter block and %s whose quarters contradict the recorded final.",
    int(nrow(per)), int(sum(!per$ok)), int(sum(!per$ok & .is_forfeit(per, "basketball"))),
    int(sum(!per$ok & !.is_forfeit(per, "basketball") & (is.na(per$n_periods) | per$n_periods == 0))),
    int(sum(!per$ok & !.is_forfeit(per, "basketball") & !is.na(per$n_periods) & per$n_periods >= 4))
  )
))
if (have(bbm) && have(bbf)) {
  ks <- -8:8
  obs <- vapply(ks, function(k) mean(bbm$margin_fit == k), 1)
  prd <- vapply(ks, function(k) mean(v2_integer_pmf(rep(k, nrow(bbm)), bbm$mean_m, bbm$s_m)), 1)
  add(
    "<figure>", svg_profile(ks, obs, prd, obs_label = "observed share of games", exp_label = "rounded Gaussian (out of sample)"),
    sprintf(
      "<figcaption>Basketball men, regulation-time margin, %s walk-forward predictions. Level after regulation: %s of games against %s under the smooth curve. Margins of one or two points either way are not depleted to pay for it. The usual explanation is end-game play: the trailing side fouls and shoots threes until the score is level or the game is gone.</figcaption></figure>",
      int(nrow(bbm)), pct(mean(bbm$margin_fit == 0), 1), pct(mean(bbm$p_draw), 1)
    )
  )
  add("<h3>Tie inflation and the overtime model</h3>")
  tt <- rbind(tie_rows("bb_m", bbm), tie_rows("bb_f", bbf))
  add(tbl(tt,
    right = names(tt)[-(1:2)],
    head = c("Cell", "Division", "Games", "Level after regulation", "Observed", "Rounded Gaussian", "With one shift per sex", "With a shift per division"),
    note = "Out of sample, seasons 2023 to 2027; each season's shift estimated on earlier seasons."
  ))
  sm <- last_par("bb_m", "none_reg", "shift_div")
  sf <- last_par("bb_f", "none_reg", "shift")
  ot_m <- bbm[bbm$margin_fit == 0, ]
  ot_f <- bbf[bbf$margin_fit == 0, ]
  ot <- rbind(ot_m[, c("margin", "mean_m", "p_home_ot_shift_div")], ot_f[, c("margin", "mean_m", "p_home_ot_shift_div")])
  fav <- ifelse(ot$mean_m > 0, ot$margin > 0, ot$margin < 0)
  ll_ot <- mean(log(ifelse(ot$margin > 0, ot$p_home_ot_shift_div, 1 - ot$p_home_ot_shift_div)))
  add(sprintf(
    paste0(
      "<p><strong>Men.</strong> The tie parameter is needed and it differs by division: shift %s in B&oacute;nusdeild (an odds ratio of %s; at these rates about %s times as many level games as the smooth curve gives, and %s times as many were observed out of sample) and %s in 1. deild. It improves the out-of-sample regulation 1X2 log likelihood by %s per game and the regulation-margin log score by the same amount. ",
      "<strong>Women.</strong> A small surplus (%s against %s), shift %s, no measurable gain yet (%s). ",
      "<strong>Overtime</strong> is modelled as a short continuation of the same game: a five-minute period is one eighth of regulation, so its margin has one eighth of the mean and of the variance, and a level overtime is replayed. On the %s overtime games in the scored seasons the home side won %s and the pre-match favourite %s (%s); the continuation model expected the favourite to win %s, and its log likelihood is %s against %s for a coin flip. ",
      "So in these leagues overtime has so far been a coin flip, unlike the NBA; the continuation model is kept because it costs nothing and tends to the coin flip for evenly matched teams, but the evidence does not yet support it over 50 : 50.</p>"
    ),
    num(sm[["BD"]], 2), num(exp(sm[["BD"]]), 1), num(rate_ratio(bbm$p_draw[bbm$division == "BD"], sm[["BD"]]), 1),
    num(mean(bbm$margin_fit[bbm$division == "BD"] == 0) / mean(bbm$p_draw[bbm$division == "BD"]), 1),
    num(sm[["1D"]], 2), pd(bbm, "ll_wdl_shift_div", "ll_wdl_raw"),
    pct(mean(bbf$margin_fit == 0), 1), pct(mean(bbf$p_draw), 1), num(sf[1], 2), pd(bbf, "ll_wdl_shift", "ll_wdl_raw"),
    int(nrow(ot)), int(sum(ot$margin > 0)), int(sum(fav)), pct(mean(fav), 0), pct(mean(pmax(ot$p_home_ot_shift_div, 1 - ot$p_home_ot_shift_div)), 0),
    num(ll_ot, 3), num(log(0.5), 3)
  ))
  rvf <- lapply(bb, function(cl) wf[[paste(cl, "reg_vs_final")]])
  if (all(vapply(rvf, have, TRUE))) {
    rt <- do.call(rbind, lapply(seq_along(bb), function(i) {
      j <- rvf[[i]]
      wl_col <- paste0("ll_final_", REG_SHIFT[[bb[i]]])
      mg_col <- paste0("lsm_final_", REG_SHIFT[[bb[i]]])
      data.frame(
        cell = LAB[[bb[i]]], n = int(nrow(j)),
        wl = sprintf("%s / %s", num(mean(j$ll_wdl_raw.final), 4), num(mean(j[[wl_col]]), 4)), wl_d = pd(j, wl_col, "ll_wdl_raw.final"),
        mg = sprintf("%s / %s", num(mean(j$lsm_raw.final), 4), num(mean(j[[mg_col]]), 4)), mg_d = pd(j, mg_col, "lsm_raw.final")
      )
    }))
    rvf_z <- vapply(seq_along(bb), function(i) {
      j <- rvf[[i]]
      zz <- function(a, b) paired(j[[a]], j[[b]])[["diff"]] / paired(j[[a]], j[[b]])[["se"]]
      c(wl = zz(paste0("ll_final_", REG_SHIFT[[bb[i]]]), "ll_wdl_raw.final"), mg = zz(paste0("lsm_final_", REG_SHIFT[[bb[i]]]), "lsm_raw.final"))
    }, numeric(2))
    add("<h3>Does training on regulation time cost anything for the final result?</h3>")
    add(tbl(rt,
      right = names(rt)[-1],
      head = c(
        "Cell", "Games", "Final-result log likelihood: final-score model / regulation + overtime", "Difference",
        "Final-margin log score: final-score model / regulation + overtime", "Difference"
      ),
      note = "Same matches, both walk-forwards. The final-score model is the same v2 model trained on final scores, with the home side winning when the continuous margin is positive. A final score cannot be level, so the final-score model's margin score uses its rounded margin with the mass on zero removed and the rest renormalised; the regulation-plus-overtime model has no mass there to begin with. Tie treatment of the regulation model: a shift per division for men, one shift for women."
    ))
    add(sprintf(
      "<p>In standard errors, the final-result difference is %s for men and %s for women, and the final-margin difference %s and %s. %s One model trained on regulation-time scores serves both products: the regulation 1X2 the bookmaker settles on, and the final result the league table needs, through the tie probability and overtime.</p>",
      num(rvf_z["wl", 1], 1, plus = TRUE), num(rvf_z["wl", 2], 1, plus = TRUE), num(rvf_z["mg", 1], 1, plus = TRUE), num(rvf_z["mg", 2], 1, plus = TRUE),
      if (all(rvf_z > -2)) {
        "So training on regulation time costs nothing measurable for the final result; what it gains on the final margin is small."
      } else {
        "So on this evidence training on regulation time costs something for the final result."
      }
    ))
  } else {
    add(missing_note("the walk-forward of the same model trained on final scores, for the regulation-versus-final comparison"))
  }
  nu_m <- last_par("bb_m", "none_reg", "student")
  nu_f <- last_par("bb_f", "none_reg", "student")
  r4r5 <- if (have(ladder)) {
    x <- ladder$tab[ladder$tab$cell == "bb_m" & ladder$tab$rung %in% c("r4_ident", "r5_gauss"), ]
    if (nrow(x) == 2) x$elpd[x$rung == "r4_ident"] - x$elpd[x$rung == "r5_gauss"] else NA
  } else {
    NA
  }
  add(sprintf(
    paste0(
      "<p><strong>Tails: is a Student-t worth its cost?</strong> Basketball margins are a little heavier-tailed than Gaussian, most in women's 1. deild. %s",
      "In the walk-forward, a Student-t predictive with the same sd and fitted degrees of freedom (%s men, %s women) improves the regulation-margin log score by %s for men and %s for women, ",
      "and leaves the result probabilities where they were (men: regulation 1X2 %s, final result %s; women: %s and %s). Variance by division or by mismatch does nothing (largest change %s). ",
      "So the heavier tail sharpens the margin distribution a little and changes no win probability. It is not worth giving up the exact Gaussian fit for; the proposal keeps the Gaussian in the engine and offers the Student-t as a prediction-layer setting for the basketball margin distribution.</p>"
    ),
    if (is.na(r4r5)) "" else sprintf("In the ladder, replacing the Student-t by a Gaussian costs the men's fit %s of leave-one-out density (handball: about 1) and changes no held-out score. ", num(r4r5, 0)),
    num(nu_m[1], 0), num(nu_f[1], 0), pd(bbm, "lsm_student", "lsm_raw"), pd(bbf, "lsm_student", "lsm_raw"),
    pd(bbm, "ll_wdl_student", "ll_wdl_shift_div"), pd(bbm, "ll_final_student", "ll_final_shift_div"),
    pd(bbf, "ll_wdl_student", "ll_wdl_shift_div"), pd(bbf, "ll_final_student", "ll_final_shift_div"),
    pd(bbf, "lsm_by_mismatch", "lsm_raw")
  ))
} else {
  add(missing_note("the basketball walk-forward"))
}

# ---------------------------------------------------------------- walk-forward
cur <- "wf"
add("<h2 id=\"wf\">Leak-free walk-forward</h2>")
add("<p>For each cell, the model was refitted at every weekly cutoff from the start of season 2022 to 14 November 2026 on the results dated up to the cutoff (production's training filter, forfeits dropped) and asked for the matches of the following seven days. Hyperparameters are re-estimated at every cutoff. Season 2022 is used only to fit the second-stage quantities; scores are for seasons 2023 to 2027. The model is the static form of v2 fitted with R-INLA (hyperparameters at their mode); basketball is trained on regulation-time scores. Margin and total are scored as two separate whole-number distributions. The joint (home, away) score distribution was not scored: the parity-respecting sampler for it exists and is unit-tested (<code>v2_sample_scores()</code> in <code>R/v2-margin-total-predict.R</code>), but no walk-forward score in this report uses it.</p>")
# `zm`, `zt`: columns holding standardised residuals against another predictive than the
# one the model is trained on (the final-score predictive of a regulation-time model).
wf_row <- function(cl, sc, label, m, t, w, zm = NULL, zt = NULL) {
  z_m <- if (is.null(zm)) (sc$margin_fit - sc$mean_m) / sc$s_m else sc[[zm]]
  z_t <- if (is.null(zt)) (sc$total_fit - sc$mean_t) / sc$s_t else sc[[zt]]
  data.frame(
    cell = LAB[[cl]], what = label, n = int(nrow(sc)), lsm = num(mean(sc[[m]]), 4), lst = num(mean(sc[[t]]), 4), wdl = num(mean(sc[[w]]), 4),
    sdz = sprintf("%s / %s", num(stats::sd(z_m), 2), num(stats::sd(z_t), 2)),
    cov = sprintf("%s / %s", pct(mean(abs(z_m) < stats::qnorm(0.9)), 0), pct(mean(abs(z_t) < stats::qnorm(0.9)), 0))
  )
}
if (have(hbm) && have(bbm)) {
  wt <- rbind(
    wf_row("hb_m", hbm, "full time", "lsm_raw", "lst_raw", "ll_wdl_raw"),
    wf_row("hb_f", hbf, "full time", "lsm_raw", "lst_raw", "ll_wdl_raw"),
    wf_row("bb_m", bbm, "regulation time, tie shift by division", "lsm_shift_div", "lst_raw", "ll_wdl_shift_div"),
    wf_row("bb_m", bbm, "final result, through overtime", "lsm_final_shift_div", "lst_final_shift_div", "ll_final_shift_div",
           zm = "zm_final_shift_div", zt = "zt_final_shift_div"),
    wf_row("bb_f", bbf, "regulation time, one tie shift", "lsm_shift", "lst_raw", "ll_wdl_shift"),
    wf_row("bb_f", bbf, "final result, through overtime", "lsm_final_shift", "lst_final_shift", "ll_final_shift",
           zm = "zm_final_shift", zt = "zt_final_shift")
  )
  add(tbl(wt,
    right = names(wt)[-(1:2)],
    head = c(
      "Cell", "Scored as", "Matches", "Margin log score", "Total log score", "W/D/L log likelihood", "Sd of standardised residual: margin / total",
      "80 % interval covers: margin / total"
    ),
    note = "Per match, integer scale. A standardised residual sd of 1 and a coverage of 80 % mean the predictive spread is right; the 80 % band is the central one of a Gaussian with the predictive's mean and sd. Handball W/D/L uses the plain rounded margin (no shift). The basketball \"final result\" rows have two outcomes, not three, and their residuals are standardised with the mean and sd of the final-score predictive itself: the regulation margin with its level mass moved through overtime, and the two-part total. A basketball final margin is scored on a distribution with no mass on a level score."
  ))
  ph <- do.call(rbind, lapply(list(list("hb_m", hbm, "ll_wdl_raw"), list("hb_f", hbf, "ll_wdl_raw"), list("bb_m", bbm, "ll_final_shift_div"), list("bb_f", bbf, "ll_final_shift")), function(a) {
    sc <- a[[2]]
    do.call(rbind, lapply(levels(sc$phase), function(p) {
      x <- sc[sc$phase == p, ]
      if (nrow(x) == 0) {
        return(NULL)
      }
      z_m <- (x$margin_fit - x$mean_m) / x$s_m
      z_t <- (x$total_fit - x$mean_t) / x$s_t
      data.frame(
        cell = LAB[[a[[1]]]], phase = p, n = int(nrow(x)), lsm = num(mean(x$lsm_raw), 3), lst = num(mean(x$lst_raw), 3), wdl = num(mean(x[[a[[3]]]]), 3),
        mz = num(mean(z_m), 2, plus = TRUE), sdz = sprintf("%s / %s", num(stats::sd(z_m), 2), num(stats::sd(z_t), 2)), psd = num(mean(x$s_m), 2)
      )
    }))
  }))
  add("<h3>By season phase</h3>")
  add(tbl(ph,
    right = names(ph)[-(1:2)],
    head = c(
      "Cell", "Phase", "Matches", "Margin log score", "Total log score", "Result log likelihood", "Mean standardised margin residual",
      "Sd of standardised residual: margin / total", "Predictive sd of the margin"
    ),
    note = "Phases are weeks of the regular season; a basketball play-off match is one of a pair that meets at least three times within three weeks, handball's are its PO division. The last six weeks are counted back from a season's last regular-season week; the season still being played has not reached them, so its matches after week six are in the middle row. For basketball the residual columns are of the regulation-time scores the model is trained on. The mean standardised margin residual is from the home side's view. The predictive sd is widest in the first six weeks because the new season's level is still uncertain: that is the season-boundary term at work."
  ))
  # Sd of the standardised margin residual in one phase, with a bootstrap standard error.
  # WHY bootstrap: the normal-theory se of an sd assumes Gaussian residuals. The second
  # check of this report found resampled standard errors 10 to 28 % larger here.
  phase_sd_of <- function(sc, p) {
    x <- sc[sc$phase == p, ]
    z <- (x$margin_fit - x$mean_m) / x$s_m
    se <- withr::with_seed(20261116, stats::sd(replicate(2000, stats::sd(sample(z, replace = TRUE)))))
    c(sd = stats::sd(z), se = se)
  }
  cells4 <- list(hbm, hbf, bbm, bbf)
  late <- vapply(cells4, phase_sd_of, numeric(2), p = "last 6 weeks")
  mid <- vapply(cells4, phase_sd_of, numeric(2), p = "middle")
  rise <- late["sd", ] - mid["sd", ]
  rise_se <- sqrt(late["se", ]^2 + mid["se", ]^2)
  add(sprintf(
    paste0(
      "<p>If team strength drifted inside a season in a way the static model misses, the residuals would be more spread late in the season than in the middle of it. ",
      "The sd of the standardised margin residual in the last six weeks is %s (the four cells in the table's order), and its rise over the middle of the season is %s, with standard errors from resampling matches. %s ",
      "This is the check asked for before dropping the within-season term for basketball, where mid-season import changes are the obvious worry.</p>"
    ),
    paste(num(late["sd", ], 2), collapse = ", "),
    paste(sprintf("%s (se %s)", num(rise, 2, plus = TRUE), num(rise_se, 2)), collapse = ", "),
    if (max(rise / rise_se) < 2) {
      sprintf(
        "None of the rises is two standard errors. Where the level is off, it is off all season: %s's predictive spread is too narrow by %s to %s %% in every regular-season phase.",
        tolower(LAB[[which.max(late["sd", ])]]),
        num(100 * (min(vapply(c("first 6 weeks", "middle", "last 6 weeks"), function(p) phase_sd_of(cells4[[which.max(late["sd", ])]], p)[["sd"]], 1)) - 1), 0),
        num(100 * (max(vapply(c("first 6 weeks", "middle", "last 6 weeks"), function(p) phase_sd_of(cells4[[which.max(late["sd", ])]], p)[["sd"]], 1)) - 1), 0)
      )
    } else {
      sprintf("The rise for %s is more than two standard errors: its late-season margins are more spread than predicted.", tolower(LAB[[which.max(rise / rise_se)]]))
    }
  ))
  # Fixed cutoff: how large is the within-season term, and what does it buy?
  add("<h3>Does a within-season term earn its place?</h3>")
  if (all(vapply(iv, have, TRUE)) && have(ladder)) {
    fx <- do.call(rbind, lapply(names(CELLS), function(cl) {
      h <- iv[[cl]][["margin ar1 auto"]]$hyper_sd
      g <- function(nm) {
        r <- h[h$parameter == nm, ]
        sprintf("%s [%s, %s]", num(r$q50, 2), num(r$q05, 2), num(r$q95, 2))
      }
      cpo <- function(w) sum(log(iv[[cl]][[paste("margin", w, "auto")]]$cpo)) + sum(log(iv[[cl]][[paste("total", w, "auto")]]$cpo))
      st <- ladder$scores[[paste(cl, "v2 static, INLA full")]]
      ar <- ladder$scores[[paste(cl, "v2 + within-season AR(1), INLA full")]]
      data.frame(cell = LAB[[cl]], sw = g("sd: w_t"), su = g("sd: u_s"), sm = g("sd: the Gaussian observations"),
                 loo = num(cpo("ar1") - cpo("none"), 1, plus = TRUE), n = int(nrow(st)),
                 m = pm(paired(ar$ls_margin, st$ls_margin)[["diff"]], paired(ar$ls_margin, st$ls_margin)[["se"]], 3),
                 w = pm(paired(ar$ll_wdl, st$ll_wdl)[["diff"]], paired(ar$ll_wdl, st$ll_wdl)[["se"]], 3))
    }))
    add(tbl(fx, right = names(fx)[-1],
            head = c("Cell", "Within-season sd (margin model)", "Season-level sd", "Match sd", "Leave-one-out gain from the within-season term",
                     "Held-out matches", "Held-out margin log score: AR(1) &minus; static", "Held-out result log likelihood"),
            note = "Fixed cutoff 31 January 2026, R-INLA with full hyperparameter integration. The within-season term is a weekly AR(1) around the season level with weekly correlation 0.98; its sd is the size of a team's typical departure from its season level. Median [5 %, 95 %]."))
    add("<p>In handball the within-season sd is half a goal (men) to one goal (women) against a match sd near five, and the term buys nothing. In basketball it is five to seven points, about half the match sd: teams do move within a season, as the import changes would suggest. Whether tracking that movement improves forecasts is what the walk-forward tests.</p>")
  }
  wr <- rd("within_rho.rds")
  if (have(wr)) {
    rng <- function(cl, col) paste(num(range(wr[wr$cell == cl, col]), if (col == "loo") 1 else 3), collapse = " to ")
    add(sprintf(
      "<p class=\"note\">The weekly correlation of the within-season term is fixed, not estimated, and the data cannot tell 0.90 from 0.98: for the basketball margin model the leave-one-out density stays within %s (men) and %s (women) across 0.90, 0.95 and 0.98, and the held-out margin log score within %s and %s.</p>",
      rng("bb_m", "loo"), rng("bb_f", "loo"), rng("bb_m", "ls_margin"), rng("bb_f", "ls_margin")
    ))
  }
  wc <- wf$within_cmp
  if (have(wc) && length(wc) > 0) {
    wtab <- do.call(rbind, lapply(names(wc), function(nm) {
      x <- wc[[nm]]
      data.frame(cell = LAB[[sub("_reg$", "", nm)]], phase = x$phase, n = int(x$n), m = pm(x$margin, x$margin_se, 4), t = pm(x$total, x$total_se, 4), w = pm(x$wdl, x$wdl_se, 4))
    }))
    add(tbl(wtab,
      right = c("n", "m", "t", "w"),
      head = c("Cell", "Phase", "Matches", "Margin log score", "Total log score", "Result log likelihood"),
      note = "Walk-forward with the within-season AR(1) minus the static model, same matches, seasons 2025 to 2027. Positive favours the within-season term."
    ))
  } else {
    add(missing_note("the walk-forward with the within-season AR(1) (queued behind the Stan fits)"))
  }
  by_s <- do.call(rbind, lapply(list(list("hb_m", hbm, "ll_wdl_raw"), list("hb_f", hbf, "ll_wdl_raw"), list("bb_m", bbm, "ll_final_shift_div"), list("bb_f", bbf, "ll_final_shift")), function(a) {
    sc <- a[[2]]
    do.call(rbind, lapply(split(sc, list(sc$division, sc$season), drop = TRUE), function(x) {
      data.frame(
        cell = LAB[[a[[1]]]], division = x$division[1], season = x$season[1], n = int(nrow(x)), lsm = num(mean(x$lsm_raw), 3),
        lst = num(mean(x$lst_raw), 3), wdl = num(mean(x[[a[[3]]]]), 3), home = sprintf("%s / %s", pct(mean(x$margin > 0), 0), pct(mean(x$p_home), 0))
      )
    }))
  }))
  by_s <- by_s[order(match(by_s$cell, LAB), by_s$division, by_s$season), ]
  add(
    "<details><summary>By division and season</summary>",
    tbl(by_s,
      right = names(by_s)[-(1:2)], head = c("Cell", "Division", "Season", "Matches", "Margin log score", "Total log score", "Result log likelihood", "Home wins: observed / predicted"),
      note = "Basketball \"predicted\" home wins are regulation-time probabilities before the tie shift, so they sit a little above the observed share."
    ), "</details>"
  )

  if (have(wfst) && have(wfst2)) {
    run_lab <- c(
      hb_m_none = "Handball men", hb_f_none = "Handball women", bb_m_none_reg = "Basketball men", bb_f_none_reg = "Basketball women",
      bb_m_ar1_reg = "Basketball men, within-season AR(1)", bb_f_ar1_reg = "Basketball women, within-season AR(1)",
      bb_m_none = "Basketball men, trained on final scores", bb_f_none = "Basketball women, trained on final scores"
    )
    main4 <- names(run_lab)[1:4]
    w <- wfst$wf
    w2 <- wfst2$wf
    add("<h3>The R-INLA fits, run four times</h3>")
    add(paste0(
      "<p>The numbers in this report come from the third of four runs of the R-INLA fits. ",
      "The <strong>first</strong> run is the one the report was first written from; it did not record whether a fit's search for the hyperparameter mode had succeeded. ",
      "The <strong>second</strong> repeated every fit with that status kept. Both used R-INLA's default settings. ",
      "The <strong>third and fourth</strong> are the same set of fits made twice more, with R-INLA's online internal optimisation switched off inside the fitter (<code>fit_v2_inla(reproducible = TRUE)</code>, now its default), which is the setting under which a fit comes out the same in two R processes. ",
      "All four used the data and the config of the branch point, and the fitting code is otherwise the same; the third and fourth also repeat a failed mode search from a cold start by themselves. ",
      "The fixed-cutoff fits of the third and fourth runs were made once more after their first comparison, for the reason given next.</p>"
    ))
    if (have(ident)) {
      f <- ident$files
      add(sprintf(
        "<p><strong>Are the third and fourth runs identical?</strong> %s Compared: every stored number of %s result files (%s numbers: the predictions and hyperparameters of all eight walk-forwards, the descriptive fits, the fixed-cutoff fits with their posterior draws, the weekly-correlation fits), in the order stored, leaving out only timings, the record of what else the machine was doing, and data-frame row names. %s</p>",
        if (all(f$identical)) "<span class=\"c\">Yes, in every stored number.</span>" else "<span class=\"x\">No.</span>",
        int(nrow(f)), int(sum(f$numbers)),
        if (all(f$identical)) {
          "So running any of these fits again, from the same commit on the same data and machine, reproduces every stored result."
        } else {
          sprintf(
            "Files that differ, with the largest absolute difference in any number: %s.",
            paste(sprintf("<code>%s</code> (%s)", f$file[!f$identical], num(f$max_abs_diff[!f$identical], 4)), collapse = "; ")
          )
        }
      ))
      if (nzchar(draws_account())) add("<p>", draws_account(), " ", provenance_txt(), "</p>")
    }
    left_out <- sum(unlist(lapply(wf[grep(" (none|none_reg|ar1|ar1_reg)$", names(wf))], function(x) x$failed_fit_rows_left_out)), na.rm = TRUE)
    add(sprintf(
      paste0(
        "<p><strong>Mode status.</strong> In the run reported here, %s of the %s walk-forward fits were repeated from a cold start because the warm-started search failed or threw (%s); after that %s were still failed, and %s predictions are left out of the scores for that reason. The %s fits outside the walk-forward: %s with a status other than 0. ",
        "In the second run, %s of %s walk-forward fits had ended with a failed search (%s); each succeeded when fitted again from a cold start. The option does not make a failed warm start rarer.%s</p>"
      ),
      int(sum(w$retried, na.rm = TRUE)), int(sum(w$fits)),
      paste(sprintf("%s (%s)", run_lab[w$run[w$retried > 0]], int(w$retried[w$retried > 0])), collapse = "; "),
      none_or(sum(w$status_not_zero)), no_or(left_out),
      int(nrow(wfst$other)), none_or(sum(!(wfst$other$status %in% 0L))),
      int(sum(w2$status_not_zero)), int(sum(w2$fits)),
      paste(sprintf("%s (%s)", run_lab[w2$run[w2$status_not_zero > 0]], int(w2$status_not_zero[w2$status_not_zero > 0])), collapse = "; "),
      # The repeat's retry flags are among the stored values the comparison found identical.
      if (have(ident) && all(ident$files$identical)) " With it off, which searches fail is repeatable as well: the repeat of the reported run repeated the same fits." else ""
    ))
    if (nzchar(cold_account(nrow(wfst$other)))) add("<p>", cold_account(nrow(wfst$other)), "</p>")
    dist_tab <- function(x) {
      data.frame(
        run = run_lab[x$run], cutoffs = int(x$cutoffs), matches = int(x$matches),
        dm = sprintf("%s / %s", num(x$median_abs_diff_margin, 3), num(x$max_abs_diff_margin, 2)), big = pct(x$share_margin_over_half, 1),
        dt = sprintf("%s / %s", num(x$median_abs_diff_total, 3), num(x$max_abs_diff_total, 2)),
        hy = sprintf("%s of %s", int(x$cutoffs_hyper_differs_25pct), int(x$cutoffs))
      )
    }
    dist_head <- c(
      "Walk-forward", "Cutoffs", "Matches", "Expected margin: median / largest difference between the runs",
      "Matches whose expected margin differs by more than half a goal or point", "Expected total: median / largest difference",
      "Cutoffs where some hyperparameter's median differs by more than 25 %"
    )
    add("<p>How far apart the runs are. First, the two runs made with R-INLA's defaults, which differ in nothing but the run:</p>")
    add(tbl(dist_tab(w2), right = c("cutoffs", "matches", "dm", "big", "dt", "hy"), head = dist_head,
            note = "First run against second run. Same fitting code, same data, same weekly cutoffs, each fit started from the previous cutoff's mode. The first run was made on a heavily loaded machine."))
    add("<p>Then the second run against the run reported here, which differ in the option only:</p>")
    add(tbl(dist_tab(w), right = c("cutoffs", "matches", "dm", "big", "dt", "hy"), head = dist_head,
            note = "Second run (R-INLA's defaults) against the third (online optimisation off)."))
    score_tab <- function(x, a, b) {
      data.frame(cell = LAB[x$cell], score = x$score, n = int(x$n), a = num(x$first_run, 4), b = num(x$second_run, 4), d = pm(x$diff, x$diff_se, 4), mx = num(x$max_abs_match_diff, 2))
    }
    sc_def <- wfst2$scores
    sc_fin <- wfst$scores
    both <- cbind(score_tab(sc_def), score_tab(sc_fin)[, c("b", "d", "mx")])
    names(both) <- c("cell", "score", "n", "r1", "r2", "d12", "mx12", "r3", "d23", "mx23")
    add(tbl(both[, c("cell", "score", "n", "r1", "r2", "r3", "d12", "d23", "mx23")],
      right = c("n", "r1", "r2", "r3", "d12", "d23", "mx23"),
      head = c("Cell", "Score", "Matches", "First run", "Second run", "Third run (reported)", "Second minus first", "Third minus second", "Largest change for one match, second to third"),
      note = "Mean out-of-sample log scores of the plain model, margin and total on the integer scale. Basketball is regulation time."
    ))
    # Cold starts in separate processes, with R-INLA's defaults and with its online optimisation off.
    cold_txt <- ""
    if (have(wfst2$cold)) {
      cd <- wfst2$cold
      d0 <- cd[cd$setting == "R-INLA defaults", ]
      d1 <- cd[cd$setting == "internal.opt = FALSE", ]
      cold_txt <- sprintf(
        "Why the option: a warm start is not the whole story, because a single fit started cold was not always the same either. The margin model of two handball men cutoffs was fitted from a cold start in separate R processes. With R-INLA's defaults, %s processes gave the most frequent result at the two cutoffs (the others differed by up to %s in an expected margin). With the online optimisation off, %s did (R-INLA's help says no more about the option than \"internal online optimisations\"). ",
        paste(sprintf("%s of %s", int(d0$most_frequent), int(d0$processes)), collapse = " and "), num(max(d0$max_abs_diff_between_processes), 3),
        paste(sprintf("%s of %s", int(d1$most_frequent), int(d1$processes)), collapse = " and ")
      )
    }
    add(sprintf(
      paste0(
        "<p><strong>What this shows.</strong> With R-INLA's defaults, two runs of the same walk-forward moved a match's expected margin by up to %s, and some hyperparameter's median by more than a quarter in %s of the %s cutoffs of the four main walk-forwards, while no cell's mean score moved by more than %s. ",
        "What differs is how a team's strength is divided between its long-run club level and its season level with the memory between seasons. With six seasons those are close to interchangeable, so several divisions describe the data about equally well; each weekly fit starts from the previous week's answer and stays with the division it began with. %s",
        "The option removes the difference between runs, not the choice between descriptions: the run reported here differs from the second by up to %s in a match's expected margin and by at most %s in a cell's mean score, which is one more draw from the same spread, now a repeatable one. ",
        "A status of 0 says the search ended at a mode, not that the mode is the only one.</p>"
      ),
      num(max(w2$max_abs_diff_margin[w2$run %in% main4]), 2), int(sum(w2$cutoffs_hyper_differs_25pct[w2$run %in% main4])), int(sum(w2$cutoffs[w2$run %in% main4])),
      num(max(abs(sc_def$diff)), 4), cold_txt,
      num(max(w$max_abs_diff_margin[w$run %in% main4]), 2), num(max(abs(sc_fin$diff)), 4)
    ))
  }

  add("<h3>Against production's stored forecasts</h3>")
  if (have(cmp)) {
    ct <- do.call(rbind, lapply(names(CELLS), function(cl) {
      j <- cmp[[cl]]$cmp
      if (is.null(j)) {
        return(NULL)
      }
      data.frame(
        cell = LAB[[cl]], n = int(nrow(j)), m = pd(j, "lsm_raw", "ls_margin"), t = pd(j, "lst_raw", "ls_total"), w = pd(j, "ll_wdl_raw", "ll_wdl"),
        em = sprintf("%s / %s / %s", num(mean(j$mean_m.prod), 2), num(mean(j$mean_m.v2), 2), num(mean(j$margin), 2)),
        rmse = sprintf("%s / %s", num(sqrt(mean((j$margin - j$mean_m.prod)^2)), 2), num(sqrt(mean((j$margin - j$mean_m.v2)^2)), 2)),
        rmse_t = sprintf("%s / %s", num(sqrt(mean((j$total - j$mean_t.prod)^2)), 2), num(sqrt(mean((j$total - j$mean_t.v2)^2)), 2))
      )
    }))
    pl <- function(cells, label) {
      a <- pool(cells, "lsm_raw", "ls_margin")
      b <- pool(cells, "lst_raw", "ls_total")
      w <- pool(cells, "ll_wdl_raw", "ll_wdl")
      data.frame(
        cell = sprintf("<strong>%s</strong>", label), n = int(a[["n"]]), m = pm(a[["diff"]], a[["se"]], 4), t = pm(b[["diff"]], b[["se"]], 4),
        w = pm(w[["diff"]], w[["se"]], 4), em = "", rmse = "", rmse_t = ""
      )
    }
    ct <- rbind(ct, pl(hb, "Handball, both sexes"), pl(bb, "Basketball, both sexes"))
    age <- do.call(rbind, lapply(names(CELLS), function(cl) cmp[[cl]]$cmp[, c("lead_days", "lead_days_v2")]))
    add(tbl(ct,
      right = names(ct)[-1], hl = grepl("strong", ct$cell),
      head = c(
        "Cell", "Matches", "Margin log score: v2 &minus; production", "Total log score", "Result log likelihood",
        "Mean expected margin: production / v2 / observed", "Margin RMSE: production / v2", "Total RMSE: production / v2"
      ),
      note = sprintf(
        "Production's forecast is the newest archived fit dated strictly before the match; v2's cutoff is one to seven days before it. Neither always has the newer information: of the %s matches, production's forecast was the older one in %s, v2's in %s, and they were the same age in %s (production's fit was more than seven days old in %s). The stored forecasts cover late April to May 2026 and September to November 2026. Basketball is scored on the final result; v2 reaches it from its regulation-time model through the tie probability (a shift per division for men, one shift for women) and overtime. A final basketball margin cannot be level, so production's draws within half a point of level are set aside and the rest renormalised; handball margins are scored on the rounded predictive for both forecasters. Positive differences favour v2.",
        int(nrow(age)), int(sum(age$lead_days > age$lead_days_v2)), int(sum(age$lead_days < age$lead_days_v2)), int(sum(age$lead_days == age$lead_days_v2)),
        int(sum(age$lead_days > 7))
      )
    ))
    hj <- rbind(cmp$hb_m$cmp[, c("mean_m.prod", "mean_m.v2", "margin")], cmp$hb_f$cmp[, c("mean_m.prod", "mean_m.v2", "margin")])
    pairs3 <- list(c("lsm_raw", "ls_margin"), c("lst_raw", "ls_total"), c("ll_wdl_raw", "ll_wdl"))
    z_of <- function(cells, pr) {
      x <- pool(cells, pr[1], pr[2])
      x[["diff"]] / x[["se"]]
    }
    hb_z <- vapply(pairs3, function(pr) z_of(hb, pr), 1)
    bb_z <- unlist(lapply(list(bb, "bb_m", "bb_f"), function(cs) vapply(pairs3, function(pr) z_of(cs, pr), 1)))
    add(sprintf(
      paste0(
        "<p><strong>What this supports.</strong> One difference is clear: the handball total (%s standard errors). ",
        "The handball margin (%s standard errors) and result (%s) point the same way and are within noise. ",
        "No basketball difference, per cell or pooled, is larger than %s standard errors: basketball is level within noise. ",
        "So against production's stored forecasts v2 is better on handball totals, and not shown to be better or worse on anything else.</p>"
      ),
      num(hb_z[2], 1), num(hb_z[1], 1), num(hb_z[3], 1), num(max(abs(bb_z)), 1)
    ))
    add(sprintf(
      "<p>In handball production expects the home side to win by %s goals on average over these matches; v2 expects %s and the result was %s. That is the home-advantage inflation showing up in the published numbers.</p>",
      num(mean(hj$mean_m.prod), 2), num(mean(hj$mean_m.v2), 2), num(mean(hj$margin), 2)
    ))
    mk <- do.call(rbind, lapply(names(CELLS), function(cl) {
      j <- cmp[[cl]]$market
      if (is.null(j)) {
        return(NULL)
      }
      hp <- !is.na(j$ll_prod) & is.finite(j$ll_prod)
      data.frame(
        cell = LAB[[cl]], n = int(nrow(j)), period = sprintf("%s to %s", format(min(j$match_date), "%d %b %Y"), format(max(j$match_date), "%d %b %Y")),
        market = num(mean(j$ll_market), 3), v2 = num(mean(j$ll_v2), 3), d = pd(j, "ll_v2", "ll_market"),
        sub = if (sum(hp) >= 5) sprintf("n = %d: %s / %s / %s", sum(hp), num(mean(j$ll_market[hp]), 3), num(mean(j$ll_prod[hp]), 3), num(mean(j$ll_v2[hp]), 3)) else "–"
      )
    }))
    add("<h3>Against the Lengjan line</h3>")
    add(tbl(mk,
      right = c("n", "market", "v2", "d"),
      head = c("Cell", "Priced matches", "Period", "Market 1X2 log likelihood", "v2", "v2 &minus; market", "Where production's forecast exists: market / production / v2"),
      note = "Proportionally de-vigged 1X2, last scrape on or before match day. Handball is full time; basketball is regulation time, where production has no draw probability of its own, so its column uses the &plusmn;0.5 band. v2 is the proposed variant for the cell, the same as in every other table: the plain rounded margin for handball; for basketball the regulation 1X2 with the level-score shift, per division for men and one shift for women. Odds exist for few matches: the handball men's sample is small and the basketball samples are too small to read."
    ))
  } else {
    add(missing_note("the comparison with production's stored forecasts"))
  }
  cut <- lapply(c("hb_m none", "hb_f none", "bb_m none_reg", "bb_f none_reg"), function(k) wf[[k]]$wf)
  secs <- unlist(lapply(c("hb_m_none", "hb_f_none", "bb_m_none_reg", "bb_f_none_reg"), function(k) {
    f <- file.path(OUT, "out", sprintf("wf_%s.rds", k))
    if (file.exists(f)) with(readRDS(f)$cut, secs_m + secs_t) else NULL
  }))
  add(sprintf(
    "<p class=\"note\">Compute: %s weekly cutoffs, two R-INLA fits each (margin and total), median %s s per cutoff for both fits together, single core per cell, four cells side by side inside one slot of the shared limiter while a sibling session's job held the other slot. Like every time in this report it was taken on a shared machine: good for the ratio to the production fits, not a benchmark.</p>",
    int(length(secs)), num(stats::median(secs), 1)
  ))
} else {
  add(missing_note("the walk-forward"))
}
