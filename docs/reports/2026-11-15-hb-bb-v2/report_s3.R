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
  sdm <- last_par("hb_m", "none", "shift_div")
  add(sprintf(
    paste0(
      "<p><strong>Men.</strong> Level scores are more common than the band predicts: %s observed against %s out of sample, and on all data the shift is %s (se %s) on the logit scale, about %s %% more draws. ",
      "Broken down by division, the surplus is in Ol&iacute;sdeild (shift %s on the seasons before 2027) and absent in Grill 66 (%s). ",
      "It is not stable enough to pay yet: the share of level scores in Ol&iacute;sdeild swings from season to season, the shift estimated on earlier seasons overshoots the later ones, and the out-of-sample W/D/L log likelihood changes by %s with one shift and %s with a shift per division. ",
      "<strong>Women.</strong> The band is calibrated as it stands: %s observed against %s, shift %s (se %s).</p>"
    ),
    pct(mean(hbm$margin_fit == 0), 1), pct(mean(hbm$p_draw), 1), num(zm[["shift"]], 2), num(zm[["se"]], 2), num(100 * (exp(zm[["shift"]]) - 1), 0),
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
      "<p><strong>Men.</strong> The tie parameter is needed and it differs by division: shift %s in B&oacute;nusdeild (about %s times the smooth rate) and %s in 1. deild. It improves the out-of-sample regulation 1X2 log likelihood by %s per game and the regulation-margin log score by the same amount. ",
      "<strong>Women.</strong> A small surplus (%s against %s), shift %s, no measurable gain yet (%s). ",
      "<strong>Overtime</strong> is modelled as a short continuation of the same game: a five-minute period is one eighth of regulation, so its margin has one eighth of the mean and of the variance, and a level overtime is replayed. On the %s overtime games in the scored seasons the home side won %s and the pre-match favourite %s (%s); the continuation model expected the favourite to win %s, and its log likelihood is %s against %s for a coin flip. ",
      "So in these leagues overtime has so far been a coin flip, unlike the NBA; the continuation model is kept because it costs nothing and tends to the coin flip for evenly matched teams, but the evidence does not yet support it over 50 : 50.</p>"
    ),
    num(sm[["BD"]], 2), num(exp(sm[["BD"]]), 1), num(sm[["1D"]], 2), pd(bbm, "ll_wdl_shift_div", "ll_wdl_raw"),
    pct(mean(bbf$margin_fit == 0), 1), pct(mean(bbf$p_draw), 1), num(sf[1], 2), pd(bbf, "ll_wdl_shift", "ll_wdl_raw"),
    int(nrow(ot)), int(sum(ot$margin > 0)), int(sum(fav)), pct(mean(fav), 0), pct(mean(pmax(ot$p_home_ot_shift_div, 1 - ot$p_home_ot_shift_div)), 0),
    num(ll_ot, 3), num(log(0.5), 3)
  ))
  rvf <- lapply(bb, function(cl) wf[[paste(cl, "reg_vs_final")]])
  if (all(vapply(rvf, have, TRUE))) {
    rt <- do.call(rbind, lapply(seq_along(bb), function(i) {
      j <- rvf[[i]]
      data.frame(
        cell = LAB[[bb[i]]], n = int(nrow(j)),
        wl = sprintf("%s / %s", num(mean(j$ll_wdl_raw.final), 4), num(mean(j$ll_final_shift_div), 4)), wl_d = pd(j, "ll_final_shift_div", "ll_wdl_raw.final"),
        mg = sprintf("%s / %s", num(mean(j$lsm_raw.final), 4), num(mean(j$lsm_final_shift_div), 4)), mg_d = pd(j, "lsm_final_shift_div", "lsm_raw.final")
      )
    }))
    add("<h3>Does training on regulation time cost anything for the final result?</h3>")
    add(tbl(rt,
      right = names(rt)[-1],
      head = c(
        "Cell", "Games", "Final-result log likelihood: final-score model / regulation + overtime", "Difference",
        "Final-margin log score: final-score model / regulation + overtime", "Difference"
      ),
      note = "Same matches, both walk-forwards. The final-score model is the same v2 model trained on final scores, with the home side winning when the continuous margin is positive."
    ))
    add("<p>One model trained on regulation-time scores serves both products: the regulation 1X2 the bookmaker settles on, and the final result the league table needs, through the tie probability and overtime.</p>")
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
add("<p>For each cell, the model was refitted at every weekly cutoff from the start of season 2022 to 14 November 2026 on the results dated up to the cutoff (production's training filter, forfeits dropped) and asked for the matches of the following seven days. Hyperparameters are re-estimated at every cutoff. Season 2022 is used only to fit the second-stage quantities; scores are for seasons 2023 to 2027. The model is the static form of v2 fitted with R-INLA (hyperparameters at their mode); basketball is trained on regulation-time scores.</p>")
wf_row <- function(cl, sc, label, m, t, w, mfit = "margin_fit") {
  z_m <- (sc$margin_fit - sc$mean_m) / sc$s_m
  z_t <- (sc$total_fit - sc$mean_t) / sc$s_t
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
    wf_row("bb_m", bbm, "final result, through overtime", "lsm_final_shift_div", "lst_final_shift_div", "ll_final_shift_div"),
    wf_row("bb_f", bbf, "regulation time, one tie shift", "lsm_shift", "lst_raw", "ll_wdl_shift"),
    wf_row("bb_f", bbf, "final result, through overtime", "lsm_final_shift", "lst_final_shift", "ll_final_shift")
  )
  add(tbl(wt,
    right = names(wt)[-(1:2)],
    head = c(
      "Cell", "Scored as", "Matches", "Margin log score", "Total log score", "W/D/L log likelihood", "Sd of standardised residual: margin / total",
      "80 % interval covers: margin / total"
    ),
    note = "Per match, integer scale. A standardised residual sd of 1 and a coverage of 80 % mean the predictive spread is right. Handball W/D/L uses the plain rounded margin (no shift). The basketball \"final result\" rows have two outcomes, not three."
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
    note = "Phases are weeks of the regular season; a basketball play-off match is one of a pair that meets at least three times within three weeks, handball's are its PO division. The mean standardised margin residual is from the home side's view. The predictive sd is widest in the first six weeks because the new season's level is still uncertain: that is the season-boundary term at work."
  ))
  add("<p>The spread of the standardised residuals stays close to 1 in every phase, including the last six weeks and the play-offs. If team strength drifted inside a season in a way the static model misses, the late-season residuals would be over-dispersed; they are not, in either sport. This is the check asked for before dropping the within-season term for basketball, where mid-season import changes are the obvious worry.</p>")
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
    add(tbl(ct,
      right = names(ct)[-1], hl = grepl("strong", ct$cell),
      head = c(
        "Cell", "Matches", "Margin log score: v2 &minus; production", "Total log score", "Result log likelihood",
        "Mean expected margin: production / v2 / observed", "Margin RMSE: production / v2", "Total RMSE: production / v2"
      ),
      note = "Production's forecast is the newest archived fit dated strictly before the match (median lead two to three days); v2's cutoff is up to seven days before the match, so v2 is the one working with older information. The stored forecasts cover late April to May 2026 and September to November 2026. Basketball is scored on the final result; v2 reaches it from its regulation-time model through the tie probability and overtime. Positive differences favour v2."
    ))
    hj <- rbind(cmp$hb_m$cmp[, c("mean_m.prod", "mean_m.v2", "margin")], cmp$hb_f$cmp[, c("mean_m.prod", "mean_m.v2", "margin")])
    add(sprintf(
      "<p>In handball production expects the home side to win by %s goals on average over these matches; v2 expects %s and the result was %s. That is the home-advantage inflation showing up in the published numbers. In basketball the two are close on every measure; none of the basketball differences is larger than about one and a half standard errors.</p>",
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
      note = "Proportionally de-vigged 1X2, last scrape on or before match day. Handball is full time; basketball is regulation time, where production has no draw probability of its own, so its column uses the &plusmn;0.5 band. Odds exist for few matches: the handball men's sample is small and the basketball samples are too small to read."
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
    "<p class=\"note\">Compute: %s weekly cutoffs, two R-INLA fits each (margin and total), median %s s per cutoff for both fits together, single core per cell, four cells side by side inside one slot of the shared limiter while a sibling session's job held the other slot.</p>",
    int(length(secs)), num(stats::median(secs), 1)
  ))
} else {
  add(missing_note("the walk-forward"))
}
