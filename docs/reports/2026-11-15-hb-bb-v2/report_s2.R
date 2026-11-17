# Report sections: what the data ask for, the ladder, the v2 specification, INLA vs Stan.

# ---------------------------------------------------------------- data
cur <- "data"
od <- desc[["hb_m"]]$d
od <- od[od$division == "OD", ]
od_first <- num(mean(od$total[od$season == min(od$season)]), 0)
od_last <- num(mean(od$total[od$season == max(od$season)]), 0)
od_draw <- tapply(od$margin == 0, od$season, mean)
add("<h2 id=\"data\">What the data ask of the likelihood</h2>")
add("<p>Before adding structure, the residuals of the simplest version of the new model (one fit per cell on every played match up to 14 November 2026) show what the scores actually look like once team strengths are accounted for. These residuals are in-sample; the calibration tables in the walk-forward section repeat the important ones out of sample.</p>")
by_div <- do.call(rbind, lapply(names(CELLS), function(cl) {
  d <- desc[[cl]]$d
  do.call(rbind, lapply(split(seq_len(nrow(d)), d$division), function(ix) {
    x <- d[ix, ]
    data.frame(
      cell = LAB[[cl]], division = x$division[1], n = int(nrow(x)),
      margin = sprintf("%s / %s", num(mean(x$margin), 2), num(stats::sd(x$margin), 1)),
      total = sprintf("%s / %s", num(mean(x$total), 1), num(stats::sd(x$total), 1)),
      home = pct(mean(x$margin > 0)), draw = if (CELLS[[cl]]$sport == "handball") pct(mean(x$margin == 0)) else "–",
      rsd = sprintf("%s / %s", num(stats::sd(x$r_m), 2), num(stats::sd(x$r_t), 2)),
      cor_mt = num(stats::cor(x$r_m, x$r_t), 2),
      pois = num(desc[[cl]]$disp$pearson_poisson[desc[[cl]]$disp$division == x$division[1]], 2),
      kurt = sprintf("%s / %s", num(mean((x$r_m / stats::sd(x$r_m))^4) - 3, 2), num(mean((x$r_t / stats::sd(x$r_t))^4) - 3, 2))
    )
  }))
}))
add(tbl(by_div,
  right = c("n", "margin", "total", "home", "draw", "rsd", "cor_mt", "pois", "kurt"),
  head = c(
    "Cell", "Division", "Matches", "Margin: mean / sd", "Total: mean / sd", "Home wins", "Level scores", "Residual sd: margin / total",
    "Correlation of margin and total residuals", "Score variance / mean, given strengths", "Excess kurtosis: margin / total"
  ),
  note = "Margin is home minus away, total is home plus away. \"Score variance / mean\" is the Pearson dispersion of each side's score around its fitted mean: 1 would be Poisson (stored as <code>pearson_poisson</code> in <code>out/descriptives.rds</code>, <code>[[cell]]$disp</code>). Handball's PO rows are the 2026 play-offs only (16 and 20 matches)."
))
hb_disp <- do.call(rbind, lapply(hb, function(cl) desc[[cl]]$disp))
hb_disp_league <- range(hb_disp$pearson_poisson[hb_disp$division != "PO"])
hb_disp_po <- hb_disp$pearson_poisson[hb_disp$division == "PO"]
add(paste0(
  "<p><strong>Margin and total are uncorrelated</strong> once strengths are in: every cell is within a few hundredths of zero. That is the condition under which the two can be modelled separately, and it holds. ",
  sprintf(
    "<strong>Handball scores are under-dispersed</strong>: the variance of a side's score is %s to %s of its mean in the league divisions, for women as for men (%s in the two small play-off samples), so a Poisson model would be badly over-dispersed and a free Gaussian variance is the honest choice. ",
    num(hb_disp_league[1], 2), num(hb_disp_league[2], 2), paste(num(hb_disp_po, 2), collapse = " and ")
  ),
  "<strong>Tails are close to Gaussian</strong> in handball. Basketball margins are somewhat heavier-tailed, and women's 1. deild markedly so, with a few very large margins; the walk-forward tests whether a heavier-tailed predictive pays.</p>"
))

season_tab <- do.call(rbind, lapply(names(CELLS), function(cl) {
  d <- desc[[cl]]$d
  do.call(rbind, lapply(split(d, list(d$division, d$season), drop = TRUE), function(x) {
    data.frame(
      cell = LAB[[cl]], division = x$division[1], season = x$season[1], n = int(nrow(x)), margin = num(mean(x$margin), 2),
      total = num(mean(x$total), 1), draw = if (CELLS[[cl]]$sport == "handball") pct(mean(x$margin == 0)) else "–",
      rsd_m = num(stats::sd(x$r_m), 2), rsd_t = num(stats::sd(x$r_t), 2)
    )
  }))
}))
season_tab <- season_tab[order(match(season_tab$cell, LAB), season_tab$division, season_tab$season), ]
add(
  "<details><summary>The same by season (all four cells)</summary>",
  tbl(season_tab,
    right = c("season", "n", "margin", "total", "draw", "rsd_m", "rsd_t"),
    head = c("Cell", "Division", "Season", "Matches", "Mean margin", "Mean total", "Level scores", "Residual sd, margin", "Residual sd, total"),
    note = sprintf("Season is the year the season ends. The share of level scores in men's Ol&iacute;sdeild swings between %s and %s from one season to the next.", pct(min(od_draw), 0), pct(max(od_draw), 0))
  ),
  "</details>"
)

mis <- do.call(rbind, lapply(names(CELLS), function(cl) {
  d <- desc[[cl]]$d
  g <- cut(abs(d$mu_m), stats::quantile(abs(d$mu_m), 0:5 / 5), include.lowest = TRUE, labels = FALSE)
  r <- vapply(1:5, function(i) stats::sd(d$r_m[g == i]), 1)
  fav <- vapply(1:5, function(i) mean(sign(d$mu_m[g == i]) * d$r_m[g == i]), 1)
  data.frame(cell = LAB[[cl]], t(num(r, 2)), fav = num(fav[5], 2, plus = TRUE), top = num(mean(abs(d$mu_m[g == 5])), 1))
}))
add("<h3>Residuals against the size of the mismatch</h3>")
add(tbl(mis,
  right = names(mis)[-1],
  head = c("Cell", "Closest fifth", "2", "3", "4", "Most lopsided fifth", "Favourite's mean residual in the most lopsided fifth", "Mean expected margin there"),
  note = "First five columns: sd of the margin residual by fifths of the absolute expected margin."
))
add("<p>The margin is slightly more variable, and the favourite wins by slightly more than a straight line predicts, when the teams are far apart. The effect is real but small next to the match-to-match noise; whether modelling it pays is tested out of sample below, and it does not.</p>")

hm <- desc[["hb_m"]]$d
ks <- -8:8
obs <- vapply(ks, function(k) mean(hm$margin == k), 1)
prd <- vapply(ks, function(k) mean(v2_integer_pmf(rep(k, nrow(hm)), hm$mu_m, hm$sd_m)), 1)
add(
  "<figure>", svg_profile(ks, obs, prd, obs_label = "observed share of matches", exp_label = "rounded Gaussian"),
  sprintf(
    "<figcaption>Handball men, margin of %s matches. A level score (highlighted) occurred in %s of matches; the rounded Gaussian puts %s there. The surplus is concentrated in matches between evenly matched teams.</figcaption></figure>",
    int(nrow(hm)), pct(mean(hm$margin == 0)), pct(mean(v2_integer_pmf(rep(0, nrow(hm)), hm$mu_m, hm$sd_m)))
  )
)

# ---------------------------------------------------------------- ladder
cur <- "ladder"
add("<h2 id=\"ladder\">The ladder</h2>")
add("<p>From R2 on, each rung changes one thing, so each of those gains is attributable. R1 changes three things at once (spelled out below), so what it gains cannot be put on one of them. All rungs are fitted with NUTS at the same cutoff and settings as production. At the request of the session that scheduled the machine, the full ladder was run for the two men's cells; the women's cells have the production fit and the final model.</p>")
rung_name <- c(
  r0_prod = "Production", r1_ragged = "R1 &middot; states at real appearances, innovations unconstrained",
  r2_common = "R2 &middot; one volatility, one score sd, one correlation", r3_pooled = "R3 &middot; pooled, sign-free home advantage",
  r4_ident = "R4 &middot; identified: one home advantage", r5_gauss = "R5 &middot; Gaussian instead of Student-t",
  `v2s (margin + total)` = "v2 static (margin + total), NUTS", `v2a (margin + total)` = "v2 + within-season AR(1), NUTS",
  `v2 (margin + total)` = "v2 + weekly random walk, NUTS"
)
if (have(ladder)) {
  tab <- ladder$tab
  for (cl in unique(tab$cell)) {
    x <- tab[tab$cell == cl, ]
    base <- ladder$scores[[paste(cl, "r0_prod")]]
    lab <- ifelse(x$rung %in% names(rung_name), rung_name[x$rung], x$rung)
    vs <- function(r, col) {
      s <- ladder$scores[[paste(cl, r)]]
      if (is.null(base) || r == "r0_prod") {
        return("")
      }
      p <- paired(s[[col]], base[[col]])
      sprintf(" <span class=\"u\">(%s)</span>", pm(p[["diff"]], p[["se"]], 3))
    }
    out <- data.frame(
      rung = lab, params = int(x$n_sampled), div = int(x$divergent), leap = int(x$leapfrog),
      ess = ifelse(is.na(x$min_ess), x$worst, sprintf("%s (<code>%s</code>)", int(x$min_ess), x$worst)),
      eff = num(1000 * x$min_ess / x$grad_evals, 2), elpd = num(x$elpd, 1),
      ha = sprintf("%s (%s)", num(x$ha_mean, 2), num(x$ha_sd, 2)),
      lsm = paste0(num(x$ls_margin, 3), vapply(x$rung, vs, "", col = "ls_margin")),
      lst = paste0(num(x$ls_total, 3), vapply(x$rung, vs, "", col = "ls_total")),
      wdl = paste0(num(x$ll_wdl, 3), vapply(x$rung, vs, "", col = "ll_wdl"))
    )
    add(sprintf("<h3>%s <span class=\"u\">&middot; %d held-out matches</span></h3>", LAB[[cl]], x$hold_n[1]))
    add(tbl(out,
      right = c("params", "div", "leap", "eff", "elpd", "ha", "lsm", "lst", "wdl"), hl = x$engine == "INLA",
      head = c(
        "Rung", "Sampled parameters", "Divergences", "Leapfrog steps / iteration", "Smallest bulk ESS (parameter)",
        "ESS per 1,000 gradients", "Leave-one-out log density", "Home advantage (sd)", "Held-out margin log score",
        "Held-out total log score", "Held-out W/D/L log likelihood"
      )
    ))
  }
  add("<p class=\"note\">Log scores are per match, on the integer scale, higher is better; in brackets the paired difference from production with its match-clustered standard error. For the v2 rows \"leapfrog steps\" is the sum over the margin and the total fit. Highlighted rows are R-INLA fits of the same v2 models (\"full\" integrates over the hyperparameters, \"plug-in\" fixes them at their mode). The leave-one-out density is on the scale of the two scores for every row, so rows are comparable.</p>")
  # Reading notes per cell with the full ladder: one sentence per rung, built from the table.
  g <- function(cl, rung, col) {
    x <- tab[tab$cell == cl & tab$rung == rung, col]
    if (length(x) == 0) NA else x[1]
  }
  dlt <- function(cl, a, b, col) {
    sa <- ladder$scores[[paste(cl, a)]]
    sb <- ladder$scores[[paste(cl, b)]]
    if (is.null(sa) || is.null(sb)) {
      return("–")
    }
    p <- paired(sb[[col]], sa[[col]])
    pm(p[["diff"]], p[["se"]], 3)
  }
  # The scoring-level prior of production (read from its Stan file) and of rung R1.
  prod_level_prior <- function(cl) {
    f <- file.path("Stan", load_leagues()[[CELLS[[cl]]$league]]$stan_model)
    line <- grep("mean_goals0 ~ normal(", readLines(f, warn = FALSE), fixed = TRUE, value = TRUE)[1]
    sub(".*normal\\(([^)]*)\\).*", "\\1", line)
  }
  r1_level_prior <- function(cl) {
    sc <- SPORT_SCALE[[CELLS[[cl]]$sport]]
    sprintf("%s, %s", sc$score_mean, sc$score_mean_sd)
  }
  for (cl in unique(tab$cell)) {
    if (is.na(g(cl, "r5_gauss", "leapfrog")) || is.na(g(cl, "r0_prod", "leapfrog"))) next
    notes <- character()
    if (!is.na(g(cl, "r1_ragged", "leapfrog"))) {
      notes <- c(notes, sprintf(
        paste0(
          "<strong>R1</strong> keeps production's likelihood and moves the states to real appearances, which removes the states no match touches. Two more things change with it. ",
          "Production constrains each round's innovations to sum to zero across teams; the ragged layout has no round to sum over, so R1's innovations are unconstrained. ",
          "And R1's prior on the scoring level is normal(%s) where production's is normal(%s). ",
          "The result: %s sampled parameters instead of %s, %s leapfrog steps per iteration instead of %s, held-out scores unchanged (margin %s). ",
          "The parameter count is arithmetic. The drop in leapfrog steps belongs to the three changes together; this ladder cannot put it on the unused states alone."
        ),
        r1_level_prior(cl), prod_level_prior(cl),
        int(g(cl, "r1_ragged", "n_sampled")), int(g(cl, "r0_prod", "n_sampled")), int(g(cl, "r1_ragged", "leapfrog")), int(g(cl, "r0_prod", "leapfrog")),
        dlt(cl, "r0_prod", "r1_ragged", "ls_margin")
      ))
    }
    prev <- if (!is.na(g(cl, "r1_ragged", "leapfrog"))) "r1_ragged" else "r0_prod"
    notes <- c(notes, sprintf(
      "<strong>R2</strong> %sreplaces the per-team volatility hierarchies%s by single values: %s parameters and %s leapfrog steps per iteration, leave-one-out %s against %s. The home advantage is still %s: it is not the hierarchies that inflate it.",
      if (prev == "r0_prod") {
        sprintf(
          "(R1 was not run for this cell, so R2 is also its first rung with the states at real appearances and with what comes with that: no sum-to-zero constraint on each round's innovations, and normal(%s) on the scoring level where production has normal(%s)) ",
          r1_level_prior(cl), prod_level_prior(cl)
        )
      } else {
        ""
      },
      if (CELLS[[cl]]$sport == "handball") " and the per-team score sds" else " and the four-parameter correlation",
      int(g(cl, "r2_common", "n_sampled")), int(g(cl, "r2_common", "leapfrog")), num(g(cl, "r2_common", "elpd"), 1), num(g(cl, prev, "elpd"), 1),
      num(g(cl, "r2_common", "ha_mean"), 2)
    ))
    notes <- c(notes, sprintf(
      "<strong>R3</strong> pools the home effects and lets them take either sign: the home advantage falls from %s to %s (raw margin %s), leave-one-out rises to %s and the held-out margin log score by %s. The ridge that the bounds were hiding appears: the scoring level's posterior sd goes from %s to %s, its correlation with the mean attack home effect is %s, and NUTS needs %s leapfrog steps per iteration.",
      num(g(cl, "r2_common", "ha_mean"), 2), num(g(cl, "r3_pooled", "ha_mean"), 2), num(ha$raw[ha$cell == cl], 2),
      num(g(cl, "r3_pooled", "elpd"), 1), dlt(cl, "r2_common", "r3_pooled", "ls_margin"),
      num(g(cl, "r2_common", "level_sd"), 2), num(g(cl, "r3_pooled", "level_sd"), 2), num(g(cl, "r3_pooled", "ridge_cor"), 2), int(g(cl, "r3_pooled", "leapfrog"))
    ))
    notes <- c(notes, sprintf(
      "<strong>R4</strong> removes the ridge (one league home advantage, team deviations around zero): the level's sd returns to %s, leapfrog steps to %s, the smallest effective sample size is %s, the home advantage is %s.",
      num(g(cl, "r4_ident", "level_sd"), 2), int(g(cl, "r4_ident", "leapfrog")), int(g(cl, "r4_ident", "min_ess")), num(g(cl, "r4_ident", "ha_mean"), 2)
    ))
    notes <- c(notes, sprintf(
      "<strong>R5</strong> drops the Student-t (fitted degrees of freedom in R4: %s): leave-one-out %s against %s, held-out margin %s and total %s. That is the measure of what the heavy tail is worth.",
      num(g(cl, "r4_ident", "nu"), 0), num(g(cl, "r5_gauss", "elpd"), 1), num(g(cl, "r4_ident", "elpd"), 1),
      dlt(cl, "r4_ident", "r5_gauss", "ls_margin"), dlt(cl, "r4_ident", "r5_gauss", "ls_total")
    ))
    notes <- c(notes, sprintf(
      "<strong>v2</strong> rotates attack and defence into net strength and scoring level, adds the club level and the season-boundary AR(1) and drops the appearance-indexed random walk: %s sampled parameters, leave-one-out %s, held-out margin %s and total %s against R5.",
      int(g(cl, "v2s (margin + total)", "n_sampled")), num(g(cl, "v2s (margin + total)", "elpd"), 1),
      dlt(cl, "r5_gauss", "v2s (margin + total)", "ls_margin"), dlt(cl, "r5_gauss", "v2s (margin + total)", "ls_total")
    ))
    add(sprintf("<h3>Reading the %s ladder</h3>", tolower(LAB[[cl]])), "<ul>", paste0("<li>", notes, "</li>", collapse = "\n"), "</ul>")
  }
  wt <- tab[!is.na(tab$wall_secs), c("cell", "rung", "wall_secs", "slots")]
  if (nrow(wt)) {
    wt_tab <- data.frame(
      cell = LAB[wt$cell], rung = ifelse(wt$rung %in% names(rung_name), rung_name[wt$rung], wt$rung),
      wall = ifelse(wt$wall_secs < 120, sprintf("%s s", num(wt$wall_secs, 0)), sprintf("%s min", num(wt$wall_secs / 60, 1))),
      slots = esc(wt$slots)
    )
    add(
      "<details><summary>Wall-clock times, with what else was running</summary>",
      tbl(wt_tab,
        right = "wall", head = c("Cell", "Rung", "Wall time", "Machine slots when the fit started"),
        note = "Every time here was taken inside the shared limiter, which allows two 4-core jobs at once on 8 performance cores; the second slot was usually busy with a sibling session's fit. Treat these as times under a half-loaded machine, good for ratios within a cell, not as benchmarks. Leapfrog counts do not depend on load."
      ),
      "</details>"
    )
  }
} else {
  add(missing_note("the ladder fits"))
}

# ---------------------------------------------------------------- spec
cur <- "spec"
add("<h2 id=\"spec\">The v2 model</h2>")
add("<p>Margin = home score minus away score, total = home plus away. The two are modelled separately because their residuals are uncorrelated.</p>")
add("<div class=\"eq\">margin ~ Normal( hfa + ha[home] + s[home] - s[away] ,  sigma_m )
total  ~ Normal( level[division, season] + p[home] + p[away] ,  sigma_t )

s[team, date] = b[team] + u[team, season] (+ w[team-season, week])      net strength
p[team, date] = b'[team] + u'[team, season] (+ w'[team-season, week])   contribution to the total

b ~ Normal(0, sigma_b)                    club level, constant over the data
u[season + 1] = rho * u[season] + noise   season level, AR(1) across seasons, sd sigma_u
w                                         within-season movement (three forms tested)
ha ~ Normal(0, sigma_ha)                  team deviation from the league home advantage
level[division, .]                        random walk over seasons, one path per division</div>")
add("<p>Nothing in it is forced to have a sign, the league home advantage is one number, and there is one row type per model, so there is no ridge to separate with priors. Every piece is Gaussian, which makes R-INLA's Laplace step exact; the only approximation left is the numerical integration over six hyperparameters per model.</p>")
story <- data.frame(
  par = c(
    "<code>hfa</code>", "<code>sigma_m</code>, <code>sigma_t</code>", "<code>b</code>, <code>sigma_b</code>", "<code>u</code>, <code>sigma_u</code>, <code>rho</code>",
    "<code>w</code>, <code>sigma_w</code>", "<code>ha</code>, <code>sigma_ha</code>", "<code>level</code>, <code>sigma_season</code>"
  ),
  story = c(
    "Playing at home is worth a fixed number of goals or points on average: travel, the hall, the crowd, referees. One number per sex, free to take either sign.",
    "What is left of a match once strengths are known: shooting luck, the day's form, refereeing. One sd for the margin and one for the total, because the data show no difference between teams and margin and total do not co-vary.",
    "Some clubs are big and some are small for reasons that outlast any roster: budget, youth system, town. This is the level a club returns to.",
    "Each summer the roster changes (imports in basketball, players leaving for clubs abroad in handball), so a club's strength in a season departs from its long-run level, and part of that departure carries into the next season. <code>rho</code> is the share that carries over.",
    "Injuries, a mid-season signing, a coach finding a system: strength can drift inside a season. Tested as a weekly random walk and as a slow AR(1) around the season level.",
    "Some halls are harder to visit than others. Team deviations from the league home advantage, shrunk hard because a team has about eleven home matches a season.",
    sprintf("Scoring levels move with rules and style (men's Ol&iacute;sdeild has gone from %s to %s goals a match between seasons 2021 and 2027). A random walk over seasons, one path per division.", od_first, od_last)
  ),
  prior = c(
    "normal(0, 5) goals / normal(0, 10) points", "sd above 15 / 20 goals or 30 / 40 points with probability 1 %", "sd above 10 goals / 25 points with probability 1 %",
    "sd above 8 goals / 20 points with probability 1 %; <code>rho</code> centred on 0.7, 95 % within 0.13 to 0.92",
    sprintf(
      "random walk: step sd above %s goal / %s points per week with probability 1 %%. AR(1) form (the one compared with Stan and proposed for basketball): sd of a team's departure from its season level above %s goals / %s points with probability 1 %%, weekly correlation fixed at 0.98",
      v2_priors("handball")$u_w, v2_priors("basketball")$u_w, v2_priors("handball")$u_w_ar1, v2_priors("basketball")$u_w_ar1
    ),
    "sd above 3 goals / 6 points with probability 1 %", "step sd above 5 goals / 10 points with probability 1 %"
  )
)
add("<h3>Each parameter's data story</h3>")
add(tbl(story,
  head = c("Parameter", "What in the game it stands for", "Prior"),
  note = "Every sd has an exponential prior stated as an upper bound it exceeds with probability 1 % (R-INLA's PC prior; the same density in Stan). The bounds are several times the size each quantity is expected to take, so the data decide."
))
sha_med <- function(cl) {
  h <- desc[[cl]]$hyper_m
  num(h$q50[h$parameter == "sd: ha"], 1)
}
post <- do.call(rbind, lapply(names(CELLS), function(cl) {
  hm_ <- desc[[cl]]$hyper_m
  ht_ <- desc[[cl]]$hyper_t
  g <- function(h, nm) {
    r <- h[h$parameter == nm, ]
    sprintf("%s [%s, %s]", num(r$q50, 2), num(r$q05, 2), num(r$q95, 2))
  }
  data.frame(
    cell = LAB[[cl]], hfa = sprintf("%s (%s)", num(desc[[cl]]$hfa$mean[1], 2), num(desc[[cl]]$hfa$sd[1], 2)),
    sm = g(hm_, "sd: the Gaussian observations"), sb = g(hm_, "sd: b"), su = g(hm_, "sd: u_s"), rho = g(hm_, "rho: u_s"), sha = g(hm_, "sd: ha"),
    st = g(ht_, "sd: the Gaussian observations"), rho_t = g(ht_, "rho: u_s")
  )
}))
add("<h3>What the data say (static form, all matches to 14 November 2026)</h3>")
add(tbl(post,
  right = names(post)[-1],
  head = c(
    "Cell", "Home advantage (sd)", "<code>sigma_m</code>", "<code>sigma_b</code>", "<code>sigma_u</code>", "<code>rho</code>", "<code>sigma_ha</code>",
    "<code>sigma_t</code>", "<code>rho</code>, total"
  ),
  note = "Median [5 %, 95 %], R-INLA with hyperparameters at their mode for the latent field. Home advantage is the posterior mean. The handball home advantage, about two thirds of a goal for both sexes, is far below the 1.87 goals reported for the German Bundesliga; it equals the raw home-minus-away margin in these leagues."
))
add(sprintf(
  "<p>Team deviations from the league home advantage are small in handball (sd %s and %s goals) and largest in women's basketball (%s points, against %s for the men): there the hall matters by team, as the NBA literature found. In women's basketball the data cannot say sharply how much of the spread between teams is club level and how much is season level (wide intervals for both).</p>",
  sha_med("hb_m"), sha_med("hb_f"), sha_med("bb_f"), sha_med("bb_m")
))

# ---------------------------------------------------------------- INLA vs Stan
cur <- "inla"
add("<h2 id=\"inla\">R-INLA against Stan, like for like</h2>")
if (have(val) && length(val) > 0) {
  add("<p>One identical model per comparison: v2 with the within-season AR(1) (the richer of the two candidate forms), the same data, the same priors, fitted with NUTS and with R-INLA.</p>")
  vt <- do.call(rbind, lapply(val, function(o) {
    data.frame(
      what = sprintf("%s, %s", LAB[[o$cell]], o$response),
      hyper = num(max(abs(o$hyper$diff_in_sd)), 2),
      state = sprintf("%s / %s", num(o$state[["max_abs_mean_diff_in_sd"]], 3), num(o$state[["median_sd_ratio"]], 3)),
      pred = sprintf("%s / %s", num(o$pred[["max_abs_mean_diff"]], 3), num(o$pred[["max_abs_mean_diff_eb"]], 3)),
      ls = sprintf("%s / %s / %s", num(o$log_score[["stan"]], 4), num(o$log_score[["inla_full_samples"]], 4), num(o$log_score[["inla_eb"]], 4)),
      loo = sprintf("%s / %s", num(o$loo[[1]], 1), num(o$loo[[2]], 1)),
      stan = sprintf("%s params, %s div., %s leapfrog", int(o$stan[["n_sampled"]]), int(o$stan[["divergent"]]), int(o$stan[["leapfrog"]])),
      inla = sprintf("%s s / %s s", num(o$inla[["secs_full"]], 0), num(o$inla[["secs_eb"]], 0))
    )
  }))
  add(tbl(vt,
    right = names(vt)[-1],
    head = c(
      "Model", "Largest hyperparameter difference, in posterior sds", "States: largest mean difference (sds) / sd ratio",
      "Held-out predictor, largest difference from Stan in a match's expected value: INLA full / INLA plug-in", "Held-out log score: Stan / INLA full / INLA plug-in",
      "Leave-one-out: Stan PSIS / INLA CPO", "Stan", "R-INLA time: full / plug-in"
    ),
    note = "\"sd ratio\" is R-INLA's posterior sd over Stan's, median over states or matches. \"Plug-in\" fixes the hyperparameters at their mode, which is what the walk-forward uses. R-INLA times are single-core wall times on a shared machine."
  ))
  # In which models R-INLA is the wider engine for most hyperparameters, and in which the narrower.
  wider <- vapply(val, function(o) mean(o$hyper$inla_sd / o$hyper$stan_sd > 1) > 0.5, TRUE)
  model_names <- function(hit) {
    x <- vapply(val[hit], function(o) sprintf("the %s %s model", tolower(LAB[[o$cell]]), o$response), "")
    if (length(x) > 2) paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)]) else paste(x, collapse = " and ")
  }
  width_txt <- if (any(wider) && any(!wider)) {
    sprintf("R-INLA is the wider engine for most hyperparameters in %s and the narrower for most in %s.", model_names(wider), model_names(!wider))
  } else if (all(wider)) {
    "R-INLA is the wider engine for most hyperparameters in every model."
  } else {
    "R-INLA is the narrower engine for most hyperparameters in every model, though not for all of them."
  }
  # The Stan side is one stored fit; the R-INLA side was fitted three times (twice with
  # R-INLA's defaults, then with its online optimisation off). How much of the agreement
  # belongs to the run?
  val1 <- rd("first-run/compare_validation.rds")
  val2 <- rd("second-run/compare_validation.rds")
  run_txt <- if (have(val1) && have(val2)) {
    st <- function(v) max(vapply(v, function(o) o$state[["max_abs_mean_diff_in_sd"]], 1))
    hy <- function(v) max(vapply(v, function(o) max(abs(o$hyper$diff_in_sd)), 1))
    ls <- function(v) max(vapply(v, function(o) abs(o$log_score[["stan"]] - o$log_score[["inla_full_samples"]]), 1))
    moved <- function(a, b) vapply(names(a), function(k) max(abs(a[[k]]$hyper$inla_mean - b[[k]]$hyper$inla_mean) / a[[k]]$hyper$stan_sd), 1)
    m12 <- moved(val2, val1)
    m23 <- moved(val, val2)
    which_fit <- function(m) paste(tolower(LAB[[val[[which.max(m)]]$cell]]), val[[which.max(m)]]$response)
    three <- function(f, digits) paste(num(f(val1), digits), num(f(val2), digits), num(f(val), digits), sep = " / ")
    sprintf(
      paste0(
        "<strong>These figures depend on the run.</strong> The R-INLA fits were made three times: twice with R-INLA's defaults and then, for the figures here, with its online optimisation off. ",
        "Between the first and the second run the %s fit landed elsewhere (a hyperparameter mean %s Stan posterior sds apart); %s ",
        "The comparison read, first / second / here: largest state difference %s posterior sds, largest hyperparameter difference %s, log scores agreeing to %s. ",
        "The full-integration log score is computed from 2,000 posterior draws, which were unseeded in the first two runs and are seeded now, so its last digits are draw noise."
      ),
      which_fit(m12), num(max(m12), 2),
      if (max(m23) < 0.005) {
        "between the second and the one here no hyperparameter mean of these four fits moved by as much as 0.005: started cold at one cutoff, they came out the same with the option off."
      } else {
        sprintf("between the second and the one here the largest such move is %s (the %s fit).", num(max(m23), 2), which_fit(m23))
      },
      three(st, 2), three(hy, 2), three(ls, 4)
    )
  } else {
    ""
  }
  worst_h <- val[[which.max(vapply(val, function(o) max(abs(o$hyper$diff_in_sd)), 1))]]
  wh <- worst_h$hyper[which.max(abs(worst_h$hyper$diff_in_sd)), ]
  add(sprintf(
    paste0(
      "<p><strong>Predictions agree.</strong> Across the %s comparisons the largest difference from Stan in a held-out match's expected margin or total is %s %s with full integration and %s with the plug-in fit, which is the one the walk-forward uses; the held-out log scores differ by at most %s per match (full) and %s (plug-in), and the two leave-one-out estimates by at most %s in total. ",
      "<strong>What differs is how weakly identified variance is split.</strong> The largest hyperparameter difference is %s posterior sds (<code>%s</code> in the %s %s model: Stan %s, R-INLA %s). ",
      "The data cannot say sharply how much of a team's strength or scoring level is club, season or within-season, so the posterior of those sds is wide and skewed, and the two engines do not differ in one direction. R-INLA's posterior sd of a hyperparameter as a ratio to Stan's, with the number of the model's hyperparameters for which R-INLA is the narrower: %s. ",
      "%s The sum of the components, which is what a forecast uses, agrees (first sentence). ",
      "Neither engine is a precise reference for these sds: R-INLA integrates over %s points around the one mode it found, and Stan's smallest effective sample size here is %s with R-hat up to %s. %s</p>"
    ),
    length(val), num(max(vapply(val, function(o) o$pred[["max_abs_mean_diff"]], 1)), 2), "goals or points",
    num(max(vapply(val, function(o) o$pred[["max_abs_mean_diff_eb"]], 1)), 2),
    num(max(vapply(val, function(o) abs(o$log_score[["stan"]] - o$log_score[["inla_full_samples"]]), 1)), 4),
    num(max(vapply(val, function(o) abs(o$log_score[["stan"]] - o$log_score[["inla_eb"]]), 1)), 4),
    num(max(vapply(val, function(o) abs(o$loo[[1]] - o$loo[[2]]), 1)), 1),
    num(abs(wh$diff_in_sd), 2), esc(wh$parameter), tolower(LAB[[worst_h$cell]]), worst_h$response, num(wh$stan_mean, 2), num(wh$inla_mean, 2),
    paste(vapply(val, function(o) {
      r <- o$hyper$inla_sd / o$hyper$stan_sd
      sprintf("%s %s %s to %s (%d of %d)", tolower(LAB[[o$cell]]), o$response, num(min(r), 2), num(max(r), 2), sum(r < 1), length(r))
    }, ""), collapse = "; "),
    width_txt,
    paste(range(vapply(val, function(o) o$inla[["hyper_points"]], 1)), collapse = " to "),
    int(min(vapply(val, function(o) o$stan[["min_ess"]], 1))), num(max(vapply(val, function(o) o$stan[["max_rhat"]], 1)), 3),
    run_txt
  ))
  for (o in val) {
    h <- o$hyper
    ht <- data.frame(
      par = esc(h$parameter), stan = sprintf("%s (%s)", num(h$stan_mean, 3), num(h$stan_sd, 3)), inla = sprintf("%s (%s)", num(h$inla_mean, 3), num(h$inla_sd, 3)),
      s90 = sprintf("%s to %s", num(h$stan_q05, 3), num(h$stan_q95, 3)), i90 = sprintf("%s to %s", num(h$inla_q05, 3), num(h$inla_q95, 3))
    )
    add(
      sprintf("<details><summary>Hyperparameters, %s, %s</summary>", LAB[[o$cell]], o$response),
      tbl(ht, right = c("stan", "inla", "s90", "i90"), head = c("Parameter", "Stan mean (sd)", "R-INLA mean (sd)", "Stan 90 %", "R-INLA 90 %")), "</details>"
    )
  }
} else {
  add(missing_note("the Stan reference fits of the v2 model and the R-INLA fits at the same cutoff"))
}
