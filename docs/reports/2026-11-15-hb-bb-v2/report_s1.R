# Report sections: scope (measured / not measured) and the production fits.

# ---------------------------------------------------------------- production fits
cur <- "prod"
add("<h2 id=\"prod\">Production fits, measured</h2>")
add(sprintf(
  "<p>Each production Stan model was fitted once at a fixed cutoff, %s, with production's sampler settings (4 chains, 1,000 warm-up and 1,000 draws, <code>adapt_delta</code> 0.95, tree depth 10, CmdStan 2.38.0, the version CI pins). The matches of 1 February to 31 May 2026 were held out; forfeits are excluded from training, as production does, and from every scored set.</p>",
  "31 January 2026"
))
cell_tab <- do.call(rbind, lapply(names(CELLS), function(cl) {
  d <- dat[[cl]]
  o <- if (have(diag)) diag[[cl]] else NULL
  res <- rd(sprintf("res_%s_r0_prod.rds", cl))
  hold <- table(d$hold$division)
  wall <- if (have(res) && !is.na(res$wall_secs)) sprintf("%s min", num(res$wall_secs / 60, 1)) else if (have(res)) "not recorded" else "–"
  data.frame(
    cell = LAB[[cl]], n = int(d$prod$N), k = int(d$prod$K), hold = sprintf("%d (%s)", nrow(d$hold), paste(names(hold), hold, collapse = ", ")),
    params = if (have(o)) int(o$n_sampled) else "queued", empty = pct(1 - 2 * d$prod$N / (d$prod$K * d$prod$N_rounds), 0),
    div = if (have(o)) int(o$n_divergent) else "–", leap = if (have(o)) int(o$leapfrog) else "–",
    rhat = if (have(o)) num(o$max_rhat, 3) else "–",
    ess = if (have(o)) sprintf("%s (<code>%s</code>)", int(o$min_ess_bulk), o$worst_ess$variable[1]) else "–", wall = wall
  )
}))
add(tbl(cell_tab,
  right = c("n", "k", "params", "empty", "div", "leap", "rhat"),
  head = c(
    "Cell", "Training matches", "Teams", "Held out (by division)", "Sampled parameters", "Innovation slots no match touches",
    "Divergences", "Leapfrog steps per iteration", "Largest R-hat", "Smallest bulk ESS (parameter)", "Wall time"
  ),
  note = paste0(
    "\"Innovation slots no match touches\" is a property of the data layout: the model has one state per team per appearance index up to the league maximum, so a team with a short history carries slots that only the prior sees. ",
    "Wall times were taken inside the shared machine limiter (two, later three, 4-core jobs at once on 8 performance cores), usually with one or two sibling fits running: good for ratios, not benchmarks. The handball men fit was first run before the limiter existed, at two to three times oversubscription; it was run again inside a slot with the same seed, gave identical results, and that second run is the time shown."
  )
))

if (n_r0 > 0) {
  add("<h3>Are the football defects present?</h3>")
  add("<p>The football review found five structural problems. Each has a direct measurement here.</p>")
  cells_d <- names(diag)
  fmt3 <- function(v, d = 2) sprintf("%s [%s, %s]", num(v[2], d), num(v[1], d), num(v[3], d))
  def_rows <- list(
    c("Home advantage implied by the fit, weighted by home matches", vapply(cells_d, function(cl) sprintf("%s (sd %s)", num(diag[[cl]]$ha_weighted[1], 2), num(diag[[cl]]$ha_weighted[2], 2)), "")),
    c("Raw mean of home score minus away score", vapply(cells_d, function(cl) sprintf("%s (se %s)", num(diag[[cl]]$raw_margin[1], 2), num(diag[[cl]]$raw_margin[2], 2)), "")),
    c("<strong>Ratio</strong>", vapply(cells_d, function(cl) sprintf("<strong>%s</strong>", num(diag[[cl]]$ha_weighted[1] / diag[[cl]]$raw_margin[1], 2)), "")),
    c("Home advantage in the identified v2 model (all data)", vapply(cells_d, function(cl) sprintf("%s (sd %s)", num(ha$v2[ha$cell == cl], 2), num(ha$v2_sd[ha$cell == cl], 2)), "")),
    c("Correlation of the scoring level with the mean attack home effect / defence home effect", vapply(cells_d, function(cl) sprintf("%s / %s", num(diag[[cl]]$cor_level_ha_off, 2), num(diag[[cl]]$cor_level_ha_def, 2)), "")),
    c("Scale of the per-team volatility hierarchy, attack (prior median 0.35, 90 % 0.03 to 1.50)", vapply(cells_d, function(cl) fmt3(diag[[cl]]$scale_sigma_off), "")),
    c("&hellip; defence", vapply(cells_d, function(cl) fmt3(diag[[cl]]$scale_sigma_def), "")),
    c("Largest / smallest per-team random-walk sd (attack, defence)", vapply(cells_d, function(cl) sprintf("%s, %s", num(diag[[cl]]$sigma_off_ratio, 2), num(diag[[cl]]$sigma_def_ratio, 2)), "")),
    c("Width of one team's 90 % interval for its random-walk sd, relative to its median", vapply(cells_d, function(cl) sprintf("%s, %s", num(diag[[cl]]$sigma_off_rel_width, 2), num(diag[[cl]]$sigma_def_rel_width, 2)), "")),
    c("Student-t degrees of freedom (prior median 17.8, 90 % 5.5 to 42.0)", vapply(cells_d, function(cl) fmt3(diag[[cl]]$nu, 1), "")),
    c("Per-team score sd: largest / smallest (handball only)", vapply(cells_d, function(cl) if (is.null(diag[[cl]]$sigma_team_ratio)) "one sd already" else num(diag[[cl]]$sigma_team_ratio, 2), "")),
    c("Innovation slots no match touches", vapply(cells_d, function(cl) pct(diag[[cl]]$prior_only_share, 0), ""))
  )
  def_tab <- as.data.frame(do.call(rbind, def_rows), stringsAsFactors = FALSE)
  names(def_tab) <- c("what", cells_d)
  add(tbl(def_tab,
    right = cells_d, head = c("Measurement", LAB[cells_d]),
    note = "Posterior summaries are median [5 %, 95 %]. Home advantage is in goals (handball) or points (basketball)."
  ))

  bd <- do.call(rbind, lapply(cells_d, function(cl) cbind(cell = LAB[[cl]], diag[[cl]]$by_div)))
  bd_tab <- data.frame(
    cell = bd$cell, division = bd$division, n = int(bd$n), raw = sprintf("%s (se %s)", num(bd$raw_margin, 2), num(bd$raw_se, 2)),
    prod = sprintf("%s (sd %s)", num(bd$prod_ha, 2), num(bd$prod_ha_sd, 2)), ratio = num(bd$prod_ha / bd$raw_margin, 2)
  )
  add("<h3>Home advantage by division</h3>")
  add(tbl(bd_tab,
    right = c("n", "ratio"), head = c("Cell", "Division", "Training matches", "Raw home-minus-away margin", "Production's implied home advantage", "Ratio"),
    note = "Per the owner's standing rule, the aggregate is broken down before anything is redesigned: the inflation is present in every division measured, largest where the raw advantage is smallest."
  ))

  vals <- cbind(ha$prod, ha$raw, ha$v2)
  rownames(vals) <- LAB[ha$cell]
  add(
    "<figure>", svg_bars(LAB[ha$cell], c("production (weighted by home matches)", "raw home minus away", "v2, identified"), vals, digits = 2),
    "<figcaption>Home advantage in score units (goals for handball, points for basketball). A missing production bar means that cell's production fit had not finished. The v2 bar is the league home advantage of the identified model fitted to all data.</figcaption></figure>"
  )

  first <- cells_d[1]
  o <- diag[[first]]
  # Finding 14: say what the posterior of the volatility scales does, not that it "matches the prior".
  vol_txt <- if ("hb_m" %in% cells_d) {
    so <- diag[["hb_m"]]$scale_sigma_off
    sd_ <- diag[["hb_m"]]$scale_sigma_def
    sprintf(
      "in handball men the scale parameters' posterior medians (%s and %s) sit on the exponential(2) prior's median (%s), and all the data do is shorten the upper tail (95 %% points %s and %s against the prior's %s)",
      num(so[2], 2), num(sd_[2], 2), num(stats::qexp(0.5, 2), 2), num(so[3], 2), num(sd_[3], 2), num(stats::qexp(0.95, 2), 2)
    )
  } else {
    "the scale parameters stay close to their exponential(2) prior"
  }
  add(sprintf(
    paste0(
      "<p><strong>Reading the table.</strong> Home advantage is overstated because each team has its own attack and defence home effect, each forced to be non-negative with a half-normal(0, 10) prior and no pooling. ",
      "In %s the typical team's attack home effect has posterior sd %s around a mean of %s: the estimates are noise-dominated, the zero bound pushes every one of them upward, and the error cannot average out. ",
      "The intercept ridge that the football review found (scoring level traded against the two home means) is truncated by the same bounds instead of being removed, which is why the correlations in the table are weak here; the ladder below shows it reappear the moment the bounds are lifted. ",
      "The volatility hierarchies are weakly identified at best: %s, and in every cell one team's interval for its own random-walk sd is about as wide as the whole spread between teams. ",
      "The Student-t degrees of freedom sit above their prior in handball, so those data are not asking for heavy tails; in basketball they sit a little below it.</p>"
    ),
    tolower(LAB[[first]]), num(o$ha_off_team_means[["within_sd"]], 2), num(o$ha_off_team_means[["mean"]], 2), vol_txt
  ))
} else {
  add(missing_note("the production fits"))
}
