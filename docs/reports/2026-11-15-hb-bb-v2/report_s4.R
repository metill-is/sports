# Report sections: cutover proposal, decisions, appendix, scope and (last, because it
# quotes everything else) the summary.

lad <- function(cl, rung, col) {
  if (!have(ladder)) {
    return(NA)
  }
  x <- ladder$tab[ladder$tab$cell == cl & ladder$tab$rung == rung, col]
  if (length(x) == 0) NA else x[1]
}
wc <- if (have(wf)) wf$within_cmp else NULL
wj <- if (have(wf)) wf$within_j else NULL
within_known <- have(wj) && length(wj) > 0
# Pooled walk-forward verdict per sport: AR(1) minus static, both cells together.
within_pool <- function(cells) {
  x <- do.call(rbind, lapply(cells, function(cl) {
    j <- wj[[cl]]
    if (is.null(j)) {
      return(NULL)
    }
    data.frame(m = j$lsm_raw.ar1 - j$lsm_raw.static, w = j$ll_wdl_raw.ar1 - j$ll_wdl_raw.static, t = j$lst_raw.ar1 - j$lst_raw.static)
  }))
  if (is.null(x)) {
    return(NULL)
  }
  se <- function(v) stats::sd(v) / sqrt(length(v))
  list(n = nrow(x), m = mean(x$m), m_se = se(x$m), w = mean(x$w), w_se = se(x$w), t = mean(x$t), t_se = se(x$t))
}
bb_within <- if (within_known) within_pool(c("bb_m_reg", "bb_f_reg")) else NULL
bb_keep_ar1 <- have(bb_within) && (bb_within$m / bb_within$m_se > 2 || bb_within$w / bb_within$w_se > 2)

# ---------------------------------------------------------------- proposal
cur <- "proposal"
add("<h2 id=\"proposal\">Cutover proposal</h2>")
add("<p>This is a proposal. Nothing below has been done to the production path; each step that would alter live fits or published output is listed under <a href=\"#decisions\">Your decisions</a>.</p>")
add("<h3>Recommended model</h3>")
hb_txt <- "v2 <strong>static</strong> form (strength constant within a season), margin and total, fitted with R-INLA on final scores."
bb_form <- if (!have(bb_within)) {
  "The same model, trained on <strong>regulation-time</strong> scores. Static or with the within-season AR(1): open until the walk-forward of the AR(1) form is in (see the note below)."
} else if (bb_keep_ar1) {
  "The same model <strong>with the within-season AR(1)</strong>, trained on <strong>regulation-time</strong> scores."
} else {
  "The same model, <strong>static</strong> form, trained on <strong>regulation-time</strong> scores."
}
within_txt <- if (!have(bb_within)) {
  "Within-season movement: handball does not need it (within-season sd half a goal to one goal, no gain in leave-one-out or held-out scores). In basketball the fitted within-season sd is five to seven points and leave-one-out improves, so the term may earn its place there; the walk-forward that settles it had not finished when this report was built."
} else {
  sprintf(
    "Within-season movement: handball does not need it (within-season sd half a goal to one goal, no gain). In basketball the fitted within-season sd is five to seven points; over %s walk-forward matches (seasons 2025 to 2027, both sexes) the AR(1) form changes the margin log score by %s, the total by %s and the result log likelihood by %s per match against the static form, so the term %s.",
    int(bb_within$n), pm(bb_within$m, bb_within$m_se, 4), pm(bb_within$t, bb_within$t_se, 4), pm(bb_within$w, bb_within$w_se, 4),
    if (bb_keep_ar1) "is kept for basketball" else "is left out for now: margin and result forecasts do not improve, a small gain on totals (mostly the women's) is the only sign of it, and the static form is the simpler one to run. It stays available as a tested option"
  )
}
add(
  "<div class=\"cols\"><div class=\"pick\"><h3>Handball, men and women</h3><ul>",
  sprintf("<li>%s</li>", hb_txt),
  "<li>Gaussian, one free sd for the margin and one for the total. No possession-informed variance, no Student-t.</li>",
  "<li>Prediction layer: whole-number scores with margin and total on the same parity; draw = level score; a draw shift per sex, set to zero for women and, for men, either zero or a shrunk value (a decision below).</li>",
  "</ul></div><div class=\"pick\"><h3>Basketball, men and women</h3><ul>",
  sprintf("<li>%s</li>", bb_form),
  "<li>Tie shift per division for men, one small shift (or none) for women.</li>",
  "<li>Final result = regulation result, or overtime when level; overtime as a five-minute continuation (which the data so far cannot tell from a coin flip).</li>",
  "<li>Both regulation-time and final-result probabilities come out of one fit.</li>",
  "</ul></div></div>"
)
add(sprintf("<p>%s Stan stays in the repository as the reference: the same v2 model in <code>docs/reports/2026-11-15-hb-bb-v2/stan/v2_margin.stan</code> and <code>v2_total.stan</code>, to be run weekly during the shadow period.</p>", within_txt))

add("<h3>What changes, layer by layer</h3>")
layers <- data.frame(
  layer = c("Config", "Fit entry", "Diagnostics", "Beliefs", "Extract", "Season simulation", "Publish", "Decide", "Basketball ingest", "CI", "Tests"),
  where = c(
    "<code>config/leagues.yml</code>, <code>config/leagues.schema.json</code>",
    "<code>R/model-league.R</code> (<code>fit_league()</code>)",
    "<code>R/model-fit.R</code> (<code>persist_fit_diagnostics()</code>), <code>R/health.R</code>",
    "<code>R/model-posteriors.R</code> (<code>extract_posteriors()</code>)",
    "<code>R/extract-iceland-2dt-shared.R</code>, <code>R/extract-strength-trajectory.R</code>",
    "<code>R/simulate-2dt.R</code>",
    "<code>R/publish-iceland-league.R</code>, <code>R/publish-next-games.R</code>",
    "<code>R/decide-kelly.R</code>",
    "<code>R/ingest-kki-basketball.R</code>, <code>R/storage-schemas.R</code>",
    "<code>.github/workflows/fit.yml</code>, <code>ci-tests.yml</code>",
    "<code>tests/testthat/</code>"
  ),
  change = c(
    "A per-league <code>engine</code> key (<code>stan</code> by default, <code>inla_v2</code> for the cutover). The schema requires <code>stan_model</code> today; it becomes required only for the Stan engine.",
    "After <code>prepare_data()</code>, branch on the engine: <code>build_v2_data()</code> from the same training rows and prediction fixtures, two <code>fit_v2_inla()</code> calls with joint posterior samples, then the two outputs below. <code>fit_league()</code> currently stops if <code>stan_model</code> is missing.",
    "An R-INLA fit has no divergences or R-hat. Write a row with <code>n_obs</code> (which <code>needs_refit()</code> depends on) and engine-specific gates: optimiser converged, no hyperparameter at a bound, home advantage and sds inside sanity ranges, change from yesterday's fit below a threshold. The health check reads <code>div_frac</code> and <code>max_rhat</code> and must accept their absence for this engine.",
    "Same columns (<code>draw_id</code>, <code>home_goals</code>, <code>away_goals</code>), filled by <code>v2_sample_scores()</code>: whole numbers, margin and total on the same parity, 4,000 draws.",
    "<strong>No code change in phase 1.</strong> <code>v2_as_attack_defence()</code> expresses the v2 draws under the Stan variable names the extract reads (<code>cur_offense_*</code>, <code>cur_defense_*</code>, <code>home_advantage_off/def/tot</code>, <code>offense[r,k]</code>, <code>defense[r,k]</code>, <code>mean_goals</code>, <code>sigma</code>, <code>rho</code>, <code>nu</code>), exactly: attack = (net + level) / 2, defence = (net &minus; level) / 2. The strength trajectory is then flat within a season and steps at the season boundary.",
    "Phase 1: unchanged; the existing bivariate draw with a very large <code>nu</code> is the v2 Gaussian (tested). Phase 2: draw whole-number scores with the tie shift, and for basketball regulation time then overtime, so the simulated league tables use the same result model as the match forecasts. New-team draws come from the club-level and season-level priors.",
    "<strong>JSON contract unchanged.</strong> Two meanings shift and should be stated on the site: a team's home advantage for attack and for defence become equal halves of one number, and attack / defence strength become the sum and difference of net strength and scoring level. The handball <code>p_draw</code> stays the band in phase 1.",
    "Unchanged: it reads the same three columns, and its &plusmn;0.5 band on whole-number scores is exactly \"level score\". Regulation-time basketball betting, if it resumes, needs regulation-time draws as two further columns.",
    "Keep the game id the export already carries; fetch the game card once per newly finished game; store regulation-time scores as two nullable columns in <code>results</code>. Backfill from the 3,283 cards already parsed.",
    "Install R-INLA from its own repository (it is not on CRAN). CmdStan is still needed for football and for the weekly Stan reference. The four handball and basketball fits drop from the bulk of a two-hour job to about a minute.",
    "The tests that pin Stan variable names keep passing in phase 1, because the adapter emits those names. New: a stub fit for the R-INLA engine, the adapter and prediction-layer tests already on this branch, and an engine-agnostic contract test on the extract outputs."
  ),
  contract = c("none", "none", "new diagnostics shape", "none (values become whole numbers)", "none", "none in phase 1", "none", "none", "two new nullable columns", "none", "none")
)
add(tbl(layers,
  head = c("Layer", "Where", "Change", "Stored or published contract"),
  note = "File and line references behind this table came from a read-only map of the fit, extract, publish and decide code on this branch; the entry point, the extract's draw pulls and the simulator's match function were re-read directly. The two-phase split keeps the engine swap separable from any change to what the site shows."
))

add("<h3>Engine risks found on the way</h3>")
add("<ul>",
    "<li><strong>Starting values.</strong> From R-INLA's default starting precision (an sd of 0.14) the optimiser settled, for men's basketball totals, in a mode with the team effects switched off: a fit that converges and is wrong. <code>fit_v2_inla()</code> starts every sd at a fifth of its prior bound, and the proposed diagnostics gate checks that no sd sits at a bound.</li>",
    "<li><strong>Constraints are what cost time.</strong> A within-season random walk that sums to zero needs one linear constraint per team-season (over a hundred), and R-INLA then takes 40 to 60 s per fit against 5 s for the static form. The AR(1) form needs none and takes about 15 s.</li>",
    "<li><strong>Weakly identified variances.</strong> R-INLA's hyperparameter posterior is narrower than NUTS's where the data cannot separate club, season and within-season variance. Forecasts are unaffected; published interval widths for individual team strengths are within a few per cent at the median but differ more for some teams.</li>",
    "<li><strong>NUTS has its own trouble with this model.</strong> The season walk of the total model has six steps; in its centred form NUTS produced up to 75 divergences, and it was re-parameterised. R-INLA integrates that variance numerically and has no such failure.</li>",
    "<li><strong>Installation.</strong> R-INLA is not on CRAN and ships its own binary; the version should be pinned in CI as CmdStan is.</li>",
    "</ul>")
add("<h3>Shadow period</h3>")
add(
  "<ol>",
  "<li><strong>Shadow fits.</strong> A separate workflow (or the local machine) fits v2 daily for the four cells and writes to its own tree, for example <code>data/shadow/</code>, which neither decide nor publish reads. Nothing on the site changes.</li>",
  "<li><strong>Weekly scorecard.</strong> The comparison in this report, rerun on the matches played since the shadow began: margin and total log scores, result log likelihood, mean expected margin against observed, draw and overtime calibration, against production's forecast and the Lengjan line where one exists.</li>",
  "<li><strong>Reference check.</strong> Once a week the same v2 model is fitted with NUTS and compared with that day's R-INLA fit (states and held-out predictions, as in the validation section).</li>",
  "<li><strong>Gate, per sport.</strong> At least 150 scored matches in shadow; pooled margin and total log scores not worse than production by more than one standard error; mean expected home margin within one standard error of the observed margin; published JSON built from v2 passes <code>validate-publish</code> and a side-by-side review of one cell on the site's staging build.</li>",
  "<li><strong>Cutover.</strong> Flip the engine for handball first, which needs no new data. Basketball follows once regulation-time scores are in <code>results</code>. Rollback is flipping the key back; the Stan path is untouched.</li>",
  "</ol>"
)
add("<p class=\"note\">In season the four cells play roughly 35 to 45 matches a week between them, so the gate is about four to five weeks of shadow per sport.</p>")

add("<h3>The data decision for basketball</h3>")
add(sprintf(
  "<p>Recommended: store regulation-time scores. They are available for %s of stored games from the source already in use, at one extra request per finished game, and they remove the reason basketball is stuck at <code>mode: scrape</code> (the bookmaker settles on regulation time, production models final scores). The alternative, keeping final scores only, leaves a model that can say nothing about the %s of men's top-division games that are level after forty minutes.</p>",
  pct(mean(per$ok), 1), pct(ot_rate("male", "BD"), 1)
))

# ---------------------------------------------------------------- decisions
cur <- "decisions"
add("<h2 id=\"decisions\">Your decisions</h2>")
add(
  "<ol>",
  "<li><strong>Direction.</strong> v2 on R-INLA as the target for handball and basketball, with Stan kept as the weekly reference?</li>",
  "<li><strong>Shadow workflow.</strong> May a new workflow fit v2 daily into a shadow tree? It adds R-INLA to CI and commits data to a new path; it does not touch live fits or published output.</li>",
  "<li><strong>Regulation-time scores.</strong> Add two nullable columns and the game id to <code>results</code>, change the KK&Iacute; ingest to fetch one game card per finished game, and backfill? This alters the ingest and a stored schema.</li>",
  "<li><strong>Men's handball draws.</strong> Publish with the plain rounded margin (today's band), or switch the draw shift on at a shrunk value? The surplus of level scores is there, but it has not yet paid out of sample.</li>",
  "<li><strong>Home advantage on the site.</strong> v2 has one home advantage per team for the margin. The published attack and defence components would be equal halves of it. Acceptable, or should the site show one number?</li>",
  "<li><strong>Regulation-time forecasts for basketball.</strong> Publish them alongside the final-result forecast (a contract addition, needs a matching change in metill-platform), or keep them internal for now?</li>",
  "</ol>"
)

# ---------------------------------------------------------------- appendix
cur <- "appendix"
add("<h2 id=\"appendix\">Appendix</h2>")
add("<h3>Compute conditions</h3>")
add("<p>All fits ran on one M1 Pro (8 performance cores, 16 GB) shared with three other sessions fitting models. From 21:30 a two-slot limiter allowed at most two 4-core jobs at once; before that the machine was two to three times oversubscribed and swapping, and no wall time from that period is reported as a timing. Leapfrog counts, gradient counts and effective sample sizes do not depend on load. CmdStan 2.38.0 for every NUTS fit, R-INLA 25.6.7.</p>")
add("<h3>Reproducing</h3>")
add("<p>From the repository root on branch <code>feat/hb-bb-v2</code>, with output going to the folder named in <code>HBBB_OUT</code> (default <code>/Users/brynjolfurjonsson/.claude-reviews/sports-hb-bb-v2</code>):</p>")
add("<div class=\"eq\">Rscript docs/reports/2026-11-15-hb-bb-v2/prep.R hb_m 2026-01-31 2026-05-31      # data at the fixed cutoff (also hb_f, bb_m, bb_f)
Rscript -e 'source(\"docs/reports/2026-11-15-hb-bb-v2/run_fit.R\")' hb_m r0_prod 2026-01-31   # one rung; r0_prod, r1_ragged ... r5_gauss, v2s_margin, v2a_total ...
Rscript docs/reports/2026-11-15-hb-bb-v2/kki_games.R &amp;&amp; docs/reports/2026-11-15-hb-bb-v2/kki_fetch_periods.sh   # game ids, then one card per game
Rscript -e 'source(\"docs/reports/2026-11-15-hb-bb-v2/kki_periods.R\")'           # regulation-time scores
Rscript -e 'source(\"docs/reports/2026-11-15-hb-bb-v2/wf_inla.R\")' hb_m none 2022   # walk-forward (add: reg, for basketball)
Rscript -e 'source(\"docs/reports/2026-11-15-hb-bb-v2/wf_score.R\")'              # scores, second-stage tests
Rscript -e 'source(\"docs/reports/2026-11-15-hb-bb-v2/wf_compare.R\")'            # production's stored forecasts, Lengjan
Rscript -e 'source(\"docs/reports/2026-11-15-hb-bb-v2/inla_validate.R\")'         # R-INLA at the fixed cutoff
Rscript -e 'source(\"docs/reports/2026-11-15-hb-bb-v2/compare_validation.R\")'    # against the Stan reference
Rscript -e 'source(\"docs/reports/2026-11-15-hb-bb-v2/ladder_summary.R\")'
Rscript -e 'source(\"docs/reports/2026-11-15-hb-bb-v2/build_report.R\")'</div>")
add("<h3>What is on the branch</h3>")
add(
  "<ul><li>Prototype functions, none called by production code: <code>R/v2-margin-total-data.R</code>, <code>R/v2-margin-total-inla.R</code>, <code>R/v2-margin-total-predict.R</code>, <code>R/v2-margin-total-adapter.R</code>, <code>R/ingest-kki-periods.R</code>.</li>",
  "<li>Tests for each: <code>tests/testthat/test-v2-margin-total-{data,inla,predict,adapter}.R</code>, <code>test-ingest-kki-periods.R</code> (the R-INLA tests skip where R-INLA is not installed).</li>",
  "<li>The experiment: <code>docs/reports/2026-11-15-hb-bb-v2/</code>, scripts and the Stan ladder in <code>stan/</code>.</li>",
  "<li>Not changed: <code>config/leagues.yml</code>, <code>Stan/</code>, the ingest, the fit, extract, decide and publish code.</li></ul>"
)

# ---------------------------------------------------------------- scope
cur <- "scope"
add("<h2 id=\"scope\">Measured and not measured</h2>")
r0_done <- if (have(diag)) names(diag) else character()
measured <- c(
  sprintf("Production fits at one cutoff with production settings: %s.", if (length(r0_done)) paste(LAB[r0_done], collapse = ", ") else "none finished"),
  if (have(ladder)) sprintf("Ladder rungs fitted with NUTS: %s.", paste(vapply(unique(ladder$tab$cell), function(cl) sprintf("%s (%d rows)", LAB[[cl]], sum(ladder$tab$cell == cl)), ""), collapse = "; ")),
  if (have(val) && length(val)) sprintf("R-INLA against Stan on one identical model: %s.", paste(unique(vapply(val, function(o) LAB[[o$cell]], "")), collapse = ", ")),
  sprintf(
    "Weekly walk-forward of v2 over seasons 2022 to 2027, all four cells: %s matches predicted, %s scored (seasons 2023 on).",
    int(sum(vapply(c("hb_m none", "hb_f none", "bb_m none_reg", "bb_f none_reg"), function(k) nrow(wf[[k]]$wf), 1L))),
    int(sum(vapply(list(hbm, hbf, bbm, bbf), nrow, 1L)))
  ),
  "Out-of-sample tests of: the level-score shift (per sex, per division, by closeness), four variance forms including the possession-informed one, a Student-t predictive, the overtime model.",
  sprintf(
    "Comparison with production's stored forecasts on %s matches and with the Lengjan line where odds exist.",
    int(sum(vapply(names(CELLS), function(cl) if (is.null(cmp[[cl]]$cmp)) 0L else nrow(cmp[[cl]]$cmp), 1L)))
  ),
  sprintf("Regulation-time scores for %s of %s stored basketball games.", int(sum(per$ok)), int(nrow(per))),
  "Residual diagnostics by division, season and sex."
)
not_measured <- c(
  if (length(setdiff(names(CELLS), r0_done))) sprintf("Production fits for %s had not finished when this report was built.", paste(LAB[setdiff(names(CELLS), r0_done)], collapse = ", ")),
  if (!have(ladder)) "The ladder rungs had not finished.",
  if (!(have(val) && length(val))) "The like-for-like R-INLA against Stan comparison had not finished.",
  if (!have(bb_within)) "The walk-forward with the within-season AR(1) for basketball had not finished.",
  "A walk-forward of the within-season AR(1) for handball (dropped from the queue: the fixed-cutoff fit shows nothing for it to find).",
  "Weekly correlations for the within-season term other than 0.98.",
  "Production in a walk-forward: one production fit takes tens of minutes, so production is represented by its fixed-cutoff fit and by the forecasts it actually stored (April to May and September to November 2026 only).",
  "Clean wall-clock benchmarks: every time was taken on a shared machine.",
  "The full ladder for the women's cells (production and the final model only, by agreement with the scheduling session).",
  "The published JSON built end to end from a v2 fit: the adapter's algebra is tested, the wiring into <code>fit_league()</code> is proposed, not built.",
  "A league-table (season simulation) comparison between the engines.",
  "The W/D/L effect of a Student-t predictive; only its margin and total log scores were measured.",
  "Genius Sports LiveStats as a second source of period scores: not needed, not tried.",
  "Whether the market beats the model: too few priced matches (65 handball, 20 basketball)."
)
add(
  "<div class=\"cols\"><div><h3>Measured</h3><ul>", paste0("<li>", measured, "</li>", collapse = ""), "</ul></div>",
  "<div><h3>Not measured</h3><ul>", paste0("<li>", not_measured, "</li>", collapse = ""), "</ul></div></div>"
)

# ---------------------------------------------------------------- summary
cur <- "summary"
add("<h2 id=\"summary\">Summary</h2>")
items <- character()
if (length(r0_done)) {
  items <- c(items, sprintf(
    "<strong>The football defects are present.</strong> Home advantage implied by the production fit against the raw home-minus-away margin: %s. The per-team volatility hierarchies and (in handball) the per-team score sds return their priors, the Student-t degrees of freedom sit at or above their prior, and %s of the sampled innovation slots touch no match.",
    paste(vapply(r0_done, function(cl) {
      sprintf(
        "%s %s against %s (&times;%s)", tolower(LAB[[cl]]), num(diag[[cl]]$ha_weighted[1], 2), num(diag[[cl]]$raw_margin[1], 2),
        num(diag[[cl]]$ha_weighted[1] / diag[[cl]]$raw_margin[1], 2)
      )
    }, ""), collapse = "; "),
    paste(range(vapply(names(CELLS), function(cl) round(100 * (1 - 2 * dat[[cl]]$prod$N / (dat[[cl]]$prod$K * dat[[cl]]$prod$N_rounds))), 1)), collapse = " to ") |> paste("%")
  ))
}
items <- c(items, sprintf(
  "<strong>The home advantage itself is small.</strong> Identified, it is %s goals in handball (men, women) and %s points in basketball, in each case equal to the raw margin within its standard error. The handball figure is about a third of the Bundesliga's 1.87.",
  paste(num(ha$v2[ha$cell %in% hb], 2), collapse = " and "), paste(num(ha$v2[ha$cell %in% bb], 2), collapse = " and ")
))
items <- c(items, "<strong>Margin and total can be modelled separately</strong>: their residuals are uncorrelated in every cell. Handball scores have about half the variance a Poisson count would; a Gaussian with a free sd fits, for the women as for the men.")
if (have(ladder)) {
  l1 <- function(cl) {
    sprintf(
      "%s: %s sampled parameters and %s leapfrog steps per iteration in production, %s and %s in the identified Gaussian rung, with the home advantage going from %s to %s when it is pooled and freed of its sign",
      tolower(LAB[[cl]]), int(lad(cl, "r0_prod", "n_sampled")), int(lad(cl, "r0_prod", "leapfrog")), int(lad(cl, "r5_gauss", "n_sampled")),
      int(lad(cl, "r5_gauss", "leapfrog")), num(lad(cl, "r2_common", "ha_mean"), 2), num(lad(cl, "r4_ident", "ha_mean"), 2)
    )
  }
  ok <- vapply(c("hb_m", "bb_m"), function(cl) !is.na(lad(cl, "r5_gauss", "leapfrog")) && !is.na(lad(cl, "r0_prod", "leapfrog")), TRUE)
  if (any(ok)) items <- c(items, sprintf("<strong>The ladder attributes the gains.</strong> %s.", paste(vapply(c("hb_m", "bb_m")[ok], l1, ""), collapse = "; ")))
}
if (have(val) && length(val)) {
  items <- c(items, sprintf(
    "<strong>R-INLA reproduces NUTS.</strong> On one identical model, the largest difference in a state's posterior mean is %s posterior sds, the largest difference in a held-out match's expected margin or total is %s, and the held-out log scores agree to %s. R-INLA with hyperparameters at their mode, which the walk-forward uses, differs by at most %s.",
    num(max(vapply(val, function(o) o$state[["max_abs_mean_diff_in_sd"]], 1)), 2), num(max(vapply(val, function(o) o$pred[["max_abs_mean_diff"]], 1)), 2),
    num(max(vapply(val, function(o) abs(o$log_score[["stan"]] - o$log_score[["inla_full_samples"]]), 1)), 4),
    num(max(vapply(val, function(o) abs(o$log_score[["stan"]] - o$log_score[["inla_eb"]]), 1)), 4)
  ))
}
items <- c(items, sprintf(
  "<strong>Walk-forward.</strong> %s matches scored over seasons 2023 to 2027 with weekly refits. The predictive spread is calibrated in every phase of the season (standardised residual sd between %s and %s for the margin). On the %s matches with a stored production forecast: handball margin %s, total %s, result %s; basketball margin %s, total %s, result %s (log score per match, v2 minus production, pooled over the sexes). Better in handball, level within noise in basketball.",
  int(sum(vapply(list(hbm, hbf, bbm, bbf), nrow, 1L))),
  num(min(vapply(list(hbm, hbf, bbm, bbf), function(s) stats::sd((s$margin_fit - s$mean_m) / s$s_m), 1)), 2),
  num(max(vapply(list(hbm, hbf, bbm, bbf), function(s) stats::sd((s$margin_fit - s$mean_m) / s$s_m), 1)), 2),
  int(sum(vapply(names(CELLS), function(cl) if (is.null(cmp[[cl]]$cmp)) 0L else nrow(cmp[[cl]]$cmp), 1L))),
  pm(pool(hb, "lsm_raw", "ls_margin")[["diff"]], pool(hb, "lsm_raw", "ls_margin")[["se"]], 3),
  pm(pool(hb, "lst_raw", "ls_total")[["diff"]], pool(hb, "lst_raw", "ls_total")[["se"]], 3),
  pm(pool(hb, "ll_wdl_raw", "ll_wdl")[["diff"]], pool(hb, "ll_wdl_raw", "ll_wdl")[["se"]], 3),
  pm(pool(bb, "lsm_raw", "ls_margin")[["diff"]], pool(bb, "lsm_raw", "ls_margin")[["se"]], 3),
  pm(pool(bb, "lst_raw", "ls_total")[["diff"]], pool(bb, "lst_raw", "ls_total")[["se"]], 3),
  pm(pool(bb, "ll_wdl_raw", "ll_wdl")[["diff"]], pool(bb, "ll_wdl_raw", "ll_wdl")[["se"]], 3)
))
items <- c(items, sprintf(
  "<strong>Handball.</strong> The &plusmn;0.5 band is the rounded margin, so it stays; what is added is one draw-shift parameter. Men's level scores run above the band (%s against %s out of sample) but the surplus moves from season to season and the shift has not yet improved the out-of-sample score; women's are calibrated. The possession-informed variance does not beat a free variance and is clearly worse for the women's margin.",
  pct(mean(hbm$margin_fit == 0), 1), pct(mean(hbm$p_draw), 1)
))
items <- c(items, sprintf(
  "<strong>Basketball.</strong> Regulation-time scores exist for %s of stored games from the source already in use. %s of men's B&oacute;nusdeild games are level after regulation, three times the smooth rate; a tie parameter per division improves the regulation 1X2 score out of sample. Overtime has so far been a coin flip (the favourite won %s of %s).",
  pct(mean(per$ok), 1), pct(ot_rate("male", "BD"), 1), int(sum(with(rbind(bbm[bbm$margin_fit == 0, c("mean_m", "margin")], bbf[bbf$margin_fit == 0, c("mean_m", "margin")]), ifelse(mean_m > 0, margin > 0, margin < 0)))),
  int(sum(bbm$margin_fit == 0) + sum(bbf$margin_fit == 0))
))
items <- c(items, "<strong>Proposal.</strong> v2 on R-INLA for both sports (static for handball), basketball on regulation time; swap the engine behind an adapter that keeps the published JSON contract; a four to five week shadow period per sport with a stated gate; store regulation-time scores. Six decisions are yours.")
add("<ol>", paste0("<li>", items, "</li>", collapse = "\n"), "</ol>")
