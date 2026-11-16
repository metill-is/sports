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
# Keep the within-season term for basketball when the walk-forward never counts against
# it and at least one of the three scores improves by more than two standard errors.
bb_keep_ar1 <- have(bb_within) && min(bb_within$m, bb_within$t, bb_within$w) >= 0 &&
  max(bb_within$m / bb_within$m_se, bb_within$t / bb_within$t_se, bb_within$w / bb_within$w_se) > 2
# The same comparison for the lower division alone, where the gain sits.
bb_within_1d <- if (within_known) {
  x <- do.call(rbind, lapply(c("bb_m_reg", "bb_f_reg"), function(cl) {
    j <- wj[[cl]]
    j <- j[j$division == "1D", ]
    data.frame(m = j$lsm_raw.ar1 - j$lsm_raw.static, w = j$ll_wdl_raw.ar1 - j$ll_wdl_raw.static, t = j$lst_raw.ar1 - j$lst_raw.static)
  }))
  se <- function(v) stats::sd(v) / sqrt(length(v))
  list(n = nrow(x), m = mean(x$m), m_se = se(x$m), w = mean(x$w), w_se = se(x$w), t = mean(x$t), t_se = se(x$t))
} else {
  NULL
}

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
    paste0(
      "Within-season movement: handball does not need it (within-season sd half a goal to one goal, no gain). In basketball the fitted within-season sd is five to seven points and leave-one-out improves by %s (men) and %s (women). ",
      "Over %s walk-forward matches (seasons 2025 to 2027, both sexes) the AR(1) form changes the margin log score by %s, the total by %s and the regulation 1X2 log likelihood by %s per match against the static form. ",
      "The gain sits in 1. deild (%s matches: margin %s, total %s, 1X2 %s) and is nil in B&oacute;nusdeild. %s"
    ),
    num(lad("bb_m", "v2 + within-season AR(1), INLA full", "elpd") - lad("bb_m", "v2 static, INLA full", "elpd"), 0),
    num(lad("bb_f", "v2 + within-season AR(1), INLA full", "elpd") - lad("bb_f", "v2 static, INLA full", "elpd"), 0),
    int(bb_within$n), pm(bb_within$m, bb_within$m_se, 4), pm(bb_within$t, bb_within$t_se, 4), pm(bb_within$w, bb_within$w_se, 4),
    int(bb_within_1d$n), pm(bb_within_1d$m, bb_within_1d$m_se, 4), pm(bb_within_1d$t, bb_within_1d$t_se, 4), pm(bb_within_1d$w, bb_within_1d$w_se, 4),
    if (bb_keep_ar1) {
      "It is small, never negative when pooled, and costs about ten more seconds of fitting (timed on the shared machine), so the term is kept for basketball; the static form is the fallback."
    } else {
      "That is not enough to carry the term: it is left out for now and stays available as a tested option."
    }
  )
}
add(
  "<div class=\"cols\"><div class=\"pick\"><h3>Handball, men and women</h3><ul>",
  sprintf("<li>%s</li>", hb_txt),
  "<li>Gaussian, one free sd for the margin and one for the total. No possession-informed variance, no Student-t.</li>",
  "<li>Prediction layer: whole-number scores with margin and total on the same parity; draw = level score; a draw shift per sex, set to zero for women and, for men, either zero or a shrunk value (a decision below). The joint sampler is built and unit-tested; the walk-forward scored margin and total separately, so joint scores are not yet evaluated.</li>",
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
    "Main already has an optional per-league <code>engine</code> key (<code>stan</code> by default, <code>inla</code> for football v2, merged with PR #104). Handball and basketball would use the same key. <code>stan_model</code> stays required for every league: <code>fit_league()</code> checks that the Stan file exists before it reads the key.",
    "<code>fit_league()</code> branches on <code>engine</code> for football. A handball / basketball branch is not written: after <code>prepare_data()</code> it would call <code>build_v2_data()</code> on the same training rows and prediction fixtures, make two <code>fit_v2_inla()</code> calls with joint posterior samples, and hand over the outputs below.",
    "An R-INLA fit has no divergences or R-hat. Write a row with <code>n_obs</code> (which <code>needs_refit()</code> depends on) and engine-specific gates: mode status 0 (<code>fit_v2_inla()</code> now returns it), no hyperparameter at a bound, home advantage and sds inside sanity ranges, change from yesterday's fit below a threshold. Main's football engine has such a gate (<code>fv2_inla_gate()</code>). The health check reads <code>div_frac</code> and <code>max_rhat</code> and must accept their absence for this engine.",
    "<strong>Handball:</strong> the same columns (<code>draw_id</code>, <code>home_goals</code>, <code>away_goals</code>) can be filled by <code>v2_sample_scores()</code>: whole numbers, margin and total on the same parity. The sampler is unit-tested; its joint scores were not evaluated in the walk-forward. <strong>Basketball: not possible yet.</strong> The model is trained on regulation time, so <code>v2_sample_scores()</code> draws regulation-time scores and about 7 % of men's top-division draws are level. It has no overtime step. Final-score draws need one that is not written: send each level draw through the overtime model and add the overtime points.",
    "<strong>Needs code.</strong> The extract reads a cmdstanr fit by variable name: <code>cur_offense_home/away</code>, <code>cur_defense_home/away</code>, <code>cur_strength_home/away</code>, <code>home_advantage_off/def/tot</code>, and for the trajectory the per-round <code>offense[r, k]</code> and <code>defense[r, k]</code>. <code>v2_as_attack_defence()</code> returns seven arrays: <code>offense</code>, <code>defense</code>, <code>home_advantage_off</code>, <code>home_advantage_def</code>, <code>mean_goals</code>, <code>sigma</code>, <code>rho</code>. It does not supply the <code>cur_*</code> variables, <code>home_advantage_tot</code> or anything indexed by round, and it returns arrays, not a fit object. So the extract needs an engine branch that takes those arrays, or a wrapper that builds the missing names from them (a per-round array has no v2 counterpart: the state is per season, or per week in the AR(1) form). What is written and tested is the rotation itself: attack = (net + level) / 2, defence = (net &minus; level) / 2.",
    "<strong>Needs an input adapter that is not written.</strong> <code>.extract_sim_inputs_2dt()</code> pulls from the fit <code>nu</code>, <code>mean_goals</code>, <code>delta_mean_goals</code> and <code>sigma_mean_goals</code>; for handball the per-team <code>sigma_team</code> with <code>mean_sigma_team</code>, <code>scale_sigma_team</code> and <code>rho</code>; for basketball <code>sigma</code> and <code>alpha_rho</code>, <code>beta_rho</code>, <code>beta2_rho</code>, <code>beta3_rho</code>. The sport match functions use those, not the adapter's <code>sigma</code> (handball) or <code>rho</code> (basketball). By the algebra there are values that reproduce the v2 Gaussian (every <code>sigma_team</code> equal to the adapter's sigma and no spread; <code>alpha_rho</code> the logit of (rho + 1) / 2 with the three slopes at zero; a very large <code>nu</code>), and a test shows that the simulator's bivariate draw has the v2 predictive when given sigma and rho directly; nothing writes those inputs yet and the mapping is untested. For basketball the simulator would then draw continuous final scores from a regulation-time model, ignoring the level-score surplus and overtime; what that does to a simulated table was not measured.",
    "<strong>JSON contract unchanged, once the beliefs and extract above exist.</strong> Two meanings shift and should be stated on the site: a team's home advantage for attack and for defence become equal halves of one number, and attack / defence strength become the sum and difference of net strength and scoring level. The handball <code>p_draw</code> stays the band in phase 1. <strong>Basketball cannot publish from regulation-time draws:</strong> its publish profile has <code>has_ties = FALSE</code>, so the extract takes the home win probability as the share of draws with a positive margin and the away one as the share with a negative margin, and the standings rule gives a level draw to the away side. Level draws would vanish from the two probabilities and count as away wins in the simulated tables. Basketball has to publish from final-score draws (the overtime step) or from <code>v2_basketball_result_probs()</code> directly.",
    "Handball: unchanged; it reads the same three columns, and its &plusmn;0.5 band on whole-number scores is exactly \"level score\". Regulation-time basketball betting, if it resumes, needs regulation-time draws as two further columns.",
    "Keep the game id the export already carries; fetch the game card once per newly finished game; store regulation-time scores as two nullable columns in <code>results</code>. Backfill from the 3,283 cards already parsed.",
    "Main's INLA Engine workflow already installs R-INLA and runs every test file that reaches it through <code>skip_if_no_inla()</code>, this branch's included. The fit workflow would need the same install step. CmdStan is still needed for football and for the weekly Stan reference. On the times taken here (shared machine, not a benchmark) the four handball and basketball fits would drop from the bulk of a two-hour job to about a minute.",
    "The tests that pin Stan variable names keep passing only if the wrapper above emits those names. New: a stub fit for the R-INLA engine, the adapter and prediction-layer tests already on this branch, and an engine-agnostic contract test on the extract outputs."
  ),
  contract = c(
    "none", "none", "new diagnostics shape", "none for handball (values become whole numbers); basketball blocked on the overtime step", "none, after new code",
    "none, after new code", "none", "none", "two new nullable columns", "none", "none"
  )
)
add(tbl(layers,
  head = c("Layer", "Where", "Change", "Stored or published contract"),
  note = "The first version of this table said the extract and the simulator needed no code in phase 1 and that the adapter supplied the names they read. That was wrong: the rows above were rewritten after reading what <code>R/extract-iceland-2dt-shared.R</code>, <code>R/extract-strength-trajectory.R</code>, <code>R/simulate-2dt.R</code> and <code>R/publish-iceland-2dt-helpers.R</code> pull from a fit. Nothing in this table is built; the cutover is not approved."
))
add("<p>So the engine swap is more work than first stated. Written and tested on this branch: the fitter, the prediction layer, the attack / defence rotation. Not written: the fit branch, the wrapper or branch that feeds the extract, the simulator's input adapter, and for basketball the overtime step that turns regulation-time draws into final scores. The shadow period below does not need any of these, because it scores forecasts and publishes nothing.</p>")

HYPER_LAB <- c(
  "sd: the Gaussian observations" = "match sd", "sd: b" = "club-level sd", "sd: u_s" = "season-level sd", "rho: u_s" = "season-to-season correlation",
  "sd: ha" = "sd of the team home-advantage deviations", "sd: s_s" = "sd of the division level's walk over seasons"
)
repro_li <- if (have(repro)) {
  odd <- repro$fits[repro$fits$n_distinct > 1, ]
  odd_txt <- vapply(seq_len(nrow(odd)), function(i) {
    o <- odd[i, ]
    h <- repro$hfa[repro$hfa$cell == o$cell, ]
    ha_txt <- if (o$response == "margin") {
      sprintf("home advantage %s against %s, with a posterior sd of %s; ", num(h$hfa_other, 3), num(h$hfa_modal, 3), num(h$hfa_sd, 2))
    } else {
      ""
    }
    sprintf(
      "the %s model for %s (fitted values up to %s %s from the usual result; %sthe median of the %s %s against %s)",
      o$response, tolower(LAB[[o$cell]]), num(o$max_abs_fitted_diff, 2), UNIT[[o$cell]], ha_txt, HYPER_LAB[[o$hyper_param]],
      num(o$hyper_q50_other, 2), num(o$hyper_q50_modal, 2)
    )
  }, "")
  sprintf(
    paste0(
      "<li><strong>Not bit-reproducible between runs.</strong> The eight plug-in fits behind <a href=\"#data\">What the data ask of the likelihood</a> were run %s times in separate R processes on the same data (the original run, the rerun that stored the dispersion table, and four repeats). ",
      "%s of the %s results were bit-identical with the most frequent one; %s were not, each in a different run: %s. The cause was not investigated. ",
      "The shifts are a small fraction of a posterior sd, but they mean that a refit on unchanged data can move a published number in the second decimal, which the proposed gate on the change from yesterday's fit has to allow for, ",
      "and that a rerun of the walk-forward need not reproduce its scores to the last digit. The stored <code>descriptives.rds</code> is a run in which every fit is the most frequent result.</li>"
    ),
    int(length(repro$runs)), int(sum(repro$fits$n_modal)), int(sum(repro$fits$n_runs)), int(sum(repro$fits$n_runs) - sum(repro$fits$n_modal)),
    paste(odd_txt, collapse = " and ")
  )
} else {
  ""
}
mode_li <- if (have(wfst)) {
  sprintf(
    paste0(
      "<li><strong>A fit is returned whether or not the mode search succeeded.</strong> R-INLA reports the status of its search for the hyperparameter mode and returns a fit either way. <code>fit_v2_inla()</code> did not read it. It now returns it, warns unless it is 0, and every script stores it. ",
      "Every R-INLA fit behind this report was run again with the status recorded: %s walk-forward fits and %s others. %s ended with a status other than 0. The walk-forward numbers in this report are those of that second run.</li>"
    ),
    int(sum(wfst$wf$fits)), int(nrow(wfst$other)), int(sum(wfst$wf$status_not_zero) + sum(!(wfst$other$status %in% 0L)))
  )
} else {
  ""
}
ridge_li <- if (have(wfst)) {
  main <- wfst$wf[wfst$wf$run %in% c("hb_m_none", "hb_f_none", "bb_m_none_reg", "bb_f_none_reg"), ]
  sc <- wfst$scores
  sprintf(
    paste0(
      "<li><strong>A successful search is not a unique answer.</strong> The second run of the walk-forward used the same code path for the fits and the same data as the first, and the two do not give the same predictions. ",
      "Over the four main walk-forwards a match's expected margin differs between the runs by %s to %s goals or points at the median and by up to %s; in %s of %s cutoffs some hyperparameter's median differs by more than a quarter. ",
      "The scores hardly move: the mean margin, total and result scores of a cell change by at most %s between the runs (<a href=\"#wf\">table in the walk-forward section</a>). ",
      "What moves is how a team's strength is split between the club level and the season level with its memory. With six seasons the two are close to interchangeable, each weekly fit starts from last week's answer, and a run stays with the split it began with. ",
      "For a daily production fit this means a team's published strength components can differ between two equally good descriptions of the same data. The gate on the change from yesterday's fit would trip on it. Whether to start every fit cold, to integrate over the hyperparameters, or to fix the split has to be settled before a cutover, and the shadow period should record the day-to-day movement of the published numbers.</li>"
    ),
    num(min(main$median_abs_diff_margin), 3), num(max(main$median_abs_diff_margin), 3), num(max(main$max_abs_diff_margin), 2),
    int(sum(main$cutoffs_hyper_differs_25pct)), int(sum(main$cutoffs)), num(max(abs(sc$diff)), 4)
  )
} else {
  ""
}
add("<h3>Engine risks found on the way</h3>")
add("<ul>", mode_li, ridge_li,
    "<li><strong>Starting values.</strong> From R-INLA's default starting precision (an sd of 0.14) the optimiser settled, for men's basketball totals, in a mode with the team effects switched off: a fit that converges and is wrong. <code>fit_v2_inla()</code> starts every sd at a fifth of its prior bound, and the proposed diagnostics gate checks that no sd sits at a bound.</li>",
    "<li><strong>Constraints are what cost time.</strong> A within-season random walk that sums to zero needs one linear constraint per team-season (over a hundred), and R-INLA then takes 40 to 60 s per fit against 5 s for the static form. The AR(1) form needs none and takes about 15 s. All three were timed on the shared machine: ratios, not benchmarks.</li>",
    "<li><strong>Weakly identified variances.</strong> R-INLA's hyperparameter posterior is narrower than NUTS's where the data cannot separate club, season and within-season variance. Forecasts are unaffected; published interval widths for individual team strengths are within a few per cent at the median but differ more for some teams.</li>",
    "<li><strong>NUTS has its own trouble with this model.</strong> The season walk of the total model has six steps; in its centred form NUTS produced up to 75 divergences, and it was re-parameterised. R-INLA integrates that variance numerically and has no such failure.</li>",
    "<li><strong>Priors can be dropped silently.</strong> R-INLA ignores a hyperparameter specification it cannot read from the model term and fits with its defaults, without a warning (found the same night by the national-team session). <code>fit_v2_inla()</code> was checked against the fitted object, and three tests now read the specifications back from it: the PC prior and starting value of every sd in the margin and the total model (observation, club, season, team home advantage, the division level's season walk, both within-season forms), the prior on the season-to-season correlation, the fixed weekly correlation of the AR(1) form, and the normal priors on the home advantage and on the division intercepts. Each fails when the corresponding specification is changed or dropped.</li>",
    repro_li,
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
  "<li><strong>Package metadata.</strong> The branch edits <code>DESCRIPTION</code>: five <code>Collate</code> entries for the new R files, added by <code>devtools::document()</code>. <code>Matrix</code> is declared on main and came in with the merge. R-INLA is declared nowhere: the football cutover approved \"INLA under Suggests, version pinned\" and that is not done on main yet. Proposed: declare it once, with that cutover, together with the <code>Additional_repositories</code> entry; this branch then needs nothing of its own, and its R-INLA tests already run in main's INLA Engine workflow. Keep the Collate entries when the branch merges?</li>",
  "</ol>"
)

# ---------------------------------------------------------------- appendix
cur <- "appendix"
add("<h2 id=\"appendix\">Appendix</h2>")
add("<h3>Compute conditions</h3>")
add("<p>All fits ran on one M1 Pro (8 performance cores, 16 GB) shared with three other sessions fitting models. From 21:30 a shared limiter allowed at most two (later three) 4-core jobs at once; before that the machine was two to three times oversubscribed and swapping, and no wall time from that period is reported as a timing. Leapfrog counts, gradient counts and effective sample sizes do not depend on load. CmdStan 2.38.0 for every NUTS fit, R-INLA 25.6.7.</p>")
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
docs/reports/2026-11-15-hb-bb-v2/repro_descriptives.sh &amp;&amp; Rscript -e 'source(\"docs/reports/2026-11-15-hb-bb-v2/repro_descriptives.R\")'   # repeat runs, compared
Rscript -e 'source(\"docs/reports/2026-11-15-hb-bb-v2/cold_start_check.R\")' 1   # one fit from a cold start; repeat with 2, 3
Rscript -e 'source(\"docs/reports/2026-11-15-hb-bb-v2/wf_status.R\")'            # mode status; the two runs compared
Rscript -e 'source(\"docs/reports/2026-11-15-hb-bb-v2/review_corrections.R\")'   # old value beside new
Rscript -e 'source(\"docs/reports/2026-11-15-hb-bb-v2/ladder_summary.R\")'
Rscript -e 'source(\"docs/reports/2026-11-15-hb-bb-v2/build_report.R\")'</div>")
add("<p class=\"note\">The stored results were computed against the data and the config of the branch point (commit 67fcb6baa). Main has since been merged into the branch, and its <code>data/</code> holds newer results and a pruned forecast archive, so the scripts that read data (<code>prep.R</code>, <code>wf_inla.R</code>, <code>descriptives.R</code>, <code>wf_compare.R</code>, <code>kki_periods.R</code>, <code>cold_start_check.R</code>) reproduce the stored results only from a checkout of the last commit before the merge, or with <code>HBBB_DATA</code> pointing at that checkout's <code>data/</code>.</p>")
add("<h3>What is on the branch</h3>")
add(
  "<ul><li>Prototype functions, none called by production code: <code>R/v2-margin-total-data.R</code>, <code>R/v2-margin-total-inla.R</code>, <code>R/v2-margin-total-predict.R</code>, <code>R/v2-margin-total-adapter.R</code>, <code>R/ingest-kki-periods.R</code>.</li>",
  "<li>Tests for each: <code>tests/testthat/test-v2-margin-total-{data,inla,predict,adapter}.R</code>, <code>test-ingest-kki-periods.R</code> (the R-INLA tests skip where R-INLA is not installed).</li>",
  "<li>The experiment: <code>docs/reports/2026-11-15-hb-bb-v2/</code>, scripts and the Stan ladder in <code>stan/</code>; <code>check_headlines.R</code> recomputes the headline numbers from the result files.</li>",
  "<li>The report's source: <code>build_report.R</code> with <code>report_s1.R</code> to <code>report_s4.R</code> and <code>report_helpers.R</code>; <code>report.md</code>, a Markdown rendering written by the same run; <code>README.md</code>, which says how to rebuild. The HTML itself is git-ignored.</li>",
  "<li><code>DESCRIPTION</code>: five <code>Collate</code> entries for the new R files. R-INLA is not declared in <code>Suggests</code>.</li>",
  "<li>Not changed: <code>config/leagues.yml</code>, <code>Stan/</code>, the ingest, the fit, extract, decide and publish code.</li></ul>"
)

add("<h3>Where each number comes from</h3>")
add(sprintf("<p>Every number in this report is read from a result file under <code>%s/</code>. The table names the file and the fields behind each headline number; <code>docs/reports/2026-11-15-hb-bb-v2/check_headlines.R</code> recomputes them from exactly these fields without going through the report builder (its output: <code>logs/check_headlines.log</code>).</p>", OUT))
src <- data.frame(
  what = c(
    "Production's implied home advantage, raw margin, ratio, by division; volatility scales; Student-t degrees of freedom",
    "Production sampler: sampled parameters, divergences, leapfrog steps, R-hat, ESS, wall time, machine slots",
    "Identified home advantage and the hyperparameter table of the v2 model; residual and mismatch tables; score variance / mean given strengths (the Poisson comparison)",
    "Ladder tables: parameters, leapfrog, leave-one-out density, home advantage, held-out scores per rung; paired differences from production",
    "R-INLA against Stan: hyperparameters, states, held-out predictor, log scores, leave-one-out",
    "Within-season term at the fixed cutoff (sd, leave-one-out gain)",
    "Weekly-correlation sensitivity (0.90, 0.95, 0.98)",
    "Walk-forward predictions; seconds per cutoff (about 6 s, on the shared machine)",
    "Walk-forward scores (3,930 matches); level-score shift, variance forms, Student-t, overtime; calibration by phase",
    "Within-season AR(1) minus static in the walk-forward (1,211 matches; 1. deild 549)",
    "Regulation-trained plus overtime against the final-score model",
    "v2 against production's stored forecasts (338 matches); mean expected margins; RMSE",
    "v2 and production against the Lengjan line",
    "Regulation-time coverage (99.7 %), overtime shares (7.1 % in men's B&oacute;nusdeild)",
    "Run-to-run reproducibility of the R-INLA plug-in fits",
    "Mode status of every fit; the two runs of the walk-forward against each other; cold-start fits in separate processes",
    "Old value beside new for every number the review changed",
    "Package tests"
  ),
  file = c(
    "<code>out/diag_prod.rds</code>", "<code>out/res_&lt;cell&gt;_r0_prod.rds</code>", "<code>out/descriptives.rds</code>",
    "<code>out/ladder.rds</code> (built from <code>out/res_&lt;cell&gt;_&lt;rung&gt;.rds</code>, <code>out/draws_&lt;cell&gt;_&lt;rung&gt;.rds</code>, <code>out/inla_validate_&lt;cell&gt;.rds</code>)",
    "<code>out/compare_validation.rds</code>", "<code>out/inla_validate_&lt;cell&gt;.rds</code>, <code>out/ladder.rds</code>", "<code>out/within_rho.rds</code>",
    "<code>out/wf_&lt;cell&gt;_none.rds</code> (handball; basketball trained on final scores), <code>out/wf_&lt;cell&gt;_none_reg.rds</code>, <code>out/wf_&lt;cell&gt;_ar1_reg.rds</code>",
    "<code>out/wf_scores.rds</code>", "<code>out/wf_scores.rds</code>", "<code>out/wf_scores.rds</code>", "<code>out/wf_compare.rds</code>", "<code>out/wf_compare.rds</code>",
    "<code>kki/periods_matched.rds</code> (parsed cards: <code>kki/periods.tsv</code>, raw cards: <code>kki/raw/</code>)",
    "<code>out/repro_descriptives.rds</code> (from the runs kept in <code>repro/run_*/out/descriptives.rds</code>)",
    "<code>out/wf_status.rds</code> (first run: <code>out/first-run/</code>; cold starts: <code>out/cold_start_*.rds</code>)",
    "<code>out/review_corrections.rds</code> (published results: <code>out/at-84b9e6e5b/</code>)", "<code>logs/full_tests.log</code>"
  ),
  field = c(
    "<code>[[cell]]$ha_weighted</code>, <code>$raw_margin</code>, <code>$by_div</code>, <code>$scale_sigma_off</code>, <code>$scale_sigma_def</code>, <code>$nu</code>",
    "<code>$n_sampled</code>, <code>$n_divergent</code>, <code>$mean_leapfrog</code>, <code>$max_rhat</code>, <code>$worst_ess</code>, <code>$wall_secs</code>, <code>$machine</code>",
    "<code>[[cell]]$hfa</code>, <code>$hyper_m</code>, <code>$hyper_t</code>, <code>$d</code> (one row per match with fitted means and residuals), <code>$by_div</code>, <code>$disp</code> (per division: <code>pearson_poisson</code> is score variance / mean, <code>binom_n55</code> the same against a binomial with 55 attacks)",
    "<code>$tab</code> (one row per cell and rung: <code>n_sampled</code>, <code>leapfrog</code>, <code>elpd</code>, <code>ha_mean</code>, <code>level_sd</code>, <code>ridge_cor</code>, <code>wall_secs</code>); <code>$scores[[\"&lt;cell&gt; &lt;rung&gt;\"]]</code> (per held-out match: <code>ls_margin</code>, <code>ls_total</code>, <code>ll_wdl</code>)",
    "<code>[[\"&lt;cell&gt; &lt;response&gt;\"]]$hyper</code>, <code>$state</code>, <code>$pred</code>, <code>$log_score</code>, <code>$loo</code>",
    "<code>[[\"margin ar1 auto\"]]$hyper_sd</code>, <code>$cpo</code>", "one row per cell and correlation: <code>loo</code>, <code>ls_margin</code>",
    "<code>$pred</code> (one row per predicted match: <code>mean_m</code>, <code>sd_m</code>, <code>sig2_m</code>, <code>mean_t</code>, <code>sd_t</code>, <code>sig2_t</code>); <code>$cut$secs_m + $cut$secs_t</code>",
    "<code>[[\"&lt;cell&gt; none\"]]$scores</code> (handball), <code>[[\"&lt;cell&gt; none_reg\"]]$scores</code> (basketball): <code>lsm_*</code> margin, <code>lst_*</code> total, <code>ll_wdl_*</code> result, <code>ll_final_*</code> final result, <code>p_draw*</code>, <code>margin_fit</code>, <code>phase</code>",
    "<code>$within_j[[\"bb_m_reg\"]]</code>, <code>[[\"bb_f_reg\"]]</code>: <code>lsm_raw.ar1 - lsm_raw.static</code> and the same for <code>lst_raw</code>, <code>ll_wdl_raw</code>",
    "<code>[[\"bb_m reg_vs_final\"]]</code>, <code>[[\"bb_f reg_vs_final\"]]</code>: <code>ll_final_shift_div - ll_wdl_raw.final</code>, <code>lsm_final_shift_div - lsm_raw.final</code>",
    "<code>[[cell]]$cmp</code>: production <code>ls_margin</code>, <code>ls_total</code>, <code>ll_wdl</code>, <code>mean_m.prod</code>; v2 <code>lsm_raw</code>, <code>lst_raw</code>, <code>ll_wdl_raw</code>, <code>mean_m.v2</code>",
    "<code>[[cell]]$market</code>: <code>ll_market</code>, <code>ll_v2</code>, <code>ll_prod</code>",
    "one row per stored game: <code>ok</code>, <code>overtime</code>, <code>home_reg</code>, <code>away_reg</code>, <code>sex</code>, <code>division</code>",
    "<code>$fits</code> (per cell and response: <code>n_runs</code>, <code>n_modal</code>, <code>max_abs_fitted_diff</code>, <code>hyper_q50_modal</code>, <code>hyper_q50_other</code>), <code>$hfa</code>",
    "<code>$wf</code> (per walk-forward: <code>fits</code>, <code>status_not_zero</code>, differences between the runs), <code>$scores</code>, <code>$other</code> (status of the fits outside the walk-forward), <code>$cold</code>",
    "one row per quantity: <code>published</code>, <code>corrected</code> (same predictions, corrected scripts), <code>final</code> (second run)",
    "summary reporter output of <code>devtools::test()</code>"
  )
)
add(tbl(src, head = c("Numbers", "Result file", "Fields"),
        note = "Cells are <code>hb_m</code>, <code>hb_f</code>, <code>bb_m</code>, <code>bb_f</code>. The printed tables behind every section are also kept as text in <code>logs/</code>: <code>diag_prod.log</code>, <code>ladder_summary.log</code>, <code>compare_validation.log</code>, <code>wf_score.log</code>, <code>wf_compare.log</code>, <code>descriptives.log</code>, <code>kki_periods.log</code>."))

add("<h3>Corrected after review</h3>")
add("<p>An independent review of this report on 16 November confirmed 24 defects. Where a number changed, the old value is beside the new one. \"Published\" is the report as first reviewed; \"corrected\" is the same walk-forward predictions scored by the corrected scripts, so published against corrected is the effect of a fix; \"final\" is the second run of the walk-forward, so corrected against final is the effect of running the fits again.</p>")
if (have(corr)) {
  ctab <- data.frame(f = corr$finding, what = esc(corr$what), pub = esc(corr$published), cor = esc(corr$corrected), fin = esc(corr$final))
  add(tbl(ctab, right = c("pub", "cor", "fin"), head = c("Finding", "Quantity", "Published", "Corrected", "Final")))
} else {
  add(missing_note("the table of corrected numbers"))
}
add(
  "<ul>",
  "<li><strong>Findings 1 and 10.</strong> A final basketball margin cannot be level. Production's stored forecasts and the v2 model trained on final scores were scored with a rounded margin that keeps about 2 % of its mass on a level score; the regulation-plus-overtime model they were compared with has none there. Every basketball final-margin score now uses a distribution with no mass on zero, in the walk-forward, the production comparison, the ladder and the comparison with Stan.</li>",
  "<li><strong>Finding 2.</strong> Rung R1 was described as removing only the unused states. It also drops production's sum-to-zero constraint on each round's innovations and uses a different prior centre for the scoring level, so its drop in leapfrog steps cannot be put on the unused states alone. The text and the rung's name say so.</li>",
  "<li><strong>Finding 4.</strong> The sentence that R-INLA describes the total models' variance split more narrowly than NUTS was fixed text and false for the handball men total model. It is now generated from the comparison.</li>",
  "<li><strong>Findings 3, 5, 6, 15, 18 and 22</strong> are in the code and its tests: the R-INLA tests run in main's INLA Engine workflow; the priors of the total model, the random-walk form and the intercepts are asserted; a season nobody played and a two-division league are tested; with posterior draws the overtime share is weighted draw by draw. None changes a reported number.</li>",
  "<li><strong>Finding 7.</strong> The fitter reads and returns the status of R-INLA's mode search, and every fit was run again with it kept. See <a href=\"#wf\">The walk-forward, run twice</a>.</li>",
  "<li><strong>Findings 8 and 9.</strong> The cutover table said the extract and the simulator needed no code and that the adapter supplied what they read. It supplies seven arrays, not the names or the fit object they read, and regulation-time basketball draws cannot be published without an overtime step. The table is rewritten.</li>",
  "<li><strong>Finding 11.</strong> This autumn's weeks 7 to 11 were labelled \"last 6 weeks\". They are now in the middle row.</li>",
  "<li><strong>Finding 12.</strong> The report said v2 always works with the older information. Production's forecast was the older one in about four matches in ten.</li>",
  "<li><strong>Finding 13.</strong> The Lengjan table scored basketball without the tie shift and the production comparison used the per-division shift for women. Every table now uses the proposed variant: per division for men, one shift for women.</li>",
  "<li><strong>Finding 14.</strong> \"The posteriors match the prior\" overstated it: the medians do, the upper tails are shorter.</li>",
  "<li><strong>Findings 16 and 17.</strong> The fetch script now follows <code>HBBB_OUT</code>; the period log no longer prints a count of double overtimes that could only be zero.</li>",
  "<li><strong>Finding 19.</strong> The agreement with Stan was quoted for full integration only; the plug-in fit, which the walk-forward uses, is now beside it.</li>",
  "<li><strong>Finding 20.</strong> The data-story table now states the prior of the AR(1) within-season form.</li>",
  "<li><strong>Finding 21.</strong> A logit shift is an odds ratio; it was printed as a ratio of rates. Both are now given.</li>",
  "<li><strong>Finding 23.</strong> The variance form that grows with the expected total was centred on the scored season's own mean; the centre now comes from the training seasons.</li>",
  "<li><strong>Finding 24.</strong> The \"final result\" rows of the walk-forward table repeated the regulation-time calibration; they now show the calibration of the final-score predictive.</li>",
  "</ul>"
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
  "Residual diagnostics by division, season and sex.",
  if (have(repro)) sprintf("Run-to-run reproducibility of the R-INLA plug-in fits: %s runs of the eight fits on the same data.", int(length(repro$runs))),
  if (have(wfst)) sprintf("The status of the mode search of every R-INLA fit behind the report (%s fits), and two complete runs of the walk-forward against each other.", int(sum(wfst$wf$fits) + nrow(wfst$other)))
)
not_measured <- c(
  if (length(setdiff(names(CELLS), r0_done))) sprintf("Production fits for %s had not finished when this report was built.", paste(LAB[setdiff(names(CELLS), r0_done)], collapse = ", ")),
  if (!have(ladder)) "The ladder rungs had not finished.",
  if (!(have(val) && length(val))) "The like-for-like R-INLA against Stan comparison had not finished.",
  if (!have(bb_within)) "The walk-forward with the within-season AR(1) for basketball had not finished.",
  "A walk-forward of the within-season AR(1) for handball (dropped from the queue: the fixed-cutoff fit shows nothing for it to find).",
  "Weekly correlations other than 0.98 for the within-season term in the walk-forward (0.90 and 0.95 were only checked at the fixed cutoff).",
  "Production in a walk-forward: one production fit takes tens of minutes, so production is represented by its fixed-cutoff fit and by the forecasts it actually stored (April to May and September to November 2026 only).",
  "Joint (home, away) score distributions. The parity-respecting sampler <code>v2_sample_scores()</code> exists and is unit-tested (<code>R/v2-margin-total-predict.R</code>), but the walk-forward scored margin and total as two separate whole-number distributions, so joint scores were not evaluated.",
  "Clean wall-clock benchmarks: every time was taken on a shared machine, the R-INLA fit times included.",
  "The full ladder for the women's cells (production and the final model only, by agreement with the scheduling session).",
  "The published JSON built end to end from a v2 fit: the adapter's algebra is tested, the wiring into <code>fit_league()</code> is proposed, not built.",
  "A league-table (season simulation) comparison between the engines.",
  "Genius Sports LiveStats as a second source of period scores: not needed, not tried.",
  "Whether the market beats the model: too few priced matches (65 handball, 20 basketball).",
  "Why the two runs of the walk-forward differ from their first cutoff on, where neither has a warm start. The first run was made on a heavily loaded machine; that is the only known difference.",
  "A walk-forward that starts every fit cold, or one that integrates over the hyperparameters instead of fixing them at their mode.",
  "The inputs that would make the season simulator reproduce v2: stated from the algebra, not tested.",
  "An overtime step for basketball score draws: not written, so nothing publishable for basketball was built from v2.",
  "Whether the first run's fits ended their mode search with status 0: that run did not keep the status."
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
    "<strong>The football defects are present.</strong> Home advantage implied by the production fit against the raw home-minus-away margin: %s. The per-team volatility hierarchies are weakly identified at best (one team's interval for its volatility is about as wide as the whole spread between teams), handball's per-team score sds are indistinguishable, and %s of the sampled innovation slots touch no match. Student-t degrees of freedom: %s (prior median 18), so handball does not ask for heavy tails and basketball asks for a little.",
    paste(vapply(r0_done, function(cl) {
      sprintf(
        "%s %s against %s (&times;%s)", tolower(LAB[[cl]]), num(diag[[cl]]$ha_weighted[1], 2), num(diag[[cl]]$raw_margin[1], 2),
        num(diag[[cl]]$ha_weighted[1] / diag[[cl]]$raw_margin[1], 2)
      )
    }, ""), collapse = "; "),
    paste(range(vapply(names(CELLS), function(cl) round(100 * (1 - 2 * dat[[cl]]$prod$N / (dat[[cl]]$prod$K * dat[[cl]]$prod$N_rounds))), 1)), collapse = " to ") |> paste("%"),
    paste(vapply(r0_done, function(cl) sprintf("%s %s", tolower(LAB[[cl]]), num(diag[[cl]]$nu[2], 0)), ""), collapse = ", ")
  ))
}
items <- c(items, sprintf(
  "<strong>The home advantage itself is small.</strong> Identified, it is %s goals in handball (men, women) and %s points in basketball, in each case equal to the raw margin within its standard error. The handball figure is about a third of the Bundesliga's 1.87.",
  paste(num(ha$v2[ha$cell %in% hb], 2), collapse = " and "), paste(num(ha$v2[ha$cell %in% bb], 2), collapse = " and ")
))
items <- c(items, sprintf(
  "<strong>Margin and total can be modelled separately</strong>: their residuals are uncorrelated in every cell. Given the strengths, a handball side's score has %s to %s of the variance a Poisson count would in the league divisions; a Gaussian with a free sd fits, for the women as for the men.",
  num(hb_disp_league[1], 2), num(hb_disp_league[2], 2)
))
if (have(ladder)) {
  l1 <- function(cl) {
    has_r1 <- !is.na(lad(cl, "r1_ragged", "leapfrog"))
    first <- if (has_r1) {
      sprintf("moving the states to real appearances (which also drops production's sum-to-zero constraint on each round's innovations and shifts the level prior) takes the sampled parameters from %s to %s and the leapfrog steps per iteration from %s to %s without moving a score; ",
              int(lad(cl, "r0_prod", "n_sampled")), int(lad(cl, "r1_ragged", "n_sampled")), int(lad(cl, "r0_prod", "leapfrog")), int(lad(cl, "r1_ragged", "leapfrog")))
    } else {
      ""
    }
    sprintf(
      "%s: %scollapsing the volatility hierarchies changes nothing (home advantage still %s); pooling the home effects and freeing their sign takes the home advantage to %s and adds %s to the leave-one-out density; dropping the Student-t costs %s; the identified Gaussian rung has %s parameters at %s leapfrog steps, against %s at %s in production",
      tolower(LAB[[cl]]), first, num(lad(cl, "r2_common", "ha_mean"), 2), num(lad(cl, "r3_pooled", "ha_mean"), 2),
      num(lad(cl, "r3_pooled", "elpd") - lad(cl, "r2_common", "elpd"), 0), num(lad(cl, "r4_ident", "elpd") - lad(cl, "r5_gauss", "elpd"), 1),
      int(lad(cl, "r5_gauss", "n_sampled")), int(lad(cl, "r5_gauss", "leapfrog")), int(lad(cl, "r0_prod", "n_sampled")), int(lad(cl, "r0_prod", "leapfrog"))
    )
  }
  ok <- vapply(c("hb_m", "bb_m"), function(cl) !is.na(lad(cl, "r5_gauss", "leapfrog")) && !is.na(lad(cl, "r0_prod", "leapfrog")), TRUE)
  if (any(ok)) items <- c(items, sprintf("<strong>The ladder attributes the gains.</strong> %s.", paste(vapply(c("hb_m", "bb_m")[ok], l1, ""), collapse = "; ")))
}
if (have(ladder) && !is.na(lad("bb_f", "r0_prod", "elpd"))) {
  bf0 <- ladder$scores[["bb_f r0_prod"]]
  bfs <- ladder$scores[["bb_f v2 static, INLA full"]]
  bfa <- ladder$scores[["bb_f v2 + within-season AR(1), INLA full"]]
  pw <- paired(bfs$ll_wdl, bf0$ll_wdl)
  pa <- paired(bfa$ll_wdl, bf0$ll_wdl)
  items <- c(items, sprintf(
    "<strong>Where v2 is not ahead: women's basketball at the fixed cutoff.</strong> On its %s held-out matches production has the better win/loss log likelihood (v2 static %s, with the within-season term %s) and the better leave-one-out density (%s against %s and %s). On the %s women's matches with stored forecasts the sign is the other way (%s). Basketball's production fits also beat static v2 on leave-one-out density for the men (%s against %s) while losing on the held-out matches. The within-season term and a heavier tail are what production has and static v2 lacks; both are tested above.",
    int(nrow(bf0)), pm(pw[["diff"]], pw[["se"]], 3), pm(pa[["diff"]], pa[["se"]], 3),
    num(lad("bb_f", "r0_prod", "elpd"), 0), num(lad("bb_f", "v2 static, INLA full", "elpd"), 0), num(lad("bb_f", "v2 + within-season AR(1), INLA full", "elpd"), 0),
    int(nrow(cmp$bb_f$cmp)), pm(paired(cmp$bb_f$cmp$ll_wdl_raw, cmp$bb_f$cmp$ll_wdl)[["diff"]], paired(cmp$bb_f$cmp$ll_wdl_raw, cmp$bb_f$cmp$ll_wdl)[["se"]], 3),
    num(lad("bb_m", "r0_prod", "elpd"), 0), num(lad("bb_m", "v2 static, INLA full", "elpd"), 0)
  ))
}
if (have(val) && length(val)) {
  items <- c(items, sprintf(
    "<strong>R-INLA reproduces NUTS.</strong> On one identical model, the largest difference in a state's posterior mean is %s posterior sds, the largest difference in a held-out match's expected margin or total is %s (%s for the plug-in fit the walk-forward uses), and the held-out log scores agree to %s (plug-in: %s).%s%s",
    num(max(vapply(val, function(o) o$state[["max_abs_mean_diff_in_sd"]], 1)), 2), num(max(vapply(val, function(o) o$pred[["max_abs_mean_diff"]], 1)), 2),
    num(max(vapply(val, function(o) o$pred[["max_abs_mean_diff_eb"]], 1)), 2),
    num(max(vapply(val, function(o) abs(o$log_score[["stan"]] - o$log_score[["inla_full_samples"]]), 1)), 4),
    num(max(vapply(val, function(o) abs(o$log_score[["stan"]] - o$log_score[["inla_eb"]]), 1)), 4),
    if (have(wfst)) {
      sprintf(
        " Every one of the %s R-INLA fits behind this report ended its mode search with status 0, but the mode is not unique: a second run of the walk-forward moved a match's expected margin by up to %s while moving no cell's mean score by more than %s.",
        int(sum(wfst$wf$fits) + nrow(wfst$other)), num(max(wfst$wf$max_abs_diff_margin[wfst$wf$run %in% c("hb_m_none", "hb_f_none", "bb_m_none_reg", "bb_f_none_reg")]), 1),
        num(max(abs(wfst$scores$diff)), 4)
      )
    } else {
      ""
    },
    if (have(repro)) {
      sprintf(
        " R-INLA is not bit-reproducible between runs: of %s fits (eight models, %s runs on the same data), %s landed on a slightly different optimum (handball men's home advantage %s against %s).",
        int(sum(repro$fits$n_runs)), int(length(repro$runs)), int(sum(repro$fits$n_runs) - sum(repro$fits$n_modal)),
        num(repro$hfa$hfa_other[repro$hfa$cell == "hb_m"], 3),
        num(repro$hfa$hfa_modal[repro$hfa$cell == "hb_m"], 3)
      )
    } else {
      ""
    }
  ))
}
phase_sd <- unlist(lapply(list(hbm, hbf, bbm, bbf), function(sc) {
  z <- (sc$margin_fit - sc$mean_m) / sc$s_m
  keep <- sc$phase != "play-off"
  tapply(z[keep], droplevels(sc$phase[keep]), stats::sd)
}))
items <- c(items, sprintf(
  "<strong>Walk-forward.</strong> %s matches scored over seasons 2023 to 2027 with weekly refits. The predictive spread is calibrated: the standardised margin residual has sd between %s and %s per cell, and between %s and %s in every regular-season phase of every cell. On the %s matches with a stored production forecast: handball margin %s, total %s, result %s; basketball margin %s, total %s, result %s (log score per match, v2 minus production, pooled over the sexes). Better on handball totals; handball margin and result within noise; basketball within noise. Margin and total were scored separately; joint (home, away) scores were not evaluated.",
  int(sum(vapply(list(hbm, hbf, bbm, bbf), nrow, 1L))),
  num(min(vapply(list(hbm, hbf, bbm, bbf), function(s) stats::sd((s$margin_fit - s$mean_m) / s$s_m), 1)), 2),
  num(max(vapply(list(hbm, hbf, bbm, bbf), function(s) stats::sd((s$margin_fit - s$mean_m) / s$s_m), 1)), 2),
  num(min(phase_sd), 2), num(max(phase_sd), 2),
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
items <- c(items, sprintf("<strong>Proposal.</strong> v2 on R-INLA for both sports (static for handball, %s for basketball), basketball on regulation time; swap the engine behind an adapter layer that keeps the published JSON contract (the rotation is written and tested; the pieces that feed the extract, the simulator and the beliefs are not, and basketball needs an overtime step before its score draws can be published); a four to five week shadow period per sport with a stated gate; store regulation-time scores. Seven decisions are yours.", if (bb_keep_ar1) "with the within-season term" else "static"))
add("<ol>", paste0("<li>", items, "</li>", collapse = "\n"), "</ol>")
