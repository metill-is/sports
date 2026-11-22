# Held-out scoring shared by every rung. A rung supplies, per posterior draw and per
# held-out match, the location and scale of the margin and of the total (and the
# Student-t degrees of freedom); everything else is the same code for all rungs.

pick_cols <- function(dr, stem, n) {
  m <- unclass(dr[, sprintf("%s[%d]", stem, seq_len(n)), drop = FALSE])
  matrix(as.numeric(m), nrow(dr), n)
}
pick_scalar <- function(dr, nm) as.numeric(dr[, nm])

# Production (r0): strengths frozen at each team's last match, as the production
# generated quantities do.
pred_params_prod <- function(dr, d, sport) {
  k <- d$prod$K
  h <- d$hold
  ns <- d$prod$N_seasons
  att <- pick_cols(dr, "cur_offense_away", k)
  def <- pick_cols(dr, "cur_defense_away", k)
  hao <- pick_cols(dr, "home_advantage_off", k)
  had <- pick_cols(dr, "home_advantage_def", k)
  level <- pick_scalar(dr, sprintf("mean_goals[%d]", ns))
  off1 <- att[, h$home_nr, drop = FALSE] + hao[, h$home_nr, drop = FALSE]
  def1 <- def[, h$home_nr, drop = FALSE] + had[, h$home_nr, drop = FALSE]
  off2 <- att[, h$away_nr, drop = FALSE]
  def2 <- def[, h$away_nr, drop = FALSE]
  mu1 <- level + off1 - def2
  mu2 <- level + off2 - def1
  if (sport == "handball") {
    st <- pick_cols(dr, "sigma_team", k)
    s1 <- st[, h$home_nr, drop = FALSE]
    s2 <- st[, h$away_nr, drop = FALSE]
    rho <- pick_scalar(dr, "rho")
    v <- s1^2 + s2^2
    cv <- 2 * rho * s1 * s2
  } else {
    sigma <- pick_scalar(dr, "sigma")
    sdiff <- abs(off1 + def1 - off2 - def2)
    stot <- abs(off1 + def1 + off2 + def2)
    rho <- 2 * plogis(pick_scalar(dr, "alpha_rho") + pick_scalar(dr, "beta_rho") * sdiff +
      pick_scalar(dr, "beta2_rho") * stot + pick_scalar(dr, "beta3_rho") * stot * sdiff) - 1
    v <- matrix(2 * sigma^2, nrow(dr), nrow(h))
    cv <- 2 * rho * sigma^2
  }
  list(loc_m = mu1 - mu2, sc_m = sqrt(v - cv), loc_t = mu1 + mu2, sc_t = sqrt(v + cv),
       nu = pick_scalar(dr, "nu"))
}

# Rungs r1 to r5 write the four predictive parameters in generated quantities.
pred_params_lean <- function(dr, d) {
  n <- nrow(d$hold)
  nu <- pick_scalar(dr, "nu_out")
  list(loc_m = pick_cols(dr, "loc_m", n), sc_m = pick_cols(dr, "sc_m", n),
       loc_t = pick_cols(dr, "loc_t", n), sc_t = pick_cols(dr, "sc_t", n), nu = nu)
}

# v2: margin and total are separate fits; each supplies mu_pred and its sigma.
pred_params_v2 <- function(dr_m, dr_t, d) {
  n <- nrow(d$hold)
  list(loc_m = pick_cols(dr_m, "mu_pred", n), sc_m = pick_scalar(dr_m, "sigma"),
       loc_t = pick_cols(dr_t, "mu_pred", n), sc_t = pick_scalar(dr_t, "sigma"), nu = Inf)
}

# Per-match scores. Margin and total log scores are on the integer scale (rounded
# predictive). W/D/L: handball draws are level scores (the rounded mass on zero, which is
# production's +/-0.5 band); basketball final scores cannot be level, so for basketball
# the margin pmf is the rounded predictive with its mass on zero removed and the rest
# renormalised, for every rung alike.
score_holdout <- function(pp, hold, sport, zero_shift = 0) {
  m <- hold$home_score - hold$away_score
  tt <- hold$home_score + hold$away_score
  ties <- sport == "handball"
  o <- v2_outcome_probs(pp$loc_m, pp$sc_m, pp$nu, ties = ties, zero_shift = if (ties) zero_shift else 0)
  p_obs <- ifelse(m > 0, o$p_home, ifelse(m < 0, o$p_away, o$p_draw))
  mean_of <- function(x) if (is.matrix(x)) colMeans(x) else x
  data.frame(
    match_date = hold$match_date, division = hold$division, home_team = hold$home_team,
    away_team = hold$away_team, margin = m, total = tt,
    ls_margin = log(v2_integer_pmf(m, pp$loc_m, pp$sc_m, pp$nu, zero_shift = if (ties) zero_shift else 0, ties = ties)),
    ls_total = log(v2_integer_pmf(tt, pp$loc_t, pp$sc_t, pp$nu)),
    ll_wdl = log(p_obs), p_home = o$p_home, p_draw = o$p_draw, p_away = o$p_away,
    exp_margin = mean_of(pp$loc_m), exp_total = mean_of(pp$loc_t)
  )
}

# Mean and standard error of a paired difference, one row per match, the matches treated
# as independent. WHY this matters: matches share team-season strengths, the division's
# season level and a season's tie rate, so their differences can be correlated and the
# true standard error larger (whole-branch review, 22 November 2026; not measured).
paired <- function(a, b) {
  dlt <- a - b
  c(diff = mean(dlt), se = stats::sd(dlt) / sqrt(length(dlt)))
}
