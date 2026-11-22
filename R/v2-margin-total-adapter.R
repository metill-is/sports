#' Express v2 (margin, total) quantities in the attack / defence terms the
#' downstream 2DT code reads.
#'
#' PROTOTYPE (2026-11-15, handball / basketball v2): not called by the
#' production extract or publish paths.
#'
#' The extract, season-simulation and publish code for handball and
#' basketball consumes the Stan model's variables: an attack and a defence
#' strength per team, a home effect for each, a scoring level, and a score
#' sd and correlation, with
#' `mu_home = level + off_home + ha_off - def_away` and
#' `mu_away = level + off_away - def_home - ha_def`.
#' Every mean that code computes depends on attack and defence only through
#' their sum and difference, so the v2 means map onto it exactly. The scales
#' do not map one to one: the basketball simulator (`R/simulate-2dt.R`) builds
#' a correlation per fixture from `alpha_rho`, `beta*_rho` and the strengths,
#' and handball reads an sd per team (`sigma_team`). The adapter returns one
#' sd and one correlation; a caller would set `alpha_rho = qlogis((1 + rho) / 2)`
#' with the betas at 0, and fill `sigma_team` with `sigma`. The mapping:
#'
#' * `off = (net + pace - mean(pace)) / 2`, `def = (net - pace + mean(pace)) / 2`,
#'   where `net` is the margin model's strength and `pace` the total model's
#'   (a team's division scoring level folded in);
#' * `level = mean(pace)`;
#' * the home advantage, which v2 estimates as one number for the margin, is
#'   split evenly between attack and defence, so it leaves the expected
#'   total alone;
#' * one score sd `sigma = sqrt((sigma_m^2 + sigma_t^2) / 4)` and correlation
#'   `rho = (sigma_t^2 - sigma_m^2) / (sigma_t^2 + sigma_m^2)` reproduce the
#'   margin sd and the total sd.
#'
#' With these, `mu_home - mu_away = hfa + net_home - net_away` and
#' `mu_home + mu_away = pace_home + pace_away`, which is the v2 model. Feeding
#' them to the existing bivariate draw with a very large `nu` gives the v2
#' Gaussian predictive, so the published JSON keeps its contract while the
#' engine underneath changes.
#'
#' @param net Net strength per team (margin model), a vector or a
#'   draws-by-teams matrix.
#' @param pace Contribution to the total per team (total model), same shape
#'   as `net`, with the team's division scoring level already added as
#'   `level_division / 2`.
#' @param ha Home advantage per team in margin units (league value plus team
#'   deviation), same shape as `net`.
#' @param sigma_m,sigma_t Margin and total sds, a scalar or one per draw.
#' @return List: `offense`, `defense`, `home_advantage_off`,
#'   `home_advantage_def` (shaped like `net`), `mean_goals`, `sigma`, `rho`
#'   (one per draw).
#' @noRd
v2_as_attack_defence <- function(net, pace, ha, sigma_m, sigma_t) {
  as_draws <- function(x) if (is.matrix(x)) x else matrix(x, nrow = 1L)
  net <- as_draws(net)
  pace <- as_draws(pace)
  ha <- as_draws(ha)
  stopifnot(identical(dim(net), dim(pace)), identical(dim(net), dim(ha)))
  level <- rowMeans(pace)
  centred <- pace - level
  list(
    offense = (net + centred) / 2,
    defense = (net - centred) / 2,
    home_advantage_off = ha / 2,
    home_advantage_def = ha / 2,
    mean_goals = level,
    sigma = sqrt((sigma_m^2 + sigma_t^2) / 4),
    rho = (sigma_t^2 - sigma_m^2) / (sigma_t^2 + sigma_m^2)
  )
}
