#' @include config.R
NULL

# ---- The per-sport publish registry -----------------------------------------
#
# One data structure answers every "does this sport do X?" question the publish
# layer used to answer with a parallel boolean or an `identical(key, "...")`
# branch. It is the SINGLE author of these facts: nothing downstream may
# re-derive them (SC-1 of docs/superpowers/plans/2026-09-04-plan-b-publish-layer.md).
#
# Fields, and who reads them:
#   required_extracts       reader  -- files that must exist for a fit_date
#                                   partition to count as complete
#   optional_extracts       reader  -- files that degrade to a 0-row tibble
#   empty_extracts          reader  -- the 0-row tibble per file type
#   predicted_matches_shape next_games -- "scoreline_counts" | "match_summary"
#   value_link              (nothing yet) -- the link function the extractor
#                                   already applied to each stored component
#   surfaces                publisher -- publishable surfaces (JSON basenames
#                                   plus the football-only payload features)
#   has_ties / tie_threshold standings -- mirrors config/leagues.yml
#                                   `betting.scoring`
#   points                  meta    -- the league points scheme
#   units                   meta + extractor -- the scale of the published
#                                   strength / home-advantage bands, and the
#                                   goal-difference bin width the extractor bins with
#   season_scope            meta    -- what the published table covers
#   postseason              meta    -- the unmodelled post-season, or NULL
#   placement_basis         final_positions -- what a placement means
#
# `value_link`, `units$strength` and `units$home_advantage` are not read by any
# caller yet; they exist so WS8 and WS10 consume this one registry rather than
# re-deriving the scales from the extractor source. `units$diff_bin_width` IS
# read -- by the two 2DT extractor entry points.

# Files every sport degrades gracefully on. `fit_meta` is written by all three
# extractors now, but it stays OPTIONAL: `required_extracts` drives the reader's
# completeness check, and requiring it would mark every football partition
# written before this contract existed incomplete -- i.e. it would retire the
# whole replay history.
.PUBLISH_OPTIONAL_ALWAYS <- "fit_meta"

# The ten JSON basenames every Icelandic league cell publishes.
.PUBLISH_COMMON_SURFACES <- c(
  "meta", "next_games",
  "standings", "standings_history",
  "team_strengths", "team_strengths_history",
  "final_positions", "final_positions_history",
  "points_distribution", "home_advantage"
)

# 0-row tibbles, one per extract file type. Shapes are transcribed from the
# producers, not invented:
#   football  -- R/extract-football-iceland.R (the `empty_tibbles` literal the
#                football-only reader used to carry inline)
#   2DT       -- R/extract-iceland-2dt-shared.R's five writers, cross-checked
#                against tests/testthat/fixtures/extracts/
#   fit_meta  -- the partition-level contract in the Plan B design (one row:
#                n_draws, fit_date, stan_model, model_units)
.publish_empty_extracts <- function(shape) {
  shared <- list(
    team_strengths_quantiles = tibble::tibble(
      team = character(), component = character(), location = character(),
      quantile = integer(), value = numeric()
    ),
    round_strengths_quantiles = tibble::tibble(
      round = integer(), team = character(),
      component = character(), location = character(),
      quantile = integer(), value = numeric()
    ),
    home_advantage_quantiles = tibble::tibble(
      team = character(), component = character(),
      quantile = integer(), value = numeric()
    ),
    final_positions = tibble::tibble(
      team = character(), placement = integer(), probability = numeric()
    ),
    points_distribution = tibble::tibble(
      team = character(), points = integer(), probability = numeric()
    ),
    fit_meta = tibble::tibble(
      n_draws = integer(), fit_date = as.Date(character()),
      stan_model = character(), model_units = character()
    )
  )
  if (identical(shape, "scoreline_counts")) {
    c(
      list(
        predicted_matches = tibble::tibble(
          home_team = character(), away_team = character(),
          match_date = as.Date(character()),
          home_goals = integer(), away_goals = integer(),
          count = integer()
        ),
        tournament_placements = tibble::tibble(
          team = character(), round_name = character(),
          probability = numeric()
        )
      ),
      shared
    )
  } else {
    c(
      list(
        predicted_matches = tibble::tibble(
          game_nr = integer(), match_date = as.Date(character()),
          home_team = character(), away_team = character(),
          mean_home_goals = numeric(), mean_away_goals = numeric(),
          mean_goal_diff = numeric(),
          p_home_win = numeric(), p_draw = numeric(), p_away_win = numeric(),
          goal_diff_distribution = list()
        )
      ),
      shared
    )
  }
}

.publish_profiles <- function() {
  football_required <- c(
    "predicted_matches",
    "team_strengths_quantiles",
    "round_strengths_quantiles",
    "home_advantage_quantiles",
    "final_positions",
    "points_distribution"
  )
  # The 2DT extractor now writes the same six division-keyed parquets football
  # does, `round_strengths_quantiles` included: all three Stan models declare
  # the identical `array[N_rounds] vector[K] offense` / `defense` surface. It is
  # REQUIRED rather than optional because data/beliefs/extracts/ holds no
  # basketball or handball partition at all yet -- there is no pre-contract
  # history for it to mark incomplete, unlike football's.
  twodt_required <- football_required

  twodt <- function(units, has_ties, tie_threshold) {
    list(
      required_extracts = twodt_required,
      optional_extracts = .PUBLISH_OPTIONAL_ALWAYS,
      empty_extracts = .publish_empty_extracts("match_summary"),
      predicted_matches_shape = "match_summary",
      # The 2DT models are additive in raw points/goals -- there is no link to
      # undo. Applying football's exp() here is exactly the B5 bug
      # (R/extract-iceland-2dt-shared.R:188-207).
      value_link = c(
        team_strength = "identity",
        round_strength = "identity",
        home_advantage = "identity",
        home_advantage_total = "identity"
      ),
      surfaces = .PUBLISH_COMMON_SURFACES,
      has_ties = has_ties,
      tie_threshold = tie_threshold,
      points = list(
        win = 2L,
        # NULL, not 0L: `meta.points.draw` must serialise as JSON null so no
        # consumer infers a draw is possible.
        draw = if (has_ties) 1L else NULL,
        loss = 0L
      ),
      units = units,
      season_scope = "regular_season",
      postseason = list(name_is = "\u00darslitakeppni", modelled = FALSE),
      placement_basis = "regular_season_table"
    )
  }

  list(
    football = list(
      required_extracts = football_required,
      optional_extracts = c("tournament_placements", .PUBLISH_OPTIONAL_ALWAYS),
      empty_extracts = .publish_empty_extracts("scoreline_counts"),
      predicted_matches_shape = "scoreline_counts",
      # Strength bands are stored RAW (R/publish-iceland-league.R:47-61 applies
      # no transform); home advantage is stored already exponentiated, with the
      # `total` component halved for the per-side split
      # (R/extract-football-iceland.R:1523-1547).
      value_link = c(
        team_strength = "identity",
        round_strength = "identity",
        home_advantage = "exp",
        home_advantage_total = "exp_half"
      ),
      surfaces = c(
        .PUBLISH_COMMON_SURFACES,
        "tournament_placements",
        "round_predictions_history",
        "xg",
        "cup_bracket",
        "split",
        "preseason_strengths"
      ),
      has_ties = TRUE,
      tie_threshold = 0,
      points = list(win = 3L, draw = 1L, loss = 0L),
      units = list(
        # offence/defence/total enter the bivariate-Poisson mean additively
        # inside exp() (R/publish-iceland-league.R:1028), and the extractor
        # stores the parameter untransformed.
        strength = "log_goals",
        home_advantage = "goal_multiplier",
        diff_bin_width = 1L
      ),
      season_scope = "full_season",
      postseason = NULL,
      placement_basis = "final_table"
    ),
    basketball = twodt(
      units = list(
        strength = "points", home_advantage = "points", diff_bin_width = 5L
      ),
      has_ties = FALSE,
      tie_threshold = 0
    ),
    handball = twodt(
      units = list(
        strength = "goals", home_advantage = "goals", diff_bin_width = 2L
      ),
      has_ties = TRUE,
      tie_threshold = 0.5
    )
  )
}

# The one-row, partition-level provenance table every extractor writes.
#
# `model_units` names the scale the stored strength / home-advantage components
# are on, and it comes from the SPORT rather than from config: the 2DT models are
# additive in raw points/goals (Stan/basketball_iceland/
# 2d_student_t_scalarsigma.stan:112,116) while football's bivariate Poisson
# parameterises on the log scale (Stan/football_iceland/
# bivariate_poisson_no_inflation.stan:155,185). Getting that wrong is the B5 bug
# wearing a metadata label.
#
# No `division` column, deliberately -- see the write sites.
# @noRd
.fit_meta_tibble <- function(fit, fit_date, stan_model, sport) {
  tibble::tibble(
    n_draws = as.integer(posterior::ndraws(fit$draws("lp__"))),
    fit_date = as.Date(fit_date),
    stan_model = as.character(stan_model),
    model_units = switch(
      sport,
      basketball = "points",
      handball = "goals",
      football = "log_rate",
      cli::cli_abort("No model_units for sport {.val {sport}}.", call = NULL)
    )
  )
}

#' Per-sport publish profile
#'
#' The single registry of everything the publish layer needs to know about a
#' sport: which extract parquets it produces, what shape its
#' `predicted_matches` table has, which surfaces it publishes, its points
#' scheme, the units its strength bands are on, and what a "final position"
#' means for it.
#'
#' `has_ties` / `tie_threshold` mirror `config/leagues.yml::<key>.betting.scoring`
#' and are asserted against it in `tests/testthat/test-publish-profile.R`.
#'
#' `value_link`, `units$strength` and `units$home_advantage` have no reader yet.
#' They are declared here so the workstreams that need them consume this one
#' registry instead of re-deriving the scales from extractor source.
#'
#' @param sport `"football"`, `"basketball"` or `"handball"` (the `sport` field
#'   of a `config/leagues.yml` league, not the league key).
#' @return A named list; see the field table at the top of `R/publish-profile.R`.
#' @export
sport_publish_profile <- function(sport) {
  profiles <- .publish_profiles()
  if (!is.character(sport) || length(sport) != 1L || is.na(sport) ||
    !sport %in% names(profiles)) {
    cli::cli_abort(
      c(
        "No publish profile for sport {.val {sport}}.",
        "i" = "Known sports: {.val {names(profiles)}}."
      ),
      call = NULL
    )
  }
  profiles[[sport]]
}
