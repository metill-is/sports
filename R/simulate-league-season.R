#' @include extract-football-iceland.R
NULL

# League season simulator
#
# Forward-simulates ALL remaining league fixtures from posterior draws, per
# draw, using the same frozen-strength bivariate-Poisson match model the Stan
# generated-quantities block uses for predictions (lines 297-307 of
# Stan/football_iceland/bivariate_poisson_no_inflation.stan) and the cup
# bracket simulator (`.simulate_cup_match_pfi`). Unlike the cup simulator,
# league matches keep their drawn scoreline (3/1/0 points + goal difference)
# rather than rejection-sampling a winner.
#
# This replaces the previous final-positions logic, which only integrated over
# the matches in the model's 14-day prediction window (~2 rounds) and therefore
# reported "position after the next ~2 rounds" mislabelled as the final table.
#
# Strength is held at the latest-round posterior (`cur_offense`/`cur_defense`)
# for every remaining match — identical to how the model predicts next_games,
# match scores, and the cup bracket. No random-walk drift is projected forward;
# that is a deliberate consistency choice (the model itself freezes strength at
# the training cutoff for all predictions).
#
# The match model, points rule and tie-break are injected (`match_fn`,
# `points_fn`, `tie_break`); football's are the defaults, and the 2DT sports
# supply their own (R/simulate-2dt.R).

# ---- Match models -------------------------------------------------------------
#
# A match model plays one fixture across every posterior draw at once. It is a
# function `(home, away, scalars)` returning `list(home, away)` score vectors.
# `home` and `away` are named lists of draw-aligned vectors: `off`, `def` (the
# home side's already carry its home advantage, on both) and every extra team
# column the model declares. `scalars` is the scalar input sorted by `.draw`.
#
# A model declares the columns it reads, so the simulator can refuse bad inputs
# before any work and name what is missing (spec 2026-09-16 §3).

.new_match_fn <- function(fn, scalar_cols, team_cols = character()) {
  stopifnot(
    is.function(fn), is.character(scalar_cols), is.character(team_cols)
  )
  structure(fn, scalar_cols = scalar_cols, team_cols = team_cols)
}

# Football: frozen-strength bivariate Poisson (`.simulate_match_goals_slss`).
.match_fn_football <- .new_match_fn(
  function(home, away, scalars) {
    .simulate_match_goals_slss(
      off_h = home$off, def_h = home$def,
      off_a = away$off, def_a = away$def,
      mlg = scalars$mean_log_goals,
      amu3 = scalars$alpha_mu3,
      bmu3 = scalars$beta_mu3_strength_diff
    )
  },
  scalar_cols = c("mean_log_goals", "alpha_mu3", "beta_mu3_strength_diff")
)

# Football: 3 / 1 / 0 on exact equality.
.points_fn_football <- function(g_h, g_a) {
  list(
    home = ifelse(g_h > g_a, 3L, ifelse(g_h == g_a, 1L, 0L)),
    away = ifelse(g_a > g_h, 3L, ifelse(g_h == g_a, 1L, 0L))
  )
}

.require_cols_slss <- function(df, cols, what) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0L) {
    stop(
      "simulate_league_season: ", what,
      " lacks column(s) the match model needs: ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(df)
}

.check_points_slss <- function(p) {
  if (!is.integer(p$home) || !is.integer(p$away)) {
    stop(
      "simulate_league_season: `points_fn` must return integer points ",
      "(list(home, away)); points_distribution tabulates exact totals.",
      call. = FALSE
    )
  }
  p
}

#' Simulate a league's remaining season across posterior draws
#'
#' For each posterior draw of team-strength parameters, plays out every
#' remaining fixture with a bivariate-Poisson scoreline, adds the realised
#' (already-played) standings, ranks the final table by points -> goal
#' difference -> goals for, and aggregates to per-(team, placement) and
#' per-(team, points) probabilities.
#'
#' @section Split-season leagues (efri/neðri hluti):
#' When `split_format` (or `split_groups`) is supplied the season has a
#' Besta-deild-style split: after the regular phase the table divides into an
#' upper and a lower group which each play a further single round-robin with
#' FULL carry-over of points/GD/GF, and the final table is group-locked (every
#' upper-group team ranks above every lower-group team regardless of points).
#' `placement = 1` is then the champion of the whole season, and relegation
#' places are the bottom of the lower block. Format facts verified 2026-07-10
#' against KSI tables + ingested `BD_UPPER_PO`/`BD_LOWER_PO` results — see
#' docs/superpowers/specs/2026-07-10-split-season-simulator-design.md.
#'
#' Two phases are supported:
#' - Regular phase ongoing (`split_groups = NULL`): `remaining_fixtures` are
#'   the remaining REGULAR fixtures. Per draw, the simulated regular table is
#'   split `split_format$upper` / `split_format$lower`, the split fixtures are
#'   generated from the deterministic KSI home-allocation template
#'   (`.split_fixture_template`) applied to the draw's split ranks, and played
#'   out with the same frozen-strength match model.
#' - Split phase underway (`split_groups` supplied): membership is known;
#'   the caller passes played split matches inside `base_standings` (full
#'   carry-over makes that a plain sum) and only the remaining SPLIT fixtures
#'   in `remaining_fixtures`. No fixtures are generated.
#'
#' @param sim_inputs_team Tibble with columns `team`, `.draw`, `cur_offense`,
#'   `cur_defense`, `home_advantage_off`, `home_advantage_def` (latest-round
#'   strengths, on the match model's own scale), plus any team columns
#'   `match_fn` declares. Football's comes from `.extract_sim_inputs_pfi()`.
#' @param sim_inputs_scalar Tibble with `.draw` plus the scalar columns
#'   `match_fn` declares (football: `mean_log_goals`, `alpha_mu3`,
#'   `beta_mu3_strength_diff`).
#' @param remaining_fixtures Tibble with columns `home_team`, `away_team` — the
#'   unplayed fixtures of the season for this division. May be empty (season
#'   over), in which case placements are the deterministic ranking of the
#'   realised table.
#' @param base_standings Tibble with one row per league team: `team`,
#'   `base_points`, `base_gd`, `base_gf` — the realised points / goal
#'   difference / goals for from already-played matches (0 for an unplayed
#'   season). Defines the team set whose final positions are computed.
#' @param seed Optional integer. When set, `set.seed()` is called once at the
#'   entry point so the simulation is reproducible.
#' @param split_format Optional `list(upper = <int>, lower = <int>)` declaring
#'   a split-season format (see the split-season section). `upper + lower`
#'   must equal the number of league teams. `NULL` (default) = flat league.
#' @param split_groups Optional tibble(`team`, `group`) with
#'   `group %in% c("upper", "lower")`, covering every league team — supply
#'   once the split membership is decided (regular phase complete). Implies a
#'   split-season format even when `split_format` is `NULL`.
#' @param match_fn Match model built with `.new_match_fn()`: `(home, away,
#'   scalars)` -> `list(home, away)` score vectors, one element per draw.
#'   Default: football's bivariate Poisson.
#' @param points_fn `(g_h, g_a)` -> `list(home, away)` of INTEGER points.
#'   Default: 3/1/0 on exact equality.
#' @param tie_break `"first"` (default) gives exact points/GD/GF ties to the
#'   earlier `base_standings` team, as football always has; `"jitter"` breaks
#'   them with a seeded per-(draw, team) uniform, as the 2DT sports need.
#'
#' @return A list with:
#'   - `final_positions`: tibble(`team`, `placement`, `probability`) — one row
#'     per (team, placement in 1..n_teams), `probability` = share of draws.
#'   - `points_distribution`: tibble(`team`, `points`, `probability`) — one row
#'     per realised (team, total season points) with `probability` = share of
#'     draws.
#'
#' @export
simulate_league_season <- function(sim_inputs_team,
                                   sim_inputs_scalar,
                                   remaining_fixtures,
                                   base_standings,
                                   seed = NULL,
                                   split_format = NULL,
                                   split_groups = NULL,
                                   match_fn = .match_fn_football,
                                   points_fn = .points_fn_football,
                                   tie_break = c("first", "jitter")) {
  tie_break <- match.arg(tie_break)
  stopifnot(
    is.data.frame(sim_inputs_team),
    is.data.frame(sim_inputs_scalar),
    is.data.frame(remaining_fixtures),
    is.data.frame(base_standings),
    is.function(points_fn),
    all(c("team", "base_points", "base_gd", "base_gf") %in% names(base_standings))
  )
  if (!is.function(match_fn) || is.null(attr(match_fn, "scalar_cols"))) {
    stop(
      "simulate_league_season: `match_fn` must be built with .new_match_fn(), ",
      "which records the columns it reads.",
      call. = FALSE
    )
  }
  extra_cols <- attr(match_fn, "team_cols")
  team_cols <- c(
    "cur_offense", "cur_defense", "home_advantage_off", "home_advantage_def",
    extra_cols
  )
  .require_cols_slss(
    sim_inputs_team, c("team", ".draw", team_cols), "sim_inputs_team"
  )
  .require_cols_slss(
    sim_inputs_scalar, c(".draw", attr(match_fn, "scalar_cols")),
    "sim_inputs_scalar"
  )

  if (!is.null(seed)) {
    set.seed(seed)
  }

  teams <- as.character(base_standings$team)
  n_teams <- length(teams)

  has_split <- !is.null(split_format) || !is.null(split_groups)
  if (!is.null(split_groups)) {
    stopifnot(
      is.data.frame(split_groups),
      all(c("team", "group") %in% names(split_groups)),
      all(split_groups$group %in% c("upper", "lower"))
    )
    if (!setequal(split_groups$team, teams) ||
      nrow(split_groups) != n_teams) {
      stop(
        "simulate_league_season: split_groups must cover every league team ",
        "exactly once.",
        call. = FALSE
      )
    }
  } else if (!is.null(split_format)) {
    stopifnot(
      is.list(split_format),
      all(c("upper", "lower") %in% names(split_format))
    )
    upper_size <- as.integer(split_format$upper)
    lower_size <- as.integer(split_format$lower)
    if (upper_size + lower_size != n_teams) {
      stop(
        "simulate_league_season: split_format upper + lower (",
        upper_size, " + ", lower_size,
        ") must equal the number of league teams (", n_teams, ").",
        call. = FALSE
      )
    }
    # Fail on unverified group sizes before any simulation work.
    .split_fixture_template(upper_size)
    .split_fixture_template(lower_size)
  }
  if (n_teams == 0L) {
    return(list(
      final_positions = tibble::tibble(
        team = character(), placement = integer(), probability = numeric()
      ),
      points_distribution = tibble::tibble(
        team = character(), points = integer(), probability = numeric()
      )
    ))
  }

  # Align scalar draws into a fixed row order; every per-team matrix below is
  # built against this same draw ordering so a fixture's home/away vectors and
  # the scalars index the same posterior sample row-for-row.
  scalar <- sim_inputs_scalar[order(sim_inputs_scalar$.draw), , drop = FALSE]
  draw_order <- scalar$.draw
  nd <- length(draw_order)
  draws <- seq_len(nd)

  st <- sim_inputs_team[sim_inputs_team$team %in% teams, , drop = FALSE]
  missing_teams <- setdiff(teams, unique(st$team))
  if (length(missing_teams) > 0L) {
    stop(
      "simulate_league_season: no strength draws for league team(s): ",
      paste(missing_teams, collapse = ", "),
      ". Every league team must be in the fit registry.",
      call. = FALSE
    )
  }

  # Build a (draw x team) matrix for one strength column, rows aligned to
  # `draw_order`, columns to `teams`.
  to_matrix <- function(col) {
    w <- st |>
      dplyr::select(".draw", "team", value = dplyr::all_of(col)) |>
      tidyr::pivot_wider(names_from = "team", values_from = "value")
    w <- w[match(draw_order, w$.draw), , drop = FALSE]
    as.matrix(w[, teams, drop = FALSE])
  }
  M <- lapply(stats::setNames(team_cols, team_cols), to_matrix)

  # One side of a fixture, draw-aligned. `idx` is a (draw, team column) index
  # matrix. The home side carries its home advantage on BOTH offence and
  # defence, as every model here does; declared extra columns (handball's
  # sigma_team) travel unchanged.
  side <- function(idx, home) {
    off <- M$cur_offense[idx]
    def <- M$cur_defense[idx]
    if (home) {
      off <- off + M$home_advantage_off[idx]
      def <- def + M$home_advantage_def[idx]
    }
    c(list(off = off, def = def), lapply(M[extra_cols], function(m) m[idx]))
  }

  pts <- matrix(0L, nd, n_teams, dimnames = list(NULL, teams))
  # Double, not integer: 2DT scores are continuous and would be truncated.
  gd <- matrix(0, nd, n_teams, dimnames = list(NULL, teams))
  gf <- matrix(0, nd, n_teams, dimnames = list(NULL, teams))

  fx <- remaining_fixtures[
    remaining_fixtures$home_team %in% teams &
      remaining_fixtures$away_team %in% teams, ,
    drop = FALSE
  ]
  for (i in seq_len(nrow(fx))) {
    idx_h <- cbind(draws, match(as.character(fx$home_team[i]), teams))
    idx_a <- cbind(draws, match(as.character(fx$away_team[i]), teams))

    goals <- match_fn(side(idx_h, home = TRUE), side(idx_a, home = FALSE), scalar)
    p <- .check_points_slss(points_fn(goals$home, goals$away))

    pts[idx_h] <- pts[idx_h] + p$home
    pts[idx_a] <- pts[idx_a] + p$away
    gf[idx_h] <- gf[idx_h] + goals$home
    gf[idx_a] <- gf[idx_a] + goals$away
    gd[idx_h] <- gd[idx_h] + (goals$home - goals$away)
    gd[idx_a] <- gd[idx_a] + (goals$away - goals$home)
  }

  # Add the realised (already-played) table.
  bs <- base_standings[match(teams, base_standings$team), , drop = FALSE]
  pts <- sweep(pts, 2L, as.integer(bs$base_points), `+`)
  gd <- sweep(gd, 2L, as.numeric(bs$base_gd), `+`)
  gf <- sweep(gf, 2L, as.numeric(bs$base_gf), `+`)

  # Rank each draw's table by points -> goal difference -> goals for (desc);
  # see `.rank_table_slss()`.
  if (has_split) {
    if (is.null(split_groups)) {
      # Phase 1: split membership decided by each draw's simulated regular
      # table, then the split fixtures are generated from the KSI template
      # (keyed by split rank) and played with the same match model.
      split_placement <- .rank_table_slss(pts, gd, gf, teams, tie_break = tie_break)
      # Inverse permutation: ord[draw, p] = column index of the p-th team.
      ord <- matrix(0L, nd, n_teams)
      ord[cbind(rep(seq_len(nd), n_teams), as.vector(split_placement))] <-
        rep(seq_len(n_teams), each = nd)

      for (g in list(
        list(offset = 0L, size = upper_size),
        list(offset = upper_size, size = lower_size)
      )) {
        tpl <- .split_fixture_template(g$size)
        for (k in seq_len(nrow(tpl))) {
          idx_h <- cbind(draws, ord[, g$offset + tpl$home_rank[k]])
          idx_a <- cbind(draws, ord[, g$offset + tpl$away_rank[k]])

          goals <- match_fn(
            side(idx_h, home = TRUE), side(idx_a, home = FALSE), scalar
          )
          p <- .check_points_slss(points_fn(goals$home, goals$away))

          pts[idx_h] <- pts[idx_h] + p$home
          pts[idx_a] <- pts[idx_a] + p$away
          gf[idx_h] <- gf[idx_h] + goals$home
          gf[idx_a] <- gf[idx_a] + goals$away
          gd[idx_h] <- gd[idx_h] + (goals$home - goals$away)
          gd[idx_a] <- gd[idx_a] + (goals$away - goals$home)
        }
      }
      in_upper <- split_placement <= upper_size
    } else {
      upper_teams <- split_groups$team[split_groups$group == "upper"]
      in_upper <- matrix(
        rep(teams %in% upper_teams, each = nd),
        nrow = nd, ncol = n_teams
      )
    }
    # Group-locked final table: every upper-group team ranks above every
    # lower-group team regardless of carried points (2024: nedri winner on 37
    # pts still 7th behind efri's last on 34).
    group <- in_upper
  } else {
    group <- NULL
  }
  placement <- .rank_table_slss(
    pts, gd, gf, teams,
    group = group, tie_break = tie_break
  )

  final_positions <- lapply(seq_len(n_teams), function(j) {
    counts <- tabulate(placement[, j], nbins = n_teams)
    tibble::tibble(
      team = teams[j],
      placement = seq_len(n_teams),
      probability = counts / nd
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$team, .data$placement)

  points_distribution <- lapply(seq_len(n_teams), function(j) {
    tbl <- table(pts[, j])
    tibble::tibble(
      team = teams[j],
      points = as.integer(names(tbl)),
      probability = as.numeric(tbl) / nd
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$team, .data$points)

  list(
    final_positions = final_positions,
    points_distribution = points_distribution
  )
}

#' Simulate one fixture's scoreline across posterior draws
#'
#' Frozen-strength bivariate-Poisson rates (log scale), identical to the Stan
#' GQ prediction loop (Stan/football_iceland/
#' bivariate_poisson_no_inflation.stan lines 297-307) and
#' `.simulate_cup_match_pfi`. Trivariate reduction: the shared component `x3`
#' is drawn once per (fixture, draw). All strength arguments are draw-aligned
#' vectors; the home side's must already include its home advantage.
#'
#' @return list(`home`, `away`) integer goal vectors, one element per draw.
#' @noRd
.simulate_match_goals_slss <- function(off_h, def_h, off_a, def_a,
                                       mlg, amu3, bmu3) {
  nd <- length(off_h)
  mu_h <- mlg + off_h - def_a
  mu_a <- mlg + off_a - def_h
  strength_diff <- abs(off_h + def_h - off_a - def_a)
  mu3 <- stats::plogis(amu3 + bmu3 * strength_diff, log.p = TRUE) +
    0.5 * (mu_h + mu_a)

  x3 <- stats::rpois(nd, exp(mu3))
  list(
    home = stats::rpois(nd, exp(mu_h)) + x3,
    away = stats::rpois(nd, exp(mu_a)) + x3
  )
}

#' Rank each draw's table (1 = best)
#'
#' Lexicographic: split group (upper first), then points, goal difference and
#' goals for, all descending. This replaces a packed numeric key
#' (`pts * 1e6 + gd * 1e3 + gf`) that was only order-preserving for football's
#' small integer scores: a basketball season's goals-for (~2000) swamps goal
#' difference (spec 2026-09-16 F5). For every football table the two orders are
#' identical.
#'
#' Residual exact ties: `"first"` gives them to the earlier column (the
#' `base_standings` team order), as football always has. `"jitter"` breaks them
#' with a per-(draw, team) uniform under a fixed seed, with the caller's RNG
#' state preserved -- a team must not win every tie in every draw, or one
#' systematic bias is swapped for another (see `.compute_final_positions_2dt`).
#'
#' @param pts,gd,gf Numeric matrices (draws x teams).
#' @param teams Character vector naming the columns.
#' @param group Optional logical matrix; `TRUE` ranks above `FALSE`.
#' @param tie_break `"first"` or `"jitter"`.
#' @return Integer matrix (draws x teams) of placements.
#' @noRd
.rank_table_slss <- function(pts, gd, gf, teams, group = NULL,
                             tie_break = c("first", "jitter")) {
  tie_break <- match.arg(tie_break)
  nd <- nrow(pts)
  nt <- ncol(pts)
  # Column-major positions of every cell.
  draw <- rep(seq_len(nd), times = nt)
  col <- rep(seq_len(nt), each = nd)
  last <- if (identical(tie_break, "jitter")) {
    withr::with_preserve_seed({
      set.seed(20260905L)
      stats::runif(nd * nt)
    })
  } else {
    col
  }
  grp <- if (is.null(group)) numeric(nd * nt) else as.numeric(group)
  o <- order(
    draw, -grp, -as.vector(pts), -as.vector(gd), -as.vector(gf), last
  )
  placement <- matrix(0L, nd, nt, dimnames = list(NULL, teams))
  # `o` walks draw 1's teams best-first, then draw 2's, ...
  placement[cbind(draw[o], col[o])] <- rep(seq_len(nt), times = nd)
  placement
}

#' Deterministic KSI split-phase fixture template
#'
#' Single round-robin within a split group, with home advantage allocated by
#' the group teams' regular-season finishing rank (1 = best in group). The
#' template is KSI's own: it was unanimous across every observed
#' (season x sex x group) in `data/facts/results` — men's 6-team groups
#' 2022–2025, women's 6-team upper groups 2023–2024, and women's 4-team lower
#' groups 2023–2024. Home-game counts come out {3,3,3,2,2,2} (n = 6) and
#' {2,2,1,1} (n = 4).
#'
#' @param n_group Group size. Only the empirically verified sizes (4, 6) are
#'   supported; anything else errors rather than inventing an allocation.
#' @return tibble(`home_rank`, `away_rank`), one row per fixture.
#' @noRd
.split_fixture_template <- function(n_group) {
  n_group <- as.integer(n_group)
  if (identical(n_group, 6L)) {
    return(tibble::tibble(
      home_rank = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L),
      away_rank = c(2L, 5L, 6L, 3L, 4L, 5L, 1L, 4L, 5L, 1L, 6L, 4L, 6L, 2L, 3L)
    ))
  }
  if (identical(n_group, 4L)) {
    return(tibble::tibble(
      home_rank = c(1L, 1L, 2L, 2L, 3L, 4L),
      away_rank = c(2L, 3L, 3L, 4L, 4L, 1L)
    ))
  }
  stop(
    "No verified split fixture template for a ", n_group,
    "-team group (verified sizes: 4 and 6).",
    call. = FALSE
  )
}
