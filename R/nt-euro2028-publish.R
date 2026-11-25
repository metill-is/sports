#' @include nt-euro2028.R
NULL

# Publish layer for the EURO 2028 qualifying odds. Writes a JSON data contract
# under data/publish/euro2028/karla/, its own namespace beside world_cup/, with
# Icelandic team names at the boundary. Like publish_world_cup() it never goes
# through publish_one() and has no schema under config/publish-schemas/: an
# added schema would arm metill-platform's validator for this tree at once,
# so it belongs with the platform page. The checks here fail closed instead.
#
# metill-platform mirrors the whole of data/publish/ (pull-sports-data.yml),
# so committing this tree on main ships it to the platform repository, where a
# file without a schema is listed as unmatched and passes. Publishing is the
# commit, not the function call.

#' EURO 2028 qualifying odds under both readings of Article 15 d).
#'
#' Runs [nt_simulate_euro2028()] twice on one draw with one seed, once per
#' reading, and returns everything the draw-night report and
#' [publish_euro2028()] need. `scripts/nt/euro2028_odds.R` saves it as
#' `odds.rds`.
#'
#' @param fit A fit from [nt_fit_inla()].
#' @param draw Output of [nt_euro2028_draw()] or [nt_read_euro2028_draw()].
#' @param n_sims Number of simulations.
#' @param seed Seed shared by the two runs.
#' @param fixtures_source Where the fixtures came from: `"generated"` (every
#'   pair home and away) or the path of UEFA's fixture list.
#' @return List: `teams` and `slots` (the default "repeat" reading),
#'   `teams_once`, `compare` ([nt_compare_h2h_readings()]), `n_host_places`,
#'   `groups`, `quarter_finals`, `seeding`, `hosts`, `fixtures_source`,
#'   `n_sims`, `seed`, `fit_source`, `fit_settings`, `fit_n_draws`.
#' @export
nt_euro2028_odds <- function(fit, draw, n_sims = 20000L, seed = 2028L,
                             fixtures_source = "generated") {
  rep_run <- nt_simulate_euro2028(fit, draw, n_sims = n_sims, seed = seed)
  once_run <- nt_simulate_euro2028(fit, draw, n_sims = n_sims, h2h_reapply = "once", seed = seed)
  list(
    teams = rep_run$teams, teams_once = once_run$teams, slots = rep_run$slots,
    compare = nt_compare_h2h_readings(rep_run, once_run),
    n_host_places = rep_run$n_host_places, groups = draw$groups,
    quarter_finals = draw$quarter_finals, seeding = draw$seeding, hosts = draw$hosts,
    fixtures_source = fixtures_source, n_sims = as.integer(n_sims), seed = seed,
    fit_source = fit$source %||% list(), fit_settings = fit$settings,
    fit_n_draws = length(fit$draws$mu)
  )
}

# Icelandic names of the entrants, keyed by the results file's names.
.nt_euro2028_names_is <- function(names_csv) {
  nm <- utils::read.csv(names_csv, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  stats::setNames(nm$name_is, nm$team)
}

#' Publish EURO 2028 qualifying odds as JSON.
#'
#' Writes `meta.json`, `groups.json` (one entry per group slot, the
#' quarter-final placeholders with the two teams of their tie) and
#' `teams.json` (all 54 entrants) into a staging folder, checks them, and only
#' then copies them over `out_dir`; a payload that fails a check leaves the
#' previous files as they were. Probabilities are the "repeat" reading of
#' Article 15 d); `meta.json` records the reading, that the owner has not
#' ruled on it, and how often the two readings differed.
#'
#' The default `out_dir` is inside `data/publish/`, which metill-platform
#' mirrors: committing it publishes it. Write elsewhere to look first.
#'
#' @param odds Output of [nt_euro2028_odds()] (or `odds.rds` from
#'   `scripts/nt/euro2028_odds.R`).
#' @param out_dir Output folder.
#' @param names_csv Icelandic names (`team,name_is`, the results file's names).
#' @param draw_date Date of the qualifying draw.
#' @param generated_at Time stamp written to every file.
#' @return Invisibly the output folder.
#' @export
publish_euro2028 <- function(odds,
                             out_dir = here::here("data", "publish", "euro2028", "karla"),
                             names_csv = here::here("data", "nt", "euro2028", "team_names_is.csv"),
                             draw_date = as.Date("2026-12-06"),
                             generated_at = Sys.time()) {
  names_is <- .nt_euro2028_names_is(names_csv)
  payload <- .nt_euro2028_payload(odds, names_is, draw_date, generated_at)
  stage <- tempfile("euro2028-publish-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  for (f in names(payload)) {
    jsonlite::write_json(
      payload[[f]], file.path(stage, paste0(f, ".json")),
      auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null"
    )
  }
  problems <- nt_check_euro2028_publish(stage, names_is)
  if (length(problems) > 0L) {
    cli::cli_abort(c(
      "EURO 2028 publish JSON failed its checks; {.path {out_dir}} is unchanged.",
      stats::setNames(problems, rep("x", length(problems)))
    ))
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(file.path(stage, paste0(names(payload), ".json")), out_dir, overwrite = TRUE)
  if (!all(ok)) cli::cli_abort("Could not write {.path {out_dir}}.")
  cli::cli_alert_success("Wrote EURO 2028 qualifying JSON to {.path {out_dir}}")
  invisible(out_dir)
}

.nt_euro2028_payload <- function(odds, names_is, draw_date, generated_at) {
  rnd <- function(x, d = 4L) round(unname(x), d)
  stamp <- format(as.POSIXct(generated_at), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  is_name <- function(team) unname(names_is[team])
  qf <- odds$quarter_finals
  seeding <- odds$seeding
  pos_cols <- grep("^p_pos[0-9]+$", names(odds$slots), value = TRUE)

  slot_record <- function(r, size) {
    k <- match(r$slot, qf$winner_slot)
    role <- "winner"
    if (is.na(k)) {
      k <- match(r$slot, qf$defeated_slot)
      role <- "defeated"
    }
    placeholder <- !is.na(k)
    tie <- if (placeholder) c(qf$first_leg_home[k], qf$second_leg_home[k]) else character()
    list(
      slot = r$slot,
      team = if (placeholder) NULL else r$slot,
      team_is = if (placeholder) NULL else is_name(r$slot),
      placeholder = if (placeholder) {
        list(
          quarter_final = as.integer(qf$qf[k]), role = role,
          role_is = if (role == "winner") "sigurvegari" else "tapli\u00f0",
          teams = as.list(tie), teams_is = as.list(is_name(tie))
        )
      } else {
        NULL
      },
      pot = if (placeholder) 1L else as.integer(seeding$pot[match(r$slot, seeding$team)]),
      p_positions = as.list(rnd(unlist(r[pos_cols]))[seq_len(size)]),
      p_winner = rnd(r$p_winner),
      p_second = rnd(r$p_pos2),
      p_qualify_direct = rnd(r$p_qualify_direct),
      p_host_place = rnd(r$p_host_place),
      p_playoff = rnd(r$p_playoff),
      p_qualify = rnd(r$p_qualify),
      exp_points = rnd(r$exp_points, 2L)
    )
  }
  groups <- lapply(names(odds$groups), function(g) {
    sl <- odds$slots[odds$slots$group == g, , drop = FALSE]
    sl <- sl[order(-sl$p_qualify), , drop = FALSE]
    size <- length(odds$groups[[g]])
    list(
      group = g, size = size,
      slots = lapply(seq_len(nrow(sl)), function(i) slot_record(as.list(sl[i, ]), size))
    )
  })

  tm <- odds$teams[order(-odds$teams$p_qualify, odds$teams$team), , drop = FALSE]
  s <- seeding[match(tm$team, seeding$team), , drop = FALSE]
  teams <- lapply(seq_len(nrow(tm)), function(i) {
    r <- as.list(tm[i, ])
    list(
      team = r$team, team_is = is_name(r$team),
      groups = as.list(strsplit(r$group, "/", fixed = TRUE)[[1]]),
      pot = as.integer(s$pot[i]), nl_rank = as.integer(s$nl_rank[i]),
      nl_league = s$nl_league[i], host = isTRUE(s$host[i]),
      quarter_finalist = identical(s$nl_knockout[i], "QF"),
      p_positions = as.list(rnd(unlist(r[grep("^p_pos[0-9]+$", names(r))]))),
      p_winner = rnd(r$p_winner),
      p_qualify_direct = rnd(r$p_qualify_direct),
      p_host_place = rnd(r$p_host_place),
      p_playoff = rnd(r$p_playoff),
      p_playoff_runner_up = rnd(r$p_playoff_runner_up),
      p_playoff_nations_league = rnd(r$p_playoff_nations_league),
      p_qualify_playoff = rnd(r$p_qualify_playoff),
      p_qualify = rnd(r$p_qualify),
      exp_points = rnd(r$exp_points, 2L)
    )
  })

  hp <- tabulate(odds$n_host_places + 1L, 3L) / length(odds$n_host_places)
  src <- odds$fit_source %||% list()
  cmp <- odds$compare
  meta <- list(
    tournament = "EM 2028", stage = "qualifying", stage_is = "undankeppni",
    sport = "football", country = "europe", sex = "male",
    generated_at = stamp, draw_date = format(draw_date),
    n_teams = length(odds$teams$team), n_groups = length(odds$groups),
    n_places = 24L, n_sims = as.integer(odds$n_sims), seed = odds$seed,
    fixtures = if (identical(odds$fixtures_source, "generated")) "generated" else "uefa",
    fit = list(
      cutoff = format(odds$fit_settings$cutoff %||% NA),
      last_match = src$last_match %||% NULL,
      martj42_commit = src$martj42_commit %||% NULL,
      n_draws = odds$fit_n_draws %||% NULL
    ),
    article_15d = list(
      reading = "repeat", ruled = FALSE, note = "moot, not ruled",
      p_any_position_differs = rnd(cmp$any_position, 6L),
      p_direct_differs = rnd(cmp$direct, 6L),
      p_field_differs = rnd(cmp$field, 6L)
    ),
    host_places = list(p_none = rnd(hp[1]), p_one = rnd(hp[2]), p_two = rnd(hp[3]))
  )
  common <- list(generated_at = stamp)
  list(
    meta = meta,
    groups = c(common, list(groups = groups)),
    teams = c(common, list(teams = teams))
  )
}

#' Check a EURO 2028 publish folder.
#'
#' Reads the three JSONs back and checks what a JSON Schema could not:
#' probabilities in 0..1 that add up to the tournament's places (12 group
#' winners, 20 direct places, 24 places in all, one per position in each
#' group), every slot and team present once, and Icelandic names that
#' survived the round trip (a C locale renders them as `<U+00FE>`).
#'
#' @param dir Folder with `meta.json`, `groups.json` and `teams.json`.
#' @param names_is Named character vector of Icelandic names.
#' @return Character vector of problems, empty when the folder passes.
#' @export
nt_check_euro2028_publish <- function(dir, names_is) {
  out <- character()
  read <- function(f) {
    p <- file.path(dir, f)
    if (!file.exists(p)) {
      out <<- c(out, sprintf("%s is missing.", f))
      return(NULL)
    }
    jsonlite::read_json(p, simplifyVector = FALSE)
  }
  meta <- read("meta.json")
  groups <- read("groups.json")
  teams <- read("teams.json")
  if (is.null(meta) || is.null(groups) || is.null(teams)) {
    return(out)
  }
  tol <- 0.01
  near <- function(x, want) abs(x - want) <= tol
  num <- function(lst, key) vapply(lst, function(r) as.numeric(r[[key]] %||% NA), numeric(1))

  tm <- teams$teams
  if (length(tm) != meta$n_teams) out <- c(out, sprintf("teams.json has %d teams, meta.json says %d.", length(tm), meta$n_teams))
  team_names <- vapply(tm, function(r) r$team, character(1))
  if (anyDuplicated(team_names)) out <- c(out, "teams.json repeats a team.")
  keys <- c("p_winner", "p_qualify_direct", "p_host_place", "p_playoff", "p_qualify")
  for (k in keys) {
    v <- num(tm, k)
    if (anyNA(v) || any(v < 0 | v > 1)) out <- c(out, sprintf("teams.json: %s outside 0..1 or missing.", k))
  }
  totals <- c(p_winner = meta$n_groups, p_qualify_direct = 20, p_qualify = meta$n_places)
  for (k in names(totals)) {
    s <- sum(num(tm, k))
    if (!near(s, totals[[k]])) out <- c(out, sprintf("teams.json: %s sums to %.3f, not %d.", k, s, as.integer(totals[[k]])))
  }
  if (any(num(tm, "p_qualify") + 1e-4 < num(tm, "p_qualify_direct") + num(tm, "p_host_place"))) {
    out <- c(out, "teams.json: p_qualify is below p_qualify_direct + p_host_place for a team.")
  }
  is_written <- vapply(tm, function(r) r$team_is %||% "", character(1))
  want_is <- unname(names_is[team_names])
  bad_is <- team_names[is.na(want_is) | is_written != want_is]
  if (length(bad_is) > 0L) out <- c(out, sprintf("teams.json: Icelandic name missing or mangled for %s.", paste(bad_is, collapse = ", ")))

  gs <- groups$groups
  if (length(gs) != meta$n_groups) out <- c(out, sprintf("groups.json has %d groups, meta.json says %d.", length(gs), meta$n_groups))
  n_slots <- 0L
  for (g in gs) {
    sl <- g$slots
    n_slots <- n_slots + length(sl)
    if (length(sl) != g$size) out <- c(out, sprintf("group %s: %d slots for size %d.", g$group, length(sl), g$size))
    pos <- lapply(sl, function(r) as.numeric(unlist(r$p_positions)))
    if (any(lengths(pos) != g$size)) {
      out <- c(out, sprintf("group %s: a slot's position probabilities are not one per position.", g$group))
      next
    }
    pos <- matrix(unlist(pos), nrow = g$size)
    if (any(!near(rowSums(pos), 1)) || any(!near(colSums(pos), 1))) {
      out <- c(out, sprintf("group %s: position probabilities do not sum to 1 by slot and by position.", g$group))
    }
    for (r in sl) {
      if (is.null(r$team) == is.null(r$placeholder)) out <- c(out, sprintf("group %s, slot %s: needs a team or a placeholder, not both.", g$group, r$slot))
    }
  }
  if (n_slots != meta$n_teams) out <- c(out, sprintf("groups.json has %d slots for %d teams.", n_slots, meta$n_teams))
  out
}
