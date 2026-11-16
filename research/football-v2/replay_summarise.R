# Collect the replay comparisons into one small file for the report, and check
# that every difference belongs to a named class.
#
#   Rscript research/football-v2/replay_summarise.R
#
# Two comparisons per date:
#   published -> stored   what production published that day (from git
#                         history) against its stored extract put through
#                         today's publish code: is the baseline faithful?
#   stored -> inla        that baseline against the INLA replay: what the
#                         model and engine change.
# Run from the repo root after run_replay.R (both modes) for every date.
options(width = 200)
root <- Sys.getenv("FV2_REPLAY", "/Users/brynjolfurjonsson/.claude-reviews/sports-football-v2/replay")
out <- Sys.getenv("FV2_OUT", "/Users/brynjolfurjonsson/.claude-reviews/sports-football-v2/out")

cells <- data.frame(
  sex = c("male", "male", "male", "female", "female", "female"),
  as_of = c("2026-04-09", "2026-10-17", "2026-10-20", "2026-04-23", "2026-10-17", "2026-10-19"),
  # The last production publish of that day; none exists for the pre-season
  # baseline fits, which were backfilled.
  sha = c(NA, "ba7597381", "4a55ade30", NA, "ba7597381", "d9dae0a9e")
)
cells$stored <- file.path(root, sprintf("stored_%s_%s", cells$sex, cells$as_of))
cells$inla <- file.path(root, sprintf("inla_%s_%s", cells$sex, cells$as_of))
cells$published <- ifelse(is.na(cells$sha), NA, file.path(root, "published", sprintf("%s_%s", cells$sha, cells$sex), "data", "publish"))
cells <- cells[dir.exists(file.path(cells$inla, "publish")) & dir.exists(file.path(cells$stored, "publish")), ]
stopifnot(nrow(cells) > 0L)

`%||%` <- function(x, y) if (is.null(x)) y else x

run_diff <- function(a, b, name) {
  file <- file.path(root, "diff", paste0(name, ".rds"))
  status <- system2("Rscript", c("research/football-v2/replay_diff.R", a, b, file), stdout = FALSE)
  stopifnot(status == 0L)
  readRDS(file)
}

history <- c("final_positions_history.json", "standings_history.json", "team_strengths_history.json")
simulated <- c(
  "final_positions.json", "final_positions_history.json", "points_distribution.json",
  "tournament_placements.json", "bracket.json"
)

# Why a field differs between what production published and the baseline.
cause_published <- function(d) {
  ifelse(
    d$field == "generated_at", "Run timestamp",
    ifelse(
      d$surface %in% history & d$number == 0L & d$text == 0L & d$only_b == 0L,
      "Accumulated history: earlier rounds' rows", "UNEXPLAINED"
    )
  )
}

# Why a field differs between the baseline and the INLA replay.
cause_inla <- function(d) {
  value <- d$field %in% c("lower", "median", "upper")
  ifelse(
    d$field == "generated_at", "Run timestamp",
    ifelse(
      d$surface == "home_advantage.json" & value, "Home advantage",
      ifelse(
        d$surface %in% c("team_strengths.json", "team_strengths_history.json") & value, "Team strengths",
        ifelse(
          d$surface == "next_games.json", "Match forecasts",
          ifelse(d$surface %in% simulated, "Season and cup simulation", "UNEXPLAINED")
        )
      )
    )
  )
}

tally <- function(r, cause_fn, cell) {
  b <- r$by_field[r$by_field$equal < r$by_field$n, ]
  if (!nrow(b)) {
    return(NULL)
  }
  b$cause <- cause_fn(b)
  cbind(sex = cell$sex, as_of = cell$as_of, b)
}

component_of <- function(record) sub(".*component=([^],]+).*", "\\1", record)
location_of <- function(record) ifelse(grepl("location=", record), sub(".*location=([^],]+).*", "\\1", record), NA)

per_cell <- lapply(seq_len(nrow(cells)), function(i) {
  cell <- cells[i, ]
  tag <- sprintf("%s_%s", cell$sex, cell$as_of)
  cat("==", tag, "\n")
  read_summary <- function(dir) jsonlite::fromJSON(file.path(dir, "summary.json"))
  s_inla <- read_summary(cell$inla)
  s_stored <- read_summary(cell$stored)
  d2 <- run_diff(file.path(cell$stored, "publish"), file.path(cell$inla, "publish"), paste0("stored_vs_inla_", tag))
  d1 <- if (!is.na(cell$published)) run_diff(cell$published, file.path(cell$stored, "publish"), paste0("published_vs_stored_", tag))

  ch <- d2$changed[d2$changed$status == "number", ]
  ch$a <- as.numeric(ch$value_a)
  ch$b <- as.numeric(ch$value_b)
  medians <- function(surface) {
    m <- ch[ch$surface == surface & ch$field == "median" & !grepl("/preseason", ch$record), ]
    m$component <- component_of(m$record)
    m$location <- location_of(m$record)
    m <- m[!duplicated(paste(m$division, sub(",coverage=[^]]*", "", m$record))), ]
    m
  }
  by_group <- function(m, group) {
    if (!nrow(m)) {
      return(NULL)
    }
    do.call(rbind, lapply(split(m, m[group], drop = TRUE), function(g) {
      data.frame(
        g[1, group, drop = FALSE],
        n = nrow(g), mean_stored = mean(g$a), mean_inla = mean(g$b),
        sd_stored = stats::sd(g$a), sd_inla = stats::sd(g$b),
        mean_abs = mean(abs(g$b - g$a)), correlation = if (nrow(g) > 2L && stats::sd(g$a) > 0 && stats::sd(g$b) > 0) stats::cor(g$a, g$b) else NA_real_
      )
    }))
  }
  pick <- function(surface, fields) ch[ch$surface == surface & ch$field %in% fields, ]
  forecasts <- pick("next_games.json", c("p_home_win", "p_draw", "p_away_win"))
  goals <- pick("next_games.json", c("mean_home_goals", "mean_away_goals"))
  places <- pick("final_positions.json", "probability")
  points <- pick("points_distribution.json", "mean_points")

  list(
    cell = data.frame(
      sex = cell$sex, as_of = cell$as_of, sha = cell$sha,
      horizon_days = s_inla$horizon_days, n_obs = s_inla$fit$n_obs, n_teams = s_inla$fit$n_teams,
      production_n_obs = s_inla$production_n_obs %||% NA_integer_,
      returned_to_schedule = length(s_inla$returned_to_schedule),
      fit_seconds = s_inla$fit$seconds$fit, draw_seconds = s_inla$fit$seconds$draws,
      fit_league_seconds = s_inla$timing$fit_league$elapsed, gate_passed = isTRUE(s_inla$fit$gate$passed),
      json_inla = s_inla$json_files, json_stored = s_stored$json_files,
      schema_passed_inla = s_inla$schema$n_passed, schema_failed_inla = s_inla$schema$n_failed,
      schema_passed_stored = s_stored$schema$n_passed, schema_failed_stored = s_stored$schema$n_failed,
      only_in_stored = length(d2$only_in_a), only_in_inla = length(d2$only_in_b),
      values_compared = d2$n_leaves, values_differing = nrow(d2$changed),
      n_fixtures = nrow(forecasts) / 3,
      forecast_mean_abs = mean(abs(forecasts$diff)), forecast_max_abs = if (nrow(forecasts)) max(abs(forecasts$diff)) else NA_real_,
      goals_mean_abs = mean(abs(goals$diff)),
      place_changed = nrow(places), place_mean_abs = mean(abs(places$diff)), place_max_abs = if (nrow(places)) max(abs(places$diff)) else NA_real_,
      points_mean_abs = mean(abs(points$diff)), points_max_abs = if (nrow(points)) max(abs(points$diff)) else NA_real_
    ),
    hyper = s_inla$fit$hyper,
    schema_errors_stored = s_stored$schema$errors,
    files_only_in_stored = d2$only_in_a, files_only_in_inla = d2$only_in_b,
    layer1 = if (!is.null(d1)) tally(d1, cause_published, cell),
    layer1_files = if (!is.null(d1)) c(only_published = length(d1$only_in_a), only_stored = length(d1$only_in_b), values = d1$n_leaves),
    layer2 = tally(d2, cause_inla, cell),
    home_advantage = cbind(sex = cell$sex, as_of = cell$as_of, by_group(medians("home_advantage.json"), "component")),
    strengths = cbind(sex = cell$sex, as_of = cell$as_of, by_group(medians("team_strengths.json"), c("component", "location")))
  )
})

bind <- function(name) do.call(rbind, lapply(per_cell, `[[`, name))
layer1 <- bind("layer1")
layer2 <- bind("layer2")
classes <- function(d) {
  if (is.null(d)) {
    return(NULL)
  }
  d$differing <- d$number + d$text + d$only_a + d$only_b
  a <- stats::aggregate(cbind(differing, number, text, only_a, only_b) ~ cause, d, sum)
  a$surfaces <- vapply(a$cause, function(k) paste(sort(unique(d$surface[d$cause == k])), collapse = ", "), character(1))
  a$fields <- vapply(a$cause, function(k) paste(sort(unique(d$field[d$cause == k])), collapse = ", "), character(1))
  a
}
result <- list(
  generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
  cells = bind("cell"),
  hyper = stats::setNames(lapply(per_cell, `[[`, "hyper"), paste(cells$sex, cells$as_of)),
  layer1 = layer1, layer1_classes = classes(layer1),
  layer1_files = do.call(rbind, lapply(per_cell, `[[`, "layer1_files")),
  layer2 = layer2, layer2_classes = classes(layer2),
  home_advantage = bind("home_advantage"), strengths = bind("strengths"),
  schema_errors_stored = stats::setNames(lapply(per_cell, `[[`, "schema_errors_stored"), paste(cells$sex, cells$as_of)),
  files_only_in_stored = stats::setNames(lapply(per_cell, `[[`, "files_only_in_stored"), paste(cells$sex, cells$as_of)),
  files_only_in_inla = stats::setNames(lapply(per_cell, `[[`, "files_only_in_inla"), paste(cells$sex, cells$as_of))
)
saveRDS(result, file.path(out, "replay_summary.rds"))

print(result$cells, row.names = FALSE, digits = 3)
cat("\npublished -> stored baseline\n")
print(result$layer1_classes[c("cause", "differing", "surfaces")], row.names = FALSE)
cat("\nstored baseline -> INLA replay\n")
print(result$layer2_classes[c("cause", "differing", "number", "text", "only_a", "only_b", "surfaces")], row.names = FALSE)
cat("\nhome advantage medians\n")
print(result$home_advantage, row.names = FALSE, digits = 3)
unexplained <- rbind(
  layer1[layer1$cause == "UNEXPLAINED", c("sex", "as_of", "surface", "field", "number", "text", "only_a", "only_b")],
  layer2[layer2$cause == "UNEXPLAINED", c("sex", "as_of", "surface", "field", "number", "text", "only_a", "only_b")]
)
if (nrow(unexplained)) {
  cat("\nUNEXPLAINED differences:\n")
  print(unexplained, row.names = FALSE)
  quit(status = 1L)
}
cat("\nevery difference is in a named class\nwrote", file.path(out, "replay_summary.rds"), "\n")
