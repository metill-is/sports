#!/usr/bin/env Rscript
# run.R -- Thin CLI wrapper around targets::tar_make().
#
# Usage:
#   Rscript run.R --league football_iceland --sex male --step fit
#   Rscript run.R --all --step odds
#   Rscript run.R --step publish                   # all active leagues, both sexes
#   Rscript run.R --dry-run --step fit             # print plan, don't execute
#   Rscript run.R --help

# Set UTF-8 locale before sourcing R/. Mirrors _targets.R: some R/ files
# (e.g. R/ingest-hsi-handball.R) embed Icelandic strings that fail to parse
# under a C locale.
Sys.setlocale("LC_ALL", "en_US.UTF-8")

suppressPackageStartupMessages({
  if (!requireNamespace("targets", quietly = TRUE)) {
    stop("Install {targets} to use run.R: install.packages('targets')")
  }
  devtools::load_all(here::here(), quiet = TRUE)
})

args <- commandArgs(trailingOnly = TRUE)

print_help <- function() {
  cat(
    "Usage: Rscript run.R [options]\n",
    "\n",
    "Options:\n",
    "  --league KEY     Run for a single league (e.g. football_iceland)\n",
    "  --sex SEX        Restrict to one sex (male | female | all)\n",
    "  --step STEP      One of: data | odds | fit | decide | publish | all\n",
    "  --all            Run all active leagues for the chosen step\n",
    "  --dry-run        Print the targets that would run; don't execute\n",
    "  --help           Show this message\n",
    "\n",
    "Examples:\n",
    "  Rscript run.R --all --step odds\n",
    "  Rscript run.R --league handball_iceland --step fit\n",
    "  Rscript run.R --league football_iceland --sex female --step decide\n",
    sep = ""
  )
}

if ("--help" %in% args || length(args) == 0L) {
  print_help()
  quit(save = "no", status = 0L)
}

get_flag <- function(name, default = NULL) {
  i <- which(args == paste0("--", name))
  if (length(i) == 0L) default else args[[i + 1L]]
}
has_flag <- function(name) paste0("--", name) %in% args

league <- get_flag("league")
sex <- get_flag("sex")
step <- get_flag("step", "all")
all_flag <- has_flag("all")
dry_run <- has_flag("dry-run")

# Resolve target names
leagues <- load_leagues()
active <- filter_leagues(leagues, active_only = TRUE)
keys <- if (all_flag || is.null(league)) names(active) else league

target_names <- character(0)
prefix_for_step <- list(
  data    = "ingest_",
  odds    = "odds_",
  fit     = "fit_",
  decide  = "decide_",
  publish = "publish_"
)

steps_to_run <- if (step == "all") {
  c("data", "odds", "fit", "decide", "publish")
} else {
  step
}

for (s in steps_to_run) {
  prefix <- prefix_for_step[[s]]
  if (is.null(prefix)) {
    stop(
      "Unknown --step: ", s, ". Use one of ",
      paste(c(names(prefix_for_step), "all"), collapse = ", ")
    )
  }
  for (k in keys) {
    if (s %in% c("data", "odds")) {
      target_names <- c(target_names, paste0(prefix, k))
    } else {
      sexes <- if (is.null(sex) || sex == "all") {
        active[[k]]$sexes
      } else {
        sex
      }
      target_names <- c(target_names, paste0(prefix, k, "_", sexes))
    }
  }
}

target_names <- unique(target_names)

if (dry_run) {
  cli::cli_h2("Planned targets")
  for (t in target_names) cat(" -", t, "\n")
  quit(save = "no", status = 0L)
}

cli::cli_h1("tar_make({length(target_names)} targets)")
targets::tar_make(names = tidyselect::all_of(target_names))
