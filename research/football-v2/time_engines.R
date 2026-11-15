# Time the two engines on the one specification, in sequence, and record what
# else the machine was doing. Meant to be run alone inside a compute slot.
#
#   Rscript research/football-v2/time_engines.R <cutoff> <horizon_end> [note]
# Run from the repo root.
options(width = 160)
suppressMessages(devtools::load_all(".", quiet = TRUE))
source("research/football-v2/data.R")
fv2_source()
args <- commandArgs(trailingOnly = TRUE)
cutoff <- as.Date(args[[1]])
horizon_end <- as.Date(args[[2]])
note <- if (length(args) >= 3L) args[[3]] else ""
out <- fv2_out_dir()
league <- load_leagues()[["football_iceland"]]
load_avg <- function() as.numeric(strsplit(trimws(sub(".*load averages?:", "", system("uptime", intern = TRUE))), "[ ,]+")[[1]][1])
clock <- function(expr) {
  l0 <- load_avg()
  p0 <- proc.time()
  value <- force(expr)
  p <- proc.time() - p0
  list(value = value, wall = p[["elapsed"]], cpu = sum(p[c("user.self", "sys.self", "user.child", "sys.child")]), load_before = l0, load_after = load_avg())
}

rows <- list()
for (sex in c("female", "male")) {
  sp <- fv2_split(fv2_results(sex), league, cutoff, horizon_end = horizon_end)
  spec <- fv2_build(sp$train, sp$pred)
  csv_dir <- file.path(out, sprintf("timing_csv_%s", sex))
  dir.create(csv_dir, showWarnings = FALSE)
  st <- clock(fv2_fit_stan(spec, "research/football-v2/stan/fv2.stan", file.path(dirname(out), "exe"), csv_dir, refresh = 0L))
  unlink(csv_dir, recursive = TRUE)
  d <- st$value$diagnostics
  rows[[length(rows) + 1L]] <- data.frame(
    sex = sex, engine = "stan-nuts", threads = 4L, n_draws = 4000L,
    fit_secs = d$wall_secs, total_wall = st$wall, cpu_secs = st$cpu,
    leapfrog = d$mean_leapfrog, divergences = d$n_divergent,
    load_before = st$load_before, load_after = st$load_after
  )
  rm(st)
  gc()
  for (strategy in c("eb", "auto")) {
    f <- clock(fv2_fit_inla(spec, int_strategy = strategy, n_samp = 1000L, threads = "4:1"))
    rows[[length(rows) + 1L]] <- data.frame(
      sex = sex, engine = paste0("inla-", strategy), threads = 4L, n_draws = 1000L,
      fit_secs = f$value$t_fit, total_wall = f$wall, cpu_secs = f$cpu,
      leapfrog = NA_real_, divergences = NA_integer_,
      load_before = f$load_before, load_after = f$load_after
    )
    rm(f)
    gc()
  }
  print(do.call(rbind, rows), digits = 4)
}
slots <- tryCatch(system("/Users/brynjolfurjonsson/.claude-reviews/machine-slots/slot.sh status", intern = TRUE), error = function(e) NA_character_)
saveRDS(
  list(timing = do.call(rbind, rows), note = note, slots = slots, when = format(Sys.time()), cmdstan = cmdstanr::cmdstan_version()),
  file.path(out, "engine_timing.rds")
)
cat("TIMING DONE\n")
