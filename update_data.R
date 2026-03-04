#### Update data + schedules for all sports (no model fitting) ####
#
# Run from Sports/ directory:
#   source("update_data.R")
#
# Downloads fresh data and schedules from external sources.
# Does NOT fit any Bayesian models — use update.R for that.
#

sports_dir <- here::here()
status <- list()

# ── Helper ────────────────────────────────────────────────────────────────────

safe_source <- function(script, label) {
  tryCatch(
    {
      source(script)
      cat(paste("[OK]", label, "\n"))
      TRUE
    },
    error = function(e) {
      cat(paste("[FAILED]", label, "-", conditionMessage(e), "\n"))
      FALSE
    }
  )
}

# ── Basketball Iceland ────────────────────────────────────────────────────────

cat("\n=== BASKETBALL ICELAND ===\n")

res <- withr::with_dir(file.path(sports_dir, "basketball/iceland"), {
  here::i_am("basketball_iceland.Rproj")
  list(
    basketball_kk = safe_source("R/prep_data_kk.R", "basketball male data"),
    basketball_kvk = safe_source("R/prep_data_kvk.R", "basketball female data")
  )
})
here::i_am(".here")
status <- c(status, res)

# ── Handball Iceland ──────────────────────────────────────────────────────────

cat("\n=== HANDBALL ICELAND ===\n")

withr::with_dir(file.path(sports_dir, "handball/iceland"), {
  here::i_am("handball_iceland.Rproj")
  for (sex in c("male", "female")) {
    safe_source(
      here::here("R", "utils", sex, "download_newest_data_div1.R"),
      paste("handball iceland", sex, "div1")
    )
    safe_source(
      here::here("R", "utils", sex, "download_newest_data_div2.R"),
      paste("handball iceland", sex, "div2")
    )
    safe_source(
      here::here("R", "utils", sex, "process_data.R"),
      paste("handball iceland", sex, "process")
    )
  }
})
here::i_am(".here")

# ── Football England ──────────────────────────────────────────────────────────

cat("\n=== FOOTBALL ENGLAND ===\n")

status$football_eng <- withr::with_dir(file.path(sports_dir, "football/england"), {
  here::i_am("football_england.Rproj")
  safe_source("R/update_schedules.R", "football england schedules")
})
here::i_am(".here")

# ── Football Italy ────────────────────────────────────────────────────────────

cat("\n=== FOOTBALL ITALY ===\n")

status$football_ita <- withr::with_dir(file.path(sports_dir, "football/italy"), {
  here::i_am("football_italy.Rproj")
  safe_source("R/update_schedules.R", "football italy schedules")
})
here::i_am(".here")

# ── Football Spain ────────────────────────────────────────────────────────────

cat("\n=== FOOTBALL SPAIN ===\n")

status$football_esp <- withr::with_dir(file.path(sports_dir, "football/spain"), {
  here::i_am("football_spain.Rproj")
  safe_source("R/update_schedules.R", "football spain schedules")
})
here::i_am(".here")

# ── Handball European Leagues ─────────────────────────────────────────────────

cat("\n=== HANDBALL EUROPEAN LEAGUES ===\n")

res <- withr::with_dir(file.path(sports_dir, "handball/other"), {
  here::i_am("handball_other.Rproj")
  list(
    handball_eu_data = safe_source(
      here::here("R", "update_historical_data.R"),
      "handball european data"
    ),
    handball_eu_sched = safe_source(
      here::here("R", "update_schedules.R"),
      "handball european schedules"
    )
  )
})
here::i_am(".here")
status <- c(status, res)

# ── Summary ───────────────────────────────────────────────────────────────────

cat("\n=== Data Update Summary ===\n")
for (key in names(status)) {
  cat(sprintf("  %-35s %s\n", key, ifelse(isTRUE(status[[key]]), "OK", "FAILED")))
}
cat("\nDone! Run check_schedules.R to scan for upcoming games.\n")
