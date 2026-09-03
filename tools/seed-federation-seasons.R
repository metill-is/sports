#!/usr/bin/env Rscript
# Seed config/federation-seasons.json with the HSI ids observed on 2026-09-02.
# Idempotent: re-running merges the same rows back onto themselves.
#
#   Rscript tools/seed-federation-seasons.R

suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

observed <- "2026-09-02"

seed <- tibble::tribble(
  ~sex,     ~division,  ~season,      ~id,     ~title_ascii,                    ~source,                 ~verified, ~note,
  "male",   "div1",     2027L,        9142L,   "Olis deild karla 2026-27",      "live-nav",              TRUE,      NA_character_,
  "male",   "div2",     2027L,        9140L,   "Grill 66 deild karla 2026-27",  "live-nav",              TRUE,      NA_character_,
  "female", "div1",     2027L,        9141L,   "Olis deild kvenna 2026-27",     "live-nav",              TRUE,      NA_character_,
  "female", "div2",     2027L,        9143L,   "Grill 66 deild kvenna umspil",  "live-nav",              TRUE,      "Nav title carries 'umspil'; confirm the 2026-27 female G66 format at first ingest.",
  "male",   "cup",      NA_integer_,  8437L,   "Bikar karla",                   "live-nav-unattributed", FALSE,     "Pre-change HSI_URLS attributed 8437 to 2025-26 (season 2026); it is still in the 2026-09-02 nav. Registered at 2026 in HSI_TOURNAMENT_IDS on the attributed evidence; season 2027 stays deferred until discovery attributes it.",
  "female", "cup",      NA_integer_,  8436L,   "Bikar kvenna",                  "live-nav-unattributed", FALSE,     "Observed but unreachable: hsi_divisions_for_sex('female') has no 'cup' division, so nothing would ever fetch it."
)

entries <- tibble::tibble(
  federation = "hsi",
  sex = seed$sex,
  division = seed$division,
  season = seed$season,
  id = seed$id,
  title = seed$title_ascii,
  source = seed$source,
  discovered_at = observed,
  verified = seed$verified,
  note = seed$note
)

merged <- refresh_federation_seasons(entries)
cli::cli_alert_success("Seeded {nrow(merged)} federation-season entries.")
