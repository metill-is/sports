# fv2_build() numbers the teams by name, and the team index sets R-INLA's node
# order, which moves a fit slightly (2026-11-26, football cells at 2026-11-23,
# C against en_US.UTF-8 collation: outcome probabilities by 0.0016 to 0.0025 on
# average, states by up to 0.11 posterior sd). Byte order is the same under
# every collation, so a test (testthat sorts under C), a runner without the
# en_US locale and production (en_US) all fit the same model.
test_that("fv2_build() orders teams by byte, whatever the collation", {
  teams <- c("\u00c6gir", "Afturelding", "\u00cdA", "\u00de\u00f3r", "KA", "K\u00e1ri", "V\u00edkingur R.", "Valur")
  n <- length(teams)
  train <- data.frame(
    match_date = as.Date("2026-05-01") + seq_len(n),
    home_team = teams, away_team = teams[c(n, seq_len(n - 1L))],
    home_score = 1L, away_score = 0L, season = 2026L
  )
  byte <- sort(unique(teams), method = "radix")
  expect_identical(withr::with_collate("C", fv2_build(train)$teams), byte)
  en <- suppressWarnings(withr::with_collate("en_US.UTF-8", sort(teams)))
  skip_if(identical(en, byte), "no en_US.UTF-8 collation on this machine")
  expect_identical(withr::with_collate("en_US.UTF-8", fv2_build(train)$teams), byte)
})
