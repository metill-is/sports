test_that("run.R --help prints usage", {
  out <- system2(
    "Rscript", c(here::here("run.R"), "--help"),
    stdout = TRUE, stderr = TRUE
  )
  expect_true(any(grepl("--league", out)))
  expect_true(any(grepl("--sex", out)))
  expect_true(any(grepl("--step", out)))
})

test_that("run.R --dry-run --step fit prints the planned target names", {
  out <- system2(
    "Rscript",
    c(
      here::here("run.R"),
      "--league", "football_iceland",
      "--sex", "male",
      "--step", "fit",
      "--dry-run"
    ),
    stdout = TRUE, stderr = TRUE
  )
  expect_true(any(grepl("fit_football_iceland_male", out)))
})
