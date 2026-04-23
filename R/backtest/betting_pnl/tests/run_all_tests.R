# Runs every test_*.R file under tests/.
# Invocation:
#   Rscript Sports/R/backtest/betting_pnl/tests/run_all_tests.R

suppressPackageStartupMessages({
  library(here)
  library(testthat)
})

test_dir <- here::here("R", "backtest", "betting_pnl", "tests")
test_files <- list.files(test_dir, pattern = "^test_.*\\.R$", full.names = TRUE)

if (length(test_files) == 0) {
  cat("No test files found under", test_dir, "- did you forget to write one?\n")
  quit(status = 1)
}

all_passed <- TRUE
for (f in test_files) {
  cat("\n== Running", basename(f), "==\n")
  result <- tryCatch(
    {
      testthat::test_file(f, reporter = testthat::SummaryReporter$new())
      TRUE
    },
    error = function(e) {
      cat("FAILED:", conditionMessage(e), "\n")
      FALSE
    }
  )
  all_passed <- all_passed && result
}

if (!all_passed) {
  quit(status = 1)
}
cat("\nAll tests passed.\n")
