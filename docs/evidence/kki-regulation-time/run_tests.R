# The test files the regulation-time ingest touches, one row per test and a total each.
# Usage (repository root): LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript docs/evidence/kki-regulation-time/run_tests.R
options(width = 200)
devtools::load_all(".", quiet = TRUE)
files <- c("test-storage-schemas.R", "test-storage-validate.R", "test-storage-upsert.R", "test-ingest-kki.R",
           "test-ingest-kki-periods.R", "test-ingest-kki-regulation.R", "test-ingest-dispatcher.R")
cat(sprintf("HEAD %s%s\n", system("git rev-parse --short HEAD", intern = TRUE),
            if (length(system("git status --short -- R tests", intern = TRUE))) " (R or tests changed in the working tree)" else ""))
for (f in files) {
  r <- as.data.frame(testthat::test_file(file.path("tests/testthat", f), reporter = "silent", stop_on_failure = FALSE))
  cat(sprintf("%-32s expectations %3d | failed %d | errors %d | skipped %d\n", f, sum(r$nb), sum(r$failed), sum(r$error), sum(r$skipped)))
  bad <- r[r$failed > 0 | r$error, "test"]
  if (length(bad)) cat(paste0("    FAILING: ", bad, "\n"), sep = "")
}
