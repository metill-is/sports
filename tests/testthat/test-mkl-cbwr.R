# R-INLA's Linux build runs on MKL, whose code path follows the CPU, so an
# Intel and an AMD runner returned different fits of the same data (PR #120).
# MKL_CBWR=COMPATIBLE was the one setting that made them agree bit for bit;
# the owner chose to pin it on 2026-11-26 ("Pin it (one-off shift in
# numbers)"). The package sets it on load, so every R-INLA fit and posterior
# sample (separate processes that inherit the environment) uses it.

test_that("loading the package pins MKL_CBWR to COMPATIBLE", {
  withr::local_envvar(MKL_CBWR = NA)
  .pin_mkl_cbwr()
  expect_identical(Sys.getenv("MKL_CBWR"), "COMPATIBLE")
})

test_that("an MKL_CBWR set by the environment is left alone", {
  withr::local_envvar(MKL_CBWR = "AUTO")
  .pin_mkl_cbwr()
  expect_identical(Sys.getenv("MKL_CBWR"), "AUTO")
})

test_that("the pin is set in this process, so R-INLA's child processes inherit it", {
  expect_identical(Sys.getenv("MKL_CBWR"), "COMPATIBLE")
})
