test_that("chromote_login errors clearly when env vars are unset", {
  withr::local_envvar(LENGJAN_USER = "", LENGJAN_PASS = "")
  expect_error(
    chromote_login(),
    "LENGJAN_USER|LENGJAN_PASS|env"
  )
})

test_that("chromote_login skips when chromote is unavailable", {
  skip_if_not_installed("chromote")
  if (Sys.getenv("LENGJAN_USER") == "") {
    testthat::skip("LENGJAN_USER unset; skip live login")
  }
  # Live login is opt-in via local .Renviron + manual test invocation
  testthat::skip("live login is opt-in")
})
