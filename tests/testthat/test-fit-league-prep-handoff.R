# Spec 2026-09-16 §9.2. Given no prep, the 2DT extractor rebuilds one at the
# DEFAULT 14-day horizon; if the fit used any other, .compute_posterior_goals_2dt()
# sees an N_pred mismatch, warns, and predicted_matches ships empty. fit_league()
# has handed the extractor its own prep since 7a7d94d04 -- this pins it.
test_that("fit_league hands its own prep to the extractor", {
  body_txt <- paste(deparse(body(fit_league)), collapse = "\n")
  expect_match(body_txt, "prep = prep", fixed = TRUE)
})
