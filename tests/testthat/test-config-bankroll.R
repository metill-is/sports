test_that("load_bankroll returns expected fields", {
  cfg <- load_bankroll()
  expect_named(cfg, c(
    "initial_pool", "current_pool",
    "daily_budget_frac", "daily_budget_min_isk",
    "kelly_ceiling", "max_match_stake_default"
  ),
  ignore.order = TRUE
  )
  expect_gt(cfg$current_pool, 0)
})

test_that("load_bankroll degrades gracefully when ledger absent", {
  tmp <- withr::local_tempdir()
  cfg <- load_bankroll(ledger_root = tmp)
  # No ledger -> current_pool = initial_pool
  expect_equal(cfg$current_pool, cfg$initial_pool)
})
