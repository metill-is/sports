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
  # Use a yaml WITHOUT an explicit current_pool so the ledger-derivation path is
  # exercised (the shipped config/bankroll.yml now pins current_pool explicitly).
  tmp_yaml <- withr::local_tempfile(fileext = ".yml")
  writeLines(
    c("initial_pool: 1000", "daily_budget_frac: 0.05", "daily_budget_min_isk: 100"),
    tmp_yaml
  )
  tmp <- withr::local_tempdir()
  suppressWarnings(cfg <- load_bankroll(path = tmp_yaml, ledger_root = tmp))
  # No ledger -> current_pool = initial_pool
  expect_equal(cfg$current_pool, cfg$initial_pool)
})

test_that("load_bankroll warns when current_pool is derived from the ledger", {
  # The canonical ledger mixes bookmakers (Lengjan / EpicBet / CoolBet); summing
  # it for the Lengjan pool is wrong. Setting current_pool explicitly is the
  # supported path -- falling back to a ledger sum must warn, not silently
  # re-inflate the pool ~7x (the 2026-06-05 over-stake bug).
  tmp_yaml <- withr::local_tempfile(fileext = ".yml")
  writeLines(
    c("initial_pool: 1000", "daily_budget_frac: 0.05", "daily_budget_min_isk: 100"),
    tmp_yaml
  )
  empty_root <- withr::local_tempdir()
  expect_warning(
    load_bankroll(path = tmp_yaml, ledger_root = empty_root),
    "current_pool"
  )
})

test_that("load_bankroll uses an explicit current_pool without warning", {
  tmp_yaml <- withr::local_tempfile(fileext = ".yml")
  writeLines(
    c(
      "initial_pool: 29000", "current_pool: 14909",
      "daily_budget_frac: 0.05", "daily_budget_min_isk: 1000"
    ),
    tmp_yaml
  )
  empty_root <- withr::local_tempdir()
  cfg <- expect_no_warning(load_bankroll(path = tmp_yaml, ledger_root = empty_root))
  expect_equal(cfg$current_pool, 14909)
})
