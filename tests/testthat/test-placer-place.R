test_that("is_positive_ev returns TRUE iff EV is strictly positive", {
  # p * (odds - 1) - (1 - p) > 0  <=>  p * odds - 1 > 0
  expect_true(is_positive_ev(p = 0.55, odds = 2.0)) # 0.55*1 - 0.45 = 0.10 > 0
  expect_false(is_positive_ev(p = 0.50, odds = 1.50)) # 0.50*0.5 - 0.50 = -0.25 < 0
  expect_false(is_positive_ev(p = 0.50, odds = 2.0)) # 0.50*1 - 0.50 = 0 (not > 0)
})

test_that("recalculate_kelly_amount returns original amount when odds unchanged", {
  out <- recalculate_kelly_amount(
    p = 0.55, actual_odds = 2.0, original_odds = 2.0, original_amount = 1000
  )
  expect_equal(out, 1000, tolerance = 1e-6)
})

test_that("recalculate_kelly_amount returns smaller stake when odds drop", {
  out <- recalculate_kelly_amount(
    p = 0.55, actual_odds = 1.85, original_odds = 2.00, original_amount = 1000
  )
  expect_lt(out, 1000)
  expect_gte(out, 0)
})

test_that("recalculate_kelly_amount returns 0 when new EV is negative", {
  # p=0.55, odds=1.50 → kelly fraction negative → clamped to 0
  out <- recalculate_kelly_amount(
    p = 0.55, actual_odds = 1.50, original_odds = 2.00, original_amount = 1000
  )
  expect_equal(out, 0)
})

test_that("handicap_to_lengjan_line: positive handicap gives '<h>-0' format", {
  # Legacy: if (handicap >= 0) paste0(handicap, "-0")
  expect_equal(handicap_to_lengjan_line(1.5), "1.5-0")
  expect_equal(handicap_to_lengjan_line(2), "2-0")
})

test_that("handicap_to_lengjan_line: negative handicap gives '0-<abs(h)>' format", {
  # Legacy: else paste0("0-", abs(handicap))
  expect_equal(handicap_to_lengjan_line(-1.5), "0-1.5")
  expect_equal(handicap_to_lengjan_line(-2), "0-2")
})

test_that("handicap_to_lengjan_line: zero handicap gives '0-0'", {
  expect_equal(handicap_to_lengjan_line(0), "0-0")
})
