# Extract quantile grid (Plan B follow-on, sizing).
#
# Both extractors stored all 99 percentiles per (team, round, component,
# location). The ONLY consumer, .intervals_from_quantiles_pfi(), filters to
# nine of them on its first line and discards the rest -- so 90% of the
# largest artefact in the repo was computed, written, committed to git and
# shallow-cloned by nine CI workflows in order to be thrown away. Football's
# round_strengths_quantiles alone was 8.0 MB of a 22 MB partition, and it does
# not compress: `value` held 1,001,475 distinct doubles across 1,001,484 rows,
# so parquet's dictionary and RLE encodings do nothing.
#
# The grid is deliberately WIDER than the nine currently needed. Storing
# exactly what today's presentation wants would bake a presentation choice
# into the stored data -- the same class of mistake as B5, where football's
# exp() was carried into a model whose parameter was additive. Every 5th
# percentile plus the 95% band's tails costs ~1 MB per partition over the
# minimal set and lets any 5%-granular coverage band be published later
# WITHOUT refitting.

test_that("the grid covers every quantile the publisher actually needs", {
  # If someone adds a coverage band whose tails are not stored, this fails
  # here at test time rather than silently publishing NA intervals.
  src <- readLines(testthat::test_path("..", "..", "R",
                                       "publish-football-iceland.R"),
                   warn = FALSE)
  line <- grep("^\\s*needed <- c\\(", src, value = TRUE)
  expect_length(line, 1L)
  needed <- eval(parse(text = sub("^\\s*needed <- ", "", line)))
  expect_true(all(needed %in% PUBLISH_QUANTILE_GRID))
})

test_that("the grid supports the standard coverage bands without a refit", {
  # median + 50/80/90% bands are expressible from the stored grid alone.
  for (q in c(50L, 25L, 75L, 10L, 90L, 5L, 95L)) {
    expect_true(q %in% PUBLISH_QUANTILE_GRID, info = paste("missing q", q))
  }
})

test_that("the grid is a strict subset of the old 99 and is sorted", {
  expect_true(all(PUBLISH_QUANTILE_GRID %in% seq_len(99L)))
  expect_lt(length(PUBLISH_QUANTILE_GRID), 99L)
  expect_false(is.unsorted(PUBLISH_QUANTILE_GRID))
  expect_false(any(duplicated(PUBLISH_QUANTILE_GRID)))
})

test_that("both extractors emit exactly the grid", {
  draws <- tibble::tibble(
    team = rep(c("A", "B"), each = 200L),
    component = "total",
    .draw = rep(seq_len(200L), 2L),
    value = c(stats::rnorm(200), stats::rnorm(200, 5))
  )
  fb <- .summarise_quantile_band_pfi(draws, c("team", "component"))
  dt <- .summarise_quantile_band_2dt(draws, c("team", "component"))
  expect_setequal(unique(fb$quantile), PUBLISH_QUANTILE_GRID)
  expect_setequal(unique(dt$quantile), PUBLISH_QUANTILE_GRID)
})

test_that("asking for an unstored quantile aborts instead of returning NA", {
  q <- tibble::tibble(
    team = "A", component = "total", location = "avg",
    quantile = PUBLISH_QUANTILE_GRID,
    value = seq_along(PUBLISH_QUANTILE_GRID) * 1.0
  )
  # 1 and 99 are deliberately NOT in the grid (they are the noisiest tails and
  # nothing publishes them).
  expect_error(
    .assert_quantiles_available(q$quantile, c(50L, 99L), "test surface"),
    "99"
  )
  expect_silent(.assert_quantiles_available(q$quantile, c(25L, 50L, 75L), "x"))
})
