test_that("check_discovery WARNs on an un-actioned modelled proposal", {
  tmp <- withr::local_tempdir()
  findings <- list(
    competitions = list(list(
      sport = "football", country = "iceland", comp_id = "757",
      lengjan_name = "Lengjudeildin", inferred_sex = "male",
      inferred_division = "LD1", classify_confidence = "high",
      modelled = TRUE, status = "new",
      proposed_team_names = tibble::tibble(
        lengjan = "Víkingur Ól.", canonical_guess = "Víkingur Ó.", confidence = "high"
      )
    )),
    unmodelled_offered_count = 0L
  )
  write_discovery_proposal(findings, root = tmp)

  # comp 757 is NOT in the live config for this test's purposes only if the real
  # config lacks it; the check reads load_leagues(). To make the test
  # config-independent, assert the WARN path via a comp_id that cannot be wired.
  findings$competitions[[1]]$comp_id <- "DISCOVERY_TEST_UNWIRED"
  write_discovery_proposal(findings, root = tmp)

  row <- check_discovery(tmp, health_thresholds())
  expect_equal(row$check, "discovery")
  expect_equal(row$status, "WARN")
  expect_match(row$value, "DISCOVERY_TEST_UNWIRED")
})

test_that("check_discovery is OK when no proposals file exists", {
  tmp <- withr::local_tempdir()
  row <- check_discovery(tmp, health_thresholds())
  expect_equal(row$status, "OK")
})

test_that("check_discovery is OK when all proposed comps are already configured", {
  tmp <- withr::local_tempdir()
  cfg_id <- load_leagues()[["football_iceland"]]$lengjan$competitions[[1]]$id
  findings <- list(
    competitions = list(list(
      sport = "football", country = "iceland", comp_id = as.character(cfg_id),
      lengjan_name = "Besta deild karla", inferred_sex = "male",
      inferred_division = "BD", classify_confidence = "high",
      modelled = TRUE, status = "new",
      proposed_team_names = tibble::tibble(
        lengjan = character(0), canonical_guess = character(0), confidence = character(0)
      )
    )),
    unmodelled_offered_count = 0L
  )
  write_discovery_proposal(findings, root = tmp)
  row <- check_discovery(tmp, health_thresholds())
  expect_equal(row$status, "OK")
})
