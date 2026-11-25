# tests/testthat/test-workflow-ih-tournament.R
#
# Pins .github/workflows/ih-tournament.yml, the international handball
# forecast refresh, in the spirit of test-placer-ci-isolation.R. The match
# table comes from a private repository: this public repository must never
# name it, the machines or paths around it, or how it is fetched, and the
# workflow must take the repository only from its secrets.

ih_workflow <- here::here(".github", "workflows", "ih-tournament.yml")

ih_steps <- function() {
  wf <- yaml::read_yaml(ih_workflow)
  wf$jobs$forecast$steps
}

ih_step <- function(name) {
  steps <- ih_steps()
  hit <- vapply(steps, function(s) identical(s$name, name), logical(1))
  expect_equal(sum(hit), 1L, info = name)
  steps[[which(hit)]]
}

test_that("the refresh is dispatched by hand only, one run per championship at a time", {
  skip_if_not(file.exists(ih_workflow))
  wf <- yaml::read_yaml(ih_workflow)
  # YAML 1.1 reads the key `on` as the boolean TRUE.
  on <- wf[["on"]] %||% wf[["TRUE"]]
  expect_named(on, "workflow_dispatch")
  expect_setequal(names(on$workflow_dispatch$inputs), c("tournament", "as_of"))
  expect_setequal(unlist(on$workflow_dispatch$inputs$tournament$options), names(IH_TOURNAMENTS))
  expect_equal(wf$concurrency$group, "ih-tournament-${{ inputs.tournament }}")
  expect_false(isTRUE(wf$concurrency$`cancel-in-progress`))
  fit <- yaml::read_yaml(here::here(".github", "workflows", "fit.yml"))
  expect_false(identical(wf$concurrency$group, fit$concurrency$group))
  expect_identical(wf$jobs$forecast$env$INLA_VERSION, yaml::read_yaml(here::here(".github", "workflows", "inla-engine.yml"))$jobs$engine$env$INLA_VERSION)
})

test_that("the private repository and the table's path come only from secrets, into RUNNER_TEMP", {
  skip_if_not(file.exists(ih_workflow))
  fetch <- ih_step("Fetch the match table")
  expect_identical(fetch$env$IH_DATA_REPO, "${{ secrets.IH_DATA_REPO }}")
  expect_identical(fetch$env$IH_DATA_FILE, "${{ secrets.IH_DATA_FILE }}")
  expect_identical(fetch$env$GH_TOKEN, "${{ secrets.IH_DATA_TOKEN }}")
  expect_match(fetch$run, "::add-mask::$IH_DATA_REPO", fixed = TRUE)
  expect_match(fetch$run, 'dir="$RUNNER_TEMP/', fixed = TRUE)
  expect_match(fetch$run, 'repos/$IH_DATA_REPO/contents/$IH_DATA_FILE" > "$dir/', fixed = TRUE)

  lines <- readLines(ih_workflow, warn = FALSE)
  code <- sub("#.*$", "", lines)
  # Every repository the workflow reaches is either the secret or the platform.
  api <- regmatches(code, regexpr("repos/[^/\"' ]+", code))
  expect_true(all(api == "repos/$IH_DATA_REPO"), info = paste(api, collapse = ", "))
  dispatch <- regmatches(code, regexpr("--repo [^ ]+", code))
  expect_true(all(dispatch == "--repo metill-is/metill-platform"), info = paste(dispatch, collapse = ", "))
  # Nothing is fetched from the web by a browser here: the fetch is not CI's.
  expect_false(any(grepl("chrom", code, ignore.case = TRUE)))
  expect_false(any(grepl("GITHUB_WORKSPACE", code, fixed = TRUE)))
})

test_that("only the championship's publish folder is committed, and the platform is told only of a change", {
  skip_if_not(file.exists(ih_workflow))
  commit <- ih_step("Commit the forecast")
  adds <- regmatches(commit$run, gregexpr("git add[^\n]*", commit$run))[[1]]
  expect_identical(adds, 'git add -- "$pub"')
  expect_match(commit$run, "IH_TOURNAMENTS[[", fixed = TRUE)
  expect_match(commit$run, ".github/scripts/push-with-retry.sh", fixed = TRUE)
  dispatch <- ih_step("Dispatch metill-platform's pull")
  expect_identical(dispatch$`if`, "steps.commit.outputs.changed == 'true'")
  expect_match(dispatch$run, "gh workflow run pull-sports-data.yml --repo metill-is/metill-platform --ref main", fixed = TRUE)
  run <- ih_step("Fit, simulate and write the forecast")
  expect_match(run$run, "scripts/ih/run_tournament.R", fixed = TRUE)
  # The runner refuses to publish a fit that fails ih_fit_check().
  runner <- readLines(here::here("scripts", "ih", "run_tournament.R"), warn = FALSE)
  expect_true(any(grepl("problems <- ih_fit_check(fit)", runner, fixed = TRUE)))
  expect_true(any(grepl("nothing published", runner, fixed = TRUE)))
})

test_that("no tracked file names the private data source, the machines around it or how it is fetched", {
  # WHY hashes: the names themselves must not appear in this public
  # repository, not even here. Each entry is the SHA-256 of a forbidden word
  # (lower case), keyed by its length so that only words of those lengths are
  # hashed.
  forbidden <- c(
    "18" = "337f8e04bb06b8d55fac24582a84d523c39ea8a8ed191e7ce39ee489ee359254",
    "11" = "c709ad27f7e57fe146f0f755adec57696466e63159606c76ff00021b71763a48",
    "20" = "e259cd15e2a36ecf16562b55b04c016c31bede6c614862c886119ae540876706"
  )
  root <- here::here()
  skip_if(Sys.which("git") == "" || !file.exists(file.path(root, ".git")), "not a git checkout")
  files <- suppressWarnings(system2("git", c("-C", shQuote(root), "-c", "core.quotePath=false", "ls-files"), stdout = TRUE))
  skip_if(length(files) == 0L, "no tracked files listed")
  text_ext <- "\\.(R|r|Rmd|qmd|md|yml|yaml|sh|py|toml|txt|csv|json|js|html|css|stan|Rproj)$"
  files <- files[grepl(text_ext, files) & !startsWith(files, "data/")]
  hits <- character()
  for (f in files) {
    path <- file.path(root, f)
    if (!file.exists(path)) next
    # Some fixtures are Latin-1; the forbidden words are ASCII, so bytes that
    # are not valid UTF-8 are dropped before splitting.
    text <- iconv(readLines(path, warn = FALSE), "UTF-8", "UTF-8", sub = "")
    words <- unique(unlist(strsplit(tolower(text), "[^a-z0-9_-]+")))
    words <- words[nchar(words) %in% as.integer(names(forbidden))]
    if (!length(words)) next
    h <- vapply(words, digest::digest, character(1), algo = "sha256", serialize = FALSE)
    if (any(h %in% forbidden)) hits <- c(hits, f)
  }
  expect_identical(hits, character(), info = paste("names a forbidden word:", paste(hits, collapse = ", ")))
})
