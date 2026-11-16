# Builds a version-1 raw Lengjan odds archive in a temp dir from the committed
# API fixtures (tests/testthat/fixtures/lengjan-api/), for test-lengjan-archive.R.
# Each capture is list(ts, program = <fixture file or NULL>, markets = <fixture
# files>, status = "ok", reason = "baseline"). Returns the archive's path; the
# directory lives as long as `env` (the calling test).
.local_archive <- function(captures, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  fixture <- function(name) testthat::test_path("fixtures", "lengjan-api", name)

  gz_copy <- function(name, rel) {
    dest <- file.path(dir, rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    con <- gzfile(dest, "wb")
    writeBin(readBin(fixture(name), "raw", file.size(fixture(name))), con)
    close(con)
    list(
      file = rel, bytes = file.size(dest),
      sha256 = digest::digest(dest, algo = "sha256", file = TRUE)
    )
  }

  for (cap in captures) {
    ts <- as.POSIXct(cap$ts, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    stamp <- format(ts, "%Y%m%dT%H%M%SZ", tz = "UTC")
    day <- file.path("raw", format(ts, "%Y/%m/%d", tz = "UTC"))

    program <- NULL
    if (!is.null(cap$program)) {
      program <- gz_copy(cap$program, file.path(day, paste0(stamp, "-program.json.gz")))
      program$http_s <- 0.1
      program$n_events <- length(jsonlite::read_json(fixture(cap$program))$events)
    }

    markets <- lapply(seq_along(cap$markets), function(k) {
      name <- cap$markets[[k]]
      entry <- gz_copy(name, file.path(day, sprintf("%s-markets-%03d.json.gz", stamp, k)))
      groups <- jsonlite::read_json(fixture(name))
      ids <- unlist(lapply(groups, function(g) {
        vapply(g$markets, function(m) as.character(m$eventId), character(1))
      }))
      entry$event_ids <- as.list(unique(ids))
      entry[c("file", "event_ids", "bytes", "sha256")]
    })

    line <- jsonlite::toJSON(
      list(
        v = 1L, ts = cap$ts, reason = cap$reason %||% "baseline",
        status = cap$status %||% "ok", program = program, markets = markets,
        n_detail_events = length(unique(unlist(lapply(markets, `[[`, "event_ids")))),
        n_requests = as.integer(!is.null(program)) + length(markets),
        error = NULL
      ),
      auto_unbox = TRUE, null = "null", digits = NA
    )
    manifest <- file.path(dir, "manifest", paste0(format(ts, "%Y-%m", tz = "UTC"), ".jsonl"))
    dir.create(dirname(manifest), recursive = TRUE, showWarnings = FALSE)
    cat(line, "\n", sep = "", file = manifest, append = TRUE)
  }
  dir
}
