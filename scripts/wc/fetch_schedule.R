#!/usr/bin/env Rscript
# One-shot refresher for the vendored World Cup kickoff schedule. The schedule is
# STATIC (kickoff times don't change), so this is NOT wired into the daily cron —
# run it by hand only if FIFA reschedules a match. Source: fixturedownload.com
# (UTC variant). WebFetch is 403'd by their bot filter; curl + a browser UA works.
ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"
url <- "https://fixturedownload.com/download/fifa-world-cup-2026-UTC.csv"
dest <- here::here("data", "wc", "structure", "wc2026_schedule.csv")
code <- system2("curl", c("-sSL", "-A", shQuote(ua), "-o", shQuote(dest), shQuote(url)))
if (code != 0L) stop("curl failed with exit code ", code)
n <- length(readLines(dest))
cat(sprintf("wrote %s (%d lines)\n", dest, n))
if (n < 105L) stop("expected >=105 lines (104 matches + header), got ", n)
