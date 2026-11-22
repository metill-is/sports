# What the forecast archive would hold had it existed since the publish JSONs
# were first committed: replays every committed version of every cell's
# next_games.json (and the meta.json beside it) through archive_forecasts(),
# oldest first, into a scratch store. Used to size the archive; it writes
# nothing under data/.
#
#   Rscript tools/forecast_archive_history.R <out_dir> [ref]
#
# `ref` defaults to origin/main. Each commit is one publication: its commit
# time is `published_at`, the fit date comes from meta.json, and the
# kick-offs from today's schedule store. Run from the repo root.
options(width = 160)
suppressMessages(devtools::load_all(".", quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
out_dir <- args[[1]]
ref <- if (length(args) >= 2L) args[[2]] else "origin/main"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
store <- file.path(out_dir, "store")
unlink(store, recursive = TRUE)

git <- function(...) system2("git", c(...), stdout = TRUE)
log <- git("log", "--reverse", "--format=%H %ct", ref, "--", "data/publish/*/iceland/*/next_games.json")
commits <- data.frame(
  sha = sub(" .*", "", log),
  time = as.POSIXct(as.numeric(sub(".* ", "", log)), origin = "1970-01-01", tz = "UTC")
)
cells <- unique(dirname(sub("^data/publish/", "", git("ls-tree", "-r", "--name-only", ref, "--", "data/publish"))))
cells <- cells[grepl("^(football|basketball|handball)/iceland/[a-z]+-[a-z0-9]+$", cells)]
cat(sprintf("%d commits, %d cells\n", nrow(commits), length(cells)))

# Every (commit, cell) pair's two files through one `git cat-file --batch`.
want <- expand.grid(cell = cells, sha = commits$sha, stringsAsFactors = FALSE)
spec <- c(
  paste0(want$sha, ":data/publish/", want$cell, "/next_games.json"),
  paste0(want$sha, ":data/publish/", want$cell, "/meta.json")
)
f_in <- tempfile()
f_out <- tempfile()
writeLines(spec, f_in)
system2("git", c("cat-file", "--batch"), stdin = f_in, stdout = f_out)
con <- file(f_out, "rb")
blobs <- vector("list", length(spec))
for (i in seq_along(spec)) {
  header <- readLines(con, n = 1L)
  if (grepl(" missing$", header)) next
  size <- as.integer(strsplit(header, " ")[[1]][3])
  blobs[[i]] <- rawToChar(readBin(con, "raw", size))
  readBin(con, "raw", 1L)
}
close(con)
n <- nrow(want)
want$next_games <- blobs[seq_len(n)]
want$meta <- blobs[n + seq_len(n)]

leagues <- load_leagues()
schedules <- read_table("schedules")
stan_model <- function(sport) leagues[[paste0(sport, "_iceland")]]$stan_model

writes <- list()
counts <- list()
for (j in seq_len(nrow(commits))) {
  here_rows <- want[want$sha == commits$sha[j] & !vapply(want$next_games, is.null, logical(1)), ]
  rows <- list()
  for (k in seq_len(nrow(here_rows))) {
    m <- jsonlite::fromJSON(here_rows$next_games[[k]], simplifyVector = TRUE)$matches
    if (!is.data.frame(m) || nrow(m) == 0L || is.null(here_rows$meta[[k]])) next
    meta <- jsonlite::fromJSON(here_rows$meta[[k]])
    parts <- strsplit(here_rows$cell[k], "/")[[1]]
    sex <- if (startsWith(parts[3], "karla")) "male" else "female"
    pub <- file.path(tempfile(), "publish")
    dir.create(file.path(pub, here_rows$cell[k]), recursive = TRUE)
    writeLines(here_rows$next_games[[k]], file.path(pub, here_rows$cell[k], "next_games.json"))
    rows[[length(rows) + 1L]] <- forecast_archive_rows(
      publish_root = pub, sport = parts[1], country = parts[2], sex = sex,
      cells = parts[3], fit_date = as.Date(meta$fit_date),
      fit_meta = tibble::tibble(),
      schedules = schedules[schedules$sport == parts[1] & schedules$sex == sex, ],
      published_at = commits$time[j], code_commit = commits$sha[j],
      stan_model_default = stan_model(parts[1])
    )
  }
  rows <- dplyr::bind_rows(rows)
  if (nrow(rows) == 0L) next
  before <- list.files(store, pattern = "[.]parquet$", recursive = TRUE, full.names = TRUE)
  before_md5 <- if (length(before)) tools::md5sum(before) else character()
  res <- suppressWarnings(suppressMessages(archive_forecasts(rows, root = store)))
  after <- list.files(store, pattern = "[.]parquet$", recursive = TRUE, full.names = TRUE)
  changed <- after[is.na(before_md5[after]) | before_md5[after] != tools::md5sum(after)]
  if (length(changed)) {
    writes[[length(writes) + 1L]] <- data.frame(sha = commits$sha[j], file = changed, bytes = file.size(changed))
  }
  counts[[length(counts) + 1L]] <- data.frame(
    sha = commits$sha[j], time = commits$time[j],
    appended = nrow(res$appended), unchanged = res$unchanged, refused = nrow(res$refused)
  )
}
writes <- do.call(rbind, writes)
counts <- do.call(rbind, counts)

archive <- read_forecast_archive(store)
files <- list.files(store, pattern = "[.]parquet$", recursive = TRUE, full.names = TRUE)
# Football's season is the calendar year; the 2DT sports' runs Sep to May and
# is named by its closing year, as meta.json names it.
season_of <- function(sport, d) {
  y <- as.integer(format(d, "%Y"))
  y + as.integer(sport != "football" & as.integer(format(d, "%m")) >= 7L)
}
archive$season <- season_of(archive$sport, archive$fit_date)
by_cell <- archive |>
  dplyr::group_by(.data$sport, .data$sex, .data$season) |>
  dplyr::summarise(
    rows = dplyr::n(),
    matches = dplyr::n_distinct(paste(.data$published_to, .data$match_date, .data$home_team, .data$away_team)),
    fit_dates = dplyr::n_distinct(.data$fit_date),
    first_fit = format(min(.data$fit_date)), last_fit = format(max(.data$fit_date)),
    .groups = "drop"
  ) |>
  as.data.frame()
file_meta <- data.frame(
  file = files,
  sport = sub(".*sport=([^/]+)/.*", "\\1", files),
  sex = sub(".*sex=([^/]+)/.*", "\\1", files),
  fit_date = as.Date(sub(".*fit_date=([0-9-]+)/.*", "\\1", files))
)
file_meta$season <- season_of(file_meta$sport, file_meta$fit_date)
by_cell$files <- by_cell$mb_on_disk <- by_cell$mb_git_blobs <- NA_real_
for (i in seq_len(nrow(by_cell))) {
  keep <- file_meta$file[file_meta$sport == by_cell$sport[i] & file_meta$sex == by_cell$sex[i] & file_meta$season == by_cell$season[i]]
  by_cell$files[i] <- length(keep)
  by_cell$mb_on_disk[i] <- sum(file.size(keep)) / 1024^2
  by_cell$mb_git_blobs[i] <- sum(writes$bytes[writes$file %in% keep]) / 1024^2
}
size_of <- function(paths) sum(file.size(paths))

saveRDS(list(ref = ref, commits = commits, counts = counts, writes = writes, by_cell = by_cell), file.path(out_dir, "history.rds"))
cat("\nper cell and season (rows = archived forecasts; matches = distinct published fixtures):\n")
print(by_cell, row.names = FALSE, digits = 3)
cat(sprintf(
  "\ntotal: %d rows, %d files, %.2f MB on disk, %.2f MB of blobs written (what git would store), %d refused\n",
  nrow(archive), length(files), size_of(files) / 1024^2, sum(writes$bytes) / 1024^2, sum(counts$refused)
))
