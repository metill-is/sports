# How far do R-INLA's plug-in fits move between runs on identical inputs? Compares every
# run of descriptives.R kept under <OUT>/repro/run_*/out/descriptives.rds: the repeats
# written by repro_descriptives.sh and the two earlier runs filed beside them.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/repro_descriptives.R")'
options(width = 170)
OUT <- Sys.getenv("HBBB_OUT", "/Users/brynjolfurjonsson/.claude-reviews/sports-hb-bb-v2")
files <- sort(Sys.glob(file.path(OUT, "repro", "run_*", "out", "descriptives.rds")))
runs <- stats::setNames(lapply(files, readRDS), basename(dirname(dirname(files))))
cells <- names(runs[[1]])

tab <- do.call(rbind, lapply(names(runs), function(r) {
  do.call(rbind, lapply(cells, function(cl) {
    x <- runs[[r]][[cl]]
    data.frame(run = r, cell = cl, hfa = x$hfa$mean[1], hfa_sd = x$hfa$sd[1], sigma_m = x$sigma_m, sigma_t = x$sigma_t)
  }))
}))

# Runs whose fitted values are bit-identical form one group; the largest group is the
# reference the others are measured against.
fits <- do.call(rbind, lapply(cells, function(cl) {
  do.call(rbind, lapply(c("margin", "total"), function(resp) {
    col <- if (resp == "margin") "mu_m" else "mu_t"
    hyper <- if (resp == "margin") "hyper_m" else "hyper_t"
    m <- vapply(runs, function(x) x[[cl]]$d[[col]], numeric(nrow(runs[[1]][[cl]]$d)))
    grp <- integer(ncol(m))
    for (j in seq_len(ncol(m))) {
      hit <- which(vapply(seq_len(j - 1), function(i) identical(m[, i], m[, j]), TRUE))
      grp[j] <- if (length(hit) > 0) grp[hit[1]] else max(c(0L, grp)) + 1L
    }
    modal <- as.integer(names(which.max(table(grp))))
    ref <- which(grp == modal)[1]
    q50 <- vapply(runs, function(x) x[[cl]][[hyper]]$q50, numeric(nrow(runs[[1]][[cl]][[hyper]])))
    shift <- apply(abs(q50 - q50[, ref]), 1, max)
    worst <- which.max(shift)
    other <- which.max(abs(q50[worst, ] - q50[worst, ref]))
    data.frame(
      cell = cl, response = resp, n_runs = ncol(m), n_distinct = max(grp), n_modal = sum(grp == modal),
      odd_runs = paste(colnames(m)[grp != modal], collapse = ", "), max_abs_fitted_diff = max(abs(m - m[, ref])),
      hyper_param = runs[[1]][[cl]][[hyper]]$parameter[worst], hyper_q50_modal = q50[worst, ref], hyper_q50_other = q50[worst, other]
    )
  }))
}))
hfa <- do.call(rbind, lapply(cells, function(cl) {
  v <- tab$hfa[tab$cell == cl]
  modal <- v[which.max(tabulate(match(v, v)))]
  data.frame(
    cell = cl, hfa_modal = modal, hfa_other = v[which.max(abs(v - modal))], hfa_min = min(v), hfa_max = max(v),
    hfa_sd = stats::median(tab$hfa_sd[tab$cell == cl])
  )
}))
stored <- readRDS(file.path(OUT, "out", "descriptives.rds"))
stored_is_modal <- all(vapply(cells, function(cl) {
  all(vapply(c("mu_m", "mu_t"), function(col) {
    m <- vapply(runs, function(x) identical(x[[cl]]$d[[col]], stored[[cl]]$d[[col]]), TRUE)
    sum(m) > length(runs) / 2
  }, TRUE))
}, TRUE))

print(format(fits, digits = 4), row.names = FALSE)
print(format(hfa, digits = 6), row.names = FALSE)
cat(sprintf(
  "%d runs, %d fits: %d bit-identical with the most frequent result; stored descriptives.rds is the most frequent result for every fit: %s\n",
  length(runs), sum(fits$n_runs), sum(fits$n_modal), stored_is_modal
))
saveRDS(
  list(runs = names(runs), tab = tab, fits = fits, hfa = hfa, stored_is_modal = stored_is_modal),
  file.path(OUT, "out", "repro_descriptives.rds")
)
