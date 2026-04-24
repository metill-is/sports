# One-off migration to add variant= level to predictions_archive/.
#
# Before migration:
#   store/predictions_archive/sport=X/country=Y/sex=Z/fit_date=D/predictions.parquet
# After migration:
#   store/predictions_archive/sport=X/country=Y/sex=Z/variant=bvp_noinflation/fit_date=D/predictions.parquet
#
# Rationale: the 2026-04-23 betting-PnL audit introduced multi-variant archive reads;
# existing production snapshots all belong to the live BVP no-inflation model and are
# moved under variant=bvp_noinflation/.
#
# Invocation: Rscript Sports/R/storage/migrate_variant_partition.R

suppressPackageStartupMessages({
  library(here)
  library(fs)
})

sports_dir <- here::here()
archive_root <- file.path(sports_dir, "store", "predictions_archive")

if (!dir.exists(archive_root)) {
  cat("No archive to migrate (", archive_root, "does not exist).\n")
  quit(status = 0)
}

fit_date_dirs <- fs::dir_ls(archive_root,
  recurse = TRUE, type = "directory",
  regexp = "fit_date=\\d{4}-\\d{2}-\\d{2}$"
)

if (length(fit_date_dirs) == 0) {
  cat("No fit_date partitions found - nothing to migrate.\n")
  quit(status = 0)
}

migrated <- 0
for (old_dir in fit_date_dirs) {
  parent <- dirname(old_dir)
  fit_segment <- basename(old_dir)

  # Skip already-migrated leaves (parent contains variant=).
  if (grepl("variant=", parent, fixed = TRUE)) {
    next
  }

  new_parent <- file.path(parent, "variant=bvp_noinflation")
  new_dir <- file.path(new_parent, fit_segment)

  if (!dir.exists(new_parent)) dir.create(new_parent, recursive = TRUE)

  fs::dir_copy(old_dir, new_dir, overwrite = FALSE)
  fs::dir_delete(old_dir)

  cat("Migrated", old_dir, "->", new_dir, "\n")
  migrated <- migrated + 1
}

cat("Migration complete: ", migrated, "fit_date partitions moved under variant=bvp_noinflation/.\n")
