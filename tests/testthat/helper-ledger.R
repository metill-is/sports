# Plant a zero-byte Parquet fragment in one ledger partition. read_table()
# then fails to open the partition (arrow: "Could not open Parquet input
# source"), which is the audit 2026-11-03 finding-A trigger: a truncated or
# non-Parquet file under data/decisions/ledger/.
corrupt_ledger_fragment <- function(root, sport = "football",
                                    country = "iceland") {
  dir <- fs::path(
    root, "decisions", "ledger",
    paste0("sport=", sport), paste0("country=", country)
  )
  fs::dir_create(dir)
  path <- fs::path(dir, "part-corrupt.parquet")
  fs::file_create(path)
  path
}
