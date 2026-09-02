# Regenerate every committed test fixture for the bb/hb metill-parity harness.
#
#   Rscript tools/make-extract-fixtures.R            # facts + 2DT extracts
#   Rscript tools/make-extract-fixtures.R --golden   # football golden hashes
#
# WHY the odd self-execution guard: in a git worktree `here::here()` resolves to
# the MAIN checkout, so a top-level `devtools::load_all(here::here())` would load
# a different package than the one being edited and silently regenerate fixtures
# against it. The package root is derived from THIS file's own path instead, and
# the load happens only when the file is run as a script. Nothing at top level
# touches the package, so a test can `sys.source()` this file to inspect it.

FIXTURE_SEASONS <- c(2099L, 2100L)
FIXTURE_END_DATE <- as.Date("2100-01-15")
FIXTURE_FIT_DATE <- as.Date("2100-01-01")
FIXTURE_N_DRAWS <- 50L

# Division -> team count. Football BD carries the split-season group sizes from
# config/leagues.yml (male 6/6, female 6/4), so it needs 12 / 10 teams.
# MUST stay identical to tests/testthat/helper-fixture-facts.R.
FIXTURE_DIVISIONS <- list(
  basketball = list(male = c(BD = 6L, `1D` = 6L), female = c(BD = 6L, `1D` = 6L)),
  handball   = list(male = c(OD = 6L, G66 = 6L), female = c(OD = 6L, G66 = 6L)),
  football   = list(
    male   = c(BD = 12L, LD1 = 6L, LD2 = 6L, LD3 = 6L, CUP = 4L),
    female = c(BD = 10L, LD1 = 6L, LD2 = 6L, CUP = 4L)
  )
)

# Deterministic fixture team names for one (sport, sex, division) cell.
fixture_division_teams <- function(sport, sex, division) {
  n <- FIXTURE_DIVISIONS[[sport]][[sex]][[division]]
  stopifnot(!is.null(n))
  sprintf(
    "%s%s %s %02d",
    toupper(substr(sport, 1L, 2L)), toupper(substr(sex, 1L, 1L)),
    division, seq_len(n)
  )
}

# Resolve the package root from this script's own --file= argument. Returns NULL
# when the file was source()d rather than run, which disables self-execution.
.fixture_gen_pkg_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (length(hit) != 1L) return(NULL)
  script <- sub("^--file=", "", hit)
  if (basename(script) != "make-extract-fixtures.R") return(NULL)
  normalizePath(file.path(dirname(script), ".."), mustWork = FALSE)
}

# Regenerate all committed fixtures. Filled in over Tasks 2-8.
make_extract_fixtures <- function(dest = NULL, quiet = FALSE) {
  if (is.null(dest)) {
    root <- .fixture_gen_pkg_root()
    stopifnot(!is.null(root))
    dest <- file.path(root, "tests", "testthat", "fixtures")
  }
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  files <- character()
  bytes <- 0L
  if (!quiet) message("make_extract_fixtures: wrote ", length(files), " files")
  invisible(list(bytes = bytes, files = files))
}

if (sys.nframe() == 0L && !is.null(.fixture_gen_pkg_root())) {
  devtools::load_all(.fixture_gen_pkg_root(), quiet = TRUE)
  make_extract_fixtures()
}
