#' Pin R-INLA's MKL code path for this process.
#'
#' R-INLA's Linux build runs on MKL, whose code path follows the CPU, so an
#' Intel and an AMD GitHub runner returned different fits of the same data
#' (PR #120). `MKL_CBWR=COMPATIBLE` was the one setting that made them agree
#' bit for bit, and the owner chose to pin it (2026-11-26). R-INLA fits and
#' draws in child processes, which inherit this process's environment, so
#' setting it once here covers every call. An `MKL_CBWR` already in the
#' environment is left alone, for a deliberate comparison.
#' @noRd
.pin_mkl_cbwr <- function() {
  if (!nzchar(Sys.getenv("MKL_CBWR"))) Sys.setenv(MKL_CBWR = "COMPATIBLE")
  invisible(Sys.getenv("MKL_CBWR"))
}

.onLoad <- function(libname, pkgname) {
  .pin_mkl_cbwr()
}
