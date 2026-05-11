#' @include storage.R
NULL

#' Commit local changes under `data/decisions/ledger/` to git.
#'
#' Stages and commits only paths under `data/decisions/ledger/` in the
#' given working tree. Intended for the script-layer entrypoints
#' (`scripts/place_bets.R`, `scripts/06_settle.R`) — `place_bets()` and
#' `settle_ledger()` themselves never touch git, so library callers and
#' the test suite are unaffected.
#'
#' The path restriction (`-- data/decisions/ledger/`) means unrelated
#' working-tree WIP is never swept into the same commit.
#'
#' Real-money rows (L1 invariant) must never sit uncommitted: a later
#' `git reset` would silently destroy them. See commit 121710d for the
#' 2026-05-08 incident that motivated this guard.
#'
#' @param root Path to the git working tree (typically `here::here()`).
#' @param message Commit message.
#' @return A list with elements:
#'   \describe{
#'     \item{`status`}{One of `"committed"`, `"nothing_to_commit"`, or
#'       `"failed"`. Callers should treat `"failed"` as fatal — a
#'       successful placement or settlement that isn't committed is the
#'       data-loss scenario this exists to prevent.}
#'     \item{`message`}{Commit message used (or attempted).}
#'     \item{`output`}{Captured stdout+stderr from git for diagnostics.}
#'   }
#' @export
commit_ledger_changes <- function(root, message) {
  stopifnot(is.character(root), length(root) == 1L, nzchar(root))
  stopifnot(is.character(message), length(message) == 1L, nzchar(message))

  ledger_rel <- "data/decisions/ledger"

  status_out <- .git_run(
    c("-C", root, "status", "--porcelain", "--", ledger_rel)
  )
  if (!status_out$ok) {
    return(list(
      status = "failed",
      message = message,
      output = status_out$output
    ))
  }
  if (length(status_out$lines) == 0L) {
    return(list(
      status = "nothing_to_commit",
      message = message,
      output = ""
    ))
  }

  add_out <- .git_run(c("-C", root, "add", "--", ledger_rel))
  if (!add_out$ok) {
    return(list(status = "failed", message = message, output = add_out$output))
  }

  diff_out <- .git_run(
    c("-C", root, "diff", "--cached", "--name-only", "--", ledger_rel)
  )
  if (!diff_out$ok) {
    return(list(status = "failed", message = message, output = diff_out$output))
  }
  if (length(diff_out$lines) == 0L) {
    return(list(
      status = "nothing_to_commit",
      message = message,
      output = ""
    ))
  }

  commit_out <- .git_run(
    c("-C", root, "commit", "-m", message, "--", ledger_rel)
  )
  if (!commit_out$ok) {
    return(list(status = "failed", message = message, output = commit_out$output))
  }

  list(status = "committed", message = message, output = commit_out$output)
}

#' Run git and capture output uniformly.
#'
#' `system2(..., stdout = TRUE, stderr = TRUE)` routes through `/bin/sh`
#' (via `system(intern = TRUE)` under the hood), so each arg must be
#' shell-quoted. Commit messages of the form `data(ledger): ...` hit this
#' because of the parens.
#'
#' @keywords internal
#' @noRd
.git_run <- function(args) {
  quoted <- vapply(args, shQuote, character(1))
  out <- tryCatch(
    suppressWarnings(system2(
      "git", quoted,
      stdout = TRUE, stderr = TRUE
    )),
    error = function(e) {
      structure(conditionMessage(e), status = 127L)
    }
  )
  status <- attr(out, "status")
  status <- if (is.null(status)) 0L else as.integer(status)
  list(
    ok = identical(status, 0L),
    status = status,
    lines = as.character(out),
    output = paste(as.character(out), collapse = "\n")
  )
}
