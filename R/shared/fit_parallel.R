#### Parallel fit phase dispatcher ####
#
# Runs fit steps concurrently across leagues via future::multisession.
# Each worker fits one league×sex combo and writes its own progress
# fragment under ~/.cache/raycast-pipeline/fit-fragments/{master_pid}/.
# The master polls fragments every second and aggregates into the main
# fit-progress.json for Raycast.
#
# Gated behind run.R's `--fit-parallel N` flag. N=1 uses the serial
# path and never loads this file; N>=2 loads and dispatches here.
#
# Contract:
#   - Each fit runs in its own R process (future::multisession)
#   - Worker errors are isolated (tryCatch in worker + value() in master)
#   - Timing cache EMA updates preserved via tracker$record_step()
#   - fit-progress.json schema: same shape as serial + `active` array
#     and `parallel: true` flag; single-active fields mirror the first
#     active fit so existing consumers keep working.

#' Run fit phase in parallel across leagues
#'
#' @param selected Named list of league configs (from leagues.yml)
#' @param resolve_sexes Function(key, league) → character vector of sexes
#' @param sports_dir Absolute path to Sports/ root
#' @param iter_override Integer or NULL (CLI --iter override)
#' @param method_override Character or NULL (CLI --method override)
#' @param cache Timing cache (named list)
#' @param tracker Progress tracker (from create_tracker)
#' @param fit_parallel Integer, concurrency cap (>=2)
#' @param fit_league_keys Character vector of "key_sex" league identifiers
#' @return List of per-fit result records: list(step_key, key, sex, elapsed, ok, error)
#' @export
run_fit_parallel <- function(selected, resolve_sexes, sports_dir,
                             iter_override, method_override, cache,
                             tracker, fit_parallel, fit_league_keys) {
  stopifnot(fit_parallel >= 2)

  for (pkg in c("future", "progressr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf(
        "--fit-parallel requires '%s'. Install with: install.packages(\"%s\")",
        pkg, pkg
      ))
    }
  }

  # Build work items
  work_items <- list()
  for (key in names(selected)) {
    league <- selected[[key]]
    for (sex in resolve_sexes(key, league)) {
      step_key <- paste("fit", key, sex, sep = "_")
      work_items[[length(work_items) + 1]] <- list(
        key = key,
        sex = sex,
        league = league,
        step_key = step_key,
        label = paste(key, sex),
        iter_warmup = iter_override %||% league$iter_warmup,
        iter_sampling = iter_override %||% league$iter_sampling,
        method = method_override %||% league$method,
        expected_duration = cache[[step_key]],
        sports_dir = sports_dir
      )
    }
  }

  # Fragment directory (scoped to master PID so concurrent pipeline runs don't collide)
  pid <- Sys.getpid()
  fragment_dir <- path.expand(
    file.path("~/.cache/raycast-pipeline/fit-fragments", as.character(pid))
  )
  dir.create(fragment_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(fragment_dir, recursive = TRUE), add = TRUE)

  for (i in seq_along(work_items)) {
    work_items[[i]]$fragment_path <- file.path(
      fragment_dir, paste0(work_items[[i]]$step_key, ".json")
    )
  }

  old_plan <- future::plan(future::multisession, workers = fit_parallel)
  on.exit(future::plan(old_plan), add = TRUE)

  cat(sprintf(
    "\nFit phase: dispatching %d fits with concurrency %d\n\n",
    length(work_items), fit_parallel
  ))

  # Detach the worker's closure env from fit_parallel.R's source-local scope
  # so future's serialisation doesn't pick up unrelated siblings.
  worker_fn <- fit_worker_impl
  environment(worker_fn) <- globalenv()

  futures <- lapply(work_items, function(w) {
    future::future(
      worker_fn(w),
      seed = TRUE,
      globals = list(w = w, worker_fn = worker_fn),
      packages = c("jsonlite", "here", "withr")
    )
  })

  # Poll for completion; aggregate fragments each tick
  results <- vector("list", length(futures))
  pending <- seq_along(futures)

  while (length(pending) > 0) {
    aggregate_fit_fragments(fragment_dir, tracker, fit_league_keys)

    just_resolved <- integer(0)
    for (i in pending) {
      if (future::resolved(futures[[i]])) {
        w <- work_items[[i]]
        result <- tryCatch(
          future::value(futures[[i]]),
          error = function(e) {
            list(
              step_key = w$step_key, key = w$key, sex = w$sex,
              elapsed = NA_real_, ok = FALSE,
              error = conditionMessage(e)
            )
          }
        )
        tracker$record_step(
          step_key = result$step_key,
          label = paste("fit:", w$key, w$sex),
          elapsed = result$elapsed %||% 0,
          status = if (isTRUE(result$ok)) "OK" else "FAILED"
        )
        if (!isTRUE(result$ok) && !is.null(result$error)) {
          cat(sprintf("         %s\n", result$error))
        }
        results[[i]] <- result
        just_resolved <- c(just_resolved, i)
      }
    }
    pending <- setdiff(pending, just_resolved)

    if (length(pending) > 0) Sys.sleep(1)
  }

  aggregate_fit_fragments(fragment_dir, tracker, fit_league_keys)

  results
}


#' Worker body: fit one league×sex in a fresh R session
#'
#' Self-contained — does not reference any symbols from fit_parallel.R's
#' enclosing env except the `w` argument. This matters because future's
#' global-detection may not export local-scope `source()`-defined helpers
#' to the worker reliably. Everything the worker needs is either passed
#' through `w`, loaded via `source()` within the worker, or defined
#' inline here.
#'
#' @param w Work item (list from run_fit_parallel)
#' @return list(step_key, key, sex, elapsed, ok, error)
#' @keywords internal
fit_worker_impl <- function(w) {
  Sys.setlocale("LC_ALL", "is_IS.UTF-8")
  setwd(w$sports_dir)
  here::i_am(".here")

  write_fragment_local <- function(path, data) {
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    tmp <- paste0(path, ".tmp.", Sys.getpid())
    jsonlite::write_json(data, tmp, auto_unbox = TRUE, pretty = FALSE)
    file.rename(tmp, path)
  }

  worker_env <- environment()
  source(file.path(w$sports_dir, "R", "shared", "stan_progress_handler.R"),
    local = worker_env
  )

  step_fit_env <- new.env(parent = globalenv())
  source(file.path(w$sports_dir, "R", "pipeline", "step_fit.R"), local = step_fit_env)

  # Scoped progressr context. Using with_progress() instead of
  # handlers(global = TRUE) because the latter fails inside future
  # workers with "should not be called with handlers on the stack"
  # — future sets up signal handlers that progressr's global-install
  # check doesn't like. with_progress scopes to this call only.
  run_fit_with_progress <- function() {
    if (requireNamespace("cmdstanr", quietly = TRUE) &&
      exists("register_default_progress_handler", where = asNamespace("cmdstanr")) &&
      requireNamespace("progressr", quietly = TRUE) &&
      exists("stan_fragment_handler", envir = worker_env)) {
      options(progressr.enable = TRUE)
      progressr::with_progress(
        handlers = worker_env$stan_fragment_handler(
          fragment_path = w$fragment_path,
          league_key = w$step_key,
          label = w$label
        ),
        expr = step_fit_env$run_fit_step(
          league = w$league, sex = w$sex, sports_dir = w$sports_dir,
          iter_warmup = w$iter_warmup, iter_sampling = w$iter_sampling,
          method = w$method,
          fit_model = TRUE,
          generate_results = FALSE, generate_plots = FALSE,
          expected_duration = w$expected_duration
        )
      )
    } else {
      step_fit_env$run_fit_step(
        league = w$league, sex = w$sex, sports_dir = w$sports_dir,
        iter_warmup = w$iter_warmup, iter_sampling = w$iter_sampling,
        method = w$method,
        fit_model = TRUE,
        generate_results = FALSE, generate_plots = FALSE,
        expected_duration = w$expected_duration
      )
    }
  }

  start <- Sys.time()
  out <- tryCatch(
    {
      run_fit_with_progress()
      list(ok = TRUE, error = NULL)
    },
    error = function(e) list(ok = FALSE, error = conditionMessage(e))
  )
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  write_fragment_local(w$fragment_path, list(
    league = w$step_key,
    label = w$label,
    phase = if (isTRUE(out$ok)) "done" else "failed",
    iteration = 0L,
    total_iterations = 0L,
    started_at = format(start, "%Y-%m-%dT%H:%M:%S%z"),
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    status = "done"
  ))

  c(list(step_key = w$step_key, key = w$key, sex = w$sex, elapsed = elapsed), out)
}


#' Aggregate active fragments into main fit-progress.json
#'
#' Reads all per-worker fragment files, retains only those still active
#' (status != "done"), and writes an aggregated payload to the tracker's
#' configured progress_path. Preserves backwards-compatible single-active
#' fields so existing Raycast consumers don't break.
#'
#' @keywords internal
aggregate_fit_fragments <- function(fragment_dir, tracker, fit_leagues) {
  progress_path <- tracker$get_progress_path()
  if (is.null(progress_path)) {
    return(invisible(NULL))
  }
  if (!dir.exists(fragment_dir)) {
    return(invisible(NULL))
  }

  frags <- list.files(fragment_dir, pattern = "\\.json$", full.names = TRUE)

  active <- list()
  for (f in frags) {
    data <- tryCatch(
      jsonlite::fromJSON(f, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(data)) next
    if (!identical(data$status, "done")) {
      active[[length(active) + 1]] <- data
    }
  }

  state <- tracker$get_state()

  payload <- list(
    status = "fitting",
    started_at = format(state$fit_started_at, "%Y-%m-%dT%H:%M:%S%z"),
    total_leagues = length(fit_leagues),
    leagues = fit_leagues,
    active = active,
    completed_leagues = state$completed_fits,
    parallel = TRUE
  )

  if (length(active) > 0) {
    keys <- vapply(active, function(a) a$league %||% "", character(1))
    first_idx <- order(keys)[1]
    first <- active[[first_idx]]
    payload$league <- first$league
    payload$phase <- first$phase
    payload$iteration <- first$iteration
    payload$total_iterations <- first$total_iterations
    payload$league_started_at <- first$started_at
  }

  write_fit_progress(payload, progress_path)
}


#' Atomic JSON fragment write
#'
#' @keywords internal
write_fragment <- function(path, data) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  jsonlite::write_json(data, tmp, auto_unbox = TRUE, pretty = FALSE)
  file.rename(tmp, path)
}

# write_fit_progress is defined in R/shared/progress.R — callers must have
# sourced that file before calling aggregate_fit_fragments.

`%||%` <- function(x, y) if (is.null(x)) y else x
