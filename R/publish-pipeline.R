#' @include publish-profile.R publish-next-games.R extract-iceland-read.R publish-iceland-league.R validate-publish.R
NULL

#' Does an extract partition exist for this cell?
#'
#' Distinguishes "no fit yet" (no partition -> a legitimate skip, but ONLY
#' while the league has no upcoming games; in season it is an abort) from "a
#' fit_date partition exists but won't read" (corrupt / half-written extract ->
#' a loud failure) in publish_one().
#' @noRd
extract_partition_exists <- function(extracts_root, sport, country, sex) {
  cell_dir <- file.path(
    extracts_root,
    paste0("sport=", sport),
    paste0("country=", country),
    paste0("sex=", sex)
  )
  dir.exists(cell_dir) && length(list.dirs(cell_dir, recursive = FALSE)) > 0L
}

#' Publish JSONs for a single (league x sex).
#'
#' Every sport reads the per-fit extraction tree
#' (`data/beliefs/extracts/sport=X/country=Y/sex=Z/fit_date=*/`, the per-cell
#' Parquets emitted by the sport's extractor) and publishes through
#' [`publish_iceland_league()`]. There is no fit-RDS path: the RDS is
#' gitignored and CI never produces one, so a publisher reading it skipped
#' silently on CI forever -- basketball and handball had never published.
#'
#' Takes the static + betting slices separately because the publish profile
#' mirrors `betting.scoring` (tie thresholds); a `lengjan` change shouldn't
#' trigger a republish.
#'
#' After a successful publish, the resulting JSONs are validated against
#' `config/publish-schemas/<sport>/*.schema.json` via `validate_publish_dir()`
#' -- that sport's subtree only, with the sport named explicitly, so arming one
#' sport can never abort another's publish. Failures abort with the list of
#' (file, error) entries.
#'
#' The missing-schema default is FAIL-CLOSED: publishing a sport with no
#' `config/publish-schemas/<sport>/` directory aborts rather than skipping with
#' a note. A sport that can publish entirely unchecked while the pipeline stays
#' green is the shape B4 hid in for months. Set `validate = FALSE` -- the
#' documented escape hatch -- for a synthetic-data test whose payload the schema
#' would reject by design; never loosen the default.
#'
#' @param static Per-league static slice (sport, country, ...).
#' @param betting Per-league `betting` slice.
#' @param key League key (used in messages and for schema validation).
#' @param sex `"male"` or `"female"`.
#' @param root Storage root.
#' @param validate Logical. When `TRUE` (default) the published JSONs are
#'   validated against `config/publish-schemas/<sport>/`. Failures abort.
#' @param end_date Publish cutoff `Date`, forwarded to the per-sport publisher.
#'   Defaults to `Sys.Date()`, which is what production wants; a test or a
#'   replay passes a fixed date so a far-future fixture is not filtered out.
#' @param schema_dir Directory holding `<sport>/*.schema.json`. Overridable so a
#'   test can validate against a staging tree (e.g.
#'   `config/publish-schemas/_draft/`, where a sport's schemas are reviewed
#'   before they are armed) without arming the real one for the live pipeline.
#' @return invisible(NULL).
#' @export
publish_one <- function(static, betting, key, sex,
                        root = here::here("data"),
                        validate = TRUE,
                        end_date = Sys.Date(),
                        schema_dir = here::here("config", "publish-schemas")) {
  league <- static
  league$betting <- betting

  output_root <- file.path(root, "publish")
  extracts_root <- file.path(root, "beliefs", "extracts")
  archive_root <- file.path(root, "beliefs", "archive")

  extracted <- tryCatch(
    read_extracted_iceland(
      league = league,
      sex = sex,
      extracts_root = extracts_root
    ),
    error = function(e) {
      # Re-raise loudly when a fit_date partition EXISTS but won't read (a
      # corrupt / half-written extract) so the publish step fails rather than
      # silently republishing yesterday's JSON.
      if (extract_partition_exists(
        extracts_root, league$sport, league$country, sex
      )) {
        cli::cli_abort(
          c(
            "publish_one({key}/{sex}): extract read failed despite an existing fit_date partition.",
            "x" = conditionMessage(e),
            "i" = "Likely a corrupt or incomplete extract write under {.path {extracts_root}}."
          ),
          call = NULL
        )
      }
      # No partition at all. That is a legitimate skip ONLY while the league
      # has no upcoming games. Once fixtures sit inside has_upcoming_games()'s
      # horizon, a missing extract is the silent breakage this pipeline hid
      # for months (design \u00a713): the fit never ran, or ran without the
      # extractor. The run must go red, not exit 0 -- run_publish_targets()
      # records the abort and football's cells still publish and commit.
      if (has_upcoming_games(league, sex, root = root)) {
        cli::cli_abort(
          c(
            "publish_one({key}/{sex}): no extract partition for an in-season cell.",
            "x" = conditionMessage(e),
            "i" = "Fixtures fall inside has_upcoming_games()'s horizon but no fit has been extracted under {.path {extracts_root}}."
          ),
          call = NULL
        )
      }
      cli::cli_alert_warning(
        "publish_one({key}/{sex}): {conditionMessage(e)} (no extract partition yet and no upcoming games \u2014 skipping)"
      )
      NULL
    }
  )
  if (is.null(extracted)) {
    return(invisible(NULL))
  }
  publish_iceland_league(
    extracted = extracted,
    league = league,
    sex = sex,
    profile = sport_publish_profile(league$sport),
    end_date = end_date,
    root = root,
    output_root = output_root,
    extracts_root = extracts_root,
    archive_root = archive_root
  )
  if (isTRUE(validate)) {
    .validate_or_abort(
      output_root,
      sport = league$sport, key = key, sex = sex, schema_dir = schema_dir
    )
  }
  invisible(NULL)
}

.validate_or_abort <- function(output_root, sport, key, sex,
                              schema_dir = here::here("config", "publish-schemas")) {
  sport_dir <- file.path(output_root, sport)

  # FAIL CLOSED on a missing schema root. It means a broken checkout, not a
  # reason to publish unchecked.
  if (!dir.exists(schema_dir)) {
    cli::cli_abort(
      c(
        "publish_one({key}/{sex}): no schema root at {.path {schema_dir}}.",
        "i" = "A missing schema root is a broken checkout, not a reason to
               publish unvalidated."
      ),
      call = NULL
    )
  }

  # The publisher just ran and wrote nothing for this sport. Worth a line in
  # the log, but not a contract breach -- aborting here would turn every
  # legitimate no-op into a red run.
  if (!dir.exists(sport_dir)) {
    cli::cli_alert_warning(
      "publish_one({key}/{sex}): nothing written under {.path {sport_dir}} -- nothing to validate"
    )
    return(invisible(NULL))
  }

  # FAIL CLOSED on a sport with no schemas. This used to be an informational
  # skip, which is the fail-open end of the contract and the same shape as B4:
  # a sport could be completely unchecked while the pipeline stayed green. All
  # three sports that reach publish_one() are armed as of 2026-09-04.
  # publish_world_cup() (R/wc-publish.R) never calls publish_one() or this
  # function -- verified by grep, and pinned by a test -- so world_cup, which
  # has no schema directory by design, is untouched.
  if (!dir.exists(file.path(schema_dir, sport))) {
    cli::cli_abort(
      c(
        "publish_one({key}/{sex}): no schemas at {.path {file.path(schema_dir, sport)}}.",
        "i" = "Add the sport's schemas (see config/publish-schemas/README.md),
               or pass {.code validate = FALSE} for a synthetic-data test whose
               payload the schema would reject by design."
      ),
      call = NULL
    )
  }

  # The sport's OWN subtree, with the sport named explicitly. Validating
  # `output_root` here meant that arming ANY sport armed it inside every other
  # sport's publish call, so basketball's stale JSON would abort football's
  # publish -- and scripts/05_publish.R has no tryCatch, so the run would die
  # before the commit step. Naming the sport is required: narrowing `dir`
  # alone fails open (see test-publish-schema-arming.R).
  result <- validate_publish_dir(sport_dir, schema_dir = schema_dir, sport = sport)
  if (isTRUE(result$ok)) {
    cli::cli_alert_success(
      "publish_one({key}/{sex}): schema validation passed ({result$n_passed}/{result$n_files} files)"
    )
    return(invisible(NULL))
  }

  cli::cli_alert_danger(
    "publish_one({key}/{sex}): schema validation FAILED ({result$n_failed}/{result$n_files} files)"
  )
  for (e in result$errors) {
    cli::cli_alert_warning(e)
  }
  cli::cli_abort(
    paste0(
      "publish_one({key}/{sex}) produced JSONs that do not match ",
      "config/publish-schemas/{sport}/*.schema.json. See errors above. ",
      "Either fix the publisher or, if the schema is genuinely stale, ",
      "update the schema with a corresponding test."
    )
  )
}

#' Publish every target, isolating a per-cell failure.
#'
#' The publish loop's failure policy, lifted out of `scripts/05_publish.R` so it
#' is testable without spawning `Rscript`.
#'
#' WHY THIS EXISTS, and the asymmetry it removes. `scripts/05_publish.R` called
#' `publish_one()` bare in a loop, and basketball precedes handball precedes
#' football in `config/leagues.yml` order. Once `.validate_or_abort()`'s default
#' inverts to fail-closed, the FIRST basketball or handball schema breach would
#' therefore abort the whole run before football -- live and mid-season with
#' nine publishing cells -- ever republished. One new sport's teething problem
#' must not be able to take the established sport's output down.
#'
#' Failures are collected, not swallowed: the returned `failed` frame is what
#' the script turns into `quit(status = 1L)`. Exit is non-zero when ANY target
#' failed, not only when all did (INT-2) -- an all-failed rule is exactly the
#' warn-and-exit-0 shape B4 hid in for months.
#'
#' `validate`, `end_date` and `schema_dir` are named formals rather than `...`
#' so a replay or a draft-schema caller cannot silently lose one; production
#' gets the same defaults `publish_one()` has.
#'
#' @param targets Tibble of `key`/`sex` rows from `resolve_targets()`.
#' @param leagues Leagues list.
#' @param root Storage root.
#' @param validate,end_date,schema_dir Forwarded to `publish_fn` by name.
#' @param publish_fn Injectable publisher; defaults to [publish_one()].
#' @return `list(published = integer(1), failed = tibble(key, sex, message))`.
#' @export
run_publish_targets <- function(targets, leagues,
                                root = here::here("data"),
                                validate = TRUE,
                                end_date = Sys.Date(),
                                schema_dir = here::here("config", "publish-schemas"),
                                publish_fn = publish_one) {
  published <- 0L
  failed <- list()

  for (i in seq_len(nrow(targets))) {
    row <- targets[i, ]
    league_def <- leagues[[row$key]]
    static <- league_def[c(
      "sport", "country", "sexes", "active", "stan_model", "data_source"
    )]
    betting <- league_def$betting

    cli::cli_h2("{row$key} ({row$sex})")
    ok <- tryCatch(
      {
        publish_fn(
          static, betting, row$key, row$sex,
          root = root, validate = validate,
          end_date = end_date, schema_dir = schema_dir
        )
        TRUE
      },
      error = function(e) {
        cli::cli_alert_danger(
          "publish failed for {row$key} ({row$sex}): {conditionMessage(e)}"
        )
        failed[[length(failed) + 1L]] <<- tibble::tibble(
          key = row$key, sex = row$sex, message = conditionMessage(e)
        )
        FALSE
      }
    )
    if (isTRUE(ok)) published <- published + 1L
  }

  list(
    published = published,
    failed = if (length(failed) == 0L) {
      tibble::tibble(key = character(), sex = character(), message = character())
    } else {
      dplyr::bind_rows(failed)
    }
  )
}
