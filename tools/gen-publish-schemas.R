# Render config/publish-schemas/<sport>/*.schema.json from
# config/publish-schemas/_base/ + config/publish-schemas/_delta/<sport>/.
#
#   Rscript tools/gen-publish-schemas.R
#
# WHY generated. The three sports share a contract that is ~90% identical. Hand
# maintenance offers only two shapes, both bad: three copies that drift
# invisibly until the platform 404s, or one file promoted to the ROOT of
# config/publish-schemas/ -- which both resolvers treat as the fallback for
# EVERY sport, world_cup included. Generating makes the shared part literally
# one file and each sport's divergence an explicit, reviewable patch.
#
# THE MANIFEST IS THE DIRECTORY. `_delta/<sport>/<name>.json` existing is what
# declares that <sport> emits <name>.json. There is no list to keep in sync. An
# empty `{}` delta is legal and means "identical to base".
#
# RFC 7386 (JSON Merge Patch) semantics, and the trap: objects merge
# recursively, a `null` value DELETES the key, and arrays and scalars replace
# WHOLESALE. So a delta that narrows a `required` array must restate the whole
# array -- forgetting an entry silently RELAXES that sport's contract, and no
# amount of generator cleverness can detect it. Review every delta that
# mentions `required`.
#
# WHY `_base/<name>.json` and NOT `_base/<name>.schema.json`: both resolvers
# look for `<root>/<sport>/<name>.schema.json` and then `<root>/<name>.schema.json`.
# Naming the base files `*.schema.json` makes `_base` resolve as if it were a
# sport -- .resolve_schema_path(root, "_base", "meta.json") returns
# _base/meta.schema.json. Nothing can actually reach that today (a publish
# JSON's first path segment is the sport, written by the publisher), but the
# `_base is inert` claim would be false and the test asserting it would have to
# be weakened. Dropping the `.schema` infix makes the claim true instead, and
# matches `_delta/<sport>/<name>.json`.
#
# ASCII IS ENFORCED, not merely preferred. jsonlite::toJSON() renders a UTF-8
# em-dash as the literal seven-character string `<U+2014>` even when Encoding()
# already reports "UTF-8". A single non-ASCII character in a `_base`
# description would therefore be silently corrupted into all three rendered
# sports at once. The guard below aborts instead.
#
# WHY the odd self-execution guard: in a git worktree here::here() resolves to
# the MAIN checkout, so a top-level render would write into a different tree
# than the one being edited. The package root is derived from THIS file's own
# --file= path instead, and the render happens only when the file is run as a
# script -- nothing at top level touches the filesystem, so a test can
# sys.source() this file to reach gen_publish_schemas() directly.

# Sports rendered straight into config/publish-schemas/<sport>/, i.e. ARMED:
# both validators resolve their schemas and a non-conforming payload fails
# closed. Anything with a `_delta/` directory but not named here renders into
# config/publish-schemas/_draft/<sport>/ instead, which resolves in NEITHER
# validator -- that is how a sport's schemas get committed, reviewed and
# rsynced to metill-platform while staying completely inert. Arming a sport is
# adding it here plus one `git mv`.
SCHEMA_ARMED_SPORTS <- c("football", "basketball", "handball")

.SCHEMA_DRAFT_DIR <- "_draft"

# RFC 7386 JSON Merge Patch.
#
# read_json(simplifyVector = FALSE) maps a JSON object to a NAMED list, an
# array to an UNNAMED list, a scalar to a length-1 atomic vector and null to
# NULL. An empty JSON object and an empty JSON array both read as `list()`;
# this treats that ambiguous case as an object, because an empty `{}` delta is
# the common case and "replace with an empty array" has no use here. A delta
# needing a literal empty array must be expressed some other way.
.is_json_object <- function(x) {
  is.list(x) && (length(x) == 0L || !is.null(names(x)))
}

.merge_patch <- function(base, patch) {
  if (!.is_json_object(patch)) {
    return(patch)
  }
  if (!.is_json_object(base)) {
    base <- stats::setNames(list(), character())
  }
  for (nm in names(patch)) {
    value <- patch[[nm]]
    if (is.null(value)) {
      base[[nm]] <- NULL
    } else if (nm %in% names(base)) {
      # Replace in place, so base key order is preserved.
      base[[nm]] <- .merge_patch(base[[nm]], value)
    } else {
      # Delta-only keys append, which keeps the render deterministic.
      base[[nm]] <- .merge_patch(NULL, value)
    }
  }
  base
}

.assert_schema_ascii <- function(text, what) {
  bytes <- as.integer(charToRaw(text))
  bad <- bytes[!(bytes == 9L | bytes == 10L | bytes == 13L |
    (bytes >= 32L & bytes <= 126L))]
  if (length(bad) > 0L) {
    stop(
      "Non-ASCII byte(s) in ", what, ": ",
      paste(sprintf("0x%02x", unique(bad)), collapse = " "),
      ".\njsonlite::toJSON() renders a UTF-8 em-dash as the literal string ",
      "<U+2014>, so a non-ASCII character here would be silently corrupted ",
      "into every rendered sport. Transliterate it (-- for an em-dash, x for ",
      "a multiplication sign, plain ASCII for Icelandic letters).",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.read_schema_json <- function(path) {
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  .assert_schema_ascii(text, path)
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

.render_schema_json <- function(x) {
  paste0(
    jsonlite::toJSON(x, auto_unbox = TRUE, pretty = 2, null = "null"),
    "\n"
  )
}

# Sports that have a `_delta/<sport>/` directory, armed or not.
schema_delta_sports <- function(source_dir) {
  delta_root <- file.path(source_dir, "_delta")
  if (!dir.exists(delta_root)) {
    return(character())
  }
  sort(basename(list.dirs(delta_root, recursive = FALSE)))
}

#' Render `_base` + `_delta/<sport>` into `<out_dir>/<sport>/<name>.schema.json`.
#'
#' @param source_dir config/publish-schemas (holding `_base/` and `_delta/`).
#' @param out_dir Where the `<sport>/` directories are written.
#' @param sports Which sports to render; NULL = every sport with a delta dir.
#' @return Character vector of the paths written.
gen_publish_schemas <- function(source_dir, out_dir, sports = NULL) {
  base_dir <- file.path(source_dir, "_base")
  stopifnot(dir.exists(base_dir))
  if (is.null(sports)) {
    sports <- schema_delta_sports(source_dir)
  }

  written <- character()
  for (sport in sports) {
    delta_dir <- file.path(source_dir, "_delta", sport)
    if (!dir.exists(delta_dir)) {
      stop("No _delta directory for sport '", sport, "'.", call. = FALSE)
    }
    dest <- file.path(out_dir, sport)
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)

    names_here <- sort(sub("[.]json$", "", list.files(delta_dir, pattern = "[.]json$")))
    for (nm in names_here) {
      base_path <- file.path(base_dir, paste0(nm, ".json"))
      if (!file.exists(base_path)) {
        stop(
          "_delta/", sport, "/", nm, ".json has no _base/", nm,
          ".json to patch.",
          call. = FALSE
        )
      }
      merged <- .merge_patch(
        .read_schema_json(base_path),
        .read_schema_json(file.path(delta_dir, paste0(nm, ".json")))
      )
      text <- .render_schema_json(merged)
      .assert_schema_ascii(text, paste0("rendered ", sport, "/", nm, ".schema.json"))
      target <- file.path(dest, paste0(nm, ".schema.json"))
      writeBin(charToRaw(text), target)
      written <- c(written, target)
    }

    # The delta directory IS the manifest, so a rendered file with no delta
    # behind it is a leftover, not a contract.
    stale <- setdiff(
      list.files(dest, pattern = "[.]schema[.]json$"),
      paste0(names_here, ".schema.json")
    )
    for (f in stale) unlink(file.path(dest, f))
  }
  written
}

# Resolve the package root from this script's own --file= argument. Returns
# NULL when the file was sourced rather than run, which disables the render.
.schema_gen_pkg_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (length(hit) != 1L) {
    return(NULL)
  }
  script <- sub("^--file=", "", hit)
  if (basename(script) != "gen-publish-schemas.R") {
    return(NULL)
  }
  normalizePath(file.path(dirname(script), ".."), mustWork = FALSE)
}

local({
  root <- .schema_gen_pkg_root()
  if (is.null(root)) {
    return(invisible(NULL))
  }
  src <- file.path(root, "config", "publish-schemas")
  all_sports <- schema_delta_sports(src)
  armed <- intersect(SCHEMA_ARMED_SPORTS, all_sports)
  draft <- setdiff(all_sports, armed)

  out <- gen_publish_schemas(src, src, sports = armed)
  if (length(draft) > 0L) {
    out <- c(out, gen_publish_schemas(
      src, file.path(src, .SCHEMA_DRAFT_DIR),
      sports = draft
    ))
  }
  cat("Rendered", length(out), "schema file(s).\n")
  for (p in out) cat("  ", sub(paste0("^", root, "/"), "", p), "\n", sep = "")
})
