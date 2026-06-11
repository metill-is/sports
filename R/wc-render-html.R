NULL

#' Render the self-contained World Cup forecast HTML page.
#'
#' Reads the published JSON files and injects them verbatim into the forecast
#' template so the result is a single openable `.html` with no runtime network
#' calls. Each file's JSON is spliced in as raw text (no parse/re-serialise
#' round-trip), so the data the page sees is byte-for-byte the published
#' contract. Regenerable: re-run after each publish to refresh the page.
#'
#' @param root Data root (the `publish/world_cup/karla` JSON lives here).
#' @param template Path to the HTML template (a `__WC_DATA__` token marks the
#'   data injection point).
#' @param out_path Output `.html` path.
#' @return Invisibly the output path.
#' @export
wc_render_html <- function(root = here::here("data"),
                           template = NULL,
                           out_path = here::here("data", "wc", "forecast.html")) {
  if (is.null(template)) {
    template <- system.file("wc", "forecast.template.html", package = "sports")
    if (!nzchar(template) || !file.exists(template)) {
      template <- here::here("inst", "wc", "forecast.template.html")
    }
  }
  pub <- file.path(root, "publish", "world_cup", "karla")
  rd <- function(f) paste(readLines(file.path(pub, f), encoding = "UTF-8", warn = FALSE), collapse = "\n")
  json <- paste0(
    '{"meta":', rd("meta.json"),
    ',"groups":', rd("groups.json"),
    ',"predictions":', rd("predictions.json"),
    ',"placements":', rd("tournament_placements.json"),
    ',"strengths":', rd("team_strengths.json"),
    ',"bracket":', rd("bracket.json"),
    "}"
  )

  tmpl <- paste(readLines(template, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  parts <- strsplit(tmpl, "__WC_DATA__", fixed = TRUE)[[1]]
  if (length(parts) != 2L) {
    cli::cli_abort("Template must contain exactly one {.code __WC_DATA__} token.")
  }
  html <- paste0(parts[1], json, parts[2])

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  con <- file(out_path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(html)), con)

  cli::cli_alert_success("Rendered forecast page to {.path {out_path}}")
  invisible(out_path)
}
