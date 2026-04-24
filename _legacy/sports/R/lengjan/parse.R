#' Icelandic date parsing for Lengjan odds pages
#'
#' Lengjan shows dates like "3. mar19:45" or "4. feb 18:30" — this module
#' extracts the "day. month" portion, maps Icelandic month abbreviations to
#' numbers, and infers the year dynamically from Sys.Date().

#' @export
#' Full Icelandic month abbreviation mapping (3-letter as shown on Lengjan)
icelandic_months <- c(
  "jan" = "01", "feb" = "02", "mar" = "03", "apr" = "04",
  "ma\u00ed" = "05", "j\u00fan" = "06", "j\u00fal" = "07", "\u00e1g\u00fa" = "08",
  "sep" = "09", "okt" = "10", "n\u00f3v" = "11", "des" = "12"
)

#' @export
#' Parse Lengjan date strings into Date objects
#' @param date_strings Character vector like c("3. mar19:45", "4. feb 18:30")
#' @return Date vector (same length as input; unparseable entries → NA)
parse_lengjan_dates <- function(date_strings) {
  box::use(
    stringr[str_detect, str_extract, str_replace, str_c, str_trim],
    lubridate[dmy]
  )

  # Extract "day. month_abbrev" pattern (e.g., "3. mar", "12. okt")
  # This handles both "3. mar19:45" (no space) and "4. feb 18:30" (with space)
  cleaned <- str_extract(date_strings, "\\d+\\.\\s*[a-z\u00e1\u00e9\u00ed\u00f3\u00fa\u00f0\u00fe\u00e6]+")

  current_year <- as.integer(format(Sys.Date(), "%Y"))

  # Replace Icelandic month abbreviations with numeric equivalents
  for (abbr in names(icelandic_months)) {
    cleaned <- str_replace(cleaned, abbr, icelandic_months[[abbr]])
  }

  # Append year and parse
  with_year <- str_c(cleaned, " ", current_year)
  parsed <- dmy(with_year, quiet = TRUE)

  # Year boundary: if a parsed date is >6 months in the future,
  # it's likely from the previous year (e.g., scraping in Jan, seeing "des")
  future_cutoff <- Sys.Date() + 180
  past_shift <- !is.na(parsed) & parsed > future_cutoff
  if (any(past_shift)) {
    with_year_prev <- str_c(cleaned[past_shift], " ", current_year - 1L)
    parsed[past_shift] <- dmy(with_year_prev, quiet = TRUE)
  }

  parsed
}
