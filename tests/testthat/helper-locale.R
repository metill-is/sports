# Match the production locale that every scripts/0N_*.R sets at the top.
# In C locale, yaml::yaml.load() escapes non-ASCII bytes (`c3 ad` -> the
# literal placeholder `<c3><ad>`), which breaks team-name lookups against
# arrow-Parquet-sourced strings.
res <- suppressWarnings(try(
  Sys.setlocale("LC_ALL", "en_US.UTF-8"),
  silent = TRUE
))
if (inherits(res, "try-error") || identical(res, "")) {
  warning(
    "tests/testthat/helper-locale.R: could not set en_US.UTF-8 locale; ",
    "tests touching Icelandic team names may fail. ",
    "(Production pipeline scripts set this at top of every entry script.)",
    call. = FALSE
  )
}
