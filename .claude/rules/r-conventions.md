---
paths:
  - "**/*.R"
  - "**/*.r"
---

# R Code Conventions (Workspace-wide)

- Use `box::use()` for module imports. Exception: interactive/fitting scripts may use `library()`.
- Export module functions with `#' @export` roxygen tags.
- Use `here::here()` for all file paths — never hardcode absolute paths.
- Base pipe `|>` preferred over `%>%`.
- Tidyverse style: `snake_case`, verbs for function names.
- Use `arrow::read_parquet()` / `arrow::write_parquet()` for model outputs, CSV for raw input.
- All visualisations use `metill::theme_metill()` as the base ggplot2 theme.
- Icelandic locale: `Sys.setlocale("LC_ALL", "is_IS.UTF-8")` in scripts producing Icelandic text.
- Icelandic number formatting: use `metill::isk()`, `metill::hlutf()`, `metill::tala()` and their `label_*` variants.
- `.` for thousands separator, `,` for decimal (Icelandic convention) — handled by the metill package.
