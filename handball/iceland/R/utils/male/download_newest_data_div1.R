library(tidyverse)
library(rvest)
library(glue)
library(purrr)
library(here)
url <- "https://www.hsi.is/olis-deild-karla-2025-26"

page <- read_html_live(url)

tables <- list()
for (attempt in 1:5) {
  Sys.sleep(2)
  tables <- page |> html_table()
  if (length(tables) >= 2) break
}
if (length(tables) < 2) {
  page$session$close()
  stop("HS<U+00CD> page returned ", length(tables), " table(s), expected >= 2: ", url)
}

remove_colnames_from_fields <- function(table) {
  nms <- names(table)

  for (nm in nms) {
    table[[nm]] <- str_replace(table[[nm]], glue("^{nm}"), "")
  }

  table
}

results <- tables[[2]]
has_schedule <- length(tables) >= 3

page$session$close()
Sys.sleep(3)

results |>
  remove_colnames_from_fields() |>
  janitor::clean_names() |>
  select(
    dagsetning,
    lid,
    nidurstodur
  ) |>
  separate(
    lid,
    into = c("home", "away"),
    sep = " - "
  ) |>
  separate(
    nidurstodur,
    into = c("home_goals", "away_goals"),
    sep = " - ",
    convert = TRUE
  ) |>
  mutate(
    dagsetning = str_sub(dagsetning, 6, -1) |>
      str_replace_all("\\.", "") |>
      str_replace("Sept", "09") |>
      str_replace("okt", "10") |>
      str_replace("n<U+00F3>v", "11") |>
      str_replace("des", "12") |>
      str_replace("jan", "01") |>
      dmy(locale = "IS_is"),
    division = 1
  ) |>
  write_csv(
    here("data", "male", "current_div1.csv")
  )

if (has_schedule) {
  tables[[3]] |>
    remove_colnames_from_fields() |>
    janitor::clean_names() |>
    select(
      dagsetning,
      lid
    ) |>
    separate(
      lid,
      into = c("home", "away"),
      sep = " - "
    ) |>
    mutate(
      dagsetning = str_sub(dagsetning, 6, -1) |>
        dmy(locale = "IS_is"),
      division = 1
    ) |>
    write_csv(
      here("data", "male", "schedule_div1.csv")
    )
} else {
  tibble(dagsetning = Date(), home = character(), away = character(), division = integer()) |>
    write_csv(
      here("data", "male", "schedule_div1.csv")
    )
}
