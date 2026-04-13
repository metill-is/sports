library(tidyverse)
library(rvest)
library(glue)
library(purrr)
library(here)
url <- "https://www.hsi.is/tournament/8437"

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

n_tables <- length(tables)

results <- tables[[n_tables - 1]]
schedule <- tables[[n_tables]]

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
      dmy(locale = "IS_is"),
    division = 3
  ) |>
  write_csv(
    here("data", "male", "current_cup.csv")
  )

schedule |>
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
    division = 3
  ) |>
  write_csv(
    here("data", "male", "schedule_cup.csv")
  )
