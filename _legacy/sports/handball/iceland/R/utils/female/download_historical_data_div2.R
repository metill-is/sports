library(tidyverse)
library(rvest)
library(glue)
library(purrr)
library(here)
base_url <- "https://www.hsi.is/tournament/{mot_nr}"

mot_nr <- c(
  "2025" = 7644,
  "2024" = 6980,
  "2023" = 6148,
  "2022" = 5642,
  "2021" = 5263
)

urls <- glue(base_url)


read_page <- function(url) {
  page <- read_html_live(url)

  Sys.sleep(15)

  tables <- page |>
    html_table()

  remove_colnames_from_fields <- function(table) {
    nms <- names(table)

    for (nm in nms) {
      table[[nm]] <- str_replace(table[[nm]], glue("^{nm}"), "")
    }

    table
  }
  if (length(tables) == 2) {
    tbl <- tables[[length(tables)]]
  } else {
    tbl <- tables[[2]]
  }

  page$session$close()
  Sys.sleep(3)

  tbl |>
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
        dmy(locale = "IS_is")
    )
}

dats <- list()

for (i in seq_along(urls)) {
  Sys.sleep(4)
  dats[[i]] <- read_page(urls[i])
}


dats |>
  list_rbind() |>
  mutate(
    season = year(dagsetning) - 1 * (month(dagsetning) < 7),
    division = 2
  ) |>
  write_csv(
    here("data", "female", "historical_div2.csv")
  )
