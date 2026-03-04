library(tidyverse)
library(rvest)
library(glue)
library(here)
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

base_url <- "https://www.ksi.is/mot/stakt-mot/?motnumer={mot_nr}"

mot_nr <- c(
  "2025" = 49316
)

url <- glue(base_url) |> as.character()


page <- read_html(url)

d <- page |>
  html_table() |>
  pluck(-1)

get_teams <- function(string) {
  splits <- string |>
    str_split("\\r|\\n") |>
    unlist()
  out <- splits[str_detect(splits, "[A-Za-zÞ]")] |> str_squish()
  names(out) <- c("home", "away")
  out
}


d |>
  #filter(
  #  !str_detect(str_to_lower(X2), "úrslit")
  #) |>
  mutate(
    dags = str_sub(X1, 6, 16) |> dmy(),
    teams = map(X2, get_teams),
    .before = everything()
  ) |>
  unnest_wider(teams) |>
  select(dags, heima = home, gestir = away) |>
  write_csv(
    here(
      "data",
      "male",
      "schedule_cup.csv"
    )
  )
