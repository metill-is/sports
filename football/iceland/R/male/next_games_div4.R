library(tidyverse)
library(rvest)
library(glue)
library(here)
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

base_url <- "https://www.ksi.is/mot/stakt-mot/?motnumer={mot_nr}"

mot_nr <- c(
  "2025" = 49312
)

url <- glue(base_url) |> as.character()


page <- read_html(url)

d <- page |>
  html_table() |>
  pluck(-1)

d |>
  mutate(
    dags = str_sub(X1, 6, 16) |> dmy(),
    X2 = str_squish(X2) |>
      str_replace_all(" [0-9]", "") |>
      str_trim() |>
      str_replace_all(" R\\.", "-R\\.") |>
      str_replace_all(" Ó\\.", "-Ó\\.") |> 
      str_replace_all(" S\\.", "-S\\.") |> 
      str_replace_all("Hvíti riddarinn", "Hvíti-riddarinn"),
    .before = everything()
  ) |>
  separate(X2, into = c("heima", "gestir"), sep = " ") |>
  select(-X1, -X3) |>
  mutate_at(
    vars(heima, gestir),
    \(x) str_replace(x, "-", " ")
  ) |>
  write_csv(
    here(
      "data",
      "male",
      "schedule_div4.csv"
    )
  )
