library(tidyverse)
library(rvest)
library(glue)
library(here)
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

base_url <- "https://www.ksi.is/mot/stakt-mot/?motnumer={mot_nr}"


mot_nr <- c(
  "2025" = 49316,
  "2024" = 47729,
  "2023" = 46272,
  "2022" = 43707,
  "2021" = 42370,
  "2020" = 40933
)

urls <- glue(base_url) |> as.character()
names(urls) <- names(mot_nr)


data <- urls |>
  map(
    \(x) {
      Sys.sleep(2)
      page <- read_html(x)
      tables <- page |>
        html_table()

      rows <- tables |> map_dbl(nrow)

      tables |>
        pluck(which.max(rows))
    }
  )

d <- data |>
  list_rbind(names_to = "timabil") |>
  select(
    timabil,
    dags = X1,
    result = X2
  ) |>
  mutate(
    dags = str_match(dags, "^(.*)\\r.*")[, 2] |>
      str_replace_all("[A-Za-]", "") |>
      dmy(),
    result = str_squish(result),
    result = map(
      result,
      \(x)
        str_match(x, "^(.*) ([0-9]+) (.*) ([0-9]+)$")[, -1] |>
          t() |>
          as.data.frame() |>
          as_tibble() |>
          rename(heima = 1, stig_heima = 2, gestir = 3, stig_gestir = 4)
    )
  ) |>
  unnest_wider(result) |>
  mutate_at(
    vars(timabil, starts_with("stig")),
    parse_number
  )

d |>
  drop_na() |>
  mutate(
    round = row_number(),
    .by = timabil
  ) |>
  mutate(
    finals = 1 * (round == max(round)),
    .by = timabil
  ) |>
  select(
    -round
  ) |>
  write_csv(
    here(
      "data",
      "male",
      "cup.csv"
    )
  )
