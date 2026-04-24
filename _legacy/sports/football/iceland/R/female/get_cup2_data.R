library(tidyverse)
library(rvest)
library(glue)
library(here)
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

base_url <- "https://www.ksi.is/mot/stakt-mot/?motnumer={mot_nr}"


mot_nr <- c(
  "2025a" = 49261,
  "2025b" = 49250,
  "2025c" = 49254,
  "2025d" = 49253,
  "2024a" = 47611,
  "2024b" = 47684,
  "2024c" = 47609,
  "2024d" = 47626,
  "2023a" = 46042,
  "2023b" = 46030,
  "2023c" = 46035,
  "2023d" = 46029,
  "2022a" = 43645,
  "2022b" = 43651,
  "2022c" = 43641,
  "2022d" = 43659
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
    division = 4
  ) |>
  write_csv(
    here(
      "data",
      "female",
      "cup2.csv"
    )
  )
