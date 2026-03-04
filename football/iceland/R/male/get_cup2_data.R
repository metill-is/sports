library(tidyverse)
library(rvest)
library(glue)
library(here)
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

base_url <- "https://www.ksi.is/mot/stakt-mot/?motnumer={mot_nr}"


mot_nr <- c(
  "2025a" = 49248,
  "2025b" = 49257,
  "2025c" = 49263,
  "2025d" = 49252,
  "2025e" = 49262,
  "2025f" = 49247,
  "2025g" = 49255,
  "2025h" = 49260,
  "2025i" = 49251,
  "2025j" = 49259,
  "2024a" = 47664,
  "2024b" = 47606,
  "2024c" = 47614,
  "2024d" = 47607,
  "2024e" = 47627,
  "2024f" = 47613,
  "2024g" = 47616,
  "2024h" = 47624,
  "2024i" = 47623,
  "2024j" = 47605,
  "2023a" = 46027,
  "2023b" = 46041,
  "2023c" = 46023,
  "2023d" = 46040,
  "2023e" = 46026,
  "2023f" = 46038,
  "2023g" = 46039,
  "2023h" = 46031,
  "2023i" = 46034,
  "2023j" = 46025,
  "2022a" = 43654,
  "2022b" = 43664,
  "2022c" = 43648,
  "2022d" = 43644,
  "2022e" = 43643,
  "2022f" = 43649,
  "2022g" = 43647,
  "2022h" = 43653,
  "2022i" = 43642,
  "2022j" = 43663
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
      "male",
      "cup2.csv"
    )
  )
