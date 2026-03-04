library(tidyverse)
library(readxl)
library(here)

box::use(
  R / excel_utils[repair_excel_file_mac]
)

#### Div 1 ####
#### Download and fix schedule file ####
url_schedule <- "https://widgets.baskethotel.com/widget-service/export/view/schedule_and_results?api=a0d07178160bf749eb6e5e761fc623fe42e2bb57&season_id=130422&lang=is&month=all&type=schedule_only"
path_schedule <- here("data", "female", "div1", "next_games.xlsx")
download.file(url_schedule, path_schedule, mode = "wb")
# fixed_file <- repair_excel_file_mac(path_schedule)

#### Download and fix 2025-2026 games file ####
url_2026 <- "https://widgets.baskethotel.com/widget-service/export/view/schedule_and_results?api=a0d07178160bf749eb6e5e761fc623fe42e2bb57&season_id=130422&lang=is&month=all&type=results_only"
path_2026 <- here("data", "female", "div1", "2026.xlsx")
download.file(url_2026, path_2026, mode = "wb")
# fixed_file <- repair_excel_file_mac(path_2026)

#### Div 2 ####
#### Download and fix schedule file ####
url_schedule <- "https://widgets.baskethotel.com/widget-service/export/view/schedule_and_results?api=a0d07178160bf749eb6e5e761fc623fe42e2bb57&season_id=130421&lang=is&month=all&type=schedule_only"
path_schedule <- here("data", "female", "div2", "next_games.xlsx")
download.file(url_schedule, path_schedule, mode = "wb")
# fixed_file <- repair_excel_file_mac(path_schedule)

#### Download and fix 2025-2026 games file ####
url_2026 <- "https://widgets.baskethotel.com/widget-service/export/view/schedule_and_results?api=a0d07178160bf749eb6e5e761fc623fe42e2bb57&season_id=130421&lang=is&month=all&type=results_only"
path_2026 <- here("data", "female", "div2", "2026.xlsx")
download.file(url_2026, path_2026, mode = "wb")
# fixed_file <- repair_excel_file_mac(path_2026)

div1 <- here("data", "female", "div1") |>
  list.files(full.names = TRUE, pattern = "^[0-9]+") |>
  map(
    \(x) {
      read_excel(
        x,
        sheet = 1,
        skip = 1,
        col_types = "text"
      ) |>
        mutate(
          timabil = str_sub(x, start = -8, end = -5) |> parse_number()
        )
    }
  ) |>
  list_rbind() |>
  mutate(
    division = 1
  )

div2 <- here("data", "female", "div2") |>
  list.files(full.names = TRUE, pattern = "^[0-9]+") |>
  map(
    \(x) {
      read_excel(
        x,
        sheet = 1,
        skip = 1,
        col_types = "text"
      ) |>
        mutate(
          timabil = str_sub(x, start = -8, end = -5) |> parse_number()
        )
    }
  ) |>
  list_rbind() |>
  mutate(
    division = 2
  )


bind_rows(
  div1,
  div2
) |>
  drop_na() |>
  janitor::clean_names() |>
  select(
    timabil,
    division,
    dags,
    heima = heimalid,
    gestir = gestalid,
    stig
  ) |>
  separate(
    stig,
    into = c("stig_heima", "stig_gestir"),
    sep = "-",
    convert = TRUE
  ) |>
  mutate(
    dags = dmy(dags),
    timabil = year(dags) + 1 * (month(dags) > 7)
  ) |>
  write_csv(
    here("data", "female", "data.csv")
  )


schedule_div1 <- here("data", "female", "div1", "next_games.xlsx") |>
  read_excel(sheet = 1, skip = 1) |>
  janitor::clean_names() |>
  select(
    dags,
    heima = heimalid,
    gestir = gestalid
  ) |>
  drop_na(dags) |>
  mutate(
    dags = dmy(dags),
    division = 1
  )

schedule_div2 <- here("data", "female", "div2", "next_games.xlsx") |>
  read_excel(sheet = 1, skip = 1) |>
  janitor::clean_names() |>
  select(
    dags,
    heima = heimalid,
    gestir = gestalid
  ) |>
  drop_na(dags) |>
  mutate(
    dags = dmy(dags),
    division = 2
  )

bind_rows(
  schedule_div1,
  schedule_div2
) |>
  arrange(dags) |>
  write_csv(
    here("data", "female", "schedule.csv")
  )
