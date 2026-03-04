#' Process leagues historical data
#' Combines all divisions and years for one country into one dataset
#'
#' @param league_list The league list to process
#' @return A tibble of the processed data
#' @export
process_leagues_historical_data <- function() {
  box::use(
    here[here],
    readr[
      cols,
      col_character,
      col_integer,
      parse_number,
      read_csv,
      write_csv
    ],
    dplyr[
      arrange,
      bind_rows,
      desc,
      filter,
      if_else,
      lag,
      mutate_at,
      mutate,
      n,
      vars,
      select,
      summarise
    ],
    lubridate[year, years],
    purrr[map, list_rbind],
    tidyr[separate, pivot_longer],
    stringr[str_replace, str_sub],
    clock[date_build]
  )

  league_list <- list(
    "eurobasket" = 1,
    "friendly-international" = 2,
    "world-cup" = 3
  )

  # Which sexes are available?
  sexes <- list.files(here("data"), full.names = TRUE, pattern = "male")

  for (sex in sexes) {
    # Which leagues are available?
    leagues <- list.files(
      sex,
      full.names = TRUE,
      pattern = "^[a-zA-Z\\-]+$" # Exclude hidden files
    )

    # Create a list to store data for each league
    league_data <- list()

    for (league in leagues) {
      league_name <- basename(league)

      # Which years are available?
      years <- list.files(league, full.names = TRUE, pattern = "[0-9]{4}$")
      season <- list.files(league, full.names = FALSE, pattern = "[0-9]{4}$")
      files <- here(years, "results.csv")

      # Only process files that exist
      existing_files <- files[file.exists(files)]
      existing_seasons <- season[file.exists(files)]

      if (length(existing_files) == 0) {
        next
      }

      # Read all files
      data <- map(
        existing_files,
        read_csv,
        col_types = cols(
          .default = col_character(),
          home_score = col_integer(),
          away_score = col_integer()
        )
      )
      names(data) <- existing_seasons
      data <- data |>
        list_rbind(names_to = "season") |>
        mutate(
          date = str_sub(date, 1, 5)
        ) |>
        separate(date, into = c("day", "month")) |>
        mutate_at(vars(season:month), parse_number) |>
        mutate(
          year = season,
          date = date_build(year, month, day),
          league = league_list[[league_name]]
        ) |>
        mutate(
          lag_date = lag(date, default = date_build(2025, 12, 12)),
          last_year = 1 * (lag_date < date),
          date = date - years(cumsum(last_year)),
          .by = season
        ) |>
        select(-lag_date, -last_year)

      # Add the data to the list
      league_data[[league]] <- data
    }

    # Combine all data into one tibble
    data <- bind_rows(league_data) |>
      mutate_at(
        vars(home, away),
        \(x) str_replace(x, " W$", "")
      ) |>
      select(
        season,
        division = league,
        date,
        home,
        away,
        home_goals = home_score,
        away_goals = away_score
      ) |>
      arrange(desc(date))

    output_file <- here("data", basename(sex), "data.csv")

    # Create results directory if it doesn't exist
    if (!dir.exists(dirname(output_file))) {
      dir.create(dirname(output_file), recursive = TRUE)
    }

    current_teams <- data |>
      pivot_longer(c(home, away)) |>
      mutate(
        division = if_else(division == 2, Inf, division)
      ) |>
      summarise(
        min_date = min(date),
        max_date = max(date),
        min_division = min(division),
        n = n(),
        .by = value
      ) |>
      filter(
        min_division %in% c(1, 3, 4),
        year(max_date) >= 2023
      )

    data_current <- data |>
      filter(
        home %in% current_teams$value,
        away %in% current_teams$value
      )

    write_csv(data_current, output_file)
  }
}


#' Process schedule for current season
#'
#' @return A tibble of the processed data
#' @export
process_schedule <- function() {
  box::use(
    here[here],
    readr[
      cols,
      col_character,
      col_integer,
      parse_number,
      read_csv,
      write_csv
    ],
    dplyr[
      bind_rows,
      filter,
      if_else,
      mutate_at,
      mutate,
      vars,
      select
    ],
    purrr[map, list_rbind],
    tidyr[separate],
    stringr[str_replace, str_sub],
    clock[date_build]
  )

  # Which sexes are available?
  sexes <- list.files(here("data"), full.names = TRUE)
  # League list in alphabetical order
  league_list <- c(
    1
  )

  for (sex in sexes) {
    # Which leagues are available?
    leagues <- list.files(
      sex,
      full.names = TRUE,
      pattern = "eurobasket"
    )

    files <- here(leagues, "schedule.csv")

    # Only process files that exist
    existing_files <- files[file.exists(files)]
    existing_leagues <- basename(leagues)[file.exists(files)]

    if (length(existing_files) == 0) {
      next
    }

    # Read all files
    data <- map(
      existing_files,
      read_csv,
      col_types = cols(.default = col_character())
    ) |>
      list_rbind(names_to = "league") |>
      mutate(
        league = league_list[league]
      ) |>
      separate(
        date,
        into = c("day", "month", "hour", "minute"),
        convert = TRUE
      ) |>
      mutate(
        year = 2025 + 1 * (month < 8),
        date = clock::date_build(year, month, day)
      ) |>
      select(date, division = league, home, away) |>
      mutate_at(
        vars(home, away),
        \(x) str_replace(x, " W$", "")
      )

    output_file <- here("data", basename(sex), "schedule.csv")

    # Create results directory if it doesn't exist
    if (!dir.exists(dirname(output_file))) {
      dir.create(dirname(output_file), recursive = TRUE)
    }

    write_csv(data, output_file)
  }
}
