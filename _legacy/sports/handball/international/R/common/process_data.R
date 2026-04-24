#' Process leagues historical data
#' Combines all divisions and years for one country into one dataset
#'
#' @param sex Character string, either "male" or "female"
#' @return A tibble of the processed data
#' @export
process_leagues_historical_data <- function(sex) {
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

  # Validate input
  if (!sex %in% c("male", "female")) {
    stop("Sex must be either 'male' or 'female'")
  }

  # League mappings differ by sex
  if (sex == "female") {
    league_list <- list(
      "african-championship-women" = 1,
      "asian-championship-women" = 2,
      "ehf-euro-women" = 3,
      "friendly-international-women" = 4,
      "olympic-games-women" = 5,
      "world-championship-women" = 6
    )
  } else {
    # male - alphabetical ordering
    league_list <- list(
      "ehf-euro" = 1,
      "friendly-international" = 2,
      "olympic-games" = 3,
      "world-championship" = 4
    )
  }

  sex_path <- here("data", sex)

  if (!dir.exists(sex_path)) {
    warning(paste("Data directory does not exist for", sex))
    return(invisible(NULL))
  }
  # Which leagues are available?
  leagues <- list.files(
    sex_path,
    full.names = TRUE,
    pattern = "^[a-zA-Z\\-]+$" # Exclude hidden files
  )

  # Create a list to store data for each league
  league_data <- list()

  for (league in leagues) {
    league_name <- basename(league)

    # Skip if league is not in league_list
    if (!league_name %in% names(league_list)) {
      next
    }

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
        last_year = 1 * (date > Sys.Date()),
        date = date - years(last_year),
        .by = season
      ) |>
      select(-lag_date, -last_year)

    # Add the data to the list
    league_data[[league]] <- data
  }

  # Combine all data into one tibble
  if (length(league_data) == 0) {
    warning(paste("No league data found for", sex))
    return(invisible(NULL))
  }

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

  output_file <- here("data", sex, "data.csv")

  # Create results directory if it doesn't exist
  if (!dir.exists(dirname(output_file))) {
    dir.create(dirname(output_file), recursive = TRUE)
  }

  # Determine which divisions represent major tournaments (Olympics and World Championship)
  # For female: divisions 5 (Olympics) and 6 (World Championship)
  # For male: divisions 3 (Olympics) and 4 (World Championship)
  if (sex == "female") {
    major_divisions <- c(3, 5, 6)
  } else {
    # male
    major_divisions <- c(1, 3, 4)
  }

  current_teams <- data |>
    pivot_longer(c(home, away)) |>
    summarise(
      min_date = min(date),
      max_date = max(date),
      max_division = max(division),
      n = n(),
      .by = value
    ) |>
    filter(
      # Only include teams that have played in major tournaments
      # And whose last game was in 2024 or later
      ((max_division %in% major_divisions) & (year(max_date) >= 2025))
    )

  data_current <- data |>
    filter(
      home %in% current_teams$value,
      away %in% current_teams$value
    )

  write_csv(data_current, output_file)
}


#' Process schedule for current season
#'
#' @return A tibble of the processed data
#' @export
process_schedule <- function(sex, division_name, division_number) {
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
    lubridate[year],
    purrr[map, list_rbind],
    tidyr[separate],
    stringr[str_replace, str_sub],
    clock[date_build]
  )

  file <- here("data", sex, division_name, "schedule.csv")

  # Only process files that exist
  if (!file.exists(file)) {
    warning(paste("Schedule file not found for", sex, division_name))
    return(invisible(NULL))
  }

  data <- read_csv(
    file,
    col_types = cols(.default = col_character())
  ) |>
    separate(
      date,
      into = c("day", "month", "hour", "minute"),
      convert = TRUE
    ) |>
    mutate(
      year = year(Sys.Date()),
      date = clock::date_build(year, month, day)
    ) |>
    mutate(
      division = division_number
    ) |>
    select(date, division, home, away) |>
    mutate_at(
      vars(home, away),
      \(x) str_replace(x, " W$", "")
    )

  output_file <- here("data", sex, "schedule.csv")

  # Create results directory if it doesn't exist
  if (!dir.exists(dirname(output_file))) {
    dir.create(dirname(output_file), recursive = TRUE)
  }

  write_csv(data, output_file)
}
