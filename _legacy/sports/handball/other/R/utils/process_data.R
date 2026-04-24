#' Process leagues historical data
#' Combines all divisions and years for one country into one dataset
#'
#' @param league_list The league list to process
#' @return A tibble of the processed data
#' @export
process_leagues_historical_data <- function(league_list) {
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
      mutate_at,
      mutate,
      vars,
      if_else,
      select,
      arrange,
      desc
    ],
    purrr[map, list_rbind],
    tidyr[separate],
    stringr[str_replace, str_sub],
    clock[date_build]
  )
  
  country <- league_list$country
  
  # Which sexes are available?
  sexes <- list.files(here("data", country), full.names = TRUE)
  
  for (sex in sexes) {
    # Which divisions are available?
    divisions <- list.files(
      sex,
      full.names = TRUE,
      pattern = "div[0-9]+$"
    )
    
    # Create a list to store data for each division
    division_data <- list()
    div <- 1
    
    for (division in divisions) {
      # Which years are available?
      years <- list.files(division, full.names = TRUE, pattern = "[0-9]{4}$")
      season <- list.files(division, full.names = FALSE, pattern = "[0-9]{4}$")
      files <- here(years, "results.csv")
      
      # Read all files
      data <- map(
        files,
        read_csv,
        col_types = cols(
          .default = col_character(),
          home_score = col_integer(),
          away_score = col_integer()
        )
      )
      names(data) <- season
      data <- data |>
        list_rbind(names_to = "season") |>
        mutate(
          date = str_sub(date, 1, 5)
        ) |>
        separate(date, into = c("day", "month")) |>
        mutate_at(vars(season:month), parse_number) |>
        mutate(
          year = if_else(month >= 7, season, season + 1),
          date = date_build(year, month, day),
          division = div
        )
      
      # Add the data to the list
      division_data[[division]] <- data
      div <- div + 1
    }
    
    # Combine all data into one tibble
    data <- bind_rows(division_data) |>
      mutate_at(
        vars(home, away),
        \(x) str_replace(x, " W$", "")
      ) |>
      select(season, division, date, home, away, home_score, away_score) |>
      arrange(desc(date))
    
    output_file <- here(sex, "results.csv")
    write_csv(data, output_file)
  }
}


#' Process schedule for current season
#'
#' @param country The country to process
#' @return A tibble of the processed data
#' @export
process_schedule <- function(country) {
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
      mutate_at,
      mutate,
      vars,
      if_else,
      select
    ],
    purrr[map, list_rbind],
    tidyr[separate],
    stringr[str_replace, str_sub],
    clock[date_build]
  )
  
  # Which sexes are available?
  sexes <- list.files(here("data", country), full.names = TRUE)
  
  for (sex in sexes) {
    # Which divisions are available?
    divisions <- list.files(sex, full.names = TRUE, pattern = "div[0-9]+$")
    
    files <- here(divisions, "schedule.csv")
    
    # Read all files
    data <- map(
      files,
      read_csv,
      col_types = cols(.default = col_character())
    ) |>
      list_rbind(names_to = "division") |>
      separate(
        date,
        into = c("day", "month", "hour", "minute"),
        convert = TRUE
      ) |>
      mutate(
        year = dplyr::if_else(
          month >= 7L, as.integer(format(Sys.Date(), "%Y")) - 1L,
          as.integer(format(Sys.Date(), "%Y"))
        ),
        date = clock::date_build(year, month, day)
      ) |>
      select(-year) |>
      select(date, division, home, away) |>
      mutate_at(
        vars(home, away),
        \(x) str_replace(x, " W$", "")
      )
    
    output_file <- here(sex, "schedule.csv")
    write_csv(data, output_file)
  }
}