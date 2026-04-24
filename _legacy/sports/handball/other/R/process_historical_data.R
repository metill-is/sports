box::use(
  R / utils / leagues,
  R / utils / process_data
)


for (league in leagues$leagues) {
  process_data$process_leagues_historical_data(
    league
  )
}
