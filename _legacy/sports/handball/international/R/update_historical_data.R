box::use(
  R / common / leagues,
  R / common / download_data
)

download_data$update_historical_results(
  leagues$male$euro,
  sex = "male"
)

