box::use(
  R / common / leagues,
  R / common / download_data
)

for (league in leagues$leagues) {
  print(paste("Starting download of", league$name))
  download_data$update_historical_results(
    league
  )
}
