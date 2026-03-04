box::use(
  R / utils / leagues,
  R / utils / download_data
)



for (league in leagues$leagues) {
  print(paste("Starting download of", league$country))
  download_data$read_historical_results(
    league
  )
}

