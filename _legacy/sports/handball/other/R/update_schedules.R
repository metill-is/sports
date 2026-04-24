box::use(
  R / utils / leagues,
  R / utils / download_data,
  R / utils / process_data
)


for (league in leagues$leagues) {
  print(paste("Starting download of", league$country))
  download_data$update_current_schedule(league)
}

for (league in leagues$leagues) {
  process_data$process_schedule(league$country)
}


