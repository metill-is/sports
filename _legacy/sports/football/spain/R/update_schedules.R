box::use(
  R / common / leagues,
  R / common / download_data,
  R / common / process_data
)


for (league in leagues$leagues) {
  print(paste("Starting download of", league$name))
  download_data$update_current_schedule(league)
}
download_data$update_current_schedule(leagues$leagues$primera_rfef_group_2)
process_data$process_schedule()
