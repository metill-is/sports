box::use(
  R / common / leagues,
  R / common / download_data,
  R / common / process_data
)


print("Starting download of current schedule")
download_data$update_current_schedule(leagues$eurobasket)

process_data$process_schedule()
