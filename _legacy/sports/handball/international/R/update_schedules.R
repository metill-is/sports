box::use(
  R / common / leagues,
  R / common / download_data,
  R / common / process_data
)


print("Starting download of current schedule")
download_data$update_current_schedule(
  league_list = leagues$male$euro,
  sex = "male"
)
process_data$process_schedule(
  sex = "male",
  division_name = "ehf-euro",
  division_number = 3
)
