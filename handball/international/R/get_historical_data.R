box::use(
  R / common / leagues,
  R / common / download_data
)


for (sex in c("male", "female")) {
  for (league in names(leagues[[sex]])) {
    print(paste("Starting download of", leagues[[sex]][[league]]$name))
    download_data$read_historical_results(
      leagues[[sex]][[league]],
      sex = sex
    )
  }
}
