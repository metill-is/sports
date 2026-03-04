box::use(
  R / common / leagues,
  R / common / process_data
)

# Process all leagues historical data
print("Processing historical data for all international handball leagues...")
for (sex in c("male", "female")) {
  process_data$process_leagues_historical_data(sex = sex)
}

print(
  "Processing completed. Check data/male/data.csv and data/female/data.csv for output."
)
