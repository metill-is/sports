box::use(
  R / common / leagues,
  R / common / process_data
)

# Process all leagues historical data
print("Processing historical data for all Norwegian football leagues...")
process_data$process_leagues_historical_data(
  min_div = 4,
  last_year = 2024
)

print(
  "Processing completed. Check data/male/data.csv for output."
)
