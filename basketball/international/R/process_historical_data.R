box::use(
  R / common / leagues,
  R / common / process_data
)

# Process all leagues historical data
print("Processing historical data for all international basketball leagues...")
process_data$process_leagues_historical_data()

print(
  "Processing completed. Check data/male/data.csv for output."
)
