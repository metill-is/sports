#### Update Results for Both Male and Female Handball Data ####

box::use(
  R / utils / model_fitting[fit_football_model],
  R / utils / get_model_results[generate_model_results]
)

# Set the end date for the analysis
end_date <- Sys.Date()

# Function to update data for a specific sex
update_data_for_sex <- function(sex) {
  cat(paste("Updating", sex, "data...\n"))

  # Source the data download scripts for the specified sex
  source(here::here("R", "utils", sex, "download_newest_data_div1.R"))
  source(here::here("R", "utils", sex, "download_newest_data_div2.R"))

  # Process the data
  source(here::here("R", "utils", sex, "process_data.R"))

  cat(paste("Data update completed for", sex, "\n"))
}

# Function to update model for a specific sex
update_model_for_sex <- function(sex, end_date) {
  cat(paste("Fitting model for", sex, "...\n"))

  # Fit the model
  fit_football_model(
    sex = sex,
    refresh = 100,
    iter_warmup = 1000,
    iter_sampling = 1000,
    end_date = end_date
  )

  # Generate model results
  generate_model_results(
    sex,
    end_date = end_date
  )

  cat(paste("Model update completed for", sex, "\n"))
}

# Main execution
cat("Starting data and model updates for both sexes...\n")

# Update data for both sexes
for (sex in c("female", "male")) {
  cat(paste("\n=== Processing", toupper(sex), "data ===\n"))

  # Update data
  update_data_for_sex(sex)

  # Update model
  update_model_for_sex(sex, end_date)
}

cat("\nAll updates completed successfully!\n")
