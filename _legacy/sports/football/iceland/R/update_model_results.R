source(here::here("R", "male", "get_model_results.R"))
source(here::here("R", "female", "get_model_results.R"))

# Posterior-predictive xG / xPts over played matches. Writes
# results/{sex}/team_expected_by_draw.csv — consumed by export_website_data.R.
source(here::here("R", "utils", "played_match_expected.R"))
aggregate_played_match_expected("male")
aggregate_played_match_expected("female")
