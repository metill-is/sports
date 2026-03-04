box::use(
  R / utils / leagues,
  R / utils / model_fitting,
  R / utils / get_model_results
)

box::use(
  here[here]
)

end_date <- Sys.Date()


for (league in leagues$leagues) {
  for (sex in league$sexes) {
    if (
      file.exists(here(
        "results",
        league$country,
        sex,
        end_date,
        "posterior_goals.csv"
      ))
    ) {
      next
    }
    model_fitting$fit_model(
      country = league$country,
      sex = sex,
      end_date = end_date
    )
    get_model_results$generate_model_results(
      country = league$country,
      sex = sex,
      end_date = end_date
    )
  }
}
