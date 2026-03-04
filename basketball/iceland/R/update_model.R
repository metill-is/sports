box::use(
  R / common / model_fitting[fit_model],
  R / common / get_model_results[generate_model_results]
)

end_date <- Sys.Date()

for (sex in c("male", "female")) {
  fit_model(
    sex = sex,
    refresh = 100,
    iter_warmup = 1000,
    iter_sampling = 1000,
    end_date = end_date
  )

  generate_model_results(sex, end_date = end_date)
}
