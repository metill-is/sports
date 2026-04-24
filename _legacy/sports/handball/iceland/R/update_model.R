box::use(
  R / utils / model_fitting[fit_football_model],
  R / utils / get_model_results[generate_model_results]
)

end_date <- Sys.Date()
sex <- "female"

fit_football_model(
  sex = sex,
  refresh = 100,
  iter_warmup = 1000,
  iter_sampling = 1000,
  end_date = end_date
)

generate_model_results(
  sex,
  end_date = end_date
)
