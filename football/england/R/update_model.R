box::use(
  R / common / model_fitting[fit_football_model],
  R / common / get_model_results[generate_model_results]
)

fit_football_model(
  sex = "male",
  refresh = 100,
  iter_warmup = 1000,
  iter_sampling = 1000
)

generate_model_results("male")
