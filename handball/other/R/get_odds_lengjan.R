box::use(
  R / utils / lengjan_info,
  R / utils / get_1x2_lengjan
)

box::use(
  here[here],
  readr[write_csv]
)

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

results <- list()

for (country in names(lengjan_info)) {
  results[[country]] <- list()
  for (i in seq_along(lengjan_info[[country]]$leagues)) {
    results[[country]][[i]] <- get_1x2_lengjan$get_odds(
      lengjan_info[[country]]$sport,
      lengjan_info[[country]]$country,
      lengjan_info[[country]]$leagues[[i]]$competition
    )
  }
}

for (country in names(results)) {
  if (!dir.exists(here("odds", country))) {
    dir.create(here("odds", country), recursive = TRUE)
  }
  for (i in seq_along(results[[country]])) {
    write_csv(
      results[[country]][[i]],
      here("odds", country, paste0("league_", i, ".csv"))
    )
  }
}
