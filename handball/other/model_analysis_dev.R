
library(tidyverse)
library(cmdstanr)
library(posterior)
library(here)

get_par <- function(data, par, ...) {
  fit <- here(
    "results", data$country2, data$sex2, Sys.Date() - 1, "fit.rds"
  ) |> 
    read_rds()
  
  fit$summary(par)
}



fits <- crossing(
  country2 = list.files("data"),
  sex2 = c("male")
) |>
  group_by(
    country = country2,
    sex = sex2
  ) |> 
  group_modify(get_par, par = "nu")

iceland <- read_rds("../iceland/results/male/2025-09-24/fit.rds")


  

fits |> 
  ungroup() |> 
  bind_rows(
    iceland$summary("nu") |> 
      mutate(
        country = "iceland",
        sex = "male"
      )
  ) |> 
  mutate(
    country = country |> 
      str_to_title() |> 
      fct_reorder(median)
  ) |> 
  ggplot(aes(median, country)) +
  geom_point() +
  geom_linerange(
    aes(xmin = q5, xmax = q95)
  ) +
  scale_x_continuous(
    guide = guide_axis(cap = "both"),
    # limits = c(0, 0.5)
  ) +
  scale_y_discrete(
    guide = guide_axis(cap = "both")
  ) +
  labs(
    x = "Meðalfjöldi marka í leik",
    y = NULL,
    title = "Matið á meðalfjölda marka sem lið skora í handboltadeildum",
    subtitle = "Meðalfjöldi marka keppandi liða á tímabilinu 2025-2026"
  )

