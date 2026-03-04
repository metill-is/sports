cro_win <- 0.53
cro_draw <- 0.08
cro_lose <- 0.39
swe_win <- 0.92
swe_draw <- 0.02
swe_lose <- 0.06
ice_win <- 0.68
ice_draw <- 0.07
ice_lose <- 0.25
slo_win <- 0.25
slo_draw <- 0.07
slo_lose <- 0.68

cro_top <- cro_win + 
  cro_draw * (ice_win * (swe_draw + swe_lose) + ice_draw + ice_lose) +
  cro_lose * (ice_win * swe_lose + ice_draw * swe_draw + ice_lose)

ice_top <- ice_win +
  ice_draw * (swe_draw + swe_lose)

swe_top <- swe_win * (ice_win * (cro_draw + cro_lose) + ice_draw + ice_lose) +
  swe_draw * (ice_draw * cro_lose + ice_lose)

slo_top <- slo_win * (swe_lose)

cro_top + ice_top + swe_top + slo_top

p_top <- c(
  1,
  0.34,
  0.66,
  0,
  0,
  0,
  ######
  cro_top,
  swe_top,
  ice_top,
  slo_top,
  0,
  0
)

library(tidyverse)
library(gtExtras)
crossing(
  "Ísland - Slóvenía" = c("Ísland", "Jafntefli", "Slóvenía"),
  "Króatía - Ungverjaland" = c("Króatía", "Jafntefli", "Ungverjaland"),
  "Svíþjóð - Sviss" = c("Svíþjóð", "Jafntefli", "Sviss")
) |> 
  mutate(
    "Ísland áfram" = case_when(
      `Ísland - Slóvenía` == "Ísland" ~ "Já",
      (`Ísland - Slóvenía` == "Jafntefli") & (`Svíþjóð - Sviss` %in% c("Jafntefli", "Sviss")) ~ "Já",
      TRUE ~ "Nei"
    )
  ) |> 
  filter(
    `Ísland áfram` == "Já"
  ) |> 
  gt() |> 
  tab_header(
    title = "Hvenær kemst íslenska landsliðið áfram í undanúrslit?"
  ) |>
  tab_options(
    table.background.color = "#fdfcfc"
  ) |> 
  gtsave(
    filename = here(
      "results",
      sex,
      "possible_wins.png"
    ),
    expand = c(1, 5, 1, -2)
  )
