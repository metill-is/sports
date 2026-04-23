# 2-way variant disagreement tagging.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

#' Classify disagreement between two variants on a per-bet basis.
#'
#' @param df Long frame with match_id, market, outcome, line, variant, stake, pass.
#' @param variants Length-2 char vector naming the two variants to compare.
#' @return Wide frame: one row per (match_id, market, outcome, line) where at least one variant passed.
#'         Columns: <v1>_stake, <v2>_stake, tag.
classify_disagreement <- function(df, variants) {
  stopifnot(length(variants) == 2)

  v1 <- variants[1]
  v2 <- variants[2]

  wide <- df |>
    select(match_id, market, outcome, line, variant, stake, pass) |>
    pivot_wider(
      names_from = variant,
      values_from = c(stake, pass),
      values_fill = list(stake = 0, pass = FALSE)
    )

  stake_v1_col <- paste0("stake_", v1)
  stake_v2_col <- paste0("stake_", v2)
  pass_v1_col <- paste0("pass_", v1)
  pass_v2_col <- paste0("pass_", v2)

  out_stake_v1 <- paste0(v1, "_stake")
  out_stake_v2 <- paste0(v2, "_stake")

  result <- wide |>
    rename(
      !!out_stake_v1 := !!stake_v1_col,
      !!out_stake_v2 := !!stake_v2_col
    ) |>
    mutate(
      tag = dplyr::case_when(
        .data[[pass_v1_col]] & .data[[pass_v2_col]] ~ "both",
        .data[[pass_v1_col]] ~ paste0(v1, "_only"),
        .data[[pass_v2_col]] ~ paste0(v2, "_only"),
        TRUE ~ "neither"
      )
    ) |>
    filter(tag != "neither") |>
    select(match_id, market, outcome, line, all_of(out_stake_v1), all_of(out_stake_v2), tag)

  result
}
