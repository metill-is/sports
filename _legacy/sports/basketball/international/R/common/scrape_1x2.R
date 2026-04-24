get_odds <- function(sport, country, competition) {
  box::use(
    glue[glue],
    rvest[read_html_live, html_elements, html_text],
    purrr[map_chr],
    stringr[str_split, str_detect, str_sub, str_c],
    lubridate[dmy],
    tibble[tibble]
  )
  
  base_url <- "https://games.lotto.is/getraunaleikir/lengjan?sport={sport}&country={country}&competition={competition}"
  url <- glue(base_url)

  page <- read_html_live(url)
  Sys.sleep(1)

  if (
    length(page$html_elements("._14isqi70._14isqi7q._14isqi7z.pi7fsa4")) > 0
  ) {
    page$click("._14isqi70._14isqi7q._14isqi7z.pi7fsa4")
  }

  Sys.sleep(1)

  teams <- page |>
    html_elements("._469dd00._469dd0o._469dd0v.lj1n6v9") |>
    html_text()

  teams <- teams |> str_split(" - ")

  home <- teams |> map_chr(1)
  away <- teams |> map_chr(2)

  dates <- page |>
    html_elements("._469dd00._469dd0t._469dd0v") |>
    html_text()

  dates <- dates[str_detect(dates, "\\.")] |>
    str_sub(1, 7) |>
    str_c(" 2025") |>
    lubridate::dmy(locale = "IS_is")

  odds <- page |>
    html_elements(".lj1n6vd") |>
    html_elements(".uazl1c1.uazl1c5.uazl1ca") |>
    html_text()

  idx <- seq_along(home) - 1

  o_home <- odds[1 + 3 * idx]
  o_draw <- odds[2 + 3 * idx]
  o_away <- odds[3 + 3 * idx]

  tibble(
    dates,
    home,
    away,
    o_home,
    o_draw,
    o_away
  )
}
