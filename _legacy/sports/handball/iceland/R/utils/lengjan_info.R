#' @export
#' Iceland - Handball Leagues
#' Men: lengjan?sport=6&country=IS&competition=1269
#' Women: lengjan?sport=6&country=IS&competition=1270
iceland <- list(
  sport = 6,
  country = "IS",
  sexes = c("male", "female"),
  male = list(
    leagues = list(
      "div1" = list(
        competition = "1269"
      )
    )
  ),
  female = list(
    leagues = list(
      "div1" = list(
        competition = "1270"
      )
    )
  )
)
