#' @export
austria <- list(
  country = "austria",
  sexes = c("male"),
  male = list(
    divisions = c("div1"),
    div1 = list(
      name = "hla",
      years = 2021:2026
    )
  )
)

#' @export
denmark <- list(
  country = "denmark",
  sexes = c("male", "female"),
  male = list(
    divisions = c("div1", "div2"),
    div1 = list(
      name = "herre-handbold-ligaen",
      years = 2021:2026
    ),
    div2 = list(
      name = "1-division",
      years = 2021:2026
    )
  ),
  female = list(
    divisions = c("div1", "div2"),
    div1 = list(
      name = "kvindeligaen-women",
      years = 2021:2026
    ),
    div2 = list(
      name = "1-division-women",
      years = 2021:2026
    )
  )
)

#' @export
finland <- list(
  country = "finland",
  sexes = c("male", "female"),
  male = list(
    divisions = c("div1"),
    div1 = list(
      name = "aktialiiga",
      years = 2021:2026
    )
  ),
  female = list(
    divisions = c("div1"),
    div1 = list(
      name = "aktialiiga-women",
      years = 2021:2026
    )
  )
)

#' @export
germany <- list(
  country = "germany",
  sexes = c("male", "female"),
  male = list(
    divisions = c("div1", "div2"),
    div1 = list(
      name = "bundesliga",
      years = 2021:2026
    ),
    div2 = list(
      name = "2-bundesliga",
      years = 2021:2026
    )
  ),
  female = list(
    divisions = c("div1", "div2"),
    div1 = list(
      name = "1-bundesliga-women",
      years = 2021:2026
    ),
    div2 = list(
      name = "2-bundesliga-women",
      years = 2021:2026
    )
  )
)

#' @export
#' france
france <- list(
  country = "france",
  sexes = c("male", "female"),
  male = list(
    divisions = c("div1", "div2"),
    div1 = list(
      name = "starligue",
      years = 2021:2026
    ),
    div2 = list(
      name = "proligue",
      years = 2021:2026
    )
  ),
  female = list(
    divisions = c("div1"),
    div1 = list(
      name = "division-1-women",
      years = 2021:2026
    )
  )
)

#' @export
hungary <- list(
  country = "hungary",
  sexes = c("male", "female"),
  male = list(
    divisions = c("div1"),
    div1 = list(
      name = "nb-i",
      years = 2021:2026
    )
  ),
  female = list(
    divisions = c("div1"),
    div1 = list(
      name = "nb-i-women",
      years = 2021:2026
    )
  )
)

#' @export
norway <- list(
  country = "norway",
  sexes = c("male", "female"),
  male = list(
    divisions = c("div1", "div2"),
    div1 = list(
      name = "rema-1000-ligaen",
      years = 2021:2026
    ),
    div2 = list(
      name = "1-division",
      years = 2021:2026
    )
  ),
  female = list(
    divisions = c("div1", "div2"),
    div1 = list(
      name = "rema-1000-ligaen-women",
      years = 2021:2026
    ),
    div2 = list(
      name = "1-division-women",
      years = 2021:2026
    )
  )
)

#' @export
sweden <- list(
  country = "sweden",
  sexes = c("male", "female"),
  male = list(
    divisions = c("div1", "div2"),
    div1 = list(
      name = "handbollsligan",
      years = 2021:2026
    ),
    div2 = list(
      name = "allsvenskan",
      years = 2021:2026
    )
  ),
  female = list(
    divisions = c("div1", "div2"),
    div1 = list(
      name = "handbollsligan-women",
      years = 2021:2026
    ),
    div2 = list(
      name = "allsvenskan-women",
      years = 2021:2026
    )
  )
)

#' @export
spain <- list(
  country = "spain",
  sexes = c("male", "female"),
  male = list(
    divisions = c("div1", "div2"),
    div1 = list(
      name = "liga-asobal",
      years = 2021:2026
    ),
    div2 = list(
      name = "division-de-honor-plata",
      years = 2021:2026
    )
  ),
  female = list(
    divisions = c("div1"),
    div1 = list(
      name = "division-de-honor-women",
      years = 2021:2026
    )
  )
)

#' @export
poland <- list(
  country = "poland",
  sexes = c("male", "female"),
  male = list(
    divisions = c("div1", "div2"),
    div1 = list(
      name = "superliga",
      years = 2021:2026
    ),
    div2 = list(
      name = "central-league",
      years = 2021:2026
    )
  ),
  female = list(
    divisions = c("div1", "div2"),
    div1 = list(
      name = "superliga-women",
      years = 2021:2026
    ),
    div2 = list(
      name = "central-league-women",
      years = 2021:2026
    )
  )
)

#' @export
czech_republic <- list(
  country = "czech-republic",
  sexes = c("male"),
  male = list(
    divisions = c("div1"),
    div1 = list(
      name = "extraliga",
      years = 2021:2026
    )
  )
)

#' @export
portugal <- list(
  country = "portugal",
  sexes = c("male", "female"),
  male = list(
    divisions = c("div1"),
    div1 = list(
      name = "andebol-1",
      years = 2021:2026
    )
  ),
  female = list(
    divisions = c("div1"),
    div1 = list(
      name = "1a-divisao-women",
      years = 2021:2026
    )
  )
)

#' @export
leagues <- list(
  austria,
  czech_republic,
  denmark,
  finland,
  hungary,
  germany,
  france,
  norway,
  portugal,
  poland,
  spain,
  sweden
)
