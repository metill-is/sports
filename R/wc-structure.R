NULL

# Static structure of the 2026 FIFA World Cup: 12 groups of 4, then a 32-team
# knockout (Round of 32 -> R16 -> QF -> SF -> Final). Team names match the
# martj42 international-results conventions so they join the fit's team registry
# without translation. Source: Wikipedia "2026 FIFA World Cup draw" +
# "...knockout stage" (verified 2026-06-11).

#' 2026 World Cup tournament structure.
#'
#' Returns the static groups, hosts, and knockout bracket wiring used by the
#' tournament simulator. The bracket is a single fixed tree (unlike the
#' randomly-redrawn Iceland cup): each Round-of-32 match has two slot feeders
#' (`"1A"` = winner group A, `"2B"` = runner-up B, `"3#74"` = a best-third-placed
#' team allocated to match 74); every later match feeds off two earlier match
#' winners (`"W89"`). The R16->Final wiring is non-sequential, so it is encoded
#' explicitly rather than computed.
#'
#' @param allocation_csv Optional path to the FIFA Annex-C best-third allocation
#'   table (495 rows keyed on the sorted 8-letter combination of qualifying
#'   third-placed groups). When absent, the simulator falls back to a valid
#'   eligibility-respecting assignment.
#' @return A named list: `groups` (named list group -> 4 teams), `group_of`
#'   (named chr team -> group), `hosts`, `bracket` (tibble `match_no`, `round`,
#'   `feeder_a`, `feeder_b`), `third_slots` (tibble `match_no`, `eligible`
#'   list-col), `third_allocation` (tibble or `NULL`), `rounds` (ordered).
#' @export
wc_structure <- function(allocation_csv = here::here(
                           "data", "wc", "structure", "third_allocation.csv"
                         )) {
  groups <- list(
    A = c("Mexico", "South Africa", "South Korea", "Czech Republic"),
    B = c("Canada", "Bosnia and Herzegovina", "Qatar", "Switzerland"),
    C = c("Brazil", "Morocco", "Haiti", "Scotland"),
    D = c("United States", "Paraguay", "Australia", "Turkey"),
    E = c("Germany", "Cura\u00e7ao", "Ivory Coast", "Ecuador"),
    F = c("Netherlands", "Japan", "Sweden", "Tunisia"),
    G = c("Belgium", "Egypt", "Iran", "New Zealand"),
    H = c("Spain", "Cape Verde", "Saudi Arabia", "Uruguay"),
    I = c("France", "Senegal", "Iraq", "Norway"),
    J = c("Argentina", "Algeria", "Austria", "Jordan"),
    K = c("Portugal", "DR Congo", "Uzbekistan", "Colombia"),
    L = c("England", "Croatia", "Ghana", "Panama")
  )

  group_of <- stats::setNames(
    rep(names(groups), lengths(groups)),
    unlist(groups, use.names = FALSE)
  )

  hosts <- c("Mexico", "Canada", "United States")

  bracket <- tibble::tribble(
    ~match_no, ~round, ~feeder_a, ~feeder_b,
    73L, "R32", "2A", "2B",
    74L, "R32", "1E", "3#74",
    75L, "R32", "1F", "2C",
    76L, "R32", "1C", "2F",
    77L, "R32", "1I", "3#77",
    78L, "R32", "2E", "2I",
    79L, "R32", "1A", "3#79",
    80L, "R32", "1L", "3#80",
    81L, "R32", "1D", "3#81",
    82L, "R32", "1G", "3#82",
    83L, "R32", "2K", "2L",
    84L, "R32", "1H", "2J",
    85L, "R32", "1B", "3#85",
    86L, "R32", "1J", "2H",
    87L, "R32", "1K", "3#87",
    88L, "R32", "2D", "2G",
    89L, "R16", "W73", "W75",
    90L, "R16", "W74", "W77",
    91L, "R16", "W76", "W78",
    92L, "R16", "W79", "W80",
    93L, "R16", "W83", "W84",
    94L, "R16", "W81", "W82",
    95L, "R16", "W86", "W88",
    96L, "R16", "W85", "W87",
    97L, "QF", "W89", "W90",
    98L, "QF", "W93", "W94",
    99L, "QF", "W91", "W92",
    100L, "QF", "W95", "W96",
    101L, "SF", "W97", "W98",
    102L, "SF", "W99", "W100",
    104L, "Final", "W101", "W102"
  )

  third_slots <- tibble::tibble(
    match_no = c(74L, 77L, 79L, 80L, 81L, 82L, 85L, 87L),
    eligible = list(
      c("A", "B", "C", "D", "F"),
      c("C", "D", "F", "G", "H"),
      c("C", "E", "F", "H", "I"),
      c("E", "H", "I", "J", "K"),
      c("B", "E", "F", "I", "J"),
      c("A", "E", "H", "I", "J"),
      c("E", "F", "G", "I", "J"),
      c("D", "E", "I", "J", "L")
    )
  )

  third_allocation <- NULL
  if (file.exists(allocation_csv)) {
    third_allocation <- tibble::as_tibble(
      utils::read.csv(allocation_csv, colClasses = "character")
    )
  }

  list(
    groups = groups,
    group_of = group_of,
    hosts = hosts,
    bracket = bracket,
    third_slots = third_slots,
    third_allocation = third_allocation,
    rounds = c("R32", "R16", "QF", "SF", "Final", "Champion")
  )
}

#' Validate that every group team is present in a fit's team registry.
#'
#' A silent name mismatch (e.g. "Turkey" vs "Türkiye") would drop a team from
#' the simulation, so this is called before simulating.
#'
#' @param structure Output of [wc_structure()].
#' @param teams Character vector of team names known to the fit.
#' @return Invisibly the structure; aborts with the offending names on mismatch.
#' @export
wc_validate_teams <- function(structure, teams) {
  wc_teams <- unlist(structure$groups, use.names = FALSE)
  missing <- setdiff(wc_teams, teams)
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "WC structure references teams absent from the fit:",
      "x" = "{.val {missing}}",
      "i" = "Reconcile group names with the martj42 strings in the results store."
    ))
  }
  invisible(structure)
}
