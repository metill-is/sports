# Render the GISKO backtest as a self-contained HTML report.
#   Rscript scripts/wc/gisko_report.R \
#     --leader-match 145 --leader-pool 9 --leader-qual 10 --field-size 2624
# Writes data/wc/gisko/scorecard.html (gitignored; regenerable). Open it with
#   open data/wc/gisko/scorecard.html
suppressMessages(devtools::load_all(here::here()))

args <- commandArgs(trailingOnly = TRUE)
getarg <- function(flag, default) {
  i <- which(args == flag)
  if (length(i) == 1L) as.integer(args[i + 1L]) else default
}
leader <- list(
  match = getarg("--leader-match", 145L),
  pool = getarg("--leader-pool", 9L),
  qual = getarg("--leader-qual", 10L)
)
field_size <- getarg("--field-size", 2624L)

data <- gisko_scorecard_data()
dir.create(here::here("data", "wc", "gisko"), showWarnings = FALSE, recursive = TRUE)
out <- here::here("data", "wc", "gisko", "scorecard.html")
gisko_render_report(data, out,
  leader = leader, field_size = field_size,
  generated_at = as.character(Sys.Date())
)

cli::cli_alert_success("Wrote {out}")
cli::cli_text("Model {data$model_total} vs leader {leader$match + leader$pool + leader$qual} of {format(field_size, big.mark=',')}. Open with: open {out}")
