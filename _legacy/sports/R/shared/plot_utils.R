#' Save a plot with standard Metill dimensions
#'
#' @param filename Output file path
#' @param plot ggplot object (default: last plot)
#' @param width Width in inches (default: 8)
#' @param height Height in inches (default: 4.8)
#' @param scale Scaling factor (default: 1.7)
#' @param ... Additional arguments passed to ggplot2::ggsave
save_metill_plot <- function(
  filename,
  plot = ggplot2::last_plot(),
  width = 8,
  height = 4.8,
  scale = 1.7,
  ...
) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    scale = scale,
    ...
  )
}
