#' Get axis range of a ggplot2 object
#'
#' A function to calculate the range of x- and y- axes.
#'
#' @param plot ggplot2 output from csv_to_ggplot()
#'
#' @return A ggplot2 object
#' @export
#' @importFrom ggplot2 ggplot_build
#' @examples
#' library(ggplot2)
#' ggplot(mtcars) + geom_point(aes(mpg, disp)) -> p1
#' get_axis_range(p1)
#'
get_axis_range <- function(plot) {
  plot_build <- ggplot_build(plot)
  y_range <- plot_build$layout$panel_params[[1]]$y.range
  x_range <- plot_build$layout$panel_params[[1]]$x.range
  res <- list(x_range, y_range)
  names(res) <- c('x_range', 'y_range')
  return(res)
}
