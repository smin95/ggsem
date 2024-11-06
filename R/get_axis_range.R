#' Get axis range of a ggplot object
#'
#' A function to calculate the range of x- and y- axes.
#'
#' @param plot ggplot output from csv_to_ggplot()
#'
#' @return A list object that has two elements, each of which has two vector values. The first
#' element stores the minimum and maximum values of the plot's x-axis range, while the second
#' element stores the minimum and maximum values of the plot's y-axis range.
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
