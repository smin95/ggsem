#' Convert CSV files (from ggsem Shiny app) to ggplot2 output
#' @description
#' This function converts the four CSV files from the ggsem Shiny app into a ggplot2 output object.
#' The ggplot2 output can then be modified using standard ggplot2 functions, such as ggtitle() and annotate().
#'
#' @param points_data
#' An object that stores the CSV file containing information about points from the ggsem Shiny app. The default is NULL.
#' @param lines_data
#' An object that stores the CSV file containing information about lines from the ggsem Shiny app. The default is NULL.
#' @param annotations_data
#' An object that stores the CSV file containing information about text annotations from the ggsem Shiny app. The default is NULL.
#' @param loops_data
#' An object that stores the CSV file containing information about self-loop arrows from the ggsem Shiny app. The default is NULL.
#' @param element_order
#' Order of the graphical elements on display. This is the order in which the graphical elements are added. So if it is written later, then it gets added later (more front),
#' such as: c("lines", "points", "self_loops", "annotations"), which sets annotations to be added last (and hence most front).
#' @param zoom_level
#' A numeric value to control the zoom level of the plot. Default is 1.2.
#' @param horizontal_position
#' A numeric value for adjusting the horizontal position of the plot. Default is 0.
#' @param vertical_position
#' A numeric value for adjusting the vertical position of the plot. Default is 0.
#' @param n Number of points to be used for interpolation (for gradient lines or curved lines). Default is 100.
#' @return
#' A ggplot object is returned as the function's output.
#' @import ggplot2
#' @export
#' @examples
#' library(ggplot2)
#'
#' # CSV files from ggsem app
#' points_data <- data.frame(
#' x = 5, y = 5, shape = 'square', color = '#D0C5ED', size = 50,
#' border_color = '#9646D4', border_width = 2, alpha = 1,
#' locked = FALSE, lavaan = FALSE
#' )
#'
#' lines_data <- data.frame(
#' x_start = 2, y_start = -2, x_end = 8, y_end = -2, ctrl_x = NA, ctrl_y = NA,
#' type = 'Straight Line', color = '#000000', end_color = NA, color_type = 'Single',
#' gradient_position = NA, width = 1, alpha = 1, arrow = FALSE,
#' arrow_type = NA, arrow_size = NA, two_way = FALSE, lavaan = FALSE,
#' line_style = 'solid'
#' )
#'
#' csv_to_ggplot(points_data = points_data,
#'               lines_data = lines_data,
#'               zoom_level = 1.4, # From the ggsem app
#'               horizontal_position = 14, # From the ggsem app
#"               vertical_position = 0,
#'               element_order = c('lines', 'points')) # order priority: lines < points
#'
#'

csv_to_ggplot <- function(points_data = NULL, lines_data = NULL, annotations_data = NULL, loops_data = NULL,
                          element_order = c("lines", "points", "self_loops", "annotations"),
                          zoom_level = 1.2, horizontal_position = 0, vertical_position = 0, n = 100) {

  # Initialize the ggplot object

  x_limits <- c(-20, 20) * zoom_level + horizontal_position
  y_limits <- c(-20, 20) * zoom_level + vertical_position

  p <- ggplot() +
    coord_fixed(ratio = 1, xlim = x_limits, ylim = y_limits) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )

  # Adjust the order of graphical elements based on element_order
  for (element in element_order) {
    if (element == "points") {
      p <- draw_points(p, points_data, zoom_level)
    } else if (element == "lines") {
      p <- draw_lines(p, lines_data, zoom_level, n = n)
    } else if (element == "annotations") {
      p <- draw_annotations(p, annotations_data, zoom_level)
    } else if (element == "self_loops") {
      p <- draw_loops(p, loops_data, zoom_level) # self-loop arrows
    }
  }

  return(p + coord_fixed(ratio = 1, xlim = x_limits, ylim = y_limits))
}
