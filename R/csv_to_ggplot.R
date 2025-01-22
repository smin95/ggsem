#' Convert CSV Files (from ggsem Shiny App) to a ggplot Object
#'
#' @description
#' This function converts the CSV files exported from the ggsem Shiny app into a customizable
#' ggplot object. The resulting plot is compatible with ggplot2 functions, allowing users to
#' modify it further (e.g., adding titles or annotations).
#'
#' @param points_data A data frame containing point data exported from the ggsem Shiny app. Default is \code{NULL}.
#' @param lines_data A data frame containing line data exported from the ggsem Shiny app. Default is `NULL`.
#' @param annotations_data A data frame containing text annotation data exported from the ggsem Shiny app. Default is \code{NULL}.
#' @param loops_data A data frame containing self-loop arrow data exported from the ggsem Shiny app. Default is \code{NULL}.
#' @param element_order A character vector specifying the order in which graphical elements are added to the plot.
#'   For example: \code{c("lines", "points", "self_loops", "annotations")}. Later elements appear on top. Default includes all elements.
#' @param zoom_level A numeric value controlling the zoom level of the plot. A value >1 zooms in; <1 zooms out. Default is \code{1.2}.
#' @param horizontal_position A numeric value to shift the plot horizontally. Default is \code{0}.
#' @param vertical_position A numeric value to shift the plot vertically. Default is \code{0}.
#' @param n Number of points used for interpolation in gradient or curved lines. Default is \code{100}.
#'
#' @return
#' A ggplot object with an \code{axis_ranges} attribute specifying the x and y axis ranges after adjustments.
#'
#' @details
#' - The function uses `coord_fixed` to ensure square plotting space and uniform scaling.
#' - The `element_order` parameter determines the layering of graphical elements, with later elements
#'   appearing on top.
#' - The `axis_ranges` attribute is attached to the plot for additional programmatic access.
#'
#' @import ggplot2
#' @export
#' @examples
#'
#' # CSV files from ggsem app
#' points_data <- data.frame(
#' x = 20, y = 20, shape = 'rectangle', color = '#D0C5ED', size = 50,
#' border_color = '#9646D4', border_width = 2, alpha = 1, width_height_ratio = 1.6, orientation = 45,
#' lavaan = FALSE, lavaan = FALSE, network = FALSE, locked = FALSE
#' )
#'
#' lines_data <- data.frame(
#' x_start = 2, y_start = -2, x_end = 10, y_end = -2, ctrl_x = NA, ctrl_y = NA,
#' type = 'Straight Line', color = '#000000', end_color = '#cc3d3d', color_type = 'Gradient',
#' gradient_position = 0.35, width = 1.5, alpha = 1, arrow = FALSE,
#' arrow_type = NA, arrow_size = NA, two_way = FALSE, lavaan = FALSE,
#' network = FALSE, line_style = 'solid', locked = FALSE
#' )
#'
#' csv_to_ggplot(points_data = points_data,
#'               lines_data = lines_data,
#'               zoom_level = 1.2, # Value from the ggsem app
#'               horizontal_position = 0, # Value from the ggsem app
#"               vertical_position = 0,
#'               element_order = c('lines', 'points')) # order priority: lines < points
#'
#'

csv_to_ggplot <- function(points_data = NULL, lines_data = NULL, annotations_data = NULL, loops_data = NULL,
                          element_order = c("lines", "points", "self_loops", "annotations"),
                          zoom_level = 1.2, horizontal_position = 0, vertical_position = 0, n = 100) {

  # Initialize the ggplot object

  x_limits <- c(-40, 40) * zoom_level + horizontal_position
  y_limits <- c(-40, 40) * zoom_level + vertical_position

  p <- ggplot() +
    coord_fixed(ratio = 1, xlim = x_limits, ylim = y_limits, expand = FALSE, clip = "off") + # Ensure square plotting space
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
      p <- draw_loops(p, loops_data, zoom_level)
    }
  }

  return(p)
}
