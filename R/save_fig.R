#' Save a ggplot object with adjusted dimensions
#'
#' This function saves a ggplot object (created from 'csv_to_ggplot()' function) to a file with dimensions automatically determined based on
#' the x-axis and y-axis ranges of the plot. The size of the output can be further controlled using addtional arguments.
#'
#' @param filename A string. The name of the output file (e.g., "plot.png").
#' @param plot A ggplot object to save.
#' @param units A string. Units for width and height. Default is \code{"in"} (inches). Other options include \code{"cm"} or \code{"mm"}.
#' @param dpi Numeric. Resolution of the output file in dots per inch. Default is 300.
#' @param aspect_ratio Numeric or \code{NULL}. If provided, fixes the aspect ratio of the plot (e.g., \code{1} for square). If \code{NULL}, uses the natural data aspect ratio. Default is \code{NULL}.
#' @param scale_factor Numeric. A scaling factor to control the overall size of the saved plot. Default is \code{0.11}.
#' @param ... Additional arguments passed to \code{ggsave()}.
#'
#' @return Saves the ggplot object to the specified file and does not return a value.
#' @export
#' @import ggplot2
#' @examples
#' \dontrun{
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
#' p <- csv_to_ggplot(points_data = points_data,
#'               lines_data = lines_data,
#'               zoom_level = 1.2, # Value from the ggsem app
#'               horizontal_position = 0, # Value from the ggsem app
#"               vertical_position = 0,
#'               element_order = c('lines', 'points')) # order priority: lines < points
#'
#'
#' p1 <- adjust_axis_range(p, x_adjust_left_percent = 10, x_adjust_right_percent = 10,
#'              y_adjust_bottom_percent = 5, y_adjust_top_percent = 5)
#'
#'
#' # Save with default scaling
#' save_figure("p1.png", p1)
#' }
save_figure <- function(filename, plot, units = "in", dpi = 300, aspect_ratio = NULL,
                        scale_factor = 0.11, ...) {
  if (!inherits(plot, "ggplot")) {
    stop("Input must be a ggplot object.")
  }

  # if (is.null(attr(plot, 'axis_ranges'))) {
  #   stop("Plot object does not contain 'axis_ranges' attribute. Check to see if the plot was created with csv_to_ggplot.")
  # }

  axis_ranges <- get_axis_range(plot)
  x_range <- axis_ranges$x_range
  y_range <- axis_ranges$y_range

  x_span <- diff(x_range)
  y_span <- diff(y_range)

  # Determine width and height
  if (!is.null(aspect_ratio)) {
    height <- y_span * scale_factor
    width <- height * aspect_ratio
  } else {
    width <- x_span * scale_factor
    height <- y_span * scale_factor
  }

  ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    ...
  )
}
