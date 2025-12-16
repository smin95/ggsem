#' Adjust Surrounding White Space of a ggplot2 Plot
#'
#' @description
#' This function allows users to remove or manage whitespace around graphical elements. It supports asymmetrical adjustments for
#' each boundary (left, right, bottom, and top). Users can also maintain a fixed aspect ratio
#' if required.
#'
#' @param plot A ggplot2 object. The plot whose axis ranges need adjustment.
#' @param x_adjust_left_percent Numeric. Percentage by which to expand the left boundary of the x-axis. Default is \code{0}.
#' @param x_adjust_right_percent Numeric. Percentage by which to expand the right boundary of the x-axis. Default is \code{0}.
#' @param y_adjust_bottom_percent Numeric. Percentage by which to expand the bottom boundary of the y-axis. Default is \code{0}.
#' @param y_adjust_top_percent Numeric. Percentage by which to expand the top boundary of the y-axis. Default is \code{0}`.
#' @param fixed_aspect_ratio Logical. If \code{TRUE}, maintains a fixed aspect ratio (1:1). If `FALSE`, allows independent scaling for x and y axes. Default is \code{TRUE}.
#' @export
#' @return
#' A ggplot2 object with adjusted axis ranges. The adjusted plot retains its original attributes
#' and is compatible with additional ggplot2 layers and themes.
#'
#' @details
#' - **Percentage Adjustments:** The percentages provided for each axis boundary are calculated based on the current axis range. For example, \code{x_adjust_left_percent = 10} expands the left boundary by 10% of the total x-axis range.
#' - **Fixed Aspect Ratio:** When \code{fixed_aspect_ratio = TRUE}, the function adjusts either the x-axis or y-axis to maintain a 1:1 aspect ratio. The larger adjustment determines the scaling for both axes.
#'
#' @examples
#'
#' # CSV files from ggsem app
#' points_data <- data.frame(
#' x = 20, y = 20, shape = 'rectangle', color = '#D0C5ED', size = 50,
#' border_color = '#9646D4', border_width = 2, alpha = 1,
#' width_height_ratio = 1.6, orientation = 45, lavaan = FALSE,
#' network = FALSE, locked = FALSE, group = 1
#' )
#'
#' lines_data <- data.frame(
#' x_start = 2, y_start = -2, x_end = 10, y_end = -2, ctrl_x = NA, ctrl_y = NA,
#' ctrl_x2 = NA, ctrl_y2 = NA, curvature_magnitude = NA, rotate_curvature = NA,
#' curvature_asymmetry = NA, type = 'Straight Line', color = '#000000',
#' end_color = NA, color_type = 'Single',
#' gradient_position = NA, width = 1.5, alpha = 1, arrow = FALSE,
#' arrow_type = NA, arrow_size = NA, two_way = FALSE, lavaan = FALSE,
#' network = FALSE, line_style = 'solid', locked = FALSE, group = 1
#' )
#'
#'
#' p <- csv_to_ggplot(graphics_data = list(points_data, lines_data),
#'               zoom_level = 1.2, # Value from the ggsem app
#'               horizontal_position = 0, # Value from the ggsem app
#"               vertical_position = 0,
#'               element_order = c('lines', 'points')) # order priority: lines < points
#'
#'
#' adjust_axis_space(p, x_adjust_left_percent = 10, x_adjust_right_percent = 10,
#'              y_adjust_bottom_percent = 5, y_adjust_top_percent = 5)
#'
#'
#'
adjust_axis_space <- function(plot,
                         x_adjust_left_percent = 0,
                         x_adjust_right_percent = 0,
                         y_adjust_bottom_percent = 0,
                         y_adjust_top_percent = 0,
                         fixed_aspect_ratio = TRUE) {
  if (!inherits(plot, "ggplot")) {
    stop("Input must be a ggplot object.")
  }

  axis_ranges <- get_axis_range(plot)
  x_range <- axis_ranges$x_range
  y_range <- axis_ranges$y_range

  x_adjust_left <- (x_range[2] - x_range[1]) * (x_adjust_left_percent / 100)
  x_adjust_right <- (x_range[2] - x_range[1]) * (x_adjust_right_percent / 100)
  y_adjust_bottom <- (y_range[2] - y_range[1]) * (y_adjust_bottom_percent / 100)
  y_adjust_top <- (y_range[2] - y_range[1]) * (y_adjust_top_percent / 100)

  new_x_range <- c(x_range[1] - x_adjust_left, x_range[2] + x_adjust_right)
  new_y_range <- c(y_range[1] - y_adjust_bottom, y_range[2] + y_adjust_top)

  # Apply the ranges to the plot
  if (fixed_aspect_ratio) {
    adjusted_plot <- plot +
      coord_fixed(ratio = 1, xlim = new_x_range, ylim = new_y_range)
  } else {
    adjusted_plot <- plot +
      coord_cartesian(xlim = new_x_range, ylim = new_y_range)
  }

  #attr(adjusted_plot, "axis_ranges") <- list(x_range = new_x_range, y_range = new_y_range)
  return(adjusted_plot)
}

