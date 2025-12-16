#' Adjust Axis Range of a Plot of a ggplot2 Plot
#'
#' This function modifies the axis ranges of a ggplot object, with optional user-specified
#' ranges, additional buffers, and the ability to enforce a fixed aspect ratio. This is a modified
#' version of \code{adjust_axis_space()}.
#'
#'
#' @param plot A ggplot object. The plot whose axis ranges are to be adjusted.
#' @param x_range A numeric vector of length 2 specifying the desired x-axis range.
#'   If \code{NULL}, the current x-axis range is retained. Default is \code{NULL}.
#' @param y_range A numeric vector of length 2 specifying the desired y-axis range.
#'   If \code{NULL}, the current y-axis range is retained. Default is \code{NULL}.
#' @param buffer_percent A numeric value indicating the percentage of additional space
#'   to add to each axis range as a buffer. Default is `0` (no buffer).
#' @param fixed_aspect_ratio A logical value indicating whether to maintain a fixed
#'   aspect ratio for the plot. If \code{TRUE}, the function adjusts one axis to preserve
#'   the aspect ratio. Default is \code{TRUE}.
#' @export
#' @return A modified ggplot object with adjusted axis ranges.
#'
#' @details
#' - If `x_range` or `y_range` are provided, these values will override the current axis ranges.
#' - The `buffer_percent` parameter adds proportional space to the axis ranges, calculated
#'   as a percentage of the range's width or height.
#' - When `fixed_aspect_ratio` is `TRUE`, the function adjusts either the x-axis or y-axis
#'   to ensure the plot maintains a fixed aspect ratio.
#'
#' @examples
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
#' p <- csv_to_ggplot(graphics_data = list(points_data, lines_data),
#'               zoom_level = 1.2, # Value from the ggsem app
#'               horizontal_position = 0, # Value from the ggsem app
#"               vertical_position = 0,
#'               element_order = c('lines', 'points')) # order priority: lines < points
#'
#'
#' adjust_axis_range(p, x_range = c(-30,30), y_range= c(-30,30))
#'
adjust_axis_range <- function(plot,
                               x_range = NULL,
                               y_range = NULL,
                               buffer_percent = 0,
                               fixed_aspect_ratio = TRUE) {

  axis_ranges <- get_axis_range(plot)
  current_x_range <- axis_ranges$x_range
  current_y_range <- axis_ranges$y_range

  new_x_range <- if (!is.null(x_range)) x_range else current_x_range
  new_y_range <- if (!is.null(y_range)) y_range else current_y_range

  x_buffer <- (new_x_range[2] - new_x_range[1]) * (buffer_percent / 100)
  y_buffer <- (new_y_range[2] - new_y_range[1]) * (buffer_percent / 100)

  new_x_range <- c(new_x_range[1] - x_buffer, new_x_range[2] + x_buffer)
  new_y_range <- c(new_y_range[1] - y_buffer, new_y_range[2] + y_buffer)

  if (fixed_aspect_ratio) {
    x_width <- diff(new_x_range)
    y_height <- diff(new_y_range)
    aspect_ratio <- y_height / x_width

    if (aspect_ratio > 1) {
      # Adjust x_range to match aspect ratio
      x_center <- mean(new_x_range)
      new_x_range <- c(x_center - y_height / 2, x_center + y_height / 2)
    } else {
      # Adjust y_range to match aspect ratio
      y_center <- mean(new_y_range)
      new_y_range <- c(y_center - x_width / 2, y_center + x_width / 2)
    }
  }

  adjusted_plot <- plot +
    coord_cartesian(xlim = new_x_range, ylim = new_y_range)

  return(adjusted_plot)
}
