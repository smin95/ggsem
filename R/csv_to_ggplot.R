#' Convert CSV Files (from ggsem Shiny App) to a ggplot Object
#'
#' @description
#' This function converts the CSV files exported from the ggsem Shiny app into a customizable
#' ggplot object. The resulting plot is compatible with ggplot2 functions, allowing users to
#' modify it further (e.g., adding titles or annotations).
#'
#' @param graphics_data A list of data frames containing point data, line data, annotation data, and loop data. It is exported from the ggsem Shiny app. Default is \code{NULL}.
#' @param element_order A character vector specifying the order in which graphical elements are added to the plot.
#'   For example: \code{c("annotations", "loops", "lines", "points")}. Elements at the front appear on top. Default includes all elements.
#' @param zoom_level A numeric value controlling the zoom level of the plot. A value >1 zooms in; <1 zooms out. Default is \code{1}.
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
#' @importFrom purrr compact
#' @export
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
#' csv_to_ggplot(graphics_data = list(points_data, lines_data),
#'               zoom_level = 1.2, # Value from the ggsem app
#'               horizontal_position = 0, # Value from the ggsem app
#"               vertical_position = 0,
#'               element_order = c('lines', 'points')) # order priority: lines < points
#'
#'

csv_to_ggplot <- function(graphics_data = NULL,
                          element_order = c("lines", "points", "loops", "annotations"),
                          zoom_level = 1, horizontal_position = 0, vertical_position = 0, n = 100) {

  points <- NULL
  lines <- NULL
  annotations <- NULL
  loops <- NULL

  if (!is.null(graphics_data)) {
    for (i in seq_along(graphics_data)) {
      df <- graphics_data[[i]]
      if (!is.null(df) && nrow(df) > 0) {
        df_type <- identify_data_frame(df)

        switch(df_type,
               "points" = { points <- df },
               "lines" = { lines <- df },
               "annotations" = { annotations <- df },
               "loops" = { loops <- df },
               NULL
        )
      }
    }
  }

  global_ranges <- compute_global_ranges(
    points = points,
    lines = lines,
    annotations = annotations,
    loops = loops
  )

  x_center <- mean(global_ranges$x_range)
  y_center <- mean(global_ranges$y_range)
  x_width <- diff(global_ranges$x_range) * zoom_level
  y_height <- diff(global_ranges$y_range) * zoom_level

  x_limits <- c(x_center - x_width/2 + horizontal_position,
                x_center + x_width/2 + horizontal_position)
  y_limits <- c(y_center - y_height/2 + vertical_position,
                y_center + y_height/2 + vertical_position)

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

  available_data <- list(
    points = points,
    lines = lines,
    annotations = annotations,
    loops = loops
  )

  plot_elements <- vector("list", length(element_order))

  for (i in seq_along(rev(element_order))) {
    element <- element_order[i]
    element_data <- available_data[[element]]
    if (!is.null(element_data) && nrow(element_data) > 0) {
      plot_elements[[i]] <- switch(
        element,
        "points" = draw_points(element_data, zoom_level),
        "lines" = draw_lines(element_data, zoom_level, n),
        "annotations" = draw_annotations(element_data, zoom_level),
        "loops" = draw_loops(element_data, zoom_level),
        NULL
      )
    } else {
      plot_elements[[i]] <- NULL
    }
  }

  p <- p + purrr::compact(plot_elements) +
    coord_cartesian(xlim = x_limits, ylim = y_limits)

  return(p)
}

#' Recognize the type of output data tables (element type)
#' @param df data of graphics from ggsem
#' @return Identifier for each output table in strings
#' @keywords internal
#' @noRd
identify_data_frame <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  if (all(c("x_start", "x_end", "y_start", "y_end") %in% names(df))) {
    return("lines")
  } else if (all(c("loop_width", "loop_height", "radius") %in% names(df))) {
    return("loops")
  } else if (all(c("text", "font", "fontface") %in% names(df))) {
    return("annotations")
  } else if (all(c("x", "y", "shape") %in% names(df))) {
    return("points")
  } else {
    # Fallback: try to identify by partial column matches
    if (any(grepl("arrow", names(df))) && any(grepl("start|end", names(df)))) {
      return("lines")
    } else if (any(grepl("text|label", names(df), ignore.case = TRUE))) {
      return("annotations")
    } else if (any(grepl("center|radius|loop", names(df)))) {
      return("loops")
    } else if (all(c("x", "y") %in% names(df))) {
      return("points")
    } else {
      warning("Unable to identify data frame type. Skipping.")
      return(NULL)
    }
  }
}

#' Compute global ranges of x and y axes of all existing elements
#' @param points points data frame
#' @param lines lines data frame
#' @param annotations annotations data frame
#' @param loops loops data frame
#' @return A list of X and Y ranges of graphics
#' @keywords internal
#' @noRd
compute_global_ranges <- function(points = NULL, lines = NULL, annotations = NULL, loops = NULL) {
  all_x <- numeric()
  all_y <- numeric()

  if (!is.null(points)) {
    all_x <- c(all_x, points$x)
    all_y <- c(all_y, points$y)
  }

  if (!is.null(lines)) {
    all_x <- c(all_x,
               lines$x_start,
               lines$x_end)
    all_y <- c(all_y,
               lines$y_start,
               lines$y_end)
  }

  if (!is.null(annotations)) {
    all_x <- c(all_x, annotations$x)
    all_y <- c(all_y, annotations$y)
  }

  if (!is.null(loops) && nrow(loops) > 0) {
    all_x <- c(all_x, loops$x_center)
    all_y <- c(all_y, loops$y_center)
  }

  if (length(all_x) > 0 && length(all_y) > 0) {
    x_range <- c(min(all_x), max(all_x))
    y_range <- c(min(all_y), max(all_y))

    max_range <- max(x_range[2] - x_range[1], y_range[2] - y_range[1])
    buffer <- max_range * 0.05 # 5%

    x_range <- c(x_range[1] - buffer, x_range[2] + buffer)
    y_range <- c(y_range[1] - buffer, y_range[2] + buffer)

    return(list(x_range = x_range, y_range = y_range))
  } else {
    # Default ranges if no elements exist
    return(list(x_range = c(-10, 10), y_range = c(-10, 10)))
  }
}

