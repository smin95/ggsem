#' Convert CSV files (from ggsem Shiny app) to ggplot2 output
#' @description
#' This function converts the four CSV files from the ggsem Shiny app into a ggplot2 output object.
#' The ggplot2 output can then be modified using standard ggplot2 functions, such as ggtitle() and annotate().
#'
#' @param points_data
#' The object that stores the CSV file containing information about points from the ggsem Shiny app. The default is NULL.
#' @param lines_data
#' The object that stores the CSV file containing information about lines from the ggsem Shiny app. The default is NULL.
#' @param annotations_data
#' The object that stores the CSV file containing information about text annotations from the ggsem Shiny app. The default is NULL.
#' @param loops_data
#' The object that stores the CSV file containing information about self-loop arrows from the ggsem Shiny app. The default is NULL.
#' @param element_order
#' Order of the graphical elements on display. This is the order in which the graphical elements are added. So if it is written later, then it gets added later (more front),
#' such as: c("lines", "points", "self_loops", "annotations"), which sets annotations to be added last (and hence most front).
#' @param zoom_level
#' A numeric value to control the zoom level of the plot. Default is 1.2.
#' @param horizontal_position
#' A numeric value for adjusting the horizontal position of the plot. Default is 0.
#' @param vertical_position
#' A numeric value for adjusting the vertical position of the plot. Default is 0.
#' @return
#' Returns a ggplot2 object.
#' @import ggplot2
#' @export
#' @examples
#' library(ggplot2)
#'
#' # CSV files from ggsem app
#' points_data <- read.csv('https://www.smin95.com/points2.csv')
#' lines_data <- read.csv('https://www.smin95.com/lines2.csv')
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
                          zoom_level = 1.2, horizontal_position = 0, vertical_position = 0) {

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

  # Function to interpolate points along a straight line for gradient
  interpolate_points <- function(x_start, y_start, x_end, y_end, n = 100) {
    t <- seq(0, 1, length.out = n)
    x <- (1 - t) * x_start + t * x_end
    y <- (1 - t) * y_start + t * y_end
    data.frame(x = x, y = y)
  }

  # Draw points using annotate()
  draw_points_from_csv <- function(p, points_data, zoom_level) {
    if (!is.null(points_data) && nrow(points_data) > 0) {
      if (nrow(points_data) > 0) {
        for (i in 1:nrow(points_data)) {
          p <- p + annotate("point",
                            x = points_data$x[i],
                            y = points_data$y[i],
                            size = points_data$size[i] / 3 / zoom_level,
                            color = points_data$border_color[i],
                            fill = points_data$color[i],
                            alpha = points_data$alpha[i],
                            shape = ifelse(points_data$shape[i] == "circle", 21,
                                           ifelse(points_data$shape[i] == "triangle", 24,
                                                  ifelse(points_data$shape[i] == "square", 22, 23))),
                            stroke = points_data$border_width[i] / zoom_level)
        }
      }
    }
    return(p)
  }


  # Draw lines (including gradient lines) using annotate()
  draw_lines_from_csv <- function(p, lines_data, zoom_level) {
    if (!is.null(lines_data) && nrow(lines_data) > 0) {
      if (nrow(lines_data) > 0) {
        for (i in 1:nrow(lines_data)) {
          line_type <- lines_data$type[i]
          start_color <- lines_data$color[i]
          end_color <- if (lines_data$color_type[i] == "Gradient") lines_data$end_color[i] else start_color
          gradient_position <- if (!is.null(lines_data$gradient_position[i])) lines_data$gradient_position[i] else 1
          adjusted_line_width <- lines_data$width[i] / zoom_level
          adjusted_arrow_size <- if (!is.na(lines_data$arrow_size[i])) lines_data$arrow_size[i] / zoom_level else NA

          # For straight lines and arrows (including Lavaan)
          if (lines_data$lavaan[i] || line_type == "Straight Line" || line_type == "Straight Arrow" || line_type == "Auto-generated") {
            if (!is.null(lines_data$x_start[i]) && !is.null(lines_data$x_end[i])) {
              # Gradient handling for straight lines
              if (lines_data$color_type[i] == "Gradient") {
                straight_points <- interpolate_points(
                  x_start = lines_data$x_start[i], y_start = lines_data$y_start[i],
                  x_end = lines_data$x_end[i], y_end = lines_data$y_end[i]
                )
                n_points <- nrow(straight_points)
                split_index <- round(gradient_position * n_points)

                color_interpolator <- colorRampPalette(c(start_color, end_color))
                intermediate_color <- color_interpolator(3)[2]

                gradient_colors_start <- colorRampPalette(c(start_color, intermediate_color))(split_index)
                gradient_colors_end <- colorRampPalette(c(intermediate_color, end_color))(n_points - split_index + 1)


                # Draw the gradient line in segments
                for (j in 1:(split_index - 1)) {
                  p <- p + annotate("segment",
                                    x = straight_points$x[j], y = straight_points$y[j],
                                    xend = straight_points$x[j + 1], yend = straight_points$y[j + 1],
                                    color = gradient_colors_start[j],
                                    size = adjusted_line_width, alpha = lines_data$alpha[i])
                }
                for (j in split_index:(n_points - 1)) {
                  p <- p + annotate("segment",
                                    x = straight_points$x[j], y = straight_points$y[j],
                                    xend = straight_points$x[j + 1], yend = straight_points$y[j + 1],
                                    color = gradient_colors_end[j - split_index + 1],
                                    size = adjusted_line_width, alpha = lines_data$alpha[i])
                }
              } else {
                # For single-color straight lines
                p <- p + annotate("segment",
                                  x = lines_data$x_start[i], y = lines_data$y_start[i],
                                  xend = lines_data$x_end[i], yend = lines_data$y_end[i],
                                  color = start_color,
                                  size = adjusted_line_width, alpha = lines_data$alpha[i],
                                  linetype = lines_data$line_style[i])
              }

              # Add arrowhead if necessary
              arrow_type <- lines_data$arrow_type[i]
              if (!is.null(arrow_type) && !is.na(adjusted_arrow_size)) {
                offset_factor <- 0.01
                # Calculate the direction of the line to adjust the arrowhead position
                dx <- lines_data$x_end[i] - lines_data$x_start[i]
                dy <- lines_data$y_end[i] - lines_data$y_start[i]
                norm <- sqrt(dx^2 + dy^2)

                # Adjusted positions for the arrowhead
                x_adjust_start <- lines_data$x_start[i] + offset_factor * dx / norm
                y_adjust_start <- lines_data$y_start[i] + offset_factor * dy / norm

                x_adjust_end <- lines_data$x_end[i] - offset_factor * dx / norm
                y_adjust_end <- lines_data$y_end[i] - offset_factor * dy / norm

                if (isTRUE(lines_data$two_way[i])) {
                  # Draw two-way arrows
                  p <- p + annotate("segment",
                                    x = x_adjust_start, y = y_adjust_start,
                                    xend = lines_data$x_start[i], yend = lines_data$y_start[i],
                                    size = adjusted_line_width, alpha = lines_data$alpha[i],
                                    arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                                    color = start_color) +
                    annotate("segment",
                             x = x_adjust_end, y = y_adjust_end,
                             xend = lines_data$x_end[i], yend = lines_data$y_end[i],
                             size = adjusted_line_width, alpha = lines_data$alpha[i],
                             arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                             color = end_color)
                } else {
                  # Draw one-way arrows
                  p <- p + annotate("segment",
                                    x = x_adjust_end, y = y_adjust_end,
                                    xend = lines_data$x_end[i], yend = lines_data$y_end[i],
                                    size = adjusted_line_width, alpha = lines_data$alpha[i],
                                    arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                                    color = end_color)
                }
              }
            }
          }

          # Handle curved lines and arrows
          if (line_type == "Curved Line" || line_type == "Curved Arrow") {
            if (!is.null(lines_data$ctrl_x[i]) && !is.null(lines_data$ctrl_y[i])) {
              bezier_points <- create_bezier_curve(
                x_start = lines_data$x_start[i], y_start = lines_data$y_start[i],
                x_end = lines_data$x_end[i], y_end = lines_data$y_end[i],
                ctrl_x = lines_data$ctrl_x[i], ctrl_y = lines_data$ctrl_y[i]
              )

              if (lines_data$color_type[i] == "Gradient") {
                n_points <- nrow(bezier_points)
                split_index <- round(gradient_position * n_points)
                color_interpolator <- colorRampPalette(c(start_color, end_color))
                intermediate_color <- color_interpolator(3)[2]

                gradient_colors_start <- colorRampPalette(c(start_color, intermediate_color))(split_index)
                gradient_colors_end <- colorRampPalette(c(intermediate_color, end_color))(n_points - split_index + 1)

                # Draw the gradient curve
                for (j in 1:(split_index - 1)) {
                  p <- p + annotate("path",
                                    x = bezier_points$x[j:(j + 1)], y = bezier_points$y[j:(j + 1)],
                                    color = gradient_colors_start[j],
                                    size = adjusted_line_width, alpha = lines_data$alpha[i])
                }
                for (j in split_index:(n_points - 1)) {
                  p <- p + annotate("path",
                                    x = bezier_points$x[j:(j + 1)], y = bezier_points$y[j:(j + 1)],
                                    color = gradient_colors_end[j - split_index + 1],
                                    size = adjusted_line_width, alpha = lines_data$alpha[i])
                }
              } else {
                # For single-color curved lines
                p <- p + annotate("path",
                                  x = bezier_points$x, y = bezier_points$y,
                                  color = start_color,
                                  size = adjusted_line_width, alpha = lines_data$alpha[i],
                                  linetype = lines_data$line_style[i])
              }

              # Handle arrowhead for curved lines
              # arrow_type <- lines_data$arrow_type[i]
              # if (!is.null(arrow_type) && !is.na(adjusted_arrow_size)) {
              #   p <- p + annotate("segment",
              #                     x = bezier_points$x[nrow(bezier_points)], y = bezier_points$y[nrow(bezier_points)],
              #                     xend = bezier_points$x[nrow(bezier_points)] + 1e-5, yend = bezier_points$y[nrow(bezier_points)] + 1e-5,
              #                     size = adjusted_line_width,
              #                     arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
              #                     color = end_color)
              # }

              arrow_type <- lines_data$arrow_type[i]
              if (line_type == "Curved Arrow" && !is.null(arrow_type) && !is.na(adjusted_arrow_size)) {
                if (isTRUE(lines_data$two_way[i])) {

                  dx_start <- bezier_points$x[2] - bezier_points$x[1]
                  dy_start <- bezier_points$y[2] - bezier_points$y[1]

                  dx_end <- bezier_points$x[nrow(bezier_points)] - bezier_points$x[nrow(bezier_points) - 1]
                  dy_end <- bezier_points$y[nrow(bezier_points)] - bezier_points$y[nrow(bezier_points) - 1]

                  norm_start <- sqrt(dx_start^2 + dy_start^2)
                  norm_end <- sqrt(dx_end^2 + dy_end^2)

                  p <- p + annotate("segment",
                                    x = bezier_points$x[1], y = bezier_points$y[1],
                                    xend = bezier_points$x[1] - dx_start / norm_start * 1e-5,
                                    yend = bezier_points$y[1] - dy_start / norm_start * 1e-5,
                                    size = adjusted_line_width,
                                    arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                                    color = start_color) +
                    annotate("segment",
                             x = bezier_points$x[nrow(bezier_points)], y = bezier_points$y[nrow(bezier_points)],
                             xend = bezier_points$x[nrow(bezier_points)] + dx_end / norm_end * 1e-5,
                             yend = bezier_points$y[nrow(bezier_points)] + dy_end / norm_end * 1e-5,
                             size = adjusted_line_width,
                             arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                             color = end_color)

                } else {
                  dx_end <- bezier_points$x[nrow(bezier_points)] - bezier_points$x[nrow(bezier_points) - 1]
                  dy_end <- bezier_points$y[nrow(bezier_points)] - bezier_points$y[nrow(bezier_points) - 1]
                  norm_end <- sqrt(dx_end^2 + dy_end^2)

                  p <- p + annotate("segment",
                                    x = bezier_points$x[nrow(bezier_points)], y = bezier_points$y[nrow(bezier_points)],
                                    xend = bezier_points$x[nrow(bezier_points)] + dx_end / norm_end * 1e-5,
                                    yend = bezier_points$y[nrow(bezier_points)] + dy_end / norm_end * 1e-5,
                                    size = adjusted_line_width,
                                    arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                                    color = end_color)
                }
              }
            }
          }
        }
      }
    }
    return(p)
  }


  # Draw annotations using annotate()
  draw_annotations_from_csv <- function(p, annotations_data, zoom_level) {
    if (!is.null(annotations_data) && nrow(annotations_data) > 0) {
      if (nrow(annotations_data) > 0) {
        for (i in 1:nrow(annotations_data)) {
          p <- p + annotate("text",
                            x = annotations_data$x[i],
                            y = annotations_data$y[i],
                            label = annotations_data$text[i],
                            size = annotations_data$size[i] / 3 / zoom_level,  # Adjusting size scaling
                            color = annotations_data$color[i],
                            alpha = annotations_data$alpha[i],  # Apply alpha for text
                            angle = annotations_data$angle[i],
                            family = annotations_data$font[i],
                            fontface = annotations_data$fontface[i])
        }
      }
    }
    return(p)
  }

  # Draw self-loop arrows using annotate()
  draw_self_loops_from_csv <- function(p, loops_data, zoom_level) {
    if (!is.null(loops_data) && nrow(loops_data) > 0) {
      if (nrow(loops_data) > 0) {
        for (i in 1:nrow(loops_data)) {
          # Create the loop path with a gap
          t <- seq(0, 2 * pi, length.out = 100)
          gap_angle <- loops_data$gap_size[i] * pi
          loop_start <- t[t < (2 * pi - gap_angle)]

          # Scale and apply transformations for the loop's ellipse
          x_ellipse <- loops_data$x_center[i] + (loops_data$loop_width[i] / zoom_level) * loops_data$radius[i] * cos(loop_start)
          y_ellipse <- loops_data$y_center[i] + (loops_data$loop_height[i] / zoom_level) * loops_data$radius[i] * sin(loop_start)

          # Apply rotation transformation for the loop
          theta <- loops_data$orientation[i] * pi / 180  # Convert degrees to radians
          x_rotated <- cos(theta) * (x_ellipse - loops_data$x_center[i]) - sin(theta) * (y_ellipse - loops_data$y_center[i]) + loops_data$x_center[i]
          y_rotated <- sin(theta) * (x_ellipse - loops_data$x_center[i]) + cos(theta) * (y_ellipse - loops_data$y_center[i]) + loops_data$y_center[i]

          # Add arrowhead
          arrow_type <- if (loops_data$arrow_type[i] == "closed") {
            arrow(type = "closed", length = unit(loops_data$arrow_size[i] / zoom_level, "inches"))
          } else {
            arrow(type = "open", length = unit(loops_data$arrow_size[i] / zoom_level, "inches"))
          }

          # Draw the loop as a continuous segment with the arrow included
          p <- p + annotate("path",
                            x = x_rotated,
                            y = y_rotated,
                            color = loops_data$color[i],
                            size = loops_data$width[i] / zoom_level,
                            alpha = loops_data$alpha[i],
                            arrow = arrow_type
          )

          # Handle two-way arrows
          if (loops_data$two_way[i]) {
            # Reverse the loop for two-way arrow
            x_rotated_rev <- rev(x_rotated)
            y_rotated_rev <- rev(y_rotated)

            # Draw the reverse loop
            p <- p + annotate("path",
                              x = x_rotated_rev,
                              y = y_rotated_rev,
                              color = loops_data$color[i],
                              size = loops_data$width[i] / zoom_level,
                              alpha = loops_data$alpha[i],
                              arrow = arrow_type)
          }
        }
      }
    }
    return(p)
  }


  # Adjust the order of graphical elements based on element_order
  for (element in element_order) {
    if (element == "points") {
      p <- draw_points_from_csv(p, points_data, zoom_level)
    } else if (element == "lines") {
      p <- draw_lines_from_csv(p, lines_data, zoom_level)
    } else if (element == "annotations") {
      p <- draw_annotations_from_csv(p, annotations_data, zoom_level)
    } else if (element == "self_loops") {
      p <- draw_self_loops_from_csv(p, loops_data, zoom_level)
    }
  }

  return(p + coord_fixed(ratio = 1, xlim = x_limits, ylim = y_limits))
}
