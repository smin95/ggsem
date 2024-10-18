#' Convert CSV (from ggsem Shiny app) to ggplot2 output
#' @description
#' This function converts the four CSV files from the ggsem Shiny app into a ggplot2 output object.
#' The ggplot2 output can then be modified using standard ggplot2 functions, such as ggtitle() and annotate().
#'
#' @param points_data
#' The object that stores the CSV file containing information about points from the ggsem Shiny app.
#' @param lines_data
#' The object that stores the CSV file containing information about lines from the ggsem Shiny app.
#' @param annotations_data
#' The object that stores the CSV file containing information about text annotations from the ggsem Shiny app.
#' @param loops_data
#' The object that stores the CSV file containing information about self-loop arrows from the ggsem Shiny app.
#' @param element_order
#' Order of the graphical elements on display. The default is set so that annotations are at the most front and lines
#' at the most back. This can be changed by providing a vector of four elements of strings, such as: c("self_loops", "annotations", "points", "lines")
#'
#' @return
#' Returns a ggplot2 object.
#' @import ggplot2
#' @export
#'
#' @examples
#' points_df <- read_csv('points_sem1.csv') # a CSV output from the Shiny app
#' lines_df <- read_csv('lines_sem1.csv')  # a CSV output from the Shiny app
#' texts_df <- read_csv('ann_sem1.csv')  # a CSV output from the Shiny app
#' loops_df <- read_csv('loop_sem1.csv')  # a CSV output from the Shiny app
#' create_plot_from_csv(points_df, lines_df, texts_df, loops_df) # Convert CSV to ggplot2 object
#'
csv_to_ggplot <- function(points_data, lines_data, annotations_data, loops_data,
                                 element_order = c("annotations", "self_loops", "points", "lines")) {

  # Initialize the ggplot object
  p <- ggplot() +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )

  # Zoom factor (can be adjusted based on actual need)
  zoom_factor <- 1

  # Function to interpolate points along a straight line for gradient
  interpolate_points <- function(x_start, y_start, x_end, y_end, n = 100) {
    t <- seq(0, 1, length.out = n)
    x <- (1 - t) * x_start + t * x_end
    y <- (1 - t) * y_start + t * y_end
    data.frame(x = x, y = y)
  }

  # Draw points using annotate()
  draw_points_from_csv <- function(p, points_data, zoom_factor) {
    if (nrow(points_data) > 0) {
      for (i in 1:nrow(points_data)) {
        p <- p + annotate("point",
                          x = points_data$x[i],
                          y = points_data$y[i],
                          size = points_data$size[i] / 3 / zoom_factor,
                          color = points_data$border_color[i],
                          fill = points_data$color[i],
                          alpha = points_data$alpha[i],
                          shape = ifelse(points_data$shape[i] == "circle", 21,
                                         ifelse(points_data$shape[i] == "triangle", 24,
                                                ifelse(points_data$shape[i] == "square", 22, 23))),
                          stroke = points_data$border_width[i] / zoom_factor)
      }
    }
    return(p)
  }


  # Draw lines (including gradient lines) using annotate()
  draw_lines_from_csv <- function(p, lines_data, zoom_factor) {
    if (nrow(lines_data) > 0) {
      for (i in 1:nrow(lines_data)) {
        line_type <- lines_data$type[i]
        start_color <- lines_data$color[i]
        end_color <- if (lines_data$color_type[i] == "Gradient") lines_data$end_color[i] else start_color
        gradient_position <- if (!is.null(lines_data$gradient_position[i])) lines_data$gradient_position[i] else 1
        adjusted_line_width <- lines_data$width[i] / zoom_factor
        adjusted_arrow_size <- if (!is.na(lines_data$arrow_size[i])) lines_data$arrow_size[i] / zoom_factor else NA

        # Handle straight lines and arrows
        if (line_type == "Straight Line" || line_type == "Straight Arrow" || line_type == "Auto-generated") {
          if (!is.null(lines_data$x_start[i]) && !is.null(lines_data$x_end[i])) {
            # Gradient handling for straight lines
            if (lines_data$color_type[i] == "Gradient") {
              straight_points <- interpolate_points(
                x_start = lines_data$x_start[i], y_start = lines_data$y_start[i],
                x_end = lines_data$x_end[i], y_end = lines_data$y_end[i]
              )
              n_points <- nrow(straight_points)
              split_index <- round(gradient_position * n_points)
              gradient_colors_start <- colorRampPalette(c(start_color, end_color))(split_index)
              gradient_colors_end <- colorRampPalette(c(end_color, end_color))(n_points - split_index + 1)

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
                                size = adjusted_line_width, alpha = lines_data$alpha[i])
            }

            # Handle arrowheads if applicable
            arrow_type <- lines_data$arrow_type[i]
            if (!is.null(arrow_type) && !is.na(adjusted_arrow_size)) {
              if (lines_data$two_way_arrow[i]) {
                # Draw two-way arrows
                p <- p + annotate("segment",
                                  x = lines_data$x_start[i], y = lines_data$y_start[i],
                                  xend = lines_data$x_start[i] - 1e-5, yend = lines_data$y_start[i] - 1e-5,
                                  size = adjusted_line_width, alpha = lines_data$alpha[i],
                                  arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                                  color = start_color) +
                  annotate("segment",
                           x = lines_data$x_end[i], y = lines_data$x_end[i],
                           xend = lines_data$x_end[i] + 1e-5, yend = lines_data$y_end[i] + 1e-5,
                           size = adjusted_line_width, alpha = lines_data$alpha[i],
                           arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                           color = end_color)
              } else {
                # Draw one-way arrows
                p <- p + annotate("segment",
                                  x = lines_data$x_end[i], y = lines_data$y_end[i],
                                  xend = lines_data$x_end[i] + 1e-5, yend = lines_data$y_end[i] + 1e-5,
                                  size = adjusted_line_width, alpha = lines_data$alpha[i],
                                  arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                                  color = end_color)
              }
            }
          }
        } else if (line_type == 'Lavaan') { # one-segment straight arrow
          p <- p + annotate("segment",
                            x = lines_data$x_start[i], y = lines_data$y_start[i],
                            xend = lines_data$x_end[i], yend = lines_data$y_end[i],
                            color = start_color,
                            size = adjusted_line_width, alpha = lines_data$alpha[i],
                            arrow = arrow(type = lines_data$arrow_type[i], length = unit(adjusted_arrow_size, "inches")))
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
              gradient_colors_start <- colorRampPalette(c(start_color, end_color))(split_index)
              gradient_colors_end <- colorRampPalette(c(end_color, end_color))(n_points - split_index + 1)

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
                                size = adjusted_line_width, alpha = lines_data$alpha[i])
            }

            # Handle arrowhead for curved lines
            arrow_type <- lines_data$arrow_type[i]
            if (!is.null(arrow_type) && !is.na(adjusted_arrow_size)) {
              p <- p + annotate("segment",
                                x = bezier_points$x[nrow(bezier_points)], y = bezier_points$y[nrow(bezier_points)],
                                xend = bezier_points$x[nrow(bezier_points)] + 1e-5, yend = bezier_points$y[nrow(bezier_points)] + 1e-5,
                                size = adjusted_line_width,
                                arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                                color = end_color)
            }
          }
        }
      }
    }
    return(p)
  }


  # Draw annotations using annotate()
  draw_annotations_from_csv <- function(p, annotations_data, zoom_factor) {
    if (nrow(annotations_data) > 0) {
      for (i in 1:nrow(annotations_data)) {
        p <- p + annotate("text",
                          x = annotations_data$x[i],
                          y = annotations_data$y[i],
                          label = annotations_data$text[i],
                          size = annotations_data$size[i] / 3 / zoom_factor,  # Adjusting size scaling
                          color = annotations_data$color[i],
                          alpha = annotations_data$alpha[i],  # Apply alpha for text
                          angle = annotations_data$angle[i],
                          family = annotations_data$font[i],
                          fontface = annotations_data$fontface[i])
      }
    }
    return(p)
  }

  # Draw self-loop arrows using annotate()
  draw_self_loops_from_csv <- function(p, loops_data, zoom_factor) {
    if (nrow(loops_data) > 0) {
      for (i in 1:nrow(loops_data)) {
        # Create the loop path with a gap
        t <- seq(0, 2 * pi, length.out = 100)
        gap_angle <- loops_data$gap_size[i] * pi
        loop_start <- t[t < (2 * pi - gap_angle)]

        # Scale and apply transformations for the loop's ellipse
        x_ellipse <- loops_data$x_center[i] + (loops_data$loop_width[i] / zoom_factor) * loops_data$radius[i] * cos(loop_start)
        y_ellipse <- loops_data$y_center[i] + (loops_data$loop_height[i] / zoom_factor) * loops_data$radius[i] * sin(loop_start)

        # Apply rotation transformation for the loop
        theta <- loops_data$orientation[i] * pi / 180  # Convert degrees to radians
        x_rotated <- cos(theta) * (x_ellipse - loops_data$x_center[i]) - sin(theta) * (y_ellipse - loops_data$y_center[i]) + loops_data$x_center[i]
        y_rotated <- sin(theta) * (x_ellipse - loops_data$x_center[i]) + cos(theta) * (y_ellipse - loops_data$y_center[i]) + loops_data$y_center[i]

        # Add arrowhead
        arrow_type <- if (loops_data$arrow_type[i] == "closed") {
          arrow(type = "closed", length = unit(loops_data$arrow_size[i] / zoom_factor, "inches"))
        } else {
          arrow(type = "open", length = unit(loops_data$arrow_size[i] / zoom_factor, "inches"))
        }

        # Draw the loop as a continuous segment with the arrow included
        p <- p + annotate("path",
                          x = x_rotated,
                          y = y_rotated,
                          color = loops_data$color[i],
                          size = loops_data$width[i] / zoom_factor,
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
                            size = loops_data$width[i] / zoom_factor,
                            alpha = loops_data$alpha[i],
                            arrow = arrow_type)
        }
      }
    }
    return(p)
  }


  # Adjust the order of graphical elements based on element_order
  for (element in element_order) {
    if (element == "points") {
      p <- draw_points_from_csv(p, points_data, zoom_factor)
    } else if (element == "lines") {
      p <- draw_lines_from_csv(p, lines_data, zoom_factor)
    } else if (element == "annotations") {
      p <- draw_annotations_from_csv(p, annotations_data, zoom_factor)
    } else if (element == "self_loops") {
      p <- draw_self_loops_from_csv(p, loops_data, zoom_factor)
    }
  }

  p <- p + coord_fixed(ratio = 1)

  return(p)
}
