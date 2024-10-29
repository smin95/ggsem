#' Draw lines from a line CSV File (from ggsem Shiny app) on a ggplot2 object
#' @description
#' This function adds lines onto any ggplot2 output (including your own plots not created from the ggsem Shiny app).
#' @param p
#' A ggplot2 object
#' @param lines_data
#' An object that stores the CSV file containing information about lines from the ggsem Shiny app.
#' @param zoom_level
#' A numeric value to control the zoom level of the plot. Default is 1.
#' @return
#' A ggplot2 object
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' lines_data <- data.frame(
#' x_start = 2, y_start = -2, x_end = 20, y_end = -2, ctrl_x = NA, ctrl_y = NA,
#' type = 'Straight Line', color = '#000000', end_color = '#cc3d3d', color_type = 'Gradient',
#' gradient_position = 0.35, width = 2, alpha = 1, arrow = FALSE,
#' arrow_type = NA, arrow_size = NA, two_way = FALSE, lavaan = FALSE,
#' line_style = 'solid'
#' )
#'
#' p <- ggplot(mtcars) + geom_point(aes(mpg, disp))
#'
#' draw_lines(p, lines_data, zoom_level = 1.2)
#'
draw_lines <- function(p, lines_data, zoom_level = 1) {
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
