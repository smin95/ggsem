#' Draw self-loop arrows from a self-loop arrow CSV file (from ggsem Shiny app) on a ggplot2 object
#'
#' @description
#' This function adds self-loop arrows onto any ggplot2 output (including your own plots not created from the ggsem Shiny app).
#' @param p
#' A ggplot2 object
#' @param loops_data
#' An object that stores the CSV file containing information about self-loop arrows from the ggsem Shiny app.
#' @param zoom_level
#' A numeric value to control the zoom level of the plot. Default is 1.
#'
#' @return
#' A ggplot object is returned as the function's output.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' loops_data <- data.frame(
#' x_center = -5, y_center = 5, radius = 2, color = '#000000', width = 1,
#' alpha = 1, arrow_type = 'closed', arrow_size = 0.1, gap_size = 0.2,
#' loop_width = 1, loop_height = 20, orientation = 0,
#' two_way = FALSE, locked = FALSE
#' )
#'
#' p <- ggplot(mtcars) + geom_point(aes(mpg, disp))
#'
#' draw_loops(p, loops_data, zoom_level = 1.2)
draw_loops <- function(p, loops_data, zoom_level = 1) {
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
