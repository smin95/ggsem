#' Draw points from a point CSV file (from ggsem Shiny app) on a ggplot2 object
#' @description
#' This function adds points onto any ggplot2 output (including your own plots not created from the ggsem Shiny app).
#' @param p
#' A ggplot2 object
#' @param points_data
#' An object that stores the CSV file containing information about points from the ggsem Shiny app.
#' @param zoom_level
#' A numeric value to control the zoom level of the plot. Default is 1.2.
#'
#' @return
#' A ggplot2 object
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' points_data <- data.frame(
#' x = 20, y = 300, shape = 'square', color = '#D0C5ED', size = 50,
#' border_color = '#9646D4', border_width = 2, alpha = 1,
#' locked = FALSE, lavaan = FALSE
#' )
#'
#' p <- ggplot(mtcars) + geom_point(aes(mpg, disp))
#'
#' draw_points(p, points_data, zoom_level = 1.2)
#'
draw_points <- function(p, points_data, zoom_level) {
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
