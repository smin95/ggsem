#' Write Text Annotations from an Annotation CSV File (from ggsem Shiny app) on a ggplot2 object
#'
#'
#' @param p
#' A ggplot2 object
#' @param annotations_data
#' The object that stores the CSV file containing information about text annotations from the ggsem Shiny app.
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
#' annotations_data <- data.frame(
#' text = 'Square One', x = 26, y = 300, font = 'Arial',
#' size = 20, color = '#000000', angle = 0, alpha = 1,
#' fontface = 'bold', math_expression = FALSE,
#' lavaan = FALSE
#' )
#'
#' p <- ggplot(mtcars) + geom_point(aes(mpg, disp))
#'
#' p1 <- draw_annotations(p, annotations_data, zoom_level = 1.2)
draw_annotations <- function(p, annotations_data, zoom_level) {
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
