#' Write text annotations from an annotation CSV file (from ggsem Shiny app) on a ggplot object
#' @description
#' This function adds text annotations onto any ggplot output (including your own plots not created from the ggsem Shiny app).
#' @param p
#' A ggplot2 object
#' @param annotations_data
#' The object that stores the CSV file containing information about text annotations from the ggsem Shiny app.
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
#' annotations_data <- data.frame(
#' text = 'Square One', x = 26, y = 300, font = 'serif',
#' size = 20, color = '#000000', angle = 0, alpha = 1,
#' fontface = 'bold', math_expression = FALSE,
#' lavaan = FALSE
#' )
#'
#' p <- ggplot(mtcars) + geom_point(aes(mpg, disp))
#'
#' draw_annotations(p, annotations_data, zoom_level = 1.2)
draw_annotations <- function(p, annotations_data, zoom_level = 1) {

  if (!is.null(annotations_data) && nrow(annotations_data) > 0) {
    if (nrow(annotations_data) > 0) {

      annotations_data$color <- sapply(annotations_data$color, valid_hex)

      for (i in 1:nrow(annotations_data)) {

        annotation_text <- if (annotations_data$math_expression[i]) {
          suppressWarnings(tryCatch(parse(text = annotations_data$text[i]), error = function(e) annotations_data$text[i]))
        } else {
          annotations_data$text[i]
        }

        if (annotations_data$fontface[i] %in% c("Bold", "Italic", "Plain")) {
          annotations_data$fontface[i] <- switch(annotations_data$fontface[i],
                             "Bold" = "bold",
                             "Italic" = "italic",
                             "Plain" = "plain")
        }

        p <- p + annotate("text",
                          x = annotations_data$x[i],
                          y = annotations_data$y[i],
                          label = annotation_text,
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
