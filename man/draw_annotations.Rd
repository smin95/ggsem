% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_annotations.R
\name{draw_annotations}
\alias{draw_annotations}
\title{Draw Text Annotations to a ggplot Object}
\usage{
draw_annotations(p, annotations_data, zoom_level = 1)
}
\arguments{
\item{p}{A ggplot object. The plot to which the annotations will be added.}

\item{annotations_data}{A data frame containing annotation information. Typically, this comes from a CSV file generated by the ggsem Shiny app. The required columns include:
\itemize{
  \item \code{text}: The text to annotate (character).
  \item \code{x}, \code{y}: The coordinates for the text (numeric).
  \item \code{font}: The font family to use (character, e.g., "serif").
  \item \code{size}: The size of the text (numeric).
  \item \code{color}: The color of the text (character, valid hex color).
  \item \code{angle}: The rotation angle of the text (numeric, in degrees).
  \item \code{alpha}: The transparency of the text (numeric, 0 to 1).
  \item \code{fontface}: The font style (character, e.g., "bold").
  \item \code{math_expression}: Logical, whether the text should be parsed as a mathematical expression.
}}

\item{zoom_level}{Numeric. Adjusts the size of annotations based on the zoom level. Default is \code{1}.}
}
\value{
A ggplot object with the specified annotations added.
}
\description{
This function overlays text annotations onto any ggplot object. It is particularly useful for adding annotations from CSV files generated by the ggsem Shiny app but can also be used with custom annotation data.
}
\examples{
library(ggplot2)

annotations_data <- data.frame(
text = 'Square One', x = 26, y = 300, font = 'serif',
size = 20, color = '#000000', angle = 0, alpha = 1,
fontface = 'bold', math_expression = FALSE,
lavaan = FALSE, network = FALSE, locked = FALSE
)

p <- ggplot()

draw_annotations(p, annotations_data, zoom_level = 1.2)
}
