#' Convert ggsem workflow metadata directly to a ggplot object
#'
#' This function is a convenient wrapper that takes ggsem workflow metadata,
#' extracts the visualization data silently, and converts it to a ggplot object
#' in one step.
#'
#' @param metadata A list containing ggsem workflow metadata, typically loaded from
#'   an RDS file saved by the ggsem Shiny app using the "Export Workflow" functionality.
#' @param element_order A character vector specifying the order in which graphical elements are added to the plot.
#'   For example: \code{c("annotations", "loops", "lines", "points")}. Elements at the front appear on top. Default includes all elements.
#' @param zoom_level A numeric value controlling the zoom level of the plot. A value >1 zooms in; <1 zooms out. Default is \code{1}.
#' @param horizontal_position A numeric value to shift the plot horizontally. Default is \code{0}.
#' @param vertical_position A numeric value to shift the plot vertically. Default is \code{0}.
#' @param n Number of points used for interpolation in gradient or curved lines. Default is \code{100}.
#'
#' @return A ggplot object with an \code{axis_ranges} attribute specifying the x and y axis ranges after adjustments.
#'
#' @details
#' This function combines the functionality of \code{\link{ggsem_silent}} and \code{\link{csv_to_ggplot}}
#' into a single convenient call. It's useful when you want to go directly from saved workflow
#' metadata to a ggplot object without intermediate steps.
#'
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' # Load a saved ggsem workflow
#' workflow_metadata <- readRDS("ggsem_workflow_metadata.rds")
#'
#' # Convert directly to ggplot
#' p <- metadata_to_ggplot(
#'   metadata = workflow_metadata
#' )
#'
#' # Customize the plot further
#' p + ggtitle("My SEM Visualization")
#' }
metadata_to_ggplot <- function(metadata,
                               element_order = c("lines", "points", "loops", "annotations"),
                               zoom_level = 1,
                               horizontal_position = 0,
                               vertical_position = 0,
                               n = 100) {

  if (!is.list(metadata)) {
    stop("metadata must be a list containing ggsem workflow data")
  }

  # Extract visualization data silently
  viz_data <- ggsem_silent(metadata)

  # Convert to ggplot object
  p <- csv_to_ggplot(
    graphics_data = list(viz_data$points, viz_data$lines, viz_data$annotations, viz_data$loops),
    element_order = element_order,
    zoom_level = zoom_level,
    horizontal_position = horizontal_position,
    vertical_position = vertical_position,
    n = n
  )

  return(p)
}
