#' Run ggsem (shiny app) locally through a browser
#'
#' @export
#' @importFrom grDevices cairo_pdf colorRampPalette
#' @importFrom stats dist
#' @importFrom utils read.csv tail write.csv
#' @import igraph
#' @import lavaan
#' @import grid
#' @import svglite
#' @import igraph
#' @import shiny
#' @importFrom DT DTOutput, renderDT, datatable
#' @importFrom colourpicker colourInput
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#' @importFrom smplot2 sm_palette
#' @return No return value, called for side effects
#'
ggsem <- function() {
  shiny::runApp(system.file("shiny", package = "ggsem"),
                display.mode = "normal",
                launch.browser = TRUE)
}
