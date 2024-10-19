#' Run ggsem (Shiny app) in a web browser
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
#'
launch <- function() {
  shiny::runApp(system.file("shiny", package = "ggsem"),
                display.mode = "normal",
                launch.browser = TRUE)
}
