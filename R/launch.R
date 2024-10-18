#' Run ggsem (Shiny app) in a web browser
#'
#' @export
#' @examples
#' ggsem::launch() # Launches the Shiny app
#'
launch <- function() {
  shiny::runApp(system.file("shiny", package = "ggsem"),
                display.mode = "normal",
                launch.browser = TRUE)
}
