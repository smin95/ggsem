#' Launch ggsem Shiny application
#'
#' Launches the ggsem Shiny application for interactive SEM and network visualization.
#'
#' @param x A ggsem_builder object, model object, visualization object, or NULL
#' @param ... Additional arguments passed to Shiny app
#'
#' @return Runs Shiny application
#' @export
ggsem_launch <- function(x = NULL, ...) {
  if (is.null(x)) {
    # Launch empty app
    ggsem_launch.default(NULL, ...)
  } else {
    UseMethod("ggsem_launch")
  }
}

#' Launch ggsem Shiny application from builder
#'
#' @param x A ggsem_builder object
#' @param session Session type ('sem' or 'network')
#' @param ... Additional arguments passed to Shiny app
#'
#' @return Runs Shiny application
#' @export
ggsem_launch.ggsem_builder <- function(x, session = NULL, ...) {
  if (length(x$groups) == 0) {
    stop("No groups defined in builder. Use add_sem_group() to add visualization groups.")
  }

  # Convert builder groups to the format your Shiny app expects
  bundles <- x$groups

  # Convert builder to the format expected by your existing launch logic
  pipeline <- list(
    bundles = x$groups,
    center_x = 0,  # Defaults for compatibility
    center_y = 0,
    width = x$default_width,
    height = x$default_height
  )
  class(pipeline) <- "ggsem_pipeline"

  # Use your existing multiple bundles logic
  launch_multiple_bundles(x$groups, session, ...)
}

#' Launch ggsem Shiny application from objects
#'
#' @param x Any compatible R object (lavaan, igraph, qgraph, etc.)
#' @param center_x X-coordinate for center position
#' @param center_y Y-coordinate for center position
#' @param ... Additional arguments passed to Shiny app
#'
#' @return Runs Shiny application
#' @export
ggsem_launch.default <- function(x, center_x = NULL, center_y = NULL, ...) {
  # This calls your existing ggsem() function for single objects
  ggsem(object = x, center_x = center_x, center_y = center_y, ...)
}


#' Launch multiple bundles in Shiny application
#'
#' Internal function to handle launching multiple visualization bundles.
#' Not exported - for internal use only.
#'
#' @param bundles List of visualization bundles (from builder$groups)
#' @param session Session type
#' @param ... Additional arguments
#'
#' @return Runs Shiny application
#' @noRd
launch_multiple_bundles <- function(bundles, session = NULL, ...) {
  if (length(bundles) == 0) {
    stop("No bundles to launch")
  }

  if (length(bundles) == 1) {
    bundle <- bundles[[1]]

    ggsem(object = bundle$object,
          model_obj = bundle$model_obj,
          type = bundle$type,
          session = session %||% bundle$session,
          center_x = bundle$center_x,
          center_y = bundle$center_y,
          width = bundle$width,
          height = bundle$height,
          random_seed = bundle$random_seed,
          group_id = bundle$group_id,
          group_level = bundle$group_level,
          ...)
  } else {

    required_packages <- c(
      "blavaan", "colourpicker", "DiagrammeR", "DiagrammeRsvg", "dplyr",
      "DT", "ellmer", "ggplot2", "grDevices", "grid", "igraph", "lavaan",
      "memoise", "methods", "network", "purrr", "qgraph", "RColorBrewer",
      "rlang", "Rtsne", "semPlot", "shiny", "shinyjs", "smplot2", "stringr",
      "svglite", "tidyr", "tidySEM", "umap", "xml2"
    )

    # Check and install missing packages
    missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

    if (length(missing_packages) > 0) {
      packages_string <- paste0("c('", paste(missing_packages, collapse = "', '"), "')")

      stop(
        "These packages are required to run the ggsem app but are missing: ",
        paste(missing_packages, collapse = ", "), "\n\n",
        "Please install them by running:\n\n",
        "install.packages(", packages_string, ")"
      )
    }

    temp_path <- file.path(tempdir(), "ggsem_data.rds")
    saveRDS(bundles, temp_path)
    options(ggsem.path = temp_path)

    # Launch the Shiny app
    shiny::runApp(system.file("shiny", package = "ggsem"),
                  display.mode = "normal",
                  launch.browser = TRUE)
  }
}
