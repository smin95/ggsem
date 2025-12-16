#' Create multi-group SEM visualizations
#'
#' Build and launch multi-group SEM visualizations with an intuitive API.
#' Ideal for comparing multiple groups from the same or different models.
#' @param type Default type for all groups: 'sem' or 'network' (default: 'sem')
#' @return A ggsem_builder object
#' @export
ggsem_builder <- function(type = 'sem') {

  if (!type %in% c('sem', 'network')) {
    stop("type must be either 'sem' or 'network'")
  }

  structure(
    list(
      groups = list(),
      default_width = 25,
      default_height = 25,
      type = type
    ),
    class = "ggsem_builder"
  )
}

#' Add a SEM group to the visualization
#'
#' @param builder A ggsem_builder object
#' @param name Group name (required)
#' @param model Model object (lavaan, blavaan, etc.)
#' @param object Visualization object (qgraph, semPaths, etc.)
#' @param level Group level of multi-group model or multi-group data (e.g., 'Pasteur')
#' @param x X-coordinate for center position (default: 0)
#' @param y Y-coordinate for center position (default: 0)
#' @param width Width of visualization area (default: 25)
#' @param height Height of visualization area (default: 25)
#' @param type Type of analysis: 'sem' or 'network' (auto-detected)
#'
#' @return Updated ggsem_builder object
#' @export
add_group <- function(builder, name, model = NULL, object = NULL, level = NULL,
                          x = 0, y = 0, width = NULL, height = NULL, type = NULL) {

  if (!inherits(builder, "ggsem_builder")) {
    stop("First argument must be a ggsem_builder from ggsem_builder()")
  }

  if (missing(name) || is.null(name) || name == "") {
    stop("Group 'name' is required and cannot be empty")
  }

  if (missing(level) || is.null(level)) {
    level <- name
  }

  if (name %in% names(builder$groups)) {
    stop("Group '", name, "' already exists. Choose a different name.")
  }

  if (is.null(model) && is.null(object)) {
    stop("Provide either 'model' or 'object' for group '", name, "'")
  }

  # Auto-detect type if not provided
  if (is.null(type)) {
    if (!is.null(object)) {
      # Auto-detect from object
      if (inherits(object, "qgraph") || inherits(object, "sem_graph") || inherits(object, "grViz")) {
        type <- "sem"
      } else if (inherits(object, "igraph") || inherits(object, "network") || inherits(object, "ggplot")) {
        type <- "network"
      } else {
        # Can't auto-detect, use builder default
        type <- builder$type
      }
    } else if (!is.null(model)) {
      # For models without objects, use builder default
      type <- builder$type
    } else {
      # Fallback to builder default
      type <- builder$type
    }
  }

  if (!type %in% c('sem', 'network')) {
    stop("Type must be either 'sem' or 'network', got: '", type, "'")
  }

  # Use defaults if not provided
  if (is.null(width)) width <- builder$default_width
  if (is.null(height)) height <- builder$default_height

  # Create bundle using your existing create_bundle function
  bundle_data <- list(
    object = object,
    model_obj = model,
    type = type,
    group_id = name,
    group_level = level,
    center_x = x,
    center_y = y,
    width = width,
    height = height
  )

  bundle <- create_bundle(bundle_data)

  # Store the group
  builder$groups[[name]] <- bundle

  message("\u2713 Added group '", name, "'")
  builder
}

#' Set default type for the builder
#'
#' Change the default type for subsequent add_group() calls
#'
#' @param builder A ggsem_builder object
#' @param type Default type: 'sem' or 'network'
#' @return Updated ggsem_builder object
#' @export
set_type <- function(builder, type = 'sem') {
  if (!inherits(builder, "ggsem_builder")) {
    stop("First argument must be a ggsem_builder")
  }

  if (!type %in% c('sem', 'network')) {
    stop("type must be either 'sem' or 'network'")
  }

  builder$type <- type
  message("Set default type to '", type, "'")
  builder
}


#' Launch method for ggsem_builder objects
#'
#' Convenience method for builder pattern: builder |> launch()
#'
#' @param builder A ggsem_builder object
#' @param ... Additional arguments passed to Shiny app
#'
#' @return Runs Shiny application
#' @export
launch <- function(builder, ...) {
  ggsem_launch(builder, ...)
}
