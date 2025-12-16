#' Launch ggsem Shiny Application
#'
#' Main function to launch the ggsem Shiny application for interactive
#' network and structural equation modeling visualization. The app can be
#' started with pre-existing objects or used to create visualizations from scratch.
#'
#' @param object Optional visualization object. Supported types include:
#'   - For SEM: `lavaan`, `qgraph` (from semPaths), `sem_graph` (tidySEM),
#'     `MxRAMModel` (OpenMx), `mplusObject`, `grViz` (diagrammeR)
#'   - For networks: `igraph`, `network`, `qgraph`
#' @param model_obj Optional model object to accompany visualization objects.
#'   Required for some object types like `sem_graph` (tidySEM) for SEM visualizations.
#' @param model Same with model_obj
#' @param type Type of analysis: 'sem' for structural equation modeling or
#'   'network' for network analysis. Default is 'sem'.
#' @param session Initial session type (element type) when app launches. Either 'point', 'line', 'annotation', 'loop', 'sem' or 'network'.
#'   Default is 'sem'.
#' @param center_x X-coordinate for the center of the visualization. If NULL,
#'   defaults to 0.
#' @param center_y Y-coordinate for the center of the visualization. If NULL,
#'   defaults to 0.
#' @param width Width of the visualization area. If NULL, defaults to 25.
#' @param height Height of the visualization area. If NULL, defaults to 25.
#' @param random_seed Random seed for reproducibility of layouts. If NULL,
#'   uses current time in milliseconds.
#' @param group_id Identifier for grouping in multi-group models
#' @param group_level Level for grouping in multi-group models as in original data file or model object
#'
#' @return Launches a Shiny application. Does not return a value.
#'
#' @details
#' The ggsem Shiny application provides an interactive interface for:
#' - Visualizing and customizing SEM path diagrams
#' - Creating and modifying network visualizations
#' - Adjusting node/edge aesthetics, layouts, and annotations
#' - Exporting high-quality publication-ready graphics
#'
#' When starting with an object, the app will pre-load the visualization
#' and allow further customization. When starting without an object, users
#' can upload data or use built-in examples.
#'
#' @section Supported Object Types:
#' \strong{SEM Objects:}
#' \itemize{
#'   \item \code{lavaan}: Fitted lavaan models
#'   \item \code{qgraph}: semPaths objects from lavaan models
#'   \item \code{sem_graph}: tidySEM objects
#'   \item \code{MxRAMModel}: OpenMx models
#'   \item \code{mplusObject}: Mplus models
#'   \item \code{grViz}: diagrammeR objects from lavaanPlot
#' }
#'
#' \strong{Network Objects:}
#' \itemize{
#'   \item \code{igraph}: igraph network objects
#'   \item \code{network}: network package objects
#'   \item \code{qgraph}: qgraph network objects
#' }
#'
#' @examples
#' \dontrun{
#' # Launch app without pre-existing objects
#' ggsem()
#'
#' # Launch app with a lavaan model
#' library(lavaan)
#' model <- ' visual  =~ x1 + x2 + x3
#'            textual =~ x4 + x5 + x6
#'            speed   =~ x7 + x8 + x9 '
#' fit <- cfa(model, data = HolzingerSwineford1939)
#' ggsem(object = fit)
#'
#' # Launch app with an igraph network
#' library(igraph)
#' g <- make_ring(10)
#' ggsem(object = g, type = "network", session = "network")
#'
#' # Launch app with custom dimensions
#' ggsem(object = g, type = "network", center_x = 10, center_y = 10, width = 30, height = 30)
#'
#'
#' # Launch app with no input
#' ggsem()
#' }
#' @importFrom lavaan lavInspect
#' @importFrom blavaan blavInspect
#' @importFrom igraph vcount ecount as_data_frame as_edgelist is_directed E
#' @importFrom network network.vertex.names list.edge.attributes as.edgelist get.edge.attribute
#' @export
ggsem <- function(object = NULL, model_obj = NULL, model = NULL, type = 'sem', session = 'sem',
                  center_x = NULL, center_y = NULL, width = NULL, height = NULL,
                  random_seed = NULL, group_id = NULL, group_level = NULL) {

  required_packages <- c(
    "shiny", "shinyjs", "ggplot2", "dplyr", "tidyr", "purrr", "stringr",
    "xml2", "DiagrammeR", "DiagrammeRsvg", "tidySEM", "igraph", "DT", "colourpicker",
    "grid", "svglite", "grDevices", "lavaan", "blavaan", "memoise", "semPlot", "ellmer",
    "Rtsne", "umap", "smplot2", "network"
  )

  # Check and install missing packages
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    stop(
      "These packages are required to run the ggsem app but are missing: ",
      paste(missing_packages, collapse = ", "),
      ". Please install them with install.packages('<package>')."
    )
  }

  # Check if both model and model_obj are provided
  if (!is.null(model) && !is.null(model_obj)) {
    warning("Both 'model' and 'model_obj' provided. Using 'model_obj' and ignoring 'model'.")
    model <- NULL
  }

  model_obj <- if (!is.null(model)) model else model_obj

  # plot.new()
  network_state <- list(
    nodes = NULL,
    edges = NULL,
    weights = NULL,
    data = NULL
  )

  if (is.null(center_x)) center_x <- 0
  if (is.null(center_y)) center_y <- 0
  if (is.null(width)) width <- 25
  if (is.null(height)) height <- 25

  if (is.null(random_seed)) random_seed <- as.numeric(format(Sys.time(), "%OS3")) * 1000

  lavaan_string <- "
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
    "

  if (!is.null(object)) {
    data_file = TRUE

    # semPlot + lavaan / blavan
    if (inherits(object, "qgraph") && type == 'sem') { # lavaan
      if (!is.null(model_obj) && is(model_obj)[[1]] == "lavaan") {

        lavaan_string <- fit_to_lavstring(model_obj)
        lavaan_data <- lavaan::lavInspect(model_obj, "data")
        if (inherits(lavaan_data, "matrix")) {
          lavaan_data <- as.data.frame(lavaan_data)
        } else if (is.list(lavaan_data)) {
          # Get the group variable name from the lavaan object
          group_var_name <- lavaan::lavInspect(model_obj, "group")
          if (is.null(group_var_name)) {
            group_var_name <- "Group"  # default name if not found
          }

          # Add group variable to each dataset and then rbind
          group_names <- names(lavaan_data)
          lavaan_data <- do.call(rbind, lapply(seq_along(lavaan_data), function(i) {
            group_data <- as.data.frame(lavaan_data[[i]])
            group_data[[group_var_name]] <- group_names[i]
            group_data
          }))
        }

        output_df <- generate_graph_from_sempaths(object, center_x = center_x, center_y = center_y,
                                                  relative_x_position = width, relative_y_position = height)

        if (!is.null(group_id)) {
          if (nrow(output_df$points) > 0) output_df$points$group <- group_id
          if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
          if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
        }

        output_df$which_type <- "sem"
        output_df$layout <- object$layout # layout matrix

      } else if (!is.null(model_obj) && is(model_obj)[[1]] == "blavaan") {
        lavaan_string <- blavaan_to_lavstring(model_obj)

        lavaan_data <- blavInspect(model_obj, "data")
        if (inherits(lavaan_data, "matrix")) {
          lavaan_data <- as.data.frame(lavaan_data)
        } else if (is.list(lavaan_data)) {
          # Get the group variable name from the blavaan object
          group_var_name <- blavInspect(model_obj, "group")
          if (is.null(group_var_name)) {
            group_var_name <- "Group"  # default name if not found
          }

          # Add group variable to each dataset and then rbind
          group_names <- names(lavaan_data)
          lavaan_data <- do.call(rbind, lapply(seq_along(lavaan_data), function(i) {
            group_data <- as.data.frame(lavaan_data[[i]])
            group_data[[group_var_name]] <- group_names[i]
            group_data
          }))
        }

        output_df <- generate_graph_from_sempaths(object, center_x = center_x, center_y = center_y,
                                                  relative_x_position = width, relative_y_position = height)

        if (!is.null(group_id)) {
          if (nrow(output_df$points) > 0) output_df$points$group <- group_id
          if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
          if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
        }

        output_df$which_type <- "sem"
        output_df$layout <- object$layout # layout matrix

      } else if (is.null(model_obj)) {
        output_df <- generate_graph_from_sempaths(object, center_x = center_x, center_y = center_y,
                                                  relative_x_position = width, relative_y_position = height)

        if (!is.null(group_id)) {
          if (nrow(output_df$points) > 0) output_df$points$group <- group_id
          if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
          if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
        }

        output_df$which_type <- "sem"
        output_df$layout <- object$layout # layout matrix
      }
    } else if (is(object)[[1]] == "lavaan") {
      lavaan_string <- fit_to_lavstring(object)

      lavaan_data <- lavInspect(object, "data")
      if (inherits(lavaan_data, "matrix")) {
        lavaan_data <- as.data.frame(lavaan_data)
      } else if (is.list(lavaan_data)) {
        # Get the group variable name from the lavaan object
        group_var_name <- lavInspect(object, "group")
        if (is.null(group_var_name)) {
          group_var_name <- "Group"  # default name if not found
        }

        # Add group variable to each dataset and then rbind
        group_names <- names(lavaan_data)
        lavaan_data <- do.call(rbind, lapply(seq_along(lavaan_data), function(i) {
          group_data <- as.data.frame(lavaan_data[[i]])
          group_data[[group_var_name]] <- group_names[i]
          group_data
        }))
      }

      group_labels <- lavaan::lavInspect(object, "group.label")
      if (length(group_labels) == 0) {
        multigroup_data_upload <- FALSE
      } else {
        multigroup_data_upload <- TRUE
      }

      group_var <- NULL
      # group_level <- group_id

      sem_paths <- lavaan_to_sempaths(fit = object,
                                      data_file = lavaan_data,
                                      layout_algorithm = 'tree2',
                                      multi_group = multigroup_data_upload,
                                      group_var = group_var,
                                      group_level = group_level,
                                      residuals = TRUE)

      output_df <- generate_graph_from_sempaths(sem_paths, center_x = center_x, center_y = center_y,
                                                relative_x_position = width, relative_y_position = height)

      output_df$sem_paths <- sem_paths

      if (!is.null(group_id)) {
        if (nrow(output_df$points) > 0) output_df$points$group <- group_id
        if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
        if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
      }

      output_df$which_type <- "sem"
      output_df$layout <- sem_paths$layout # layout matrix

    } else if ((is(object)[[1]] == "blavaan")) {
      lavaan_string <- blavaan_to_lavstring(object)

      lavaan_data <- blavInspect(object, "data")
      if (inherits(lavaan_data, "matrix")) {
        lavaan_data <- as.data.frame(lavaan_data)
      } else if (is.list(lavaan_data)) {
        # Get the group variable name from the blavaan object
        group_var_name <- blavInspect(object, "group")
        if (is.null(group_var_name)) {
          group_var_name <- "Group"  # default name if not found
        }

        # Add group variable to each dataset and then rbind
        group_names <- names(lavaan_data)
        lavaan_data <- do.call(rbind, lapply(seq_along(lavaan_data), function(i) {
          group_data <- as.data.frame(lavaan_data[[i]])
          group_data[[group_var_name]] <- group_names[i]
          group_data
        }))
      }

      group_labels <- lavaan::lavInspect(object, "group.label")
      if (length(group_labels) == 0) {
        multigroup_data_upload <- FALSE
      } else {
        multigroup_data_upload <- TRUE
      }

      group_var <- NULL
      # group_level <- group_id

      sem_paths <- blavaan_to_sempaths(fit = object,
                                       data_file = lavaan_data,
                                       layout_algorithm = 'tree2',
                                       multi_group = multigroup_data_upload,
                                       group_var = group_var,
                                       group_level = group_level,
                                       residuals = TRUE)

      output_df <- generate_graph_from_sempaths(sem_paths, center_x = center_x, center_y = center_y,
                                                relative_x_position = width, relative_y_position = height)

      output_df$sem_paths <- sem_paths

      if (!is.null(group_id)) {
        if (nrow(output_df$points) > 0) output_df$points$group <- group_id
        if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
        if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
      }

      output_df$which_type <- "sem"
      output_df$layout <- sem_paths$layout # layout matrix

    } else if (inherits(object, "MxRAMModel")) {
      lavaan_string <- extract_mx_syntax(object)
      lavaan_data <-  object$data
      model_obj <- convert_openmx_to_lavaan(object, data = lavaan_data)
      sem_paths <- lavaan_to_sempaths(fit = model_obj, data_file = lavaan_data, residuals = TRUE)
      output_df <- generate_graph_from_sempaths(sem_paths, center_x = center_x, center_y = center_y,
                                                relative_x_position = width, relative_y_position = height)

      if (!is.null(group_id)) {
        if (nrow(output_df$points) > 0) output_df$points$group <- group_id
        if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
        if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
      }

      output_df$sem_paths <- sem_paths
      output_df$which_type <- "sem"
      output_df$layout <- sem_paths$layout # layout matrix

      # Mplus model object
    } else if (inherits(object, "mplusObject")) {
      mplus_string <- object$MODEL
      lavaan_string <- extract_mplus_syntax(mplus_string)
      lavaan_data <-  if (!is.null(object$rdata)) {
        object$rdata
      } else if (!is.null(object$data)) {
        object$data
      } else {
        stop("No data provided and no data found in Mplus object")
      }
      model_obj <- convert_mplus_to_lavaan(object)
      sem_paths <- lavaan_to_sempaths(fit = model_obj, data_file = lavaan_data, residuals = TRUE)
      output_df <- generate_graph_from_sempaths(sem_paths, center_x = center_x, center_y = center_y,
                                                relative_x_position = width, relative_y_position = height)

      if (!is.null(group_id)) {
        if (nrow(output_df$points) > 0) output_df$points$group <- group_id
        if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
        if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
      }

      output_df$sem_paths <- sem_paths
      output_df$which_type <- "sem"
      output_df$layout <- sem_paths$layout # layout matrix

      # tidySEM
    } else if (inherits(object, "sem_graph")) {

      multi_group <- "group" %in% names(object$nodes)

      if (multi_group && is.null(group_id)) {
        group_levels <- unique(object$nodes$group)
        all_group_outputs <- list()

        for (i in seq_along(group_levels)) {
          # NOTHING - IGNORE
          group_id <- group_levels[[i]]
          group_output <- generate_graph_from_tidySEM(object,
                                                      center_x = center_x,
                                                      center_y = center_y,
                                                      relative_x_position = width,
                                                      relative_y_position = height,
                                                      which_group = group_id)

          if (!is.null(group_output) && nrow(group_output$points) > 0) {
            all_group_outputs[[i]] <- group_output
          }
        }

        if (length(all_group_outputs) > 0) {
          output_df <- list(
            points = do.call(rbind, lapply(all_group_outputs, function(x) x$points)),
            lines = do.call(rbind, lapply(all_group_outputs, function(x) x$lines)),
            annotations = do.call(rbind, lapply(all_group_outputs, function(x) x$annotations))
          )
        } else {
          output_df <- list(points = data.frame(), lines = data.frame(), annotations = data.frame())
        }

      } else if (multi_group && !is.null(group_id)) {

        output_df <- generate_graph_from_tidySEM(object,
                                                 center_x = center_x,
                                                 center_y = center_y,
                                                 relative_x_position = width,
                                                 relative_y_position = height,
                                                 which_group = group_level)

        if (!is.null(group_id)) {
          if (nrow(output_df$points) > 0) output_df$points$group <- group_id
          if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
          if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
        }

      } else {
        output_df <- generate_graph_from_tidySEM(object,
                                                 center_x = center_x,
                                                 center_y = center_y,
                                                 relative_x_position = width,
                                                 relative_y_position = height)

        if (!is.null(group_id)) {
          if (nrow(output_df$points) > 0) output_df$points$group <- group_id
          if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
          if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
        }
      }

      output_df$which_type <- "sem"
      if (is(model_obj)[[1]] == "lavaan") lavaan_string <- fit_to_lavstring(model_obj)
      if (is(model_obj)[[1]] == "blavaan") lavaan_string <- blavaan_to_lavstring(model_obj)
      if (inherits(model_obj, "MxRAMModel")) {
        lavaan_string <- extract_mx_syntax(model_obj)
        model_obj <- convert_mx_to_lavaan(model_obj, model_obj$data)
      }
      if (inherits(model_obj, "mplusObject")) {
        lavaan_string <- extract_mplus_syntax(model_obj)
        model_obj <- convert_mplus_to_lavaan(model_obj)
      }

    } else if (inherits(object, 'igraph')) {
      nodes <- igraph::as_data_frame(object, what = "vertices")

      if (ncol(nodes) == 0) {
        # No vertex attributes at all
        nodes <- data.frame(name = as.character(1:igraph::vcount(object)))
      } else if (!"name" %in% colnames(nodes)) {
        # Has vertex attributes but no name column
        nodes$name <- as.character(1:igraph::vcount(object))
      } else {
        nodes$name <- as.character(nodes$name)
      }

      network_state$nodes <- data.frame(node = nodes$name)

      if (!is.null(igraph::E(object)$weight)) {
        network_state$edges <- data.frame(source = igraph::as_edgelist(object)[,1],
                                          target = igraph::as_edgelist(object)[,2],
                                          weight = igraph::E(object)$weight)
      } else {
        network_state$edges <- data.frame(source = igraph::as_edgelist(object)[,1],
                                          target = igraph::as_edgelist(object)[,2])
      }
      network_state$weights <- igraph::E(object)$weight
      network_state$data <- object
      directed <- igraph::is_directed(object)

      output_df <- generate_graph_from_network(network_state, directed = directed, random_seed = random_seed,
                                               x_center = center_x, y_center = center_y,
                                               layout_width = width, layout_height = height)

      if (!is.null(group_id)) {
        if (nrow(output_df$points) > 0) output_df$points$group <- group_id
        if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
        if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
      }

      output_df$which_type <- "network"
    } else if (inherits(object, "network")) {

      nodes <- data.frame(node = as.character(network::network.vertex.names(object)))
      if ("weights" %in% network::list.edge.attributes(object)) {
        edges <- data.frame(source = network::as.edgelist(object)[,1],
                            target = network::as.edgelist(object)[,2],
                            weight = network::get.edge.attribute(object, 'weights'))
      } else {
        edges <- data.frame(source = network::as.edgelist(object)[,1],
                            target = network::as.edgelist(object)[,2])
      }

      edges$source <- nodes$node[edges$source]
      edges$target <- nodes$node[edges$target]

      network_state$nodes <- nodes
      network_state$edges <- edges
      network_state$data <- object

      directed <- network::is.directed(object)

      output_df <- generate_graph_from_network(network_state, directed = directed, random_seed = random_seed,
                                               x_center = center_x, y_center = center_y,
                                               layout_width = width, layout_height = height)

      if (!is.null(group_id)) {
        if (nrow(output_df$points) > 0) output_df$points$group <- group_id
        if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
        if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
      }

      output_df$which_type <- "network"
    } else if (inherits(object, "qgraph") && type == 'network') {
      output_df <- generate_graph_from_qgraph(object, x_center = center_x, y_center = center_y,
                                              layout_width = width, layout_height = height) # list

      if (!is.null(group_id)) {
        if (nrow(output_df$points) > 0) output_df$points$group <- group_id
        if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
        if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
      }

      output_df$which_type <- "network"
    } else if (inherits(object, 'grViz')) {
      output_df <- generate_graph_from_diagrammeR(object, center_x = center_x, center_y = center_y,
                                                  relative_x_position = width, relative_y_position = height)

      if (!is.null(group_id)) {
        if (nrow(output_df$points) > 0) output_df$points$group <- group_id
        if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
        if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
      }

      output_df$which_type <- 'sem'
      if (inherits(model_obj, "lavaan")) lavaan_string <- fit_to_lavstring(model_obj)
    }


    bundle <- list(
      object = object,
      graph_data = output_df,
      random_seed = random_seed,
      lavaan_string = lavaan_string,
      model_obj = model_obj,
      session = output_df$which_type,
      group = if (!is.null(group_id)) group_id else if (!is.null(output_df$points$group)) unique(output_df$points$group) else NULL,
      group_level = group_level,
      center_x = center_x,
      center_y = center_y,
      width = width,
      height = height
    )


    temp_path <- file.path(tempdir(), "ggsem_data.rds")
    saveRDS(bundle, temp_path)
    options(ggsem.path = temp_path)
  } else {
    temp_path <- file.path(tempdir(), "ggsem_data.rds")
    bundle <- list(object = NULL,
                   random_seed = random_seed,
                   lavaan_string = lavaan_string,
                   session = session)
    saveRDS(bundle, temp_path)
    options(ggsem.path = temp_path)
  }

  shiny::runApp(system.file("shiny", package = "ggsem"),
                display.mode = "normal",
                launch.browser = TRUE)
}
