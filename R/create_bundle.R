#' Create a bundle from pipeline or object
#'
#' Internal function to create a data bundle for visualization.
#' Not exported - for internal use only.
#'
#' @param x A ggsem_pipeline object or list with visualization data
#' @importFrom lavaan lavInspect
#' @return A bundle list containing visualization data
#' @noRd
create_bundle <- function(x) {
  if (!is.list(x)) {
    stop("create_bundle expects a list input")
  }

  required <- c("object", "group_id")
  missing_req <- setdiff(required, names(x))
  if (length(missing_req) > 0) {
    stop("create_bundle missing required components: ", paste(missing_req, collapse = ", "))
  }

  # Extract parameters with defaults
  object <- x$object
  model_obj <- x$model_obj
  type <- x$type %||% 'sem'
  group_id <- x$group_id
  group_level <- x$group_level
  center_x <- x$center_x %||% 0
  center_y <- x$center_y %||% 0
  width <- x$width %||% 25
  height <- x$height %||% 25

  if (!is.numeric(center_x)) center_x <- 0
  if (!is.numeric(center_y)) center_y <- 0
  if (!is.numeric(width)) width <- 25
  if (!is.numeric(height)) height <- 25

  # plot.new()
  network_state <- list(
    nodes = NULL,
    edges = NULL,
    weights = NULL,
    data = NULL
  )


  random_seed <- as.numeric(format(Sys.time(), "%OS3")) * 1000
  lavaan_string <- "
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
    "

  if (!is.null(object)) {
    data_file = TRUE

    # semPlot + lavaan / blavaan
    if (inherits(object, "qgraph") && type == 'sem') { # lavaan
      if (!is.null(model_obj) && is(model_obj)[[1]] == "lavaan") {
        lavaan_string <- fit_to_lavstring(model_obj)
        lavaan_data <- lavInspect(model_obj, "data")
        if (inherits(lavaan_data, "matrix")) {
          lavaan_data <- as.data.frame(lavaan_data)
        } else if (is.list(lavaan_data)) {
          # Get the group variable name from the lavaan object
          group_var_name <- lavInspect(model_obj, "group")
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

        output_df <- generate_graph_from_sempaths(object, center_x = center_x,
                                                  center_y = center_y, relative_x_position = width,
                                                  relative_y_position = height)

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

        output_df <- generate_graph_from_sempaths(object, center_x = center_x,
                                                  center_y = center_y, relative_x_position = width,
                                                  relative_y_position = height)

        if (!is.null(group_id)) {
          if (nrow(output_df$points) > 0) output_df$points$group <- group_id
          if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
          if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
        }

        output_df$which_type <- "sem"
        output_df$layout <- object$layout # layout matrix

      } else if (is.null(model_obj)) {
        output_df <- generate_graph_from_sempaths(object, center_x = center_x,
                                                  center_y = center_y, relative_x_position = width,
                                                  relative_y_position = height)

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

      group_labels <- lavInspect(object, "group.label")
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

      output_df <- generate_graph_from_sempaths(sem_paths, center_x = center_x,
                                                center_y = center_y, relative_x_position = width,
                                                relative_y_position = height)

      if (!is.null(group_id)) {
        if (nrow(output_df$points) > 0) output_df$points$group <- group_id
        if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
        if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
      }

      output_df$sem_paths <- sem_paths
      output_df$which_type <- "sem"
      output_df$layout <- sem_paths$layout # layout matrix

      # OpenMx model object
    } else if  (is(object)[[1]] == "blavaan") {

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

      group_labels <- blavInspect(object, "group.label")
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

      output_df <- generate_graph_from_sempaths(sem_paths, center_x = center_x,
                                                center_y = center_y, relative_x_position = width,
                                                relative_y_position = height)

      if (!is.null(group_id)) {
        if (nrow(output_df$points) > 0) output_df$points$group <- group_id
        if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
        if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
      }

      output_df$sem_paths <- sem_paths
      output_df$which_type <- "sem"
      output_df$layout <- sem_paths$layout # layout matrix

    } else if (inherits(object, "MxRAMModel")) {
      lavaan_string <- extract_mx_syntax(object)
      lavaan_data <-  object$data
      model_obj <- convert_openmx_to_lavaan(object, data = lavaan_data)
      sem_paths <- lavaan_to_sempaths(fit = model_obj,
                                      data_file = lavaan_data,
                                      residuals = TRUE)
      output_df <- generate_graph_from_sempaths(sem_paths, center_x = center_x,
                                                center_y = center_y, relative_x_position = width,
                                                relative_y_position = height)

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
      sem_paths <- lavaan_to_sempaths(fit = model_obj,
                                      data_file = lavaan_data,
                                      residuals = TRUE)
      output_df <- generate_graph_from_sempaths(sem_paths, center_x = center_x,
                                                center_y = center_y, relative_x_position = width,
                                                relative_y_position = height)

      if (!is.null(group_id)) {
        if (nrow(output_df$points) > 0) output_df$points$group <- group_id
        if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
        if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
      }

      output_df$which_type <- "sem"
      output_df$layout <- sem_paths$layout # layout matrix

      # tidySEM
    } else if (inherits(object, "sem_graph")) {

      multi_group <- "group" %in% names(object$nodes)

      if (multi_group && is.null(group_id)) {
        group_levels <- unique(object$nodes$group)
        all_group_outputs <- list()

        for (i in seq_along(group_levels)) {
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

      output_df <- generate_graph_from_network(network_state, directed = directed,
                                               random_seed = random_seed,
                                               x_center = center_x,
                                               y_center = center_y,
                                               layout_width = width,
                                               layout_height = height)

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

      output_df <- generate_graph_from_network(network_state, directed = directed,
                                               random_seed = random_seed,
                                               x_center = center_x,
                                               y_center = center_y,
                                               layout_width = width,
                                               layout_height = height)

      if (!is.null(group_id)) {
        if (nrow(output_df$points) > 0) output_df$points$group <- group_id
        if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
        if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
      }

      output_df$which_type <- "network"
    } else if (inherits(object, "qgraph") && type == 'network') {
      output_df <- generate_graph_from_qgraph(object, x_center = center_x,
                                              y_center = center_y,
                                              layout_width = width,
                                              layout_height = height) # list

      if (!is.null(group_id)) {
        if (nrow(output_df$points) > 0) output_df$points$group <- group_id
        if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
        if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
      }

      output_df$which_type <- "network"
    } else if (inherits(object, 'grViz')) {
      output_df <- generate_graph_from_diagrammeR(object, center_x = center_x,
                                                  center_y = center_y,
                                                  relative_x_position = width,
                                                  relative_y_position = height)

      if (!is.null(group_id)) {
        if (nrow(output_df$points) > 0) output_df$points$group <- group_id
        if (nrow(output_df$lines) > 0) output_df$lines$group <- group_id
        if (nrow(output_df$annotations) > 0) output_df$annotations$group <- group_id
      }

      output_df$which_type <- 'sem'
      if (inherits(model_obj, "lavaan")) lavaan_string <- fit_to_lavstring(model_obj)
    } else {
      # Default case for unknown objects
      output_df <- list(
        points = data.frame(),
        lines = data.frame(),
        annotations = data.frame(),
        which_type = type,
        layout = NULL
      )
    }

    bundle <- list(
      object = object,
      graph_data = output_df,
      random_seed = random_seed,
      lavaan_string = lavaan_string,
      model_obj = model_obj,
      session = output_df$which_type,
      group_id = group_id,
      group_level = group_level,
      type = type,
      center_x = if (!is.null(center_x)) center_x else 0,
      center_y = if (!is.null(center_y)) center_y else 0,
      width = width,
      height = height
    )

  } else if (!is.null(model_obj)) {
    # Create bundle from model object only
    # Create empty output_df with group_id set
    output_df <- list(
      points = data.frame(group = if (!is.null(group_id)) group_id else "default"),
      lines = data.frame(group = if (!is.null(group_id)) group_id else "default"),
      annotations = data.frame(group = if (!is.null(group_id)) group_id else "default"),
      which_type = type,
      layout = NULL
    )

    bundle <- list(
      object = NULL,
      model_obj = model_obj,
      random_seed = random_seed,
      lavaan_string = lavaan_string,
      session = type,
      group_id = group_id,
      group_level = group_level,
      type = type,
      graph_data = output_df,
      center_x = if (!is.null(center_x)) center_x else 0,
      center_y = if (!is.null(center_y)) center_y else 0,
      width = width,
      height = height
    )
  } else {
    # Empty bundle
    output_df <- list(
      points = data.frame(group = if (!is.null(group_id)) group_id else "default"),
      lines = data.frame(group = if (!is.null(group_id)) group_id else "default"),
      annotations = data.frame(group = if (!is.null(group_id)) group_id else "default"),
      which_type = type,
      layout = NULL
    )

    bundle <- list(
      object = NULL,
      random_seed = random_seed,
      lavaan_string = lavaan_string,
      session = type,
      group_id = group_id,
      group_level = group_level,
      type = type,
      graph_data = output_df,
      center_x = if (!is.null(center_x)) center_x else 0,
      center_y = if (!is.null(center_y)) center_y else 0,
      width = if (is.null(width)) width else 25,
      height = if (is.null(height)) height else 25
    )
  }

  bundle
}
