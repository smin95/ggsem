#' Generate graph data from semPlot or model objects
#'
#' Converts semPlot (qgraph class) objects or structural equation model objects
#' (lavaan or MxRAMModel class) into standardized graph data frames for plotting.
#' Handles node positioning, edge routing, and aesthetic customization for
#' structural equation models.
#'
#'
#' @return A list containing three data frames:
#'   - `points`: Node data with coordinates, shapes, colors, and sizes
#'   - `lines`: Edge data with coordinates, curvatures, arrows, and styles
#'   - `annotations`: Text labels for nodes and edges with positioning and styling
#'
#' @importFrom semPlot semPaths
#' @importFrom lavaan sem
#' @keywords internal
#' @noRd
generate_graph_from_sempaths <- function(fit, relative_x_position = 25, relative_y_position = 25,
                                         center_x = 0, center_y = 0,
                                         latent_shape = "circle", observed_shape = "square",
                                         int_shape = "triangle",
                                         point_size_latent = 20, point_size_observed = 12,
                                         point_size_int = 10,
                                         line_width = 1, line_alpha = 1, text_size_latent = 18, text_font_latent = "sans",
                                         text_color_latent = "#FFFFFF", text_alpha_latent = 1, text_fontface_latent = 'plain',
                                         text_size_others = 16, text_font_others = "sans",
                                         text_color_others = "#FFFFFF", text_alpha_others = 1, text_fontface_others = 'plain',
                                         text_size_edges = 14, text_font_edges = "sans",
                                         text_color_edges =  "#000000", text_color_fill = "#FFFFFF", text_alpha_edges = 1, text_fontface_edges = 'plain',
                                         point_color_latent = "#cc3d3d", point_color_observed = "#1262b3",
                                         point_color_int = "#0f993d",
                                         edge_color = "#000000", line_endpoint_spacing = 0.2,
                                         node_border_color = "#FFFFFF",
                                         node_border_width = 1,
                                         arrow_type = "closed", arrow_size = 0.1,
                                         lavaan_arrow_location = "end",
                                         zoom_factor = 1.2,
                                         lavaan_curvature_magnitude = 0.3,
                                         lavaan_rotate_curvature = FALSE,
                                         lavaan_curvature_asymmetry = 0,
                                         lavaan_curved_x_shift = 0,
                                         lavaan_curved_y_shift = 0,
                                         highlight_sig_path = FALSE,
                                         sig_path_color = "#000000",
                                         non_sig_path_color = "#000000",
                                         sig_label_fontface = "plain",
                                         non_sig_label_fontface = "plain",
                                         data_file = NULL,
                                         modify_params_edge = FALSE,
                                         modified_edges = NULL,
                                         modify_params_edgelabel = FALSE,
                                         modified_edgelabels = NULL,
                                         modify_params_node = FALSE,
                                         modified_nodes = NULL,
                                         modify_params_node_xy = FALSE,
                                         modified_nodes_xy = NULL,
                                         modify_params_edge_xy = FALSE,
                                         modified_edges_xy = NULL,
                                         modify_params_cov_edge = FALSE,
                                         modified_cov_edges = NULL,
                                         modify_params_nodelabel = FALSE,
                                         modified_nodelabels = NULL,
                                         modify_params_nodelabel_xy = FALSE,
                                         modified_nodelabels_xy = NULL,
                                         modify_params_nodelabel_text = FALSE,
                                         modified_nodelabels_text = NULL,
                                         which_group = "sem") {

  apply_modifications <- function(data, modifications, config, mode) {
    if (is.null(modifications) || nrow(modifications) == 0) return(data)
    modified_data <- data

    for (i in seq_len(nrow(modifications))) {
      mod <- modifications[i, ]

      if (mode == 'edge') {
        idx <- which(
          edges_from == mod$lhs &
            edges_to == mod$rhs
        )
      } else if (mode == 'node') {
        idx <- which(
          node_coords$name == mod$text
        )
      }

      #print(idx)
      if (length(idx) == 1) {
        for (col in config$modify_cols) {
          if (col %in% names(mod) && col %in% names(modified_data)) {
            modified_data[idx, col] <- mod[[col]]
          }
        }

        if (!is.null(config$special_case)) {
          modified_data <- config$special_case(modified_data, idx, mod)
        }
      }
    }
    return(modified_data)
  }

  if (inherits(fit, "qgraph")) {
    sem_paths <- fit
    node_names <- names(sem_paths$graphAttributes$Nodes$labels)
    if (is.null(node_names)) node_names <- sem_paths$graphAttributes$Nodes$labels
    node_types <- sem_paths$graphAttributes$Nodes$shape

    # Node classification
    latent_vars <- node_names[node_types == "circle"]
    observed_vars <- node_names[node_types == "square"]
    intercept_vars <- node_names[node_types == "triangle"]

    # Extract and normalize node coordinates
    node_coords <- as.data.frame(sem_paths$layout)
    colnames(node_coords) <- c("x", "y")
    node_coords$x <- (node_coords$x - mean(range(node_coords$x))) * relative_x_position + center_x
    node_coords$y <- (node_coords$y - mean(range(node_coords$y))) * relative_y_position + center_y
    node_coords$name <- node_names

    # Process edges
    edges_df0 <- data.frame(
      from = sem_paths$Edgelist$from,
      to = sem_paths$Edgelist$to,
      weight = sem_paths$Edgelist$weight,
      directed = sem_paths$Edgelist$directed,
      bidirectional = sem_paths$Edgelist$bidirectional,
      labels = sem_paths$graphAttributes$Edges$labels,
      sig = ifelse(sem_paths$graphAttributes$Edge$color == "#000000FF", TRUE, FALSE)
    )
    edges_df <- edges_df0[!duplicated(
      t(apply(edges_df0[c("from", "to")], 1, sort))
    ), ]

    edges_df <- edges_df[edges_df$from != edges_df$to, ]

    # Handle intercepts
    intercept_indices <- which(node_names == "1")
    intercept_sources <- character(length(intercept_indices))

    for (i in seq_along(intercept_indices)) {
      intercept_idx <- intercept_indices[i]

      connected_edges <- edges_df[edges_df$from == intercept_idx | edges_df$to == intercept_idx, ]

      if (nrow(connected_edges) > 0) {
        target_nodes <- c(connected_edges$from, connected_edges$to)
        target_nodes <- target_nodes[target_nodes != intercept_idx]

        if (length(target_nodes) > 0) {
          target_var <- node_names[target_nodes[1]]
          intercept_sources[i] <- paste0("Intercept_", target_var)
        } else {
          intercept_sources[i] <- paste0("Intercept_", i)
        }
      } else {
        intercept_sources[i] <- paste0("Intercept_", i)
      }
    }

    node_names[intercept_indices] <- intercept_sources

    # Edge properties
    edge_op <- ifelse(edges_df$bidirectional, "~~", "~")
    edges_from <- node_names[edges_df$from]
    edges_to <- node_names[edges_df$to]
    edge_labels <- edges_df$labels
    edge_sig <- edges_df$sig

    # Node properties
    node_shapes <- ifelse(node_names %in% intercept_sources, int_shape,
                          ifelse(node_names %in% latent_vars, latent_shape, observed_shape))
    node_colors <- ifelse(node_names %in% intercept_sources, point_color_int,
                          ifelse(node_names %in% latent_vars, point_color_latent, point_color_observed))
    node_sizes <- ifelse(node_names %in% intercept_sources, point_size_int,
                         ifelse(node_names %in% latent_vars, point_size_latent, point_size_observed))
    node_width_height_ratios <- ifelse(node_shapes %in% c("rectangle", "oval"), 1.6, 1)

  } else {

    stop("Must be output from semPaths with class of 'qgraph'.")

  }


  # Apply node position modifications
  if (modify_params_node_xy) {
    node_coords <- apply_modifications(
      node_coords,
      modified_nodes_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0), # No direct column mods
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )

  }

  # Create points dataframe
  points_df <- data.frame(
    x = node_coords$x,
    y = node_coords$y,
    shape = node_shapes,
    color = node_colors,
    size = node_sizes,
    border_color = node_border_color,
    border_width = node_border_width,
    alpha = 1,
    width_height_ratio = node_width_height_ratios,
    orientation = 0,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  #Apply node modifications
  if (modify_params_node) {
    points_df <- apply_modifications(
      points_df,
      modified_nodes,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "alpha", "shape", "size", "border_color", "border_width", "width_height_ratio"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }


  # Create annotations
  annotations <- data.frame(
    text = node_coords$name,
    x = node_coords$x,
    y = node_coords$y,
    font = ifelse(node_names %in% latent_vars, text_font_latent, text_font_others),
    size = ifelse(node_names %in% latent_vars, text_size_latent, text_size_others),
    color = ifelse(node_names %in% latent_vars, text_color_latent, text_color_others),
    fill = NA,
    angle = 0,
    alpha = ifelse(node_names %in% latent_vars, text_alpha_latent, text_alpha_others),
    fontface = ifelse(node_names %in% latent_vars, text_fontface_latent, text_fontface_others),
    math_expression = FALSE,
    hjust = 0.5,
    vjust = 0.5,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group_label = FALSE,
    loop_label = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )


  if (modify_params_nodelabel) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "size", "alpha", "angle", "font", "fontface"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }

  if (modify_params_nodelabel_xy) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )
  }



  if (modify_params_nodelabel_text) {
    if (nrow(modified_nodelabels_text) > 0) {
      for (i in seq_len(nrow(modified_nodelabels_text))) {
        mod <- modified_nodelabels_text[i, ]
        node_idx <- which(
          node_coords$name == mod$text
        )
        if (length(node_idx) == 1) {
          annotations$text[[node_idx]] <- mod$nodelabel
        }
      }
    }
  }


  if (length(edges_from) == 0 || length(edges_to) == 0) {
    stop("No edges found in the model. Check the Lavaan syntax.")
  }

  # Create lines dataframe
  lines_df_pre <- data.frame(
    x_start = node_coords[match(edges_from, node_names), "x"],
    y_start = node_coords[match(edges_from, node_names), "y"],
    x_end = node_coords[match(edges_to, node_names), "x"],
    y_end = node_coords[match(edges_to, node_names), "y"],
    ctrl_x = NA,
    ctrl_y = NA,
    ctrl_x2 = NA,
    ctrl_y2 = NA,
    curvature_magnitude = NA,
    rotate_curvature = NA,
    curvature_asymmetry = NA,
    type = ifelse(edge_op == "~~", "Curved Arrow", "Straight Arrow"),
    color = edge_color,
    end_color = NA,
    color_type = "Single",
    gradient_position = NA,
    width = line_width,
    alpha = line_alpha,
    arrow = TRUE,
    arrow_type = arrow_type,
    arrow_size = arrow_size,
    two_way = edge_op == "~~",
    lavaan = TRUE,
    network = FALSE,
    line_style = "solid",
    locked = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  # Apply edge modifications
  if (modify_params_edge) {
    lines_df_pre <- apply_modifications(
      lines_df_pre,
      modified_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "width", "alpha", "line_style", "end_color", "gradient_position", "color_type")
      ),
      mode = 'edge'
    )
  }


  # Adjust edge coordinates
  edge_list <- cbind(match(edges_from, node_names), match(edges_to, node_names))
  lines_df <- adjust_edge_coordinates(
    lines_df = lines_df_pre,
    edge_list = edge_list,
    points_df = points_df,
    auto_endpoint_spacing = line_endpoint_spacing
  )

  # Highlight significant paths if requested
  edge_sig_idx <- which(edges_df$sig == TRUE)
  non_edge_sig_idx <- which(edges_df$sig == FALSE)
  if (highlight_sig_path) {
    lines_df$color[edge_sig_idx] <- sig_path_color
    lines_df$color[non_edge_sig_idx] <- non_sig_path_color
  }


  if ("two_way" %in% colnames(lines_df) && any(lines_df$two_way, na.rm = TRUE)) {
    lines_df[lines_df$two_way, c("x_start", "x_end", "y_start", "y_end")] <-
      lines_df[lines_df$two_way, c("x_start", "x_end", "y_start", "y_end")] +
      c(lavaan_curved_x_shift, lavaan_curved_x_shift, lavaan_curved_y_shift, lavaan_curved_y_shift)
  }


  if (any(lines_df$two_way)) {
    two_way_indices <- which(lines_df$two_way)
    control_points <- mapply(
      calculate_control_point,
      x_start = lines_df$x_start[two_way_indices],
      y_start = lines_df$y_start[two_way_indices],
      x_end = lines_df$x_end[two_way_indices],
      y_end = lines_df$y_end[two_way_indices],
      curvature_magnitude = lavaan_curvature_magnitude,
      rotate_curvature = lavaan_rotate_curvature,
      curvature_asymmetry = lavaan_curvature_asymmetry,
      SIMPLIFY = FALSE
    )
    lines_df$ctrl_x[two_way_indices] <- sapply(control_points, `[[`, "ctrl_x")
    lines_df$ctrl_y[two_way_indices] <- sapply(control_points, `[[`, "ctrl_y")
    lines_df$ctrl_x2[two_way_indices] <- sapply(control_points, `[[`, "ctrl_x2")
    lines_df$ctrl_y2[two_way_indices] <- sapply(control_points, `[[`, "ctrl_y2")

    lines_df$curvature_magnitude[two_way_indices] <- lavaan_curvature_magnitude
    lines_df$rotate_curvature[two_way_indices] <- lavaan_rotate_curvature
    lines_df$curvature_asymmetry[two_way_indices] <- lavaan_curvature_asymmetry
  }

  # Apply covariance edge modifications
  if (modify_params_cov_edge) {
    lines_df <- apply_modifications(
      lines_df,
      modified_cov_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = NULL,
        special_case = function(data, idx, mod) {
          # Add input validation
          if (length(idx) == 1 &&
              all(c("x_start", "y_start", "x_end", "y_end") %in% names(data)) &&
              all(c("curvature_magnitude", "rotate_curvature", "curvature_asymmetry", "x_shift", "y_shift") %in% names(mod))) {

            data$x_start[idx] <- data$x_start[idx] + mod$x_shift
            data$x_end[idx] <- data$x_end[idx] + mod$x_shift
            data$y_start[idx] <- data$y_start[idx] + mod$y_shift
            data$y_end[idx] <- data$y_end[idx] + mod$y_shift

            cp <- calculate_control_point(
              x_start = data$x_start[idx],
              y_start = data$y_start[idx],
              x_end = data$x_end[idx],
              y_end = data$y_end[idx],
              curvature_magnitude = mod$curvature_magnitude,
              rotate_curvature = mod$rotate_curvature,
              curvature_asymmetry = mod$curvature_asymmetry
            )

            # Safely assign control points
            if (all(c("ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2", "locked") %in% names(data))) {
              data$ctrl_x[idx] <- cp$ctrl_x
              data$ctrl_y[idx] <- cp$ctrl_y
              data$ctrl_x2[idx] <- cp$ctrl_x2
              data$ctrl_y2[idx] <- cp$ctrl_y2
              data$locked[idx] <- FALSE
            }
          }
          return(data)
        }
      ),
      mode = 'edge'
    )
  }

  if (modify_params_edge_xy) {
    lines_df <- apply_modifications(
      lines_df,
      modified_edges_xy,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x_start[idx] <- data$x_start[idx] + mod$start_x_shift
          data$y_start[idx] <- data$y_start[idx] + mod$start_y_shift
          data$x_end[idx] <- data$x_end[idx] + mod$end_x_shift
          data$y_end[idx] <- data$y_end[idx] + mod$end_y_shift
          return(data)
        }
      ),
      mode = 'edge'
    )
  }


  # Handle arrow location
  if (exists("lavaan_arrow_location") && lavaan_arrow_location == "start") {
    temp <- lines_df[, c("x_start", "y_start")]
    lines_df[, c("x_start", "y_start")] <- lines_df[, c("x_end", "y_end")]
    lines_df[, c("x_end", "y_end")] <- temp
  }

  # Prepare edge labels
  lines_df0 <- cbind(lines_df, from = edges_from, to = edges_to, text = edge_labels)
  edgelabels_xy_df <- data.frame(x = numeric(nrow(lines_df0)), y = numeric(nrow(lines_df0)))


  for (i in seq_len(nrow(lines_df0))) {
    intp_points <- if (lines_df0$type[i] == "Curved Arrow") {
      create_bezier_curve(
        x_start = lines_df0$x_start[i], y_start = lines_df0$y_start[i],
        x_end = lines_df0$x_end[i], y_end = lines_df0$y_end[i],
        ctrl_x = lines_df0$ctrl_x[i], ctrl_y = lines_df0$ctrl_y[i],
        ctrl_x2 = lines_df0$ctrl_x2[i], ctrl_y2 = lines_df0$ctrl_y2[i], n_points = 100
      )
    } else {
      interpolate_points(
        x_start = lines_df0$x_start[i], y_start = lines_df0$y_start[i],
        x_end = lines_df0$x_end[i], y_end = lines_df0$y_end[i], n = 100
      )
    }
    edgelabels_xy_df[i, ] <- intp_points[50, c("x", "y")]
  }

  pval_idx <- grep("\\*", lines_df0$text)
  label_coords <- data.frame(
    x = edgelabels_xy_df$x,
    y = edgelabels_xy_df$y,
    text = lines_df0$text,
    font = text_font_edges,
    size = text_size_edges,
    color = text_color_edges,
    fill = ifelse(lines_df0$text == "", NA, text_color_fill),
    angle = 0,
    alpha = text_alpha_edges,
    fontface = text_fontface_edges,
    math_expression = FALSE,
    hjust = 0.5,
    vjust = 0.5,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group_label = FALSE,
    loop_label = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )


  if (highlight_sig_path) {
    label_coords$fontface[pval_idx] <- sig_label_fontface
    label_coords$fontface[-pval_idx] <- non_sig_label_fontface
  }

  # Apply edge label modifications
  if (modify_params_edgelabel) {
    label_coords <- apply_modifications(
      label_coords,
      modified_edgelabels,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "fill", "size", "alpha", "angle", "font", "fontface")
      ),
      mode = 'edge'
    )

  }

  points_df[c("x", "y")] <- lapply(points_df[c("x", "y")], round, 5)

  line_cols <- c("x_start", "y_start", "x_end", "y_end",
                 "ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2")
  lines_df[line_cols] <- lapply(lines_df[line_cols], round, 5)

  annotations[c("x", "y")] <- lapply(annotations[c("x", "y")], round, 5)

  # if (!is.null(data_file)) {
  # annotations <- rbind(annotations, label_coords)
  # }

  if (all(lines_df0$text == "")) {
    annotations <- annotations
  } else {
    annotations <- rbind(annotations, label_coords)
  }

  list(points = points_df, lines = lines_df, annotations = annotations)
}

#' Generate graph data from tidySEM objects
#'
#' This function converts tidySEM graph objects into standardized graph data structures
#' with customization options for node styling, edge properties, and
#' layout parameters.
#'
#' @return A list containing three data frames:
#'   - `points`: Node data with coordinates, shapes, colors, sizes, and properties
#'   - `lines`: Edge data with coordinates, curvatures, arrows, and styles
#'   - `annotations`: Text labels for nodes and edges with positioning and styling
#'
#'
#'
#' @importFrom dplyr filter mutate select distinct
#' @importFrom stats na.omit
#' @importFrom rlang .data
#'
#' @keywords internal
#' @noRd
generate_graph_from_tidySEM <- function(fit, relative_x_position = 25, relative_y_position = 25,
                                         center_x = 0, center_y = 0,
                                         latent_shape = "circle", observed_shape = "square",
                                         int_shape = "triangle",
                                         point_size_latent = 20, point_size_observed = 12,
                                         point_size_int = 10,
                                         line_width = 1, line_alpha = 1, text_size_latent = 18, text_font_latent = "sans",
                                         text_color_latent = "#FFFFFF", text_alpha_latent = 1, text_fontface_latent = 'plain',
                                         text_size_others = 16, text_font_others = "sans",
                                         text_color_others = "#FFFFFF", text_alpha_others = 1, text_fontface_others = 'plain',
                                         text_size_edges = 14, text_font_edges = "sans",
                                         text_color_edges =  "#000000", text_color_fill = "#FFFFFF", text_alpha_edges = 1, text_fontface_edges = 'plain',
                                         point_color_latent = "#cc3d3d", point_color_observed = "#1262b3",
                                         point_color_int = "#0f993d",
                                         edge_color = "#000000", line_endpoint_spacing = 0.2,
                                         node_border_color = "#FFFFFF",
                                         node_border_width = 1,
                                         arrow_type = "closed", arrow_size = 0.1,
                                         lavaan_arrow_location = "end",
                                         zoom_factor = 1.2,
                                         lavaan_curvature_magnitude = 0.3,
                                         lavaan_rotate_curvature = FALSE,
                                         lavaan_curvature_asymmetry = 0,
                                         lavaan_curved_x_shift = 0,
                                         lavaan_curved_y_shift = 0,
                                         highlight_sig_path = FALSE,
                                         sig_path_color = "#000000",
                                         non_sig_path_color = "#000000",
                                         sig_label_fontface = "plain",
                                         non_sig_label_fontface = "plain",
                                         data_file = NULL,
                                         modify_params_edge = FALSE,
                                         modified_edges = NULL,
                                         modify_params_edgelabel = FALSE,
                                         modified_edgelabels = NULL,
                                         modify_params_node = FALSE,
                                         modified_nodes = NULL,
                                         modify_params_node_xy = FALSE,
                                         modified_nodes_xy = NULL,
                                         modify_params_edge_xy = FALSE,
                                         modified_edges_xy = NULL,
                                         modify_params_cov_edge = FALSE,
                                         modified_cov_edges = NULL,
                                         modify_params_nodelabel = FALSE,
                                         modified_nodelabels = NULL,
                                         modify_params_nodelabel_xy = FALSE,
                                         modified_nodelabels_xy = NULL,
                                         modify_params_nodelabel_text = FALSE,
                                         modified_nodelabels_text = NULL,
                                         which_group = "tidySEM") {

  apply_modifications <- function(data, modifications, config, mode) {
    if (is.null(modifications) || nrow(modifications) == 0) return(data)
    modified_data <- data

    for (i in seq_len(nrow(modifications))) {
      mod <- modifications[i, ]

      if (mode == 'edge') {
        idx <- which(
          edges_from == mod$lhs &
            edges_to == mod$rhs
        )
      } else if (mode == 'node') {
        idx <- which(
          node_coords$name == mod$text
        )
      }

      #print(idx)
      if (length(idx) == 1) {
        for (col in config$modify_cols) {
          if (col %in% names(mod) && col %in% names(modified_data)) {
            modified_data[idx, col] <- mod[[col]]
          }
        }

        if (!is.null(config$special_case)) {
          modified_data <- config$special_case(modified_data, idx, mod)
        }
      }
    }
    return(modified_data)
  }


  if (inherits(fit, "sem_graph")) {
    graph_data <- fit

    multi_group <- "group" %in% names(graph_data$nodes)

    if (multi_group) {
      nodes_data <- graph_data$nodes[graph_data$nodes$group == which_group,]
      edges_data <- graph_data$edges[graph_data$edges$group == which_group,]
    } else {
      nodes_data <- graph_data$nodes
      edges_data <- graph_data$edges
    }

    node_names <- nodes_data$name
    node_types <- nodes_data$shape

    latent_vars <- node_names[node_types == "oval"]
    observed_vars <- node_names[node_types == "rect"]
    intercept_vars <- node_names[node_types == "triangle"]

    max_val <- max(nodes_data$x, nodes_data$y) * 0.25

    # Normalize x and y relative to the same max
    nodes_data$x <- nodes_data$x / max_val
    nodes_data$y <- nodes_data$y / max_val

    # Extract node coordinates and node names
    node_coords <- as.data.frame(nodes_data[,c('x','y','name')])

    # Normalize coordinates to center the graph
    node_coords$x <- (node_coords$x - mean(range(node_coords$x))) * relative_x_position + center_x
    node_coords$y <- (node_coords$y - mean(range(node_coords$y))) * relative_y_position + center_y
    node_coords$name <- node_names

    edges_df0 <- data.frame(
      from = edges_data$from,
      to = edges_data$to,
      directed = edges_data$arrow == "last", # directed
      bidirectional = edges_data$arrow == "none", # covariance
      labels = edges_data$label,
      sig = rep(TRUE, nrow(edges_data))
    )

    edges_df <- edges_df0[!duplicated(
      t(apply(edges_df0[c("from", "to")], 1, sort))
    ), ]

    edges_df <- edges_df[edges_df$from != edges_df$to, ]

    edge_op <- ifelse(edges_df$bidirectional, "~~", "~")
    edges_from <- edges_df$from
    edges_to <- edges_df$to
    edge_labels <- edges_df$labels


    node_shapes <- ifelse(node_names %in% intercept_vars, int_shape, # Triangular shape for the intercept node
                          ifelse(node_names %in% latent_vars, latent_shape, observed_shape))
    node_colors <- ifelse(node_names %in% intercept_vars, point_color_int,
                          ifelse(node_names %in% latent_vars, point_color_latent, point_color_observed))
    node_sizes <- ifelse(node_names %in% intercept_vars, point_size_int,
                         ifelse(node_names %in% latent_vars, point_size_latent, point_size_observed))

    node_width_height_ratios <- ifelse(node_shapes %in% c("rectangle", "oval"), 1.6, 1)

  } else {

    stop("Must be output from tidySEM with class of 'sem_graph'.")

  }

  # Apply node position modifications
  if (modify_params_node_xy) {
    node_coords <- apply_modifications(
      node_coords,
      modified_nodes_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0), # No direct column mods
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )

  }

  # Create points dataframe
  points_df <- data.frame(
    x = node_coords$x,
    y = node_coords$y,
    shape = node_shapes,
    color = node_colors,
    size = node_sizes,
    border_color = node_border_color,
    border_width = node_border_width,
    alpha = 1,
    width_height_ratio = node_width_height_ratios,
    orientation = 0,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  #Apply node modifications
  if (modify_params_node) {
    points_df <- apply_modifications(
      points_df,
      modified_nodes,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "alpha", "shape", "size", "border_color", "border_width", "width_height_ratio"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }


  # Create annotations
  annotations <- data.frame(
    text = node_coords$name,
    x = node_coords$x,
    y = node_coords$y,
    font = ifelse(node_names %in% latent_vars, text_font_latent, text_font_others),
    size = ifelse(node_names %in% latent_vars, text_size_latent, text_size_others),
    color = ifelse(node_names %in% latent_vars, text_color_latent, text_color_others),
    fill = NA,
    angle = 0,
    alpha = ifelse(node_names %in% latent_vars, text_alpha_latent, text_alpha_others),
    fontface = ifelse(node_names %in% latent_vars, text_fontface_latent, text_fontface_others),
    math_expression = FALSE,
    hjust = 0.5,
    vjust = 0.5,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group_label = FALSE,
    loop_label = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )


  if (modify_params_nodelabel) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "size", "alpha", "angle", "font", "fontface"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }

  if (modify_params_nodelabel_xy) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )
  }



  if (modify_params_nodelabel_text) {
    if (nrow(modified_nodelabels_text) > 0) {
      for (i in seq_len(nrow(modified_nodelabels_text))) {
        mod <- modified_nodelabels_text[i, ]
        node_idx <- which(
          node_coords$name == mod$text
        )
        if (length(node_idx) == 1) {
          annotations$text[[node_idx]] <- mod$nodelabel
        }
      }
    }
  }


  if (length(edges_from) == 0 || length(edges_to) == 0) {
    stop("No edges found in the model. Check the Lavaan syntax.")
  }

  # Create lines dataframe
  lines_df_pre <- data.frame(
    x_start = node_coords[match(edges_from, node_names), "x"],
    y_start = node_coords[match(edges_from, node_names), "y"],
    x_end = node_coords[match(edges_to, node_names), "x"],
    y_end = node_coords[match(edges_to, node_names), "y"],
    ctrl_x = NA,
    ctrl_y = NA,
    ctrl_x2 = NA,
    ctrl_y2 = NA,
    curvature_magnitude = NA,
    rotate_curvature = NA,
    curvature_asymmetry = NA,
    type = ifelse(edge_op == "~~", "Curved Arrow", "Straight Arrow"),
    color = edge_color,
    end_color = NA,
    color_type = "Single",
    gradient_position = NA,
    width = line_width,
    alpha = line_alpha,
    arrow = TRUE,
    arrow_type = arrow_type,
    arrow_size = arrow_size,
    two_way = edge_op == "~~",
    lavaan = TRUE,
    network = FALSE,
    line_style = "solid",
    locked = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  # Apply edge modifications
  if (modify_params_edge) {
    lines_df_pre <- apply_modifications(
      lines_df_pre,
      modified_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "width", "alpha", "line_style", "end_color", "gradient_position", "color_type")
      ),
      mode = 'edge'
    )
  }


  # Adjust edge coordinates
  edge_list <- cbind(match(edges_from, node_names), match(edges_to, node_names))
  lines_df <- adjust_edge_coordinates(
    lines_df = lines_df_pre,
    edge_list = edge_list,
    points_df = points_df,
    auto_endpoint_spacing = line_endpoint_spacing
  )

  # Highlight significant paths if requested
  edge_sig_idx <- which(edges_df$sig == TRUE)
  non_edge_sig_idx <- which(edges_df$sig == FALSE)
  if (highlight_sig_path) {
    lines_df$color[edge_sig_idx] <- sig_path_color
    lines_df$color[non_edge_sig_idx] <- non_sig_path_color
  }


  if ("two_way" %in% colnames(lines_df) && any(lines_df$two_way, na.rm = TRUE)) {
    lines_df[lines_df$two_way, c("x_start", "x_end", "y_start", "y_end")] <-
      lines_df[lines_df$two_way, c("x_start", "x_end", "y_start", "y_end")] +
      c(lavaan_curved_x_shift, lavaan_curved_x_shift, lavaan_curved_y_shift, lavaan_curved_y_shift)
  }


  if (any(lines_df$two_way)) {
    two_way_indices <- which(lines_df$two_way)
    control_points <- mapply(
      calculate_control_point,
      x_start = lines_df$x_start[two_way_indices],
      y_start = lines_df$y_start[two_way_indices],
      x_end = lines_df$x_end[two_way_indices],
      y_end = lines_df$y_end[two_way_indices],
      curvature_magnitude = lavaan_curvature_magnitude,
      rotate_curvature = lavaan_rotate_curvature,
      curvature_asymmetry = lavaan_curvature_asymmetry,
      center_x = mean(node_coords$x),
      center_y = mean(node_coords$y),
      SIMPLIFY = FALSE
    )
    lines_df$ctrl_x[two_way_indices] <- sapply(control_points, `[[`, "ctrl_x")
    lines_df$ctrl_y[two_way_indices] <- sapply(control_points, `[[`, "ctrl_y")
    lines_df$ctrl_x2[two_way_indices] <- sapply(control_points, `[[`, "ctrl_x2")
    lines_df$ctrl_y2[two_way_indices] <- sapply(control_points, `[[`, "ctrl_y2")

    lines_df$curvature_magnitude[two_way_indices] <- lavaan_curvature_magnitude
    lines_df$rotate_curvature[two_way_indices] <- lavaan_rotate_curvature
    lines_df$curvature_asymmetry[two_way_indices] <- lavaan_curvature_asymmetry
  }

  # Apply covariance edge modifications
  if (modify_params_cov_edge) {
    lines_df <- apply_modifications(
      lines_df,
      modified_cov_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = NULL,
        special_case = function(data, idx, mod) {
          # Add input validation
          if (length(idx) == 1 &&
              all(c("x_start", "y_start", "x_end", "y_end") %in% names(data)) &&
              all(c("curvature_magnitude", "rotate_curvature", "curvature_asymmetry", "x_shift", "y_shift") %in% names(mod))) {

            data$x_start[idx] <- data$x_start[idx] + mod$x_shift
            data$x_end[idx] <- data$x_end[idx] + mod$x_shift
            data$y_start[idx] <- data$y_start[idx] + mod$y_shift
            data$y_end[idx] <- data$y_end[idx] + mod$y_shift

            cp <- calculate_control_point(
              x_start = data$x_start[idx],
              y_start = data$y_start[idx],
              x_end = data$x_end[idx],
              y_end = data$y_end[idx],
              curvature_magnitude = mod$curvature_magnitude,
              rotate_curvature = mod$rotate_curvature,
              curvature_asymmetry = mod$curvature_asymmetry,
              center_x = mean(node_coords$x),
              center_y = mean(node_coords$y)
            )

            # Safely assign control points
            if (all(c("ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2", "locked") %in% names(data))) {
              data$ctrl_x[idx] <- cp$ctrl_x
              data$ctrl_y[idx] <- cp$ctrl_y
              data$ctrl_x2[idx] <- cp$ctrl_x2
              data$ctrl_y2[idx] <- cp$ctrl_y2
              data$locked[idx] <- FALSE
            }
          }
          return(data)
        }
      ),
      mode = 'edge'
    )
  }

  if (modify_params_edge_xy) {
    lines_df <- apply_modifications(
      lines_df,
      modified_edges_xy,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x_start[idx] <- data$x_start[idx] + mod$start_x_shift
          data$y_start[idx] <- data$y_start[idx] + mod$start_y_shift
          data$x_end[idx] <- data$x_end[idx] + mod$end_x_shift
          data$y_end[idx] <- data$y_end[idx] + mod$end_y_shift
          return(data)
        }
      ),
      mode = 'edge'
    )
  }


  # Handle arrow location
  if (exists("lavaan_arrow_location") && lavaan_arrow_location == "start") {
    temp <- lines_df[, c("x_start", "y_start")]
    lines_df[, c("x_start", "y_start")] <- lines_df[, c("x_end", "y_end")]
    lines_df[, c("x_end", "y_end")] <- temp
  }

  # Prepare edge labels
  lines_df0 <- cbind(lines_df, from = edges_from, to = edges_to, text = edge_labels)
  edgelabels_xy_df <- data.frame(x = numeric(nrow(lines_df0)), y = numeric(nrow(lines_df0)))

  for (i in seq_len(nrow(lines_df0))) {
    intp_points <- if (lines_df0$type[i] == "Curved Arrow") {
      create_bezier_curve(
        x_start = lines_df0$x_start[i], y_start = lines_df0$y_start[i],
        x_end = lines_df0$x_end[i], y_end = lines_df0$y_end[i],
        ctrl_x = lines_df0$ctrl_x[i], ctrl_y = lines_df0$ctrl_y[i],
        ctrl_x2 = lines_df0$ctrl_x2[i], ctrl_y2 = lines_df0$ctrl_y2[i], n_points = 100
      )
    } else {
      interpolate_points(
        x_start = lines_df0$x_start[i], y_start = lines_df0$y_start[i],
        x_end = lines_df0$x_end[i], y_end = lines_df0$y_end[i], n = 100
      )
    }
    edgelabels_xy_df[i, ] <- intp_points[50, c("x", "y")]
  }

  pval_idx <- grep("\\*", lines_df0$text)
  label_coords <- data.frame(
    x = edgelabels_xy_df$x,
    y = edgelabels_xy_df$y,
    text = lines_df0$text,
    font = text_font_edges,
    size = text_size_edges,
    color = text_color_edges,
    fill = ifelse(lines_df0$text == "", NA, text_color_fill),
    angle = 0,
    alpha = text_alpha_edges,
    fontface = text_fontface_edges,
    math_expression = FALSE,
    hjust = 0.5,
    vjust = 0.5,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group_label = FALSE,
    loop_label = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )


  if (highlight_sig_path) {
    label_coords$fontface[pval_idx] <- sig_label_fontface
    label_coords$fontface[-pval_idx] <- non_sig_label_fontface
  }

  # Apply edge label modifications
  if (modify_params_edgelabel) {
    label_coords <- apply_modifications(
      label_coords,
      modified_edgelabels,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "fill", "size", "alpha", "angle", "font", "fontface")
      ),
      mode = 'edge'
    )

  }

  points_df[c("x", "y")] <- lapply(points_df[c("x", "y")], round, 5)

  line_cols <- c("x_start", "y_start", "x_end", "y_end",
                 "ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2")
  lines_df[line_cols] <- lapply(lines_df[line_cols], round, 5)

  # if (!is.null(data_file)) {
  # annotations <- rbind(annotations, label_coords)
  # }

  if (all(lines_df0$text == "")) {
    annotations <- annotations
  } else {
    annotations <- rbind(annotations, label_coords)
  }

  annotations[c("x", "y")] <- lapply(annotations[c("x", "y")], round, 5)

  list(points = points_df, lines = lines_df, annotations = annotations)
}

#' Generate graph data from tidySEM objects with enhanced multi-group support
#'
#' Enhanced version of generate_graph_from_tidySEM that provides advanced multi-group
#' comparison features, latent variable group rotation, and comprehensive modification
#' capabilities for complex SEM visualizations.
#'
#' @return A list containing three data frames:
#'   - `points`: Node data with coordinates, shapes, colors, sizes, and properties
#'   - `lines`: Edge data with coordinates, curvatures, arrows, and styles
#'   - `annotations`: Text labels for nodes and edges with positioning and styling
#'   - `loops`: Loop data with coordinates, etc
#'
#'
#' The function includes advanced group rotation logic that rotates entire
#' latent-observed variable configurations around the latent variable center,
#' maintaining structural relationships while enabling layout optimization for
#' complex multi-group comparisons.
#'
#' @importFrom dplyr filter mutate select distinct
#' @importFrom stats na.omit
#' @importFrom rlang .data
#'
#' @keywords internal
#' @noRd
generate_graph_from_tidySEM1 <- function(fit, fit_delta, relative_x_position = 25, relative_y_position = 25,
                                         center_x = 0, center_y = 0,
                                         latent_shape = "circle", observed_shape = "square",
                                         int_shape = "triangle",
                                         point_size_latent = 20, point_size_observed = 12,
                                         point_size_int = 10,
                                         width_height_ratio_latent = 1,
                                         width_height_ratio_observed = 1,
                                         width_height_ratio_int = 1,
                                         line_width = 1, line_alpha = 1, text_size_latent = 18, text_font_latent = "sans",
                                         text_color_latent = "#FFFFFF", text_alpha_latent = 1, text_fontface_latent = 'plain',
                                         text_size_others = 16, text_font_others = "sans",
                                         text_color_others = "#FFFFFF", text_alpha_others = 1, text_fontface_others = 'plain',
                                         text_size_edges = 14, text_font_edges = "sans",
                                         text_color_edges =  "#000000", text_color_fill = "#FFFFFF", text_alpha_edges = 1, text_fontface_edges = 'plain',
                                         point_color_latent = "#cc3d3d", point_color_observed = "#1262b3",
                                         point_color_int = "#0f993d",
                                         edge_color = "#000000", line_endpoint_spacing = 1.2,
                                         node_border_color = "#FFFFFF",
                                         node_border_width = 1,
                                         arrow_type = "closed", arrow_size = 0.1,
                                         lavaan_arrow_location = "end",
                                         zoom_factor = 1.2,
                                         lavaan_curvature_magnitude = 0.5,
                                         lavaan_rotate_curvature = FALSE,
                                         lavaan_curvature_asymmetry = 0,
                                         lavaan_curved_x_shift = 0,
                                         lavaan_curved_y_shift = 0,
                                         remove_edgelabels = FALSE,
                                         highlight_free_path = FALSE,
                                         ff_params_edge = NULL,
                                         ff_params_edgelabel = NULL,
                                         ff_params_loop = NULL,
                                         ff_params_looplabel = NULL,
                                         highlight_free_path_multi_group = FALSE,
                                         ff_params_edge_multi = NULL,
                                         ff_params_edgelabel_multi = NULL,
                                         ff_params_loop_multi= NULL,
                                         ff_params_looplabel_multi = NULL,
                                         highlight_sig_path = FALSE,
                                         sig_path_color = "#000000",
                                         non_sig_path_color = "#000000",
                                         sig_label_fontface = "plain",
                                         non_sig_label_fontface = "plain",
                                         highlight_multi_group = FALSE,
                                         sig_diff_edge = NULL,
                                         sig_diff_edgelabel = NULL,
                                         sig_diff_loop = NULL,
                                         sig_diff_looplabel = NULL,
                                         residuals = FALSE,
                                         residuals_orientation_type = 'Graded',
                                         lavaan_loop_offset = 0.8,
                                         lavaan_radius = 2.5,
                                         lavaan_line_color_loop = "#000000",
                                         lavaan_line_alpha_loop = 1,
                                         lavaan_arrow_type_loop = "closed",
                                         lavaan_arrow_size_loop = 0.08,
                                         lavaan_width_loop = 1,
                                         lavaan_height_loop = 1,
                                         lavaan_gap_size_loop = 0.05,
                                         lavaan_two_way_arrow_loop = TRUE,
                                         data_file = NULL,
                                         modify_params_edge = FALSE,
                                         modified_edges = NULL,
                                         modify_params_edgelabel = FALSE,
                                         modified_edgelabels = NULL,
                                         modify_params_edgelabel_xy = FALSE,
                                         modified_edgelabels_xy = NULL,
                                         modify_params_edgelabel_text = FALSE,
                                         modified_edgelabels_text = NULL,
                                         modify_params_node = FALSE,
                                         modified_nodes = NULL,
                                         modify_params_node_xy = FALSE,
                                         modified_nodes_xy = NULL,
                                         modify_params_edge_xy = FALSE,
                                         modified_edges_xy = NULL,
                                         modify_params_cov_edge = FALSE,
                                         modified_cov_edges = NULL,
                                         modify_params_nodelabel = FALSE,
                                         modified_nodelabels = NULL,
                                         modify_params_nodelabel_xy = FALSE,
                                         modified_nodelabels_xy = NULL,
                                         modify_params_nodelabel_text = FALSE,
                                         modified_nodelabels_text = NULL,
                                         modify_params_latent_node_xy = FALSE,
                                         modified_latent_nodes_xy = NULL,
                                         modify_params_latent_node_angle = FALSE,
                                         modified_latent_nodes_angle = NULL,
                                         modify_params_loop = FALSE,
                                         modified_loops = NULL,
                                         modify_params_loop_xy = FALSE,
                                         modified_loops_xy = NULL,
                                         modify_params_loop_location = FALSE,
                                         modified_loops_location = NULL,
                                         modify_params_looplabel = FALSE,
                                         modified_looplabels = NULL,
                                         modify_params_looplabel_xy = FALSE,
                                         modified_looplabels_xy = NULL,
                                         modify_params_looplabel_text = FALSE,
                                         modified_looplabels_text = NULL,
                                         loop_names_remove = NULL,
                                         flip_layout = FALSE,
                                         flip_direction = NULL,
                                         rotate_layout = FALSE,
                                         rotate_angle = 0,
                                         which_group = "1",
                                         group_level = NULL) {

  if (inherits(fit, "sem_graph")) {
    graph_data <- fit

    multi_group <- "group" %in% names(graph_data$nodes) && (length(unique(graph_data$nodes$group)) > 1)
    # multi_group <- FALSE
    if (multi_group) {
      nodes_data <- graph_data$nodes[graph_data$nodes$group == group_level,]
      edges_data <- graph_data$edges[graph_data$edges$group == group_level,]
      fit_delta_edges <- fit_delta$edges[fit_delta$edges$group == group_level,]
    } else {
      nodes_data <- graph_data$nodes
      edges_data <- graph_data$edges
      fit_delta_edges <- fit_delta$edges
    }

    node_names <- nodes_data$name
    node_types <- nodes_data$shape

    latent_vars <- node_names[node_types == "oval"]
    observed_vars <- node_names[node_types == "rect"]
    intercept_vars <- node_names[node_types == "triangle"]

    max_val <- max(nodes_data$x, nodes_data$y) * 0.25

    # Normalize x and y relative to the same max
    nodes_data$x <- nodes_data$x / max_val
    nodes_data$y <- nodes_data$y / max_val

    # Extract node coordinates and node names
    node_coords <- as.data.frame(nodes_data[,c('x','y','name')])

    if (flip_layout) {
      flipped <- flip_around_center(node_coords, flip_direction)
      node_coords$x <- flipped$x
      node_coords$y <- flipped$y
    }

    if (rotate_layout) {
      rotated <- rotate_around_center(node_coords, rotate_angle)
      node_coords$x <- rotated$x
      node_coords$y <- rotated$y
    }


    # Normalize coordinates to center the graph
    node_coords$x <- (node_coords$x - mean(range(node_coords$x))) * relative_x_position + center_x
    node_coords$y <- (node_coords$y - mean(range(node_coords$y))) * relative_y_position + center_y
    node_coords$name <- node_names

    edges_df0 <- data.frame(
      from = edges_data$from,
      to = edges_data$to,
      directed = edges_data$arrow == "last", # directed
      bidirectional = edges_data$arrow == "none", # covariance
      labels = fit_delta_edges$label,
      sig = {
        # Safely check for pval column
        if ("pval" %in% names(edges_data) && !is.null(edges_data$pval)) {
          # Calculate significance, treating NA as FALSE
          ifelse(is.na(edges_data$pval), FALSE, edges_data$pval < 0.05)
        } else {
          # If pval doesn't exist or is NULL, all TRUE (Bayesian)
          rep(TRUE, nrow(edges_data))
        }
      }
    )

    self_loop_indices <- which(edges_df0$from == edges_df0$to)

    if (length(self_loop_indices) > 0) {
      edges_df0$self_loop <- FALSE
      edges_df0$self_loop[self_loop_indices] <- TRUE
    } else {
      edges_df0$self_loop <- FALSE
    }

    edges_df0 <- edges_df0[!duplicated(
      t(apply(edges_df0[c("from", "to")], 1, sort))
    ), ]

    edges_df <- edges_df0[edges_df0$from != edges_df0$to, ]
    edges_loop_df <- edges_df0[edges_df0$self_loop,]

    loop_node_names <- edges_loop_df$from
    loop_node_names <- loop_node_names[order(match(loop_node_names, node_names))]

    edge_op <- ifelse(edges_df$bidirectional, "~~", "~")
    edges_from <- edges_df$from
    edges_to <- edges_df$to
    edge_labels <- edges_df$labels


    node_shapes <- ifelse(node_names %in% intercept_vars, int_shape, # Triangular shape for the intercept node
                          ifelse(node_names %in% latent_vars, latent_shape, observed_shape))
    node_colors <- ifelse(node_names %in% intercept_vars, point_color_int,
                          ifelse(node_names %in% latent_vars, point_color_latent, point_color_observed))
    node_sizes <- ifelse(node_names %in% intercept_vars, point_size_int,
                         ifelse(node_names %in% latent_vars, point_size_latent, point_size_observed))

    node_width_height_ratios <- node_width_height_ratios <- ifelse(node_names %in% intercept_vars, width_height_ratio_int,
                                                                   ifelse(node_names %in% latent_vars, width_height_ratio_latent, width_height_ratio_observed))


  } else {

    stop("Must be output from tidySEM with class of 'sem_graph'.")

  }

  apply_modifications <- function(data, modifications, config, mode, batch_process = FALSE) {
    if (is.null(modifications) || nrow(modifications) == 0) return(data)
    modified_data <- data

    if (batch_process && !is.null(config$batch_special_case)) {
      mods <- modifications
      modified_data <- config$batch_special_case(modified_data, mods)
    } else {

      for (i in seq_len(nrow(modifications))) {
        mod <- modifications[i, ]

        if (mode == 'edge') {
          idx <- which(
            edges_from == mod$lhs &
              edges_to == mod$rhs
          )

        } else if (mode == 'node') {
          idx <- which(
            node_coords$name == mod$text
          )
        } else if (mode == 'loop') {
          idx <- which(
            node_coords$name[!node_coords$name %in% loop_names_remove] == mod$text
          )
        }

        if (length(idx) > 0) {
          for (col in config$modify_cols) {
            if (col %in% names(mod) && col %in% names(modified_data)) {
              modified_data[idx, col] <- mod[[col]]
            }
          }

          if (!is.null(config$special_case)) {
            modified_data <- config$special_case(modified_data, idx, mod)
          }
        }
      }
    }
    return(modified_data)
  }

  if (modify_params_latent_node_angle) {
    node_coords <- apply_modifications(
      node_coords,
      modified_latent_nodes_angle,
      config = list(
        batch_special_case = function(data, mods) {

          latent_positions <- which(mods$node_type == "latent")

          most_recent_groups <- list()

          for (i in seq_along(latent_positions)) {
            current_latent_pos <- latent_positions[i]

            if (i == length(latent_positions) ||
                (current_latent_pos + 1 < latent_positions[i + 1] &&
                 mods$node_type[current_latent_pos + 1] == "observed")) {

              group_rows <- current_latent_pos
              next_row <- current_latent_pos + 1

              while (next_row <= nrow(mods) && mods$node_type[next_row] == "observed") {
                group_rows <- c(group_rows, next_row)
                next_row <- next_row + 1
              }

              group_data <- mods[group_rows, ]
              latent_name <- group_data$text[1]

              if (is.null(most_recent_groups[[latent_name]]) ||
                  group_rows[1] > most_recent_groups[[latent_name]]$positions[1]) {
                most_recent_groups[[latent_name]] <- list(
                  data = group_data,
                  positions = group_rows
                )
              }
            }
          }

          final_mods_list <- lapply(most_recent_groups, function(x) x$data)

          for (i in  seq_along(final_mods_list)) {
            curr_mods <- final_mods_list[[i]]
            latent_mod <- curr_mods[curr_mods$node_type == "latent", ]
            observed_mods <- curr_mods[curr_mods$node_type == "observed", ]

            latent_node_idx <- which(data$name == latent_mod$text) # idx in node_coords
            obs_node_idx <- which(data$name %in% observed_mods$text)

            if (length(latent_node_idx) > 0 && length(obs_node_idx) > 0) {
              # Calculate centroid (latent node xy)
              center_x <- data$x[latent_node_idx]
              center_y <- data$y[latent_node_idx]

              angle_rad <- curr_mods$angle * pi / 180

              for (obs_idx in obs_node_idx) {
                if(length(obs_idx) > 0) {
                  x_relative <- data$x[obs_idx] - center_x
                  y_relative <- data$y[obs_idx] - center_y

                  new_x <- x_relative * cos(angle_rad) - y_relative * sin(angle_rad)
                  new_y <- x_relative * sin(angle_rad) + y_relative * cos(angle_rad)

                  data$x[obs_idx] <- new_x + center_x
                  data$y[obs_idx] <- new_y + center_y
                }
              }
            }
          }

          return(data)
        }
      ),
      mode = 'node', # ignored
      batch_process = TRUE
    )
  }


  # Apply node position modifications
  if (modify_params_node_xy) {
    node_coords <- apply_modifications(
      node_coords,
      modified_nodes_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0), # No direct column mods
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )
  }

  # Apply latent node group position modifications
  if (modify_params_latent_node_xy) {
    node_coords <- apply_modifications(
      node_coords,
      modified_latent_nodes_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0), # No direct column mods
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )
  }

  # Create points dataframe
  points_df <- data.frame(
    x = node_coords$x,
    y = node_coords$y,
    shape = node_shapes,
    color = node_colors,
    size = node_sizes,
    border_color = node_border_color,
    border_width = node_border_width,
    alpha = 1,
    width_height_ratio = node_width_height_ratios,
    orientation = 0,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  #Apply node modifications
  if (modify_params_node) {
    points_df <- apply_modifications(
      points_df,
      modified_nodes,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "alpha", "shape", "size", "border_color", "border_width", "width_height_ratio"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }

  # Create annotations
  annotations <- data.frame(
    text = node_coords$name,
    x = node_coords$x,
    y = node_coords$y,
    font = ifelse(node_names %in% latent_vars, text_font_latent, text_font_others),
    size = ifelse(node_names %in% latent_vars, text_size_latent, text_size_others),
    color = ifelse(node_names %in% latent_vars, text_color_latent, text_color_others),
    fill = NA,
    angle = 0,
    alpha = ifelse(node_names %in% latent_vars, text_alpha_latent, text_alpha_others),
    fontface = ifelse(node_names %in% latent_vars, text_fontface_latent, text_fontface_others),
    math_expression = FALSE,
    hjust = 0.5,
    vjust = 0.5,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group_label = FALSE,
    loop_label = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )


  if (modify_params_nodelabel) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "size", "alpha", "angle", "font", "fontface"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }

  if (modify_params_nodelabel_xy) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )
  }


  if (modify_params_nodelabel_text) {
    if (nrow(modified_nodelabels_text) > 0) {
      for (i in seq_len(nrow(modified_nodelabels_text))) {
        mod <- modified_nodelabels_text[i, ]
        node_idx <- which(
          node_coords$name == mod$text
        )
        if (length(node_idx) == 1) {
          annotations$text[[node_idx]] <- mod$nodelabel
          annotations$math_expression[[node_idx]] <- mod$math_expression
        }
      }
    }
  }


  if (length(edges_from) == 0 || length(edges_to) == 0) {
    stop("No edges found in the model. Check the Lavaan syntax.")
  }

  # Create lines dataframe
  lines_df_pre <- data.frame(
    x_start = node_coords[match(edges_from, node_names), "x"],
    y_start = node_coords[match(edges_from, node_names), "y"],
    x_end = node_coords[match(edges_to, node_names), "x"],
    y_end = node_coords[match(edges_to, node_names), "y"],
    ctrl_x = NA,
    ctrl_y = NA,
    ctrl_x2 = NA,
    ctrl_y2 = NA,
    curvature_magnitude = NA,
    rotate_curvature = NA,
    curvature_asymmetry = NA,
    type = ifelse(edge_op == "~~", "Curved Arrow", "Straight Arrow"),
    color = edge_color,
    end_color = NA,
    color_type = "Single",
    gradient_position = NA,
    width = line_width,
    alpha = line_alpha,
    arrow = TRUE,
    arrow_type = arrow_type,
    arrow_size = arrow_size,
    two_way = edge_op == "~~",
    lavaan = TRUE,
    network = FALSE,
    line_style = "solid",
    locked = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  # Apply edge modifications

  edge_sig_idx <- which(edges_df$sig == TRUE)
  non_edge_sig_idx <- which(edges_df$sig == FALSE)

  if (highlight_free_path) {
    lines_df_pre <- apply_modifications(
      lines_df_pre,
      ff_params_edge,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color")
      ),
      mode = 'edge'
    )
  }

  if (highlight_sig_path) {
    lines_df_pre$color[edge_sig_idx] <- sig_path_color
    lines_df_pre$color[non_edge_sig_idx] <- non_sig_path_color
  }

  if (highlight_free_path_multi_group) {
    lines_df_pre <- apply_modifications(
      lines_df_pre,
      ff_params_edge_multi,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "width")
      ),
      mode = 'edge'
    )
  }

  if (highlight_multi_group) {
    lines_df_pre <- apply_modifications(
      lines_df_pre,
      sig_diff_edge,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "width")
      ),
      mode = 'edge'
    )
  }

  if (modify_params_edge) {
    lines_df_pre <- apply_modifications(
      lines_df_pre,
      modified_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "width", "alpha", "line_style", "end_color", "gradient_position", "color_type")
      ),
      mode = 'edge'
    )
  }

  # Adjust edge coordinates
  edge_list <- cbind(match(edges_from, node_names), match(edges_to, node_names))

  lines_df <- adjust_edge_coordinates(
    lines_df = lines_df_pre,
    edge_list = edge_list,
    points_df = points_df,
    auto_endpoint_spacing = line_endpoint_spacing
  )

  if ("two_way" %in% colnames(lines_df) && any(lines_df$two_way, na.rm = TRUE)) {
    lines_df[lines_df$two_way, c("x_start", "x_end", "y_start", "y_end")] <-
      lines_df[lines_df$two_way, c("x_start", "x_end", "y_start", "y_end")] +
      c(lavaan_curved_x_shift, lavaan_curved_x_shift, lavaan_curved_y_shift, lavaan_curved_y_shift)
  }



  if (any(lines_df$two_way)) {
    two_way_indices <- which(lines_df$two_way)
    control_points <- mapply(
      calculate_control_point,
      x_start = lines_df$x_start[two_way_indices],
      y_start = lines_df$y_start[two_way_indices],
      x_end = lines_df$x_end[two_way_indices],
      y_end = lines_df$y_end[two_way_indices],
      curvature_magnitude = lavaan_curvature_magnitude,
      rotate_curvature = lavaan_rotate_curvature,
      curvature_asymmetry = lavaan_curvature_asymmetry,
      center_x = mean(node_coords$x),
      center_y = mean(node_coords$y),
      SIMPLIFY = FALSE
    )
    lines_df$ctrl_x[two_way_indices] <- sapply(control_points, `[[`, "ctrl_x")
    lines_df$ctrl_y[two_way_indices] <- sapply(control_points, `[[`, "ctrl_y")
    lines_df$ctrl_x2[two_way_indices] <- sapply(control_points, `[[`, "ctrl_x2")
    lines_df$ctrl_y2[two_way_indices] <- sapply(control_points, `[[`, "ctrl_y2")

    lines_df$curvature_magnitude[two_way_indices] <- lavaan_curvature_magnitude
    lines_df$rotate_curvature[two_way_indices] <- lavaan_rotate_curvature
    lines_df$curvature_asymmetry[two_way_indices] <- lavaan_curvature_asymmetry
  }

  # Apply covariance edge modifications
  if (modify_params_cov_edge) {
    lines_df <- apply_modifications(
      lines_df,
      modified_cov_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = NULL,
        special_case = function(data, idx, mod) {
          # Add input validation
          if (length(idx) == 1 &&
              all(c("x_start", "y_start", "x_end", "y_end") %in% names(data)) &&
              all(c("curvature_magnitude", "rotate_curvature", "curvature_asymmetry", "x_shift", "y_shift") %in% names(mod))) {

            data$x_start[idx] <- data$x_start[idx] + mod$x_shift
            data$x_end[idx] <- data$x_end[idx] + mod$x_shift
            data$y_start[idx] <- data$y_start[idx] + mod$y_shift
            data$y_end[idx] <- data$y_end[idx] + mod$y_shift

            cp <- calculate_control_point(
              x_start = data$x_start[idx],
              y_start = data$y_start[idx],
              x_end = data$x_end[idx],
              y_end = data$y_end[idx],
              curvature_magnitude = mod$curvature_magnitude,
              rotate_curvature = mod$rotate_curvature,
              curvature_asymmetry = mod$curvature_asymmetry,
              center_x = mean(node_coords$x),
              center_y = mean(node_coords$y)
            )

            # Safely assign control points
            if (all(c("ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2", "locked") %in% names(data))) {
              data$ctrl_x[idx] <- cp$ctrl_x
              data$ctrl_y[idx] <- cp$ctrl_y
              data$ctrl_x2[idx] <- cp$ctrl_x2
              data$ctrl_y2[idx] <- cp$ctrl_y2
              data$curvature_magnitude[idx] <- mod$curvature_magnitude
              data$rotate_curvature[idx] <- mod$rotate_curvature
              data$curvature_asymmetry[idx] <- mod$curvature_asymmetry
              data$locked[idx] <- FALSE
            }
          }
          return(data)
        }
      ),
      mode = 'edge'
    )
  }

  lines_df$type[lines_df$curvature_magnitude == 0] <- "Straight Arrow"
  lines_df$type[lines_df$curvature_magnitude != 0] <- "Curved Arrow"

  if (modify_params_edge_xy) {
    lines_df <- apply_modifications(
      lines_df,
      modified_edges_xy,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x_start[idx] <- mod$start_x_shift # data$x_start[idx] + mod$start_x_shift
          data$y_start[idx] <- mod$start_y_shift # data$y_start[idx] + mod$start_y_shift
          data$x_end[idx] <- mod$end_x_shift # data$x_end[idx] + mod$end_x_shift
          data$y_end[idx] <- mod$end_y_shift # data$y_end[idx] + mod$end_y_shift

          if (data$type[idx] %in% c('Curved Line', 'Curved Arrow')) {
            cp <- calculate_control_point(
              x_start = data$x_start[idx],
              y_start = data$y_start[idx],
              x_end = data$x_end[idx],
              y_end = data$y_end[idx],
              curvature_magnitude = data$curvature_magnitude[idx],
              rotate_curvature = data$rotate_curvature[idx],
              curvature_asymmetry = data$curvature_asymmetry[idx],
              center_x = mean(node_coords$x),
              center_y = mean(node_coords$y)
            )

            data$ctrl_x[idx] <- cp$ctrl_x
            data$ctrl_y[idx] <- cp$ctrl_y
            data$ctrl_x2[idx] <- cp$ctrl_x2
            data$ctrl_y2[idx] <- cp$ctrl_y2
          }

          return(data)
        }
      ),
      mode = 'edge'
    )
  }

  # Handle arrow location
  if (exists("lavaan_arrow_location") && lavaan_arrow_location == "start") {
    temp <- lines_df[, c("x_start", "y_start")]
    lines_df[, c("x_start", "y_start")] <- lines_df[, c("x_end", "y_end")]
    lines_df[, c("x_end", "y_end")] <- temp
  }

  if (!is.null(loop_names_remove)) {
    loop_node_names <- loop_node_names[!loop_node_names %in% loop_names_remove]
  }

  if (residuals) {
    loops_df = data.frame(
      x_center = node_coords[match(loop_node_names, node_names), "x"],
      y_center = node_coords[match(loop_node_names, node_names), "y"],
      radius = lavaan_radius,
      color = lavaan_line_color_loop,
      width = lavaan_width_loop,
      alpha = lavaan_line_alpha_loop,
      arrow_type = lavaan_arrow_type_loop,
      arrow_size = lavaan_arrow_size_loop,
      gap_size = lavaan_gap_size_loop,
      loop_width = lavaan_width_loop,
      loop_height = lavaan_height_loop,
      orientation = 0, # later modified
      lavaan = TRUE,
      two_way = lavaan_two_way_arrow_loop,
      locked = FALSE,
      group = which_group,
      stringsAsFactors = FALSE
    )

    offset_distance <- lavaan_loop_offset

    for (i in 1:nrow(loops_df)) {
      node_name <- loop_node_names[i]
      node_index <- which(node_names == node_name)

      node_x <- points_df$x[node_index]
      node_y <- points_df$y[node_index]
      node_shape <- points_df$shape[node_index]
      node_size <- points_df$size[node_index]
      node_ratio <- points_df$width_height_ratio[node_index]
      node_orientation <- points_df$orientation[node_index]

      connected_edges <- lines_df[lines_df$edges_from == node_name | lines_df$edges_to == node_name, ]

      if (nrow(connected_edges) > 0) {
        edge_vectors <- list()

        for (j in 1:nrow(connected_edges)) {
          if (connected_edges$edges_from[j] == node_name) {
            # Outgoing edge
            dx <- connected_edges$x_end[j] - connected_edges$x_start[j]
            dy <- connected_edges$y_end[j] - connected_edges$y_start[j]
          } else {
            # Incoming edge (reverse direction)
            dx <- connected_edges$x_start[j] - connected_edges$x_end[j]
            dy <- connected_edges$y_start[j] - connected_edges$y_end[j]
          }
          edge_vectors[[j]] <- c(dx, dy)
        }

        avg_dx <- mean(sapply(edge_vectors, function(v) v[1]))
        avg_dy <- mean(sapply(edge_vectors, function(v) v[2]))

        angle_to_connections <- atan2(avg_dy, avg_dx) * 180 / pi
        gap_angle <- (angle_to_connections + 180) %% 360  # Opposite direction

      } else {
        dx_to_center <- mean(node_coords$x) - node_x
        dy_to_center <- mean(node_coords$y) - node_y
        gap_angle <- atan2(dy_to_center, dx_to_center) * 180 / pi
        gap_angle <- ifelse(gap_angle < 0, gap_angle + 360, gap_angle)
      }

      if (residuals_orientation_type == 'Quadratic') {
        quadrant_angles <- c(0, 90, 180, 270)
        angle_differences <- abs(gap_angle - quadrant_angles)
        angle_differences <- pmin(angle_differences, 360 - angle_differences)
        nearest_quadrant <- quadrant_angles[which.min(angle_differences)]
        final_gap_angle <- nearest_quadrant
      } else {
        final_gap_angle <- gap_angle
      }

      loops_df$orientation[i] <- (final_gap_angle - 90) %% 360

      alignment <- detect_local_alignment(node_x, node_y, node_coords$x, node_coords$y)

      if (alignment$type == "horizontal" && alignment$count >= 2) {
        group_dx <- 0
        group_dy <- ifelse(node_y > mean(node_coords$y), 1, -1)  # Outer side
      } else if (alignment$type == "vertical" && alignment$count >= 2) {
        group_dx <- ifelse(node_x > mean(node_coords$x), 1, -1)  # Outer side
        group_dy <- 0
      } else {
        position_angle <- (final_gap_angle + 180) %% 360
        group_dx <- cos(position_angle * pi / 180)
        group_dy <- sin(position_angle * pi / 180)
      }

      boundary_point <- find_intersection(
        x_center = node_x,
        y_center = node_y,
        x_target = node_x + group_dx,
        y_target = node_y + group_dy,
        size = node_size,
        width_height_ratio = node_ratio,
        orientation = node_orientation,
        shape = node_shape
      )

      dx_boundary <- boundary_point$x - node_x
      dy_boundary <- boundary_point$y - node_y
      distance_to_boundary <- sqrt(dx_boundary^2 + dy_boundary^2)

      if (distance_to_boundary > 0) {
        scale <- (distance_to_boundary + offset_distance) / distance_to_boundary
        loops_df$x_center[i] <- node_x + dx_boundary * scale
        loops_df$y_center[i] <- node_y + dy_boundary * scale
      } else {
        loops_df$x_center[i] <- node_x + group_dx * offset_distance
        loops_df$y_center[i] <- node_y + group_dy * offset_distance
      }

      loop_x <- loops_df$x_center[i]
      loop_y <- loops_df$y_center[i]

      dx_loop_to_node <- node_x - loop_x
      dy_loop_to_node <- node_y - loop_y

      angle_loop_to_node <- atan2(dy_loop_to_node, dx_loop_to_node) * 180 / pi
      angle_loop_to_node <- ifelse(angle_loop_to_node < 0, angle_loop_to_node + 360, angle_loop_to_node)

      loops_df$orientation[i] <- (angle_loop_to_node - 90) %% 360
    }

  } else {
    loops_df = data.frame(
      x_center = numeric(), y_center = numeric(), radius = numeric(), color = character(),
      width = numeric(), alpha = numeric(), arrow_type = character(), arrow_size = numeric(),
      gap_size = numeric(), loop_width = numeric(), loop_height = numeric(), orientation = numeric(),
      lavaan = logical(), two_way = logical(), locked = logical(), group = character(), stringsAsFactors = FALSE
    )
  }

  if (residuals) {

    if (highlight_free_path) {
      loops_df <- apply_modifications(
        loops_df,
        ff_params_loop,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = c("color")
        ),
        mode = 'loop'
      )
    }

    loop_sig_idx <- which(edges_loop_df$sig == TRUE)
    non_loop_sig_idx <- which(edges_loop_df$sig == FALSE)

    if (highlight_sig_path) {
      loops_df$color[loop_sig_idx] <- sig_path_color
      loops_df$color[non_loop_sig_idx] <- non_sig_path_color
    }

    if (highlight_free_path_multi_group) {
      loops_df <- apply_modifications(
        loops_df,
        ff_params_loop_multi,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = c("color", "width", "radius")
        ),
        mode = 'loop'
      )
    }

    if (highlight_multi_group) {
      loops_df <- apply_modifications(
        loops_df,
        sig_diff_loop,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = c("color", "width"),
          special_case = NULL
        ),
        mode = 'loop'
      )
    }

    if (modify_params_loop) {
      loops_df <- apply_modifications(
        loops_df,
        modified_loops,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = c("color", "alpha", "radius", "width", "type", "arrow_size", "gap_size", "two_way"),
          special_case = NULL
        ),
        mode = 'loop'
      )
    }

    if (modify_params_loop_xy) {
      loops_df <- apply_modifications(
        loops_df,
        modified_loops_xy,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = character(0), # No direct column mods
          special_case = function(data, idx, mod) {
            data$x_center[idx] <- data$x_center[idx] + mod$x_shift
            data$y_center[idx] <- data$y_center[idx] + mod$y_shift
            return(data)
          }
        ),
        mode = 'loop'
      )
    }

    if (modify_params_loop_location) {
      loops_df <- apply_modifications(
        loops_df,
        modified_loops_location,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = character(0),
          special_case = function(data, idx, mod) {
            # For each modified loop, recompute position and orientation
            for (i in idx) {
              node_name <- loop_node_names[i]
              node_index <- which(node_names == node_name)

              node_x <- points_df$x[node_index]
              node_y <- points_df$y[node_index]
              node_shape <- points_df$shape[node_index]
              node_size <- points_df$size[node_index]
              node_ratio <- points_df$width_height_ratio[node_index]
              node_orientation <- points_df$orientation[node_index]

              loop_angle <- as.numeric(mod$loop_location)

              angle_rad <- loop_angle * pi / 180
              group_dx <- cos(angle_rad)
              group_dy <- sin(angle_rad)

              # Find boundary point and apply offset
              boundary_point <- find_intersection(
                x_center = node_x,
                y_center = node_y,
                x_target = node_x + group_dx,
                y_target = node_y + group_dy,
                size = node_size,
                width_height_ratio = node_ratio,
                orientation = node_orientation,
                shape = node_shape
              )

              offset_distance <- lavaan_loop_offset

              dx_boundary <- boundary_point$x - node_x
              dy_boundary <- boundary_point$y - node_y
              distance_to_boundary <- sqrt(dx_boundary^2 + dy_boundary^2)

              # Update loop position
              if (distance_to_boundary > 0) {
                scale <- (distance_to_boundary + offset_distance) / distance_to_boundary
                data$x_center[i] <- node_x + dx_boundary * scale
                data$y_center[i] <- node_y + dy_boundary * scale
              } else {
                data$x_center[i] <- node_x + group_dx * offset_distance
                data$y_center[i] <- node_y + group_dy * offset_distance
              }

              # Recalculate orientation based on new position
              loop_x <- data$x_center[i]
              loop_y <- data$y_center[i]

              dx_loop_to_node <- node_x - loop_x
              dy_loop_to_node <- node_y - loop_y

              angle_loop_to_node <- atan2(dy_loop_to_node, dx_loop_to_node) * 180 / pi
              angle_loop_to_node <- ifelse(angle_loop_to_node < 0, angle_loop_to_node + 360, angle_loop_to_node)

              data$orientation[i] <- (angle_loop_to_node - 90) %% 360
            }

            return(data)
          }
        ),
        mode = 'loop'
      )
    }
  }

  # Prepare edge labels
  lines_df0 <- cbind(lines_df, from = edges_from, to = edges_to, text = edge_labels)
  edgelabels_xy_df <- data.frame(x = numeric(nrow(lines_df0)), y = numeric(nrow(lines_df0)))

  for (i in seq_len(nrow(lines_df0))) {
    intp_points <- if (lines_df0$type[i] == "Curved Arrow") {
      create_bezier_curve(
        x_start = lines_df0$x_start[i], y_start = lines_df0$y_start[i],
        x_end = lines_df0$x_end[i], y_end = lines_df0$y_end[i],
        ctrl_x = lines_df0$ctrl_x[i], ctrl_y = lines_df0$ctrl_y[i],
        ctrl_x2 = lines_df0$ctrl_x2[i], ctrl_y2 = lines_df0$ctrl_y2[i], n_points = 100
      )
    } else {
      interpolate_points(
        x_start = lines_df0$x_start[i], y_start = lines_df0$y_start[i],
        x_end = lines_df0$x_end[i], y_end = lines_df0$y_end[i], n = 100
      )
    }

    mid_idx <- ifelse(lines_df0$type[i] == "Curved Arrow",
                      find_peak_point(
                        intp_points,
                        x_start = lines_df0$x_start[i],
                        y_start = lines_df0$y_start[i],
                        x_end = lines_df0$x_end[i],
                        y_end = lines_df0$y_end[i]
                      ),
                      50)
    #mid_idx <- 50
    edgelabels_xy_df[i, ] <- intp_points[mid_idx, c("x", "y")]
  }

  pval_idx <- grep("\\*", lines_df0$text)

  label_coords <- data.frame(
    text = lines_df0$text,
    x = edgelabels_xy_df$x,
    y = edgelabels_xy_df$y,
    font = text_font_edges,
    size = text_size_edges,
    color = text_color_edges,
    fill = ifelse(lines_df0$text == "", NA, text_color_fill),
    angle = 0,
    alpha = text_alpha_edges,
    fontface = text_fontface_edges,
    math_expression = FALSE,
    hjust = 0.5,
    vjust = 0.5,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group_label = FALSE,
    loop_label = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  ) |>
    dplyr::filter(nzchar(trimws(text)))

  if (highlight_sig_path) {
    label_coords$fontface[edge_sig_idx] <- sig_label_fontface
    label_coords$fontface[non_edge_sig_idx] <- non_sig_label_fontface
    label_coords$color[edge_sig_idx] <- sig_path_color
    label_coords$color[non_edge_sig_idx] <- non_sig_path_color
  }

  if (highlight_free_path) {
    label_coords <- apply_modifications(
      label_coords,
      ff_params_edgelabel,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "fontface")
      ),
      mode = 'edge'
    )
  }

  if (highlight_free_path_multi_group) {
    label_coords <- apply_modifications(
      label_coords,
      ff_params_edgelabel_multi,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "fontface")
      ),
      mode = 'edge'
    )
  }

  if (highlight_multi_group) {
    label_coords <- apply_modifications(
      label_coords,
      sig_diff_edgelabel,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "fontface")
      ),
      mode = 'edge'
    )
  }

  # Apply edge label modifications
  if (modify_params_edgelabel) {
    label_coords <- apply_modifications(
      label_coords,
      modified_edgelabels,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "fill", "size", "alpha", "angle", "font", "fontface")
      ),
      mode = 'edge'
    )
  }

  if (modify_params_edgelabel_xy) {
    label_coords <- apply_modifications(
      label_coords,
      modified_edgelabels_xy,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'edge'
    )
  }

  if (modify_params_edgelabel_text) {
    if (nrow(modified_edgelabels_text) > 0) {
      for (i in seq_len(nrow(modified_edgelabels_text))) {
        mod <- modified_edgelabels_text[i, ]
        edge_idx <- which(
          edges_from == mod$lhs &
            edges_to == mod$rhs
        )
        if (length(edge_idx) == 1) {
          label_coords$text[[edge_idx]] <- mod$text
          label_coords$math_expression[[edge_idx]] <- mod$math_expression
        }
      }
    }
  }


  if (residuals) {
    # Prepare loop labels
    loop_labels_df <- data.frame(
      x = numeric(nrow(loops_df)),
      y = numeric(nrow(loops_df)),
      text = character(nrow(loops_df)),
      stringsAsFactors = FALSE
    )

    for (i in 1:nrow(loops_df)) {
      node_name <- loop_node_names[i]

      loop_label_row <- edges_loop_df[edges_loop_df$from == node_name & edges_loop_df$self_loop == TRUE, ]

      if (nrow(loop_label_row) > 0) {
        loop_labels_df$text[i] <- as.character(loop_label_row$labels)
      } else {
        loop_labels_df$text[i] <- ""
      }

      node_x <- loops_df$x_center[i]  # Current loop center (after positioning)
      node_y <- loops_df$y_center[i]
      loop_radius <- loops_df$radius[i]
      loop_orientation <- loops_df$orientation[i]

      gap_angle <- (loop_orientation + 90) %% 360
      label_angle <- (gap_angle + 180) %% 360  # Opposite side

      label_angle_rad <- label_angle * pi / 180

      loop_labels_df$x[i] <- node_x + loop_radius * cos(label_angle_rad)
      loop_labels_df$y[i] <- node_y + loop_radius * sin(label_angle_rad)
    }

    loop_label_coords <- data.frame(
      text = loop_labels_df$text,
      x = loop_labels_df$x,
      y = loop_labels_df$y,
      font = text_font_edges,
      size = text_size_edges,
      color = text_color_edges,
      fill = ifelse(loop_labels_df$text == "", NA, text_color_fill),
      angle = 0,
      alpha = text_alpha_edges,
      fontface = text_fontface_edges,
      math_expression = FALSE,
      hjust = 0.5,
      vjust = 0.5,
      lavaan = TRUE,
      network = FALSE,
      locked = FALSE,
      group_label = FALSE,
      loop_label = TRUE,
      group = which_group,
      stringsAsFactors = FALSE
    ) |>
      dplyr::filter(nzchar(trimws(text)))
  } else {
    loop_label_coords <- data.frame(
      text = character(), x = numeric(), y = numeric(), font = character(), size = numeric(), color = character(), fill = character(), angle = numeric(), alpha = numeric(),
      fontface = character(), math_expression = logical(), hjust = numeric(), vjust = numeric(), lavaan = logical(), network = logical(), locked = logical(), group_label = logical(), loop_label = logical(), group = character(),
      stringsAsFactors = FALSE
    )
  }


  if (residuals) {

    if (highlight_free_path) {
      loop_label_coords <- apply_modifications(
        loop_label_coords,
        ff_params_looplabel,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = c("color", "fontface")
        ),
        mode = 'loop'
      )
    }

    if (highlight_sig_path) {
      loop_label_coords$fontface[loop_sig_idx] <- sig_label_fontface
      loop_label_coords$fontface[non_loop_sig_idx] <- non_sig_label_fontface
      loop_label_coords$color[loop_sig_idx] <- sig_path_color
      loop_label_coords$color[non_loop_sig_idx] <- non_sig_path_color
    }

    if (highlight_free_path_multi_group) {
      loop_label_coords <- apply_modifications(
        loop_label_coords,
        ff_params_looplabel_multi,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = c("color", "fontface")
        ),
        mode = 'loop'
      )
    }

    if (highlight_multi_group) {
      loop_label_coords <- apply_modifications(
        loop_label_coords,
        sig_diff_looplabel,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = c("color", "fontface")
        ),
        mode = 'loop'
      )
    }

    if (modify_params_looplabel) {
      loop_label_coords <- apply_modifications(
        loop_label_coords,
        modified_looplabels,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = c("color", "size", "alpha", "angle", "font", "fontface"),
          special_case = NULL
        ),
        mode = 'loop'
      )
    }

    if (modify_params_looplabel_xy) {
      loop_label_coords <- apply_modifications(
        loop_label_coords,
        modified_looplabels_xy,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = character(0),
          special_case = function(data, idx, mod) {
            data$x[idx] <- data$x[idx] + mod$x_shift
            data$y[idx] <- data$y[idx] + mod$y_shift
            return(data)
          }
        ),
        mode = 'loop'
      )
    }

    if (modify_params_looplabel_text) {
      if (nrow(modified_looplabels_text) > 0) {
        for (i in seq_len(nrow(modified_looplabels_text))) {
          mod <- modified_looplabels_text[i, ]
          loop_idx <- which(
            node_coords$name[!node_coords$name %in% loop_names_remove] == mod$text
          )
          if (length(loop_idx) == 1) {
            loop_label_coords$text[[loop_idx]] <- mod$looplabel
            loop_label_coords$math_expression[[loop_idx]] <- mod$math_expression
          }
        }
      }
    }
  }

  if (remove_edgelabels) {
    label_coords <- data.frame(
      text = character(),
      x = numeric(),
      y = numeric(),
      font = character(),
      size = numeric(),
      color = character(),
      fill = character(),
      angle = numeric(),
      alpha = numeric(),
      fontface = character(),
      math_expression = logical(),
      hjust = numeric(),
      vjust = numeric(),
      lavaan = logical(),
      network = logical(),
      locked = logical(),
      group_label = logical(),
      loop_label = logical(),
      group = character(),
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(data_file)) {
    if (data_file) {
      annotations <- rbind(annotations, label_coords, loop_label_coords)
    }
  }

  points_df[c("x", "y")] <- lapply(points_df[c("x", "y")], round, 5)

  line_cols <- c("x_start", "y_start", "x_end", "y_end",
                 "ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2")
  lines_df[line_cols] <- lapply(lines_df[line_cols], round, 5)

  annotations[c("x", "y")] <- lapply(annotations[c("x", "y")], round, 5)

  loops_df[c("x_center", "y_center")] <- lapply(loops_df[c("x_center", "y_center")], round, 5)

  list(points = points_df, lines = lines_df, annotations = annotations, loops = loops_df)
}



#' Generate graph data from network objects with advanced layout options
#'
#' Converts network objects (igraph, network, or edge/node data frames) into
#' standardized graph data frames with extensive customization options including
#' clustering, dimensionality reduction, bipartite layout, and bezier edges.
#' Supports multiple layout algorithms and advanced visualization features.
#'
#'
#' @return A list containing three data frames:
#'   - `points`: Node data with coordinates, clustering colors, shapes, and properties
#'   - `lines`: Edge data with coordinates, bezier control points, arrow information, and styling
#'   - `annotations`: Text labels for nodes and edges with positioning and styling
#'
#' @importFrom purrr discard map
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#' @importFrom dplyr summarise group_by left_join rename mutate select bind_rows
#' @importFrom tidyr separate unnest
#' @importFrom stringr str_extract_all str_split
#' @importFrom igraph graph_from_data_frame layout_as_bipartite layout_with_fr
#' @importFrom graphics text
#' @importFrom methods show
#' @importFrom stats confint setNames
#' @importFrom utils head read.csv
#' @importFrom RColorBrewer brewer.pal
#' @importFrom smplot2 sm_palette
#' @importFrom grDevices rainbow
#' @importFrom network "%n%" "%v%<-" "%e%<-" "%eattr%<-"
#' @importFrom network network.size is.bipartite is.directed get.vertex.attribute
#' @importFrom network "%nattr%<-" "%vattr%<-"
#' @importFrom igraph  layout_with_kk layout_in_circle layout_on_grid layout_randomly cluster_louvain cluster_leiden cluster_walktrap cluster_fast_greedy membership vcount as_adjacency_matrix is_bipartite vertex_attr
#' @keywords internal
#' @noRd
generate_graph_from_network <- function(graph,
                                        layout,
                                        edges,
                                        nodes,
                                        is_bipartite,
                                        directed = TRUE,
                                        layout_width = 30,
                                        layout_height = 30, x_center = 0, y_center = 0,
                                        node_shape = "circle",
                                        node_size = 15,
                                        node_alpha = 1,
                                        node_fill_color = "#1262b3",
                                        node_border_color =  "#FFFFFF", node_border_width = 1,
                                        node_width_height_ratio = 1,
                                        line_width = 1, line_color = "#000000",
                                        line_alpha = 1,
                                        min_edge_width = 0.5, max_edge_width = 3, scale_by_weight = FALSE,
                                        line_endpoint_spacing = 0,
                                        arrow_type = "closed",
                                        arrow_size = 0.1,
                                        node_label_font = "sans", node_label_size = 15,
                                        node_label_color =  "#FFFFFF",
                                        node_label_alpha = 1, node_label_fontface = "plain",
                                        edge_label_font = "sans", edge_label_size = 15,
                                        edge_label_color = "#000000",
                                        edge_label_fill = "#FFFFFF",
                                        edge_label_alpha = 1, edge_label_fontface = "plain",
                                        zoom_factor = 1.2,
                                        annotate_nodes = TRUE,
                                        annotate_edges = TRUE,
                                        random_seed = NULL,
                                        use_clustering = FALSE,
                                        clustering_method = "louvain",
                                        cluster_palette = 'rainbow',
                                        dim_reduction_method = "tsne",
                                        bezier_network_edges = FALSE,
                                        network_edges_curvature_magnitude = 0.3,
                                        network_edges_rotate_curvature = FALSE,
                                        network_edges_curvature_asymmetry = 0,
                                        modify_params_edge = FALSE,
                                        modified_edges = NULL,
                                        modify_params_edgelabel = FALSE,
                                        modified_edgelabels = NULL,
                                        modify_params_node = FALSE,
                                        modified_nodes = NULL,
                                        modify_params_node_xy = FALSE,
                                        modified_nodes_xy = NULL,
                                        modify_params_nodelabel = FALSE,
                                        modified_nodelabels = NULL,
                                        modify_params_nodelabel_xy = FALSE,
                                        modified_nodelabels_xy = NULL,
                                        modify_params_nodelabel_text = FALSE,
                                        modified_nodelabels_text = NULL,
                                        modify_params_bezier_edge = FALSE,
                                        modified_bezier_edges = NULL,
                                        modify_params_edge_xy = FALSE,
                                        modified_edges_xy = NULL,
                                        change_bipartite_group = FALSE,
                                        apply_bipartite_nodes = FALSE,
                                        apply_bipartite_edges = FALSE,
                                        apply_bipartite_annotations = FALSE,
                                        last_state = NULL,
                                        which_group = "network") {

  apply_modifications <- function(data, modifications, config, mode) {
    if (is.null(modifications) || nrow(modifications) == 0) return(data)
    modified_data <- data

    for (i in seq_len(nrow(modifications))) {
      mod <- modifications[i, ]

      if (mode == 'edge') {
        idx <- which(
          edges$source == mod$lhs &
            edges$target == mod$rhs
        )
      } else if (mode == 'node') {
        idx <- which(
          layout$node == mod$text
        )
      }

      #print(idx)
      if (length(idx) == 1) {
        for (col in config$modify_cols) {
          if (col %in% names(mod) && col %in% names(modified_data)) {
            modified_data[idx, col] <- mod[[col]]
          }
        }

        if (!is.null(config$special_case)) {
          modified_data <- config$special_case(modified_data, idx, mod)
        }
      }
    }
    return(modified_data)
  }

  edges$source <- as.character(edges$source)
  edges$target <- as.character(edges$target)


  edge_list <- as.data.frame(edges[, c("source", "target")])


  if (use_clustering) {
    num_clusters <- NULL
    communities <- switch(
      clustering_method,
      "louvain" = cluster_louvain(graph),
      "leiden" = cluster_leiden(graph),
      "walktrap" = cluster_walktrap(graph),
      "fast_greedy" = cluster_fast_greedy(graph)
    )

    if (!is.null(communities)) {
      nodes$community <- membership(communities)
      num_clusters <- max(nodes$community)
    } else {
      nodes$community <- rep(1, vcount(graph)) # Default to one cluster if clustering fails
      num_clusters <- 1
    }

    # Define maximum colors for each palette
    palette_max_colors <- list(
      rainbow = Inf,  # Unlimited
      Set3 = 12,
      Paired = 12,
      Dark2 = 8,
      Accent = 8,
      Pastel1 = 9,
      Pastel2 = 8,
      Spectral = 11,
      YlGnBu = 9,
      RdYlBu = 11,
      smplot2 = 20
    )

    # Select the palette function based on user input
    palette_function <- switch(
      cluster_palette,
      "rainbow" = function(n) rainbow(n),
      "Set3" = function(n) RColorBrewer::brewer.pal(min(n, 12), "Set3"),
      "Paired" = function(n) RColorBrewer::brewer.pal(min(n, 12), "Paired"),
      "Dark2" = function(n) RColorBrewer::brewer.pal(min(n, 8), "Dark2"),
      "Accent" = function(n) RColorBrewer::brewer.pal(min(n, 8), "Accent"),
      "Pastel1" = function(n) RColorBrewer::brewer.pal(min(n, 9), "Pastel1"),
      "Pastel2" = function(n) RColorBrewer::brewer.pal(min(n, 8), "Pastel2"),
      "Spectral" = function(n) RColorBrewer::brewer.pal(min(n, 11), "Spectral"),
      "YlGnBu" = function(n) RColorBrewer::brewer.pal(min(n, 9), "YlGnBu"),
      "RdYlBu" = function(n) RColorBrewer::brewer.pal(min(n, 11), "RdYlBu"),
      "smplot2" = function(n) head(smplot2::sm_palette(), n)
    )

    max_colors <- palette_max_colors[[cluster_palette]]

    # Assign colors based on the number of clusters and the chosen palette
    if (cluster_palette != "rainbow" && num_clusters > max_colors) {
      palette_function <- rainbow
    }

    # Generate colors
    node_colors <- palette_function(num_clusters)[nodes$community]
  } else {
    # If clustering is disabled, use default node color
    node_colors <- node_fill_color
  }

  layout_min_x <- min(layout$x)
  layout_min_y <- min(layout$y)
  layout_max_x <- max(layout$x)
  layout_max_y <- max(layout$y)

  layout <- layout |>
    mutate(
      x = (x - layout_min_x) / (layout_max_x - layout_min_x) * layout_width + x_center - layout_width / 2,
      y = (y - layout_min_y) / (layout_max_y - layout_min_y) * layout_height + y_center - layout_height / 2
    )


  # Apply node position modifications
  if (modify_params_node_xy) {
    layout <- apply_modifications(
      layout,
      modified_nodes_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0), # No direct column mods
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )

  }

  if (is_bipartite) {
    group_selector <- if (apply_bipartite_nodes) !vertex_attr(graph)$type else vertex_attr(graph)$type
  } else {
    group_selector <- NULL
  }

  points <- layout |>
    dplyr::left_join(nodes, by = c("node" = "node")) |>
    mutate(
      shape = node_shape,
      color = node_colors,
      size =  node_size,
      border_color = node_border_color,
      border_width = node_border_width,
      alpha = node_alpha,
      width_height_ratio = node_width_height_ratio,
      orientation = 0,
      lavaan = FALSE,
      network = TRUE,
      locked = FALSE,
      group = which_group
    ) |>
    select(
      x, y, shape, color, size, border_color, border_width, alpha, width_height_ratio, orientation,
      lavaan, network, locked, group
    )

  if (modify_params_node) {
    points <- apply_modifications(
      points,
      modified_nodes,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "alpha", "shape", "size", "border_color", "border_width", "width_height_ratio"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }

  if (inherits(edges$weight, 'character')) {
    scale_by_weight <- FALSE
  }

  if ("weight" %in% colnames(edges) && scale_by_weight) {
    edges <- edges |>
      dplyr::mutate(
        scaled_width = rescale_values(weight, to = c(min_edge_width, max_edge_width))
      )
  } else {
    edges <- edges |>
      dplyr::mutate(scaled_width = line_width)
  }

  # Prepare lines data frame

  lines <- edges |>
    dplyr::left_join(layout, by = c("source" = "node")) |>
    dplyr::rename(x_start = x, y_start = y) |>
    dplyr::left_join(layout, by = c("target" = "node")) |>
    dplyr::rename(x_end = x, y_end = y) |>
    dplyr::mutate(
      ctrl_x = NA,
      ctrl_y = NA,
      ctrl_x2 = NA,
      ctrl_y2 = NA,
      curvature_magnitude = NA,
      rotate_curvature = NA,
      curvature_asymmetry = NA,
      type = ifelse(bezier_network_edges == FALSE, ifelse(directed, "Straight Arrow", "Straight Line"), ifelse(directed, "Curved Arrow", "Curved Line")),
      color = line_color,
      end_color = NA,
      color_type = "Single",
      gradient_position = NA,
      width = scaled_width,
      alpha = line_alpha,
      arrow = ifelse(directed, directed, NA),
      arrow_type = ifelse(directed, arrow_type, NA),
      arrow_size = ifelse(directed, arrow_size, NA),
      two_way = ifelse(two_way, TRUE, FALSE),
      lavaan = FALSE,
      network = TRUE,
      line_style = "solid",
      locked = FALSE,
      group = which_group
    ) |>
    dplyr::select(
      x_start, y_start, x_end, y_end, ctrl_x, ctrl_y, ctrl_x2, ctrl_y2,
      curvature_magnitude, rotate_curvature, curvature_asymmetry, type, color, end_color,
      color_type, gradient_position, width, alpha, arrow, arrow_type,
      arrow_size, two_way, lavaan, network, line_style, locked, group
    )

  node_mapping <- stats::setNames(seq_along(nodes$node), nodes$node)
  numeric_edge_list <- matrix(
    c(node_mapping[edges$source], node_mapping[edges$target]),
    ncol = 2
  )

  if (modify_params_edge) {
    if (nrow(modified_edges) > 0) {
      for (i in seq_len(nrow(modified_edges))) {
        mod <- modified_edges[i, ]

        edge_idx <- which(
          edges$source == mod$lhs &
            edges$target == mod$rhs
        )

        if (length(edge_idx) == 1) {
          lines$color[[edge_idx]] <- mod$color
          lines$width[[edge_idx]] <- mod$width
          lines$alpha[[edge_idx]] <- mod$alpha
          lines$line_style[[edge_idx]] <- mod$line_style
          lines$end_color[[edge_idx]] <- mod$end_color
          lines$gradient_position[[edge_idx]] <- mod$gradient_position
          lines$color_type[[edge_idx]] <- mod$color_type
        }
      }
    }
  }

  lines <- adjust_edge_coordinates(
    lines_df = lines,
    edge_list = numeric_edge_list,
    points_df = points,
    auto_endpoint_spacing = line_endpoint_spacing
  )


  if (bezier_network_edges == TRUE) {
    bezier_indices <- which(lines$type %in% c('Curved Arrow', 'Curved Line'))

    control_points <- mapply(
      calculate_control_point,
      x_start = lines$x_start[bezier_indices],
      y_start = lines$y_start[bezier_indices],
      x_end = lines$x_end[bezier_indices],
      y_end = lines$y_end[bezier_indices],
      curvature_magnitude = network_edges_curvature_magnitude,
      rotate_curvature = network_edges_rotate_curvature,
      curvature_asymmetry = network_edges_curvature_asymmetry,
      SIMPLIFY = FALSE
    )


    # Assign the calculated control points to lines
    lines$ctrl_x[bezier_indices] <- sapply(control_points, `[[`, "ctrl_x")
    lines$ctrl_y[bezier_indices] <- sapply(control_points, `[[`, "ctrl_y")
    lines$ctrl_x2[bezier_indices] <- sapply(control_points, `[[`, "ctrl_x2")
    lines$ctrl_y2[bezier_indices] <- sapply(control_points, `[[`, "ctrl_y2")
    lines$locked[bezier_indices] <- FALSE
  }


  if (modify_params_bezier_edge) {
    lines <- apply_modifications(
      lines,
      modified_bezier_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = NULL,
        special_case = function(data, idx, mod) {
          cp <- calculate_control_point(
            x_start = data$x_start[idx],
            y_start = data$y_start[idx],
            x_end = data$x_end[idx],
            y_end = data$y_end[idx],
            curvature_magnitude = mod$curvature_magnitude,
            rotate_curvature = mod$rotate_curvature,
            curvature_asymmetry = mod$curvature_asymmetry
          )

          # Safely assign control points
          if (all(c("ctrl_x", "ctrl_y", "locked") %in% names(data))) {
            data$ctrl_x[idx] <- cp$ctrl_x
            data$ctrl_y[idx] <- cp$ctrl_y
            data$ctrl_x2[idx] <- cp$ctrl_x2
            data$ctrl_y2[idx] <- cp$ctrl_y2
            data$locked[idx] <- FALSE
          }
          # }
          return(data)
        }
      ),
      mode = 'edge'
    )
  }


  if (modify_params_edge_xy) {
    lines <- apply_modifications(
      lines,
      modified_edges_xy,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x_start[idx] <- data$x_start[idx] + mod$start_x_shift
          data$y_start[idx] <- data$y_start[idx] + mod$start_y_shift
          data$x_end[idx] <- data$x_end[idx] + mod$end_x_shift
          data$y_end[idx] <- data$y_end[idx] + mod$end_y_shift
          return(data)
        }
      ),
      mode = 'edge'
    )
  }


  edgelabels_xy_df <- data.frame(x = rep(NA_real_, nrow(lines)),
                                 y = rep(NA_real_, nrow(lines)))

  for (i in 1:nrow(lines)) {

    intp_points <-
      if (lines$type[i] == "Curved Arrow" || lines$type[i] == "Curved Line") {
        create_bezier_curve(
          x_start = lines$x_start[i], y_start = lines$y_start[i],
          x_end = lines$x_end[i], y_end = lines$y_end[i],
          ctrl_x = lines$ctrl_x[i], ctrl_y = lines$ctrl_y[i],
          ctrl_x2 = lines$ctrl_x2[i], ctrl_y2 = lines$ctrl_y2[i], n_points = 100
        )
      } else if (lines$type[i] == "Straight Arrow" || lines$type[i] == "Straight Line") {
        interpolate_points(
          x_start = lines$x_start[i], y_start = lines$y_start[i],
          x_end = lines$x_end[i], y_end = lines$y_end[i], n = 100
        )
      }

    mid_index <- 50
    edgelabels_xy_df$x[i] <- intp_points$x[mid_index]
    edgelabels_xy_df$y[i] <- intp_points$y[mid_index]
  }

  edges$weight <- round(edges$weight, 3) # round

  weight_annotations <- if (annotate_edges == TRUE) {
    if (!all(is.na(edges$weight))) {
      lines |>
        mutate(weight = edges$weight) |>
        mutate(
          text = as.character(edges$weight),
          x = edgelabels_xy_df$x,
          y = edgelabels_xy_df$y,
          font = edge_label_font,
          size = edge_label_size,
          color = edge_label_color,
          fill = ifelse(as.character(edges$weight) == "", NA, edge_label_fill),
          angle = 0,
          alpha = edge_label_alpha,
          fontface = edge_label_fontface,
          math_expression = FALSE,
          hjust = 0.5,
          vjust = 0.5,
          lavaan = FALSE,
          network = TRUE,
          locked = FALSE,
          group_label = FALSE,
          loop_label = FALSE,
          group = which_group
        ) |>
        select(text, x, y, font, size, color, fill, angle, alpha, fontface, math_expression, hjust, vjust, lavaan, network, locked, group_label, loop_label, group)
    } else {
      data.frame(
        text = character(), x = numeric(), y = numeric(), font = character(), size = numeric(),
        color = character(), fill = character(), angle = numeric(), alpha = numeric(), fontface = character(),
        math_expression = logical(), hjust = numeric(), vjust = numeric(), lavaan = logical(), network = logical(), locked = logical(),
        group_label = logical(), loop_label = logical(), group = character(), stringsAsFactors = FALSE
      )
    }
  } else {
    data.frame(
      text = character(), x = numeric(), y = numeric(), font = character(), size = numeric(),
      color = character(), fill = character(), angle = numeric(), alpha = numeric(), fontface = character(),
      math_expression = logical(), hjust = numeric(), vjust = numeric(), lavaan = logical(), network = logical(), locked = logical(),
      group_label = logical(), loop_label = logical(), group = character(), stringsAsFactors = FALSE
    )
  }


  if (modify_params_edgelabel) {
    weight_annotations <- apply_modifications(
      weight_annotations,
      modified_edgelabels,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "fill", "size", "alpha", "angle", "font", "fontface")
      ),
      mode = 'edge'
    )

  }

  if (is_bipartite) {
    group_selector <- if (apply_bipartite_annotations) !vertex_attr(graph)$type else vertex_attr(graph)$type
  } else {
    group_selector <- NULL
  }

  annotations <- points |>
    mutate(
      text = if ("label" %in% colnames(nodes)) as.character(nodes$label) else if ("node" %in% colnames(layout)) as.character(layout$node) else NA, # Use node name as default text
      font = node_label_font,
      size = node_label_size,
      color = node_label_color,
      fill = NA,
      angle = 0,
      alpha = node_label_alpha,
      fontface = node_label_fontface,
      math_expression = FALSE,
      hjust = 0.5,
      vjust = 0.5,
      lavaan = FALSE,
      network = TRUE,
      locked = FALSE,
      group_label = FALSE,
      loop_label = FALSE,
      group = which_group
    ) |> select(text, x, y, font, size, color, fill, angle, alpha, fontface, math_expression, hjust, vjust, lavaan, network, locked, group_label, loop_label, group) |>
    dplyr::bind_rows(weight_annotations)


  if (modify_params_nodelabel_xy) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )
  }

  if (modify_params_nodelabel_text) {
    if (nrow(modified_nodelabels_text) > 0) {
      for (i in seq_len(nrow(modified_nodelabels_text))) {
        mod <- modified_nodelabels_text[i, ]
        node_idx <- which(
          layout$node == mod$text
        )
        if (length(node_idx) == 1) {
          annotations$text[[node_idx]] <- mod$nodelabel
        }
      }
    }
  }

  if (modify_params_nodelabel) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "size", "alpha", "angle", "font", "fontface"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }

  points[c("x", "y")] <- lapply(points[c("x", "y")], round, 5)

  line_cols <- c("x_start", "y_start", "x_end", "y_end",
                 "ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2")
  lines[line_cols] <- lapply(lines[line_cols], round, 5)

  annotations[c("x", "y")] <- lapply(annotations[c("x", "y")], round, 5)

  list(points = as.data.frame(points), lines = as.data.frame(lines),
       annotations = as.data.frame(annotations))
}




#' Generate graph data from qgraph objects
#'
#' Converts qgraph objects into standardized graph data frames for visualization.
#' Extracts node positions, edge connections, and styling information from qgraph
#' network visualizations and converts them to a consistent format with extensive
#' customization options.
#'
#'
#' @return A list containing three data frames:
#'   - `points`: Node data with coordinates, shapes, colors, sizes, and properties
#'   - `lines`: Edge data with coordinates, bezier control points, arrow information, and styling
#'   - `annotations`: Text labels for nodes and edges with positioning and styling
#'
#'
#' The function preserves qgraph's original styling while allowing global overrides
#' and supports both directed and undirected networks with customizable arrow types
#' and edge curvatures.
#'
#' @importFrom dplyr filter
#' @keywords internal
#' @noRd
generate_graph_from_qgraph <- function(qgraph_obj,
                                       directed = FALSE,
                                       layout_width = 30, layout_height = 30,
                                       x_center = 0, y_center = 0,
                                       node_shape = 'circle',
                                       node_size = 15,
                                       node_alpha = 1,
                                       node_fill_color = "#1262b3",
                                       node_border_color = "#0f993d", node_border_width = 1,
                                       node_width_height_ratio = 1,
                                       line_width = 1, line_color = "#000000",
                                       line_alpha = 1,
                                       min_edge_width = 0.5, max_edge_width = 3, scale_by_weight = FALSE,
                                       line_endpoint_spacing = 0,
                                       arrow_type = "closed",
                                       arrow_size = 0.1,
                                       node_label_font = "sans", node_label_size = 15,
                                       node_label_color = "#000000",
                                       node_label_alpha = 1, node_label_fontface = "plain",
                                       edge_label_font = "sans", edge_label_size = 15,
                                       edge_label_color = "#000000",
                                       edge_label_fill = "#FFFFFF",
                                       edge_label_alpha = 1, edge_label_fontface = "plain",
                                       zoom_factor = 1.2,
                                       data_file = NULL,
                                       bezier_network_edges = FALSE,
                                       network_edges_curvature_magnitude = 0.3,
                                       network_edges_rotate_curvature = FALSE,
                                       modify_params_edge = FALSE,
                                       modified_edges = NULL,
                                       modify_params_edgelabel = FALSE,
                                       modified_edgelabels = NULL,
                                       modify_params_node = FALSE,
                                       modified_nodes = NULL,
                                       modify_params_node_xy = FALSE,
                                       modified_nodes_xy = NULL,
                                       modify_params_edge_xy = FALSE,
                                       modified_edges_xy = NULL,
                                       modify_params_cov_edge = FALSE,
                                       modified_cov_edges = NULL,
                                       modify_params_nodelabel = FALSE,
                                       modified_nodelabels = NULL,
                                       modify_params_nodelabel_xy = FALSE,
                                       modify_params_bezier_edge = FALSE,
                                       modified_bezier_edges = NULL,
                                       modified_nodelabels_xy = NULL,
                                       modify_params_nodelabel_text = FALSE,
                                       modified_nodelabels_text = NULL,
                                       apply_global_nodes = FALSE,
                                       apply_global_edges = FALSE,
                                       apply_global_annotations = FALSE,
                                       which_group = "qgraph") {

  apply_modifications <- function(data, modifications, config, mode) {
    if (is.null(modifications) || nrow(modifications) == 0) return(data)
    modified_data <- data

    for (i in seq_len(nrow(modifications))) {
      mod <- modifications[i, ]

      if (mode == 'edge') {
        idx <- which(
          edges_from == mod$lhs &
            edges_to == mod$rhs
        )
      } else if (mode == 'node') {
        idx <- which(
          node_coords$name == mod$text
        )
      }

      #print(idx)
      if (length(idx) == 1) {
        for (col in config$modify_cols) {
          if (col %in% names(mod) && col %in% names(modified_data)) {
            modified_data[idx, col] <- mod[[col]]
          }
        }

        if (!is.null(config$special_case)) {
          modified_data <- config$special_case(modified_data, idx, mod)
        }
      }
    }
    return(modified_data)
  }

  if (inherits(qgraph_obj, "qgraph")) {
    node_names <- names(qgraph_obj$graphAttributes$Nodes$labels)
    if (is.null(node_names)) node_names <- as.character(qgraph_obj$graphAttributes$Nodes$labels)

    # Extract and normalize node coordinates
    node_coords <- as.data.frame(qgraph_obj$layout)
    colnames(node_coords) <- c("x", "y")
    node_coords$x <- (node_coords$x - mean(range(node_coords$x))) * layout_width + x_center
    node_coords$y <- (node_coords$y - mean(range(node_coords$y))) * layout_height + y_center
    node_coords$name <- node_names

    # Process edges
    edges_df0 <- data.frame(
      from = qgraph_obj$Edgelist$from,
      to = qgraph_obj$Edgelist$to,
      weight = qgraph_obj$Edgelist$weight,
      directed = qgraph_obj$Edgelist$directed,
      bidirectional = qgraph_obj$Edgelist$bidirectional,
      labels = qgraph_obj$graphAttributes$Edges$labels
    )

    keep_indices <- which(qgraph_obj$graphAttributes$Edges$color != "#00000000")
    edges_df0 <- edges_df0[keep_indices, ]

    edges_df <- edges_df0[!duplicated(
      t(apply(edges_df0[c("from", "to")], 1, sort))
    ), ]

    # Edge properties
    edge_op <- ifelse(edges_df$bidirectional, "~~", "~")
    edges_from <- node_names[edges_df$from]
    edges_to <- node_names[edges_df$to]
    edge_labels <- edges_df$labels

  } else {

    stop("Must be output from 'qgraph'.")

  }


  # Apply node position modifications
  if (modify_params_node_xy) {
    node_coords <- apply_modifications(
      node_coords,
      modified_nodes_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0), # No direct column mods
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )

  }

  # Create points dataframe
  points_df <- data.frame(
    x = node_coords$x,
    y = node_coords$y,
    shape = if (apply_global_nodes) node_shape else qgraph_obj$graphAttributes$Nodes$shape,
    color = if (apply_global_nodes) node_fill_color else sapply(qgraph_obj$graphAttributes$Nodes$color, to_hex2),
    size = node_size,
    border_color = if (apply_global_nodes) node_border_color else qgraph_obj$graphAttributes$Nodes$border.color,
    border_width = node_border_width,
    alpha = if (apply_global_nodes) node_alpha else sapply(sapply(qgraph_obj$graphAttributes$Nodes$color, to_hex), get_alpha),
    width_height_ratio = if (apply_global_nodes) node_width_height_ratio else qgraph_obj$graphAttributes$Nodes$width / qgraph_obj$graphAttributes$Nodes$height,
    orientation = 0,
    lavaan = FALSE,
    network = TRUE,
    locked = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  #Apply node modifications
  if (modify_params_node) {
    points_df <- apply_modifications(
      points_df,
      modified_nodes,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "alpha", "shape", "size", "border_color", "border_width", "width_height_ratio"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }


  # Create annotations
  annotations <- data.frame(
    text = as.character(node_coords$name),
    x = node_coords$x,
    y = node_coords$y,
    font = node_label_font,
    size = node_label_size,
    color = if (apply_global_annotations) node_label_color else sapply(qgraph_obj$graphAttributes$Nodes$label.color, to_hex2),
    fill = NA,
    angle = 0,
    alpha = 1,
    fontface = 'plain',
    math_expression = FALSE,
    hjust = 0.5,
    vjust = 0.5,
    lavaan = FALSE,
    network = TRUE,
    locked = FALSE,
    group_label = FALSE,
    loop_label = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  if (modify_params_nodelabel) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "size", "alpha", "angle", "font", "fontface"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }

  if (modify_params_nodelabel_xy) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )
  }



  if (modify_params_nodelabel_text) {
    if (nrow(modified_nodelabels_text) > 0) {
      for (i in seq_len(nrow(modified_nodelabels_text))) {
        mod <- modified_nodelabels_text[i, ]
        node_idx <- which(
          node_coords$name == mod$text
        )
        if (length(node_idx) == 1) {
          annotations$text[[node_idx]] <- mod$nodelabel
        }
      }
    }
  }


  if (length(edges_from) == 0 || length(edges_to) == 0) {
    stop("No edges found in the model. Check the Lavaan syntax.")
  }

  # Create lines dataframe
  lines_df_pre <- data.frame(
    x_start = node_coords[match(edges_from, node_names), "x"],
    y_start = node_coords[match(edges_from, node_names), "y"],
    x_end = node_coords[match(edges_to, node_names), "x"],
    y_end = node_coords[match(edges_to, node_names), "y"],
    ctrl_x = NA,
    ctrl_y = NA,
    ctrl_x2 = NA,
    ctrl_y2 = NA,
    curvature_magnitude = NA,
    rotate_curvature = NA,
    curvature_asymmetry = NA,
    type = ifelse(edges_df$directed, ifelse(edge_op == "~~", "Curved Arrow", "Straight Arrow"), ifelse(edge_op == "~~", "Curved Line", "Straight Line")),
    color = if (apply_global_edges) line_color else sapply(qgraph_obj$graphAttributes$Edges$color[as.numeric(rownames(edges_df))], to_hex2),
    end_color = NA,
    color_type = "Single",
    gradient_position = NA,
    width = if (apply_global_edges) line_width else qgraph_obj$graphAttributes$Edges$width[as.numeric(rownames(edges_df))] * 0.6,
    alpha = if (apply_global_edges) line_alpha else sapply(sapply(qgraph_obj$graphAttributes$Edges$color[as.numeric(rownames(edges_df))], to_hex), get_alpha),
    arrow = if (apply_global_edges) ifelse(directed, TRUE, NA) else ifelse(edges_df$directed, TRUE, NA),
    arrow_type = arrow_type,
    arrow_size = arrow_size,
    two_way = edge_op == "~~",
    lavaan = FALSE,
    network = TRUE,
    line_style = "solid",
    locked = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  # Apply edge modifications
  if (modify_params_edge) {
    lines_df_pre <- apply_modifications(
      lines_df_pre,
      modified_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "width", "alpha", "line_style", "end_color", "gradient_position", "color_type")
      ),
      mode = 'edge'
    )
  }


  # Adjust edge coordinates
  edge_list <- cbind(match(edges_from, node_names), match(edges_to, node_names))
  lines_df <- adjust_edge_coordinates(
    lines_df = lines_df_pre,
    edge_list = edge_list,
    points_df = points_df,
    auto_endpoint_spacing = line_endpoint_spacing
  )

  if (bezier_network_edges == TRUE) {
    bezier_indices <- which(lines_df$type %in% c('Curved Arrow', 'Curved Line'))

    control_points <- mapply(
      calculate_control_points,
      x_start = lines_df$x_start[bezier_indices],
      y_start = lines_df$y_start[bezier_indices],
      x_end = lines_df$x_end[bezier_indices],
      y_end = lines_df$y_end[bezier_indices],
      curvature_magnitude = network_edges_curvature_magnitude,
      rotate_curvature = network_edges_rotate_curvature,
      curvature_asymmetry = network_edges_curvature_asymmetry,
      center_x = mean(node_coords$x),
      center_y = mean(node_coords$y),
      SIMPLIFY = FALSE
    )


    # Assign the calculated control points to lines
    lines_df$ctrl_x[bezier_indices] <- sapply(control_points, `[[`, "ctrl_x")
    lines_df$ctrl_y[bezier_indices] <- sapply(control_points, `[[`, "ctrl_y")
    lines_df$ctrl_x2[bezier_indices] <- sapply(control_points, `[[`, "ctrl_x2")
    lines_df$ctrl_y2[bezier_indices] <- sapply(control_points, `[[`, "ctrl_y2")
    lines_df$locked[bezier_indices] <- FALSE
  }

  if (modify_params_bezier_edge) {
    lines_df <- apply_modifications(
      lines_df,
      modified_bezier_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = NULL,
        special_case = function(data, idx, mod) {
          cp <- calculate_control_points(
            x_start = data$x_start[idx],
            y_start = data$y_start[idx],
            x_end = data$x_end[idx],
            y_end = data$y_end[idx],
            curvature_magnitude = mod$curvature_magnitude,
            rotate_curvature = mod$rotate_curvature,
            curvature_asymmetry = mod$curvature_asymmetry,
            center_x = mean(node_coords$x),
            center_y = mean(node_coords$y)
          )

          # Safely assign control points
          if (all(c("ctrl_x", "ctrl_y", "locked") %in% names(data))) {
            data$ctrl_x[idx] <- cp$ctrl_x
            data$ctrl_y[idx] <- cp$ctrl_y
            data$ctrl_x2[idx] <- cp$ctrl_x2
            data$ctrl_y2[idx] <- cp$ctrl_y2
            data$locked[idx] <- FALSE
          }
          # }
          return(data)
        }
      ),
      mode = 'edge'
    )
  }

  if (modify_params_edge_xy) {
    lines_df <- apply_modifications(
      lines_df,
      modified_edges_xy,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x_start[idx] <- data$x_start[idx] + mod$start_x_shift
          data$y_start[idx] <- data$y_start[idx] + mod$start_y_shift
          data$x_end[idx] <- data$x_end[idx] + mod$end_x_shift
          data$y_end[idx] <- data$y_end[idx] + mod$end_y_shift
          return(data)
        }
      ),
      mode = 'edge'
    )
  }

  # Prepare edge labels
  lines_df0 <- cbind(lines_df, from = edges_from, to = edges_to, text = edge_labels)
  edgelabels_xy_df <- data.frame(x = numeric(nrow(lines_df0)), y = numeric(nrow(lines_df0)))

  for (i in seq_len(nrow(lines_df0))) {
    intp_points <- if (lines_df0$type[i] == "Curved Arrow") {
      create_bezier_curve(
        x_start = lines_df0$x_start[i], y_start = lines_df0$y_start[i],
        x_end = lines_df0$x_end[i], y_end = lines_df0$y_end[i],
        ctrl_x = lines_df0$ctrl_x[i], ctrl_y = lines_df0$ctrl_y[i],
        ctrl_x2 = lines_df0$ctrl_x2[i], ctrl_y2 = lines_df0$ctrl_y2[i], n_points = 100
      )
    } else {
      interpolate_points(
        x_start = lines_df0$x_start[i], y_start = lines_df0$y_start[i],
        x_end = lines_df0$x_end[i], y_end = lines_df0$y_end[i], 100
      )
    }
    edgelabels_xy_df[i, ] <- intp_points[50, c("x", "y")]
  }

  label_coords <- data.frame(
    x = edgelabels_xy_df$x,
    y = edgelabels_xy_df$y,
    text = as.character(lines_df0$text),
    font = edge_label_font,
    size = edge_label_size,
    color = if (apply_global_annotations) edge_label_color else sapply(qgraph_obj$graphAttributes$Edges$color[as.numeric(rownames(edges_df))], to_hex2),
    fill = edge_label_fill,
    angle = 0,
    alpha = edge_label_alpha,
    fontface = edge_label_fontface,
    math_expression = FALSE,
    hjust = 0.5,
    vjust = 0.5,
    lavaan = FALSE,
    network = TRUE,
    locked = FALSE,
    group_label = FALSE,
    loop_label = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  ) |>
    dplyr::filter(nzchar(trimws(text)))


  # Apply edge label modifications
  if (modify_params_edgelabel) {
    label_coords <- apply_modifications(
      label_coords,
      modified_edgelabels,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "fill", "size", "alpha", "angle", "font", "fontface")
      ),
      mode = 'edge'
    )

  }

  #if (!is.null(data_file)) {
  annotations <- rbind(annotations, label_coords)
  #}

  points_df[c("x", "y")] <- lapply(points_df[c("x", "y")], round, 5)

  line_cols <- c("x_start", "y_start", "x_end", "y_end",
                 "ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2")
  lines_df[line_cols] <- lapply(lines_df[line_cols], round, 5)

  annotations[c("x", "y")] <- lapply(annotations[c("x", "y")], round, 5)

  list(points = points_df, lines = lines_df, annotations = annotations)
}


#' Generate graph data from qgraph objects with enhanced features
#'
#' Enhanced version of generate_graph_from_qgraph that provides additional
#' customization options including curvature asymmetry, edge label positioning,
#' and more flexible modification parameters. Converts qgraph objects into
#' standardized graph data frames for visualization with advanced bezier curve
#' support and comprehensive styling options.
#'
#' @return A list containing three data frames:
#'   - `points`: Node data with coordinates, shapes, colors, sizes, and properties
#'   - `lines`: Edge data with coordinates, bezier control points, arrow information, and styling
#'   - `annotations`: Text labels for nodes and edges with positioning and styling
#'
#' The function automatically handles edge type conversion between straight and
#' curved based on curvature magnitude and provides robust error handling for
#' coordinate transformations.
#'
#' @importFrom qgraph qgraph
#' @importFrom igraph E is_connected cluster_louvain cluster_leiden cluster_walktrap
#' @importFrom igraph E<-
#' @importFrom igraph cluster_fast_greedy cluster_spinglass cluster_edge_betweenness
#' @importFrom igraph components membership vcount graph_from_adjacency_matrix
#' @importFrom RColorBrewer brewer.pal
#' @importFrom smplot2 sm_palette
#' @importFrom grDevices rainbow
#' @importFrom dplyr filter
#' @keywords internal
#' @noRd
generate_graph_from_qgraph1 <- function(network_object,
                                        directed = FALSE,
                                        layout_width = 30, layout_height = 30,
                                        x_center = 0, y_center = 0,
                                        node_shape = 'circle',
                                        node_size = 10,
                                        node_alpha = 1,
                                        node_fill_color = "#1262b3",
                                        node_border_color = "#0f993d", node_border_width = 1,
                                        node_width_height_ratio = 1,
                                        line_width = 1, line_color = "#000000",
                                        line_alpha = 1,
                                        min_edge_width = 0.5, max_edge_width = 3, scale_by_weight = FALSE,
                                        line_endpoint_spacing = 0,
                                        arrow_type = "closed",
                                        arrow_size = 0.1,
                                        node_label_font = "sans", node_label_size = 15,
                                        node_label_color = "#000000",
                                        node_label_alpha = 1, node_label_fontface = "plain",
                                        edge_label_font = "sans", edge_label_size = 15,
                                        edge_label_color = "#000000",
                                        edge_label_fill = "#FFFFFF",
                                        edge_label_alpha = 1, edge_label_fontface = "plain",
                                        zoom_factor = 1.2,
                                        data_file = NULL,
                                        bezier_network_edges = FALSE,
                                        network_edges_curvature_magnitude = 0.3,
                                        network_edges_rotate_curvature = FALSE,
                                        network_edges_curvature_asymmetry = 0,
                                        use_clustering = FALSE,
                                        clustering_method = NULL,
                                        cluster_palette = NULL,
                                        modify_params_edge = FALSE,
                                        modified_edges = NULL,
                                        modify_params_edgelabel = FALSE,
                                        modified_edgelabels = NULL,
                                        modify_params_edgelabel_xy = FALSE,
                                        modified_edgelabels_xy = NULL,
                                        modify_params_node = FALSE,
                                        modified_nodes = NULL,
                                        modify_params_node_xy = FALSE,
                                        modified_nodes_xy = NULL,
                                        modify_params_edge_xy = FALSE,
                                        modified_edges_xy = NULL,
                                        modify_params_cov_edge = FALSE,
                                        modified_cov_edges = NULL,
                                        modify_params_nodelabel = FALSE,
                                        modified_nodelabels = NULL,
                                        modify_params_nodelabel_xy = FALSE,
                                        modify_params_bezier_edge = FALSE,
                                        modified_bezier_edges = NULL,
                                        modified_nodelabels_xy = NULL,
                                        modify_params_nodelabel_text = FALSE,
                                        modified_nodelabels_text = NULL,
                                        modify_params_edgelabel_text = FALSE,
                                        modified_edgelabels_text = NULL,
                                        apply_global_nodes = FALSE,
                                        apply_global_edges = FALSE,
                                        apply_global_annotations = FALSE,
                                        flip_layout = FALSE,
                                        flip_direction = NULL,
                                        rotate_layout = FALSE,
                                        rotate_angle = 0,
                                        which_group = "1") {

  apply_modifications <- function(data, modifications, config, mode) {
    if (is.null(modifications) || nrow(modifications) == 0) return(data)
    modified_data <- data

    for (i in seq_len(nrow(modifications))) {
      mod <- modifications[i, ]

      if (mode == 'edge') {
        idx <- which(
          edges_from == mod$lhs &
            edges_to == mod$rhs
        )
      } else if (mode == 'node') {
        idx <- which(
          node_coords$name == mod$text
        )
      }

      if (length(idx) == 1) {
        for (col in config$modify_cols) {
          if (col %in% names(mod) && col %in% names(modified_data)) {
            modified_data[idx, col] <- mod[[col]]
          }
        }

        if (!is.null(config$special_case)) {
          modified_data <- config$special_case(modified_data, idx, mod)
        }
      }
    }
    return(modified_data)
  }

  if (inherits(network_object, "qgraph")) {
    qgraph_obj <- network_object
    node_names <- names(qgraph_obj$graphAttributes$Nodes$labels)
    if (is.null(node_names)) node_names <- as.character(qgraph_obj$graphAttributes$Nodes$labels)

    # Extract and normalize node coordinates
    node_coords <- as.data.frame(qgraph_obj$layout)
    colnames(node_coords) <- c("x", "y")
    node_coords$x <- (node_coords$x - mean(range(node_coords$x))) * layout_width + x_center
    node_coords$y <- (node_coords$y - mean(range(node_coords$y))) * layout_height + y_center
    node_coords$name <- node_names


    # Process edges
    edges_df0 <- data.frame(
      from = qgraph_obj$Edgelist$from,
      to = qgraph_obj$Edgelist$to,
      weight = qgraph_obj$Edgelist$weight,
      directed = qgraph_obj$Edgelist$directed,
      bidirectional = qgraph_obj$Edgelist$bidirectional,
      labels = qgraph_obj$graphAttributes$Edges$labels
    )

    keep_indices <- which(qgraph_obj$graphAttributes$Edges$color != "#00000000")
    edges_df0 <- edges_df0[keep_indices, ]

    edges_df <- edges_df0[!duplicated(
      t(apply(edges_df0[c("from", "to")], 1, sort))
    ), ]

    edges_df <- edges_df[edges_df$from != edges_df$to, ]

    # Edge properties
    edge_op <- ifelse(edges_df$bidirectional, "~~", "~")
    edges_from <- node_names[edges_df$from]
    edges_to <- node_names[edges_df$to]
    edge_labels <- edges_df$labels

  } else {

    stop("Must be output from 'qgraph'.")

  }

  if (use_clustering) {
    num_clusters <- NULL

    n_nodes <- length(node_names)
    adj_mat <- matrix(0, nrow = n_nodes, ncol = n_nodes)

    for (i in seq_len(nrow(edges_df))) {
      from <- edges_df$from[i]
      to <- edges_df$to[i]
      weight <- edges_df$weight[i]
      adj_mat[from, to] <- weight
      adj_mat[to, from] <- weight  # Undirected
    }

    igraph_obj <- igraph::graph_from_adjacency_matrix(adj_mat,
                                                      weighted = TRUE,
                                                      mode = "undirected")

    edge_weights <- E(igraph_obj)$weight

    if (any(edge_weights < 0, na.rm = TRUE)) {
      E(igraph_obj)$weight <- abs(edge_weights)
    }

    if (any(edge_weights == 0, na.rm = TRUE)) {
      zero_weights <- which(edge_weights == 0)
      E(igraph_obj)$weight[zero_weights] <- 0.0001
    }

    if (!is_connected(igraph_obj)) {
    }

    nodes <- qgraph_obj$graphAttributes$Nodes

    communities <- tryCatch({
      switch(
        clustering_method,
        "louvain" = cluster_louvain(igraph_obj, weights = E(igraph_obj)$weight),
        "leiden" = cluster_leiden(igraph_obj, weights = E(igraph_obj)$weight),
        "walktrap" = cluster_walktrap(igraph_obj, weights = E(igraph_obj)$weight),
        "fast_greedy" = cluster_fast_greedy(igraph_obj, weights = E(igraph_obj)$weight)
      )
    }, error = function(e) {

      tryCatch({
        cluster_spinglass(igraph_obj, weights = abs(E(igraph_obj)$weight))
      }, error = function(e2) {
        components(igraph_obj)$membership
      })
    })

    if (!is.null(communities)) {
      if (inherits(communities, "communities")) {
        nodes$community <- membership(communities)
        num_clusters <- max(nodes$community, na.rm = TRUE)
      } else if (is.numeric(communities) || is.integer(communities)) {
        nodes$community <- communities
        num_clusters <- max(communities, na.rm = TRUE)
      } else {
        nodes$community <- rep(1, vcount(igraph_obj))
        num_clusters <- 1
      }
    } else {
      nodes$community <- rep(1, vcount(igraph_obj))
      num_clusters <- 1
    }

    if (is.na(num_clusters) || num_clusters <= 0) {
      num_clusters <- 1
      nodes$community <- rep(1, length(nodes$community))
    }

    palette_max_colors <- list(
      rainbow = Inf,
      Set3 = 12,
      Paired = 12,
      Dark2 = 8,
      Accent = 8,
      Pastel1 = 9,
      Pastel2 = 8,
      Spectral = 11,
      YlGnBu = 9,
      RdYlBu = 11,
      smplot2 = 20
    )

    palette_function <- switch(
      cluster_palette,
      "rainbow" = function(n) rainbow(n),
      "Set3" = function(n) RColorBrewer::brewer.pal(min(n, 12), "Set3"),
      "Paired" = function(n) RColorBrewer::brewer.pal(min(n, 12), "Paired"),
      "Dark2" = function(n) RColorBrewer::brewer.pal(min(n, 8), "Dark2"),
      "Accent" = function(n) RColorBrewer::brewer.pal(min(n, 8), "Accent"),
      "Pastel1" = function(n) RColorBrewer::brewer.pal(min(n, 9), "Pastel1"),
      "Pastel2" = function(n) RColorBrewer::brewer.pal(min(n, 8), "Pastel2"),
      "Spectral" = function(n) RColorBrewer::brewer.pal(min(n, 11), "Spectral"),
      "YlGnBu" = function(n) RColorBrewer::brewer.pal(min(n, 9), "YlGnBu"),
      "RdYlBu" = function(n) RColorBrewer::brewer.pal(min(n, 11), "RdYlBu"),
      "smplot2" = function(n) head(smplot2::sm_palette(), n)
    )

    max_colors <- palette_max_colors[[cluster_palette]]

    if (cluster_palette != "rainbow" && num_clusters > max_colors) {
      palette_function <- rainbow
    }

    valid_communities <- nodes$community
    if (any(is.na(valid_communities))) {
      valid_communities[is.na(valid_communities)] <- 1
    }
    if (any(valid_communities <= 0)) {
      valid_communities[valid_communities <= 0] <- 1
    }
    if (any(valid_communities > num_clusters)) {
      valid_communities[valid_communities > num_clusters] <- num_clusters
    }

    node_colors <- palette_function(num_clusters)[valid_communities]

  } else {
    node_colors <- node_fill_color
  }


  if (flip_layout) {
    flipped <- flip_around_center(node_coords, flip_direction)
    node_coords$x <- flipped$x
    node_coords$y <- flipped$y
  }

  if (rotate_layout) {
    rotated <- rotate_around_center(node_coords, rotate_angle)
    node_coords$x <- rotated$x
    node_coords$y <- rotated$y
  }

  # Apply node position modifications
  if (modify_params_node_xy) {
    node_coords <- apply_modifications(
      node_coords,
      modified_nodes_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0), # No direct column mods
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )

  }

  # Create points dataframe
  points_df <- data.frame(
    x = node_coords$x,
    y = node_coords$y,
    shape = if (apply_global_nodes) node_shape else qgraph_obj$graphAttributes$Nodes$shape,
    color = if (apply_global_nodes) node_colors else sapply(qgraph_obj$graphAttributes$Nodes$color, to_hex2),
    size = node_size,
    border_color = if (apply_global_nodes) node_border_color else qgraph_obj$graphAttributes$Nodes$border.color,
    border_width = node_border_width,
    alpha = if (apply_global_nodes) node_alpha else sapply(sapply(qgraph_obj$graphAttributes$Nodes$color, to_hex), get_alpha),
    width_height_ratio = if (apply_global_nodes) node_width_height_ratio else qgraph_obj$graphAttributes$Nodes$width / qgraph_obj$graphAttributes$Nodes$height,
    orientation = 0,
    lavaan = FALSE,
    network = TRUE,
    locked = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  #Apply node modifications
  if (modify_params_node) {
    points_df <- apply_modifications(
      points_df,
      modified_nodes,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "alpha", "shape", "size", "border_color", "border_width", "width_height_ratio"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }


  # Create annotations
  annotations <- data.frame(
    text = node_coords$name,
    x = node_coords$x,
    y = node_coords$y,
    font = node_label_font,
    size = node_label_size,
    color = if (apply_global_annotations) node_label_color else sapply(qgraph_obj$graphAttributes$Nodes$label.color, to_hex2),
    fill = NA,
    angle = 0,
    alpha = 1,
    fontface = 'plain',
    math_expression = FALSE,
    hjust = 0.5,
    vjust = 0.5,
    lavaan = FALSE,
    network = TRUE,
    locked = FALSE,
    group_label = FALSE,
    loop_label = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  if (modify_params_nodelabel) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "size", "alpha", "angle", "font", "fontface"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }

  if (modify_params_nodelabel_xy) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )
  }


  if (modify_params_nodelabel_text) {
    if (nrow(modified_nodelabels_text) > 0) {
      for (i in seq_len(nrow(modified_nodelabels_text))) {
        mod <- modified_nodelabels_text[i, ]
        node_idx <- which(
          node_coords$name == mod$text
        )
        if (length(node_idx) == 1) {
          annotations$text[[node_idx]] <- mod$nodelabel
          annotations$math_expression[[node_idx]] <- mod$math_expression
        }
      }
    }
  }


  if (length(edges_from) == 0 || length(edges_to) == 0) {
    stop("No edges found in the model. Check the Lavaan syntax.")
  }

  # Create lines dataframe
  lines_df_pre <- data.frame(
    x_start = node_coords[match(edges_from, node_names), "x"],
    y_start = node_coords[match(edges_from, node_names), "y"],
    x_end = node_coords[match(edges_to, node_names), "x"],
    y_end = node_coords[match(edges_to, node_names), "y"],
    ctrl_x = NA,
    ctrl_y = NA,
    ctrl_x2 = NA,
    ctrl_y2 = NA,
    curvature_magnitude = NA,
    rotate_curvature = NA,
    curvature_asymmetry = NA,
    type = ifelse(bezier_network_edges == TRUE,
                  ifelse(edges_df$directed, "Curved Arrow", "Curved Line"),
                  ifelse(edges_df$directed,
                         ifelse(edge_op == "~~", "Curved Arrow", "Straight Arrow"),
                         ifelse(edge_op == "~~", "Curved Line", "Straight Line"))),
    color = if (apply_global_edges) line_color else sapply(qgraph_obj$graphAttributes$Edges$color[as.numeric(rownames(edges_df))], to_hex2),
    end_color = NA,
    color_type = "Single",
    gradient_position = NA,
    width = if (apply_global_edges) line_width else qgraph_obj$graphAttributes$Edges$width[as.numeric(rownames(edges_df))] * 0.6,
    alpha = if (apply_global_edges) line_alpha else sapply(sapply(qgraph_obj$graphAttributes$Edges$color[as.numeric(rownames(edges_df))], to_hex), get_alpha),
    arrow = if (apply_global_edges) ifelse(directed, TRUE, NA) else ifelse(edges_df$directed, TRUE, NA),
    arrow_type = arrow_type,
    arrow_size = arrow_size,
    two_way = edge_op == "~~",
    lavaan = FALSE,
    network = TRUE,
    line_style = "solid",
    locked = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )


  # Apply edge modifications
  if (modify_params_edge) {
    lines_df_pre <- apply_modifications(
      lines_df_pre,
      modified_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "width", "alpha", "line_style", "end_color", "gradient_position", "color_type")
      ),
      mode = 'edge'
    )
  }

  # Adjust edge coordinates
  edge_list <- cbind(match(edges_from, node_names), match(edges_to, node_names))
  lines <- adjust_edge_coordinates(
    lines_df = lines_df_pre,
    edge_list = edge_list,
    points_df = points_df,
    auto_endpoint_spacing = if (apply_global_edges) line_endpoint_spacing else 0
  )

  if (bezier_network_edges == TRUE) {
    bezier_indices <- which(lines$type %in% c('Curved Arrow', 'Curved Line'))

    control_points <- mapply(
      calculate_control_point,
      x_start = lines$x_start[bezier_indices],
      y_start = lines$y_start[bezier_indices],
      x_end = lines$x_end[bezier_indices],
      y_end = lines$y_end[bezier_indices],
      curvature_magnitude = network_edges_curvature_magnitude,
      rotate_curvature = network_edges_rotate_curvature,
      curvature_asymmetry = network_edges_curvature_asymmetry,
      center_x = mean(node_coords$x),
      center_y = mean(node_coords$y),
      SIMPLIFY = FALSE
    )


    # Assign the calculated control points to lines
    lines$ctrl_x[bezier_indices] <- sapply(control_points, `[[`, "ctrl_x")
    lines$ctrl_y[bezier_indices] <- sapply(control_points, `[[`, "ctrl_y")
    lines$ctrl_x2[bezier_indices] <- sapply(control_points, `[[`, "ctrl_x2")
    lines$ctrl_y2[bezier_indices] <- sapply(control_points, `[[`, "ctrl_y2")
    lines$curvature_magnitude[bezier_indices] <- network_edges_curvature_magnitude
    lines$rotate_curvature[bezier_indices] <- network_edges_rotate_curvature
    lines$curvature_asymmetry[bezier_indices] <- network_edges_curvature_asymmetry
    lines$locked[bezier_indices] <- FALSE
  }


  if (modify_params_bezier_edge) {
    lines <- apply_modifications(
      lines,
      modified_bezier_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = NULL,
        special_case = function(data, idx, mod) {
          cp <- calculate_control_point(
            x_start = data$x_start[idx],
            y_start = data$y_start[idx],
            x_end = data$x_end[idx],
            y_end = data$y_end[idx],
            curvature_magnitude = mod$curvature_magnitude,
            rotate_curvature = mod$rotate_curvature,
            curvature_asymmetry = mod$curvature_asymmetry,
            center_x = mean(node_coords$x),
            center_y = mean(node_coords$y)
          )

          # Safely assign control points
          if (all(c("ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2","locked") %in% names(data))) {
            data$ctrl_x[idx] <- cp$ctrl_x
            data$ctrl_y[idx] <- cp$ctrl_y
            data$ctrl_x2[idx] <- cp$ctrl_x2
            data$ctrl_y2[idx] <- cp$ctrl_y2
            data$curvature_magnitude[idx] <- mod$curvature_magnitude
            data$rotate_curvature[idx] <- mod$rotate_curvature
            data$curvature_asymmetry[idx] <- mod$curvature_asymmetry
            data$locked[idx] <- FALSE
            data$type[idx] <- ifelse (directed, 'Curved Arrow', 'Curved Line')
          }
          # }
          return(data)
        }
      ),
      mode = 'edge'
    )
  }

  lines$type[lines$curvature_magnitude == 0] <- "Straight Arrow"
  lines$type[lines$curvature_magnitude != 0] <- "Curved Arrow"


  if (modify_params_edge_xy) {
    lines <- apply_modifications(
      lines,
      modified_edges_xy,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x_start[idx] <- mod$start_x_shift # data$x_start[idx] + mod$start_x_shift
          data$y_start[idx] <- mod$start_y_shift # data$y_start[idx] + mod$start_y_shift
          data$x_end[idx] <- mod$end_x_shift # data$x_end[idx] + mod$end_x_shift
          data$y_end[idx] <- mod$end_y_shift # data$y_end[idx] + mod$end_y_shift

          if (data$type[idx] %in% c('Curved Line', 'Curved Arrow')) {
            cp <- calculate_control_point(
              x_start = data$x_start[idx],
              y_start = data$y_start[idx],
              x_end = data$x_end[idx],
              y_end = data$y_end[idx],
              curvature_magnitude = data$curvature_magnitude[idx],
              rotate_curvature = data$rotate_curvature[idx],
              curvature_asymmetry = data$curvature_asymmetry[idx],
              center_x = mean(node_coords$x),
              center_y = mean(node_coords$y)
            )

            data$ctrl_x[idx] <- cp$ctrl_x
            data$ctrl_y[idx] <- cp$ctrl_y
            data$ctrl_x2[idx] <- cp$ctrl_x2
            data$ctrl_y2[idx] <- cp$ctrl_y2
          }

          return(data)
        }
      ),
      mode = 'edge'
    )
  }

  # Prepare edge labels
  lines_df0 <- cbind(lines, from = edges_from, to = edges_to, text = edge_labels)
  edgelabels_xy_df <- data.frame(x = numeric(nrow(lines_df0)), y = numeric(nrow(lines_df0)))

  for (i in seq_len(nrow(lines_df0))) {
    intp_points <- if (lines_df0$type[i] == "Curved Arrow") {
      create_bezier_curve(
        x_start = lines_df0$x_start[i], y_start = lines_df0$y_start[i],
        x_end = lines_df0$x_end[i], y_end = lines_df0$y_end[i],
        ctrl_x = lines_df0$ctrl_x[i], ctrl_y = lines_df0$ctrl_y[i],
        ctrl_x2 = lines_df0$ctrl_x2[i], ctrl_y2 = lines_df0$ctrl_y2[i], n_points = 100
      )
    } else {
      interpolate_points(
        x_start = lines_df0$x_start[i], y_start = lines_df0$y_start[i],
        x_end = lines_df0$x_end[i], y_end = lines_df0$y_end[i], n = 100
      )
    }
    mid_idx <- 50
    edgelabels_xy_df[i, ] <- intp_points[mid_idx, c("x", "y")]
  }

  label_coords <- data.frame(
    text = lines_df0$text,
    x = edgelabels_xy_df$x,
    y = edgelabels_xy_df$y,
    font = edge_label_font,
    size = edge_label_size,
    color = if (apply_global_annotations) edge_label_color else sapply(qgraph_obj$graphAttributes$Edges$color[as.numeric(rownames(edges_df))], to_hex2),
    fill = edge_label_fill,
    angle = 0,
    alpha = edge_label_alpha,
    fontface = edge_label_fontface,
    math_expression = FALSE,
    hjust = 0.5,
    vjust = 0.5,
    lavaan = FALSE,
    network = TRUE,
    locked = FALSE,
    group_label = FALSE,
    loop_label = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  ) |>
    dplyr::filter(nzchar(trimws(text)))

  # Apply edge label modifications
  if (modify_params_edgelabel) {
    label_coords <- apply_modifications(
      label_coords,
      modified_edgelabels,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "fill", "size", "alpha", "angle", "font", "fontface")
      ),
      mode = 'edge'
    )
  }

  if (modify_params_edgelabel_xy) {
    label_coords <- apply_modifications(
      label_coords,
      modified_edgelabels_xy,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'edge'
    )
  }

  if (modify_params_edgelabel_text) {
    if (nrow(modified_edgelabels_text) > 0) {
      for (i in seq_len(nrow(modified_edgelabels_text))) {
        mod <- modified_edgelabels_text[i, ]
        edge_idx <- which(
          edges_from == mod$lhs &
            edges_to == mod$rhs
        )
        if (length(edge_idx) == 1) {
          label_coords$text[[edge_idx]] <- mod$text
          label_coords$math_expression[[edge_idx]] <- mod$math_expression
        }
      }
    }
  }


  annotations <- rbind(annotations, label_coords)

  points_df[c("x", "y")] <- lapply(points_df[c("x", "y")], round, 5)
  line_cols <- c("x_start", "y_start", "x_end", "y_end", "ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2")
  lines[line_cols] <- lapply(lines[line_cols], round, 5)
  annotations[c("x", "y")] <- lapply(annotations[c("x", "y")], round, 5)

  list(points = points_df, lines = lines, annotations = annotations)
}

#' Generate graph data from DiagrammeR grViz objects
#'
#' Converts DiagrammeR grViz objects into standardized graph data frames for
#' visualization. Extracts node positions, edge connections, and styling
#' information from DOT language diagrams and converts them to a consistent
#' format with extensive customization options.
#'
#' @return A list containing three data frames:
#'   - `points`: Node data with coordinates, shapes, colors, sizes, and properties
#'   - `lines`: Edge data with coordinates, curvatures, arrows, and styles
#'   - `annotations`: Text labels for nodes and edges with positioning and styling
#'
#' @details
#' This function processes DiagrammeR grViz objects by:
#' \itemize{
#'   \item Parsing DOT code to extract node and edge definitions
#'   \item Converting SVG output to extract precise coordinates and styling
#'   \item Scaling and centering coordinates for consistent layout
#'   \item Applying curvature to bidirectional edges
#'   \item Handling complex edge labeling with peak point detection
#'   \item Supporting extensive modification parameters for dynamic updates
#' }
#'
#' The function automatically detects node types (latent, observed, intercept)
#' based on shape attributes and applies appropriate styling. It handles both
#' directed and undirected edges with customizable arrow types and curvature.
#'
#' @importFrom purrr map discard
#' @importFrom dplyr filter mutate across bind_rows
#' @importFrom tidyr separate unnest
#' @importFrom stringr str_split str_extract_all
#' @importFrom DiagrammeRsvg export_svg
#' @importFrom xml2 read_xml xml_ns_strip xml_find_all
#' @keywords internal
#' @noRd
generate_graph_from_diagrammeR <- function(fit, relative_x_position = 25, relative_y_position = 25,
                                           center_x = 0, center_y = 0,
                                           latent_shape = "circle", observed_shape = "square",
                                           int_shape = "triangle",
                                           point_size_latent = 20, point_size_observed = 12,
                                           point_size_int = 10,
                                           line_width = 1, line_alpha = 1, text_size_latent = 18, text_font_latent = "sans",
                                           text_color_latent = "#FFFFFF", text_alpha_latent = 1, text_fontface_latent = 'plain',
                                           text_size_others = 16, text_font_others = "sans",
                                           text_color_others = "#FFFFFF", text_alpha_others = 1, text_fontface_others = 'plain',
                                           text_size_edges = 14, text_font_edges = "sans",
                                           text_color_edges =  "#000000", text_color_fill = "#FFFFFF", text_alpha_edges = 1, text_fontface_edges = 'plain',
                                           point_color_latent = "#cc3d3d", point_color_observed = "#1262b3",
                                           point_color_int = "#0f993d",
                                           edge_color = "#000000", line_endpoint_spacing = 0.2,
                                           node_border_color = "#FFFFFF",
                                           node_border_width = 1,
                                           arrow_type = "closed", arrow_size = 0.1,
                                           lavaan_arrow_location = "end",
                                           zoom_factor = 1.2,
                                           lavaan_curvature_magnitude = 0.3,
                                           lavaan_rotate_curvature = FALSE,
                                           lavaan_curvature_asymmetry = 0,
                                           lavaan_curved_x_shift = 0,
                                           lavaan_curved_y_shift = 0,
                                           highlight_sig_path = FALSE,
                                           sig_path_color = "#000000",
                                           non_sig_path_color = "#000000",
                                           sig_label_fontface = "plain",
                                           non_sig_label_fontface = "plain",
                                           data_file = NULL,
                                           modify_params_edge = FALSE,
                                           modified_edges = NULL,
                                           modify_params_edgelabel = FALSE,
                                           modified_edgelabels = NULL,
                                           modify_params_node = FALSE,
                                           modified_nodes = NULL,
                                           modify_params_node_xy = FALSE,
                                           modified_nodes_xy = NULL,
                                           modify_params_edge_xy = FALSE,
                                           modified_edges_xy = NULL,
                                           modify_params_cov_edge = FALSE,
                                           modified_cov_edges = NULL,
                                           modify_params_nodelabel = FALSE,
                                           modified_nodelabels = NULL,
                                           modify_params_nodelabel_xy = FALSE,
                                           modified_nodelabels_xy = NULL,
                                           modify_params_nodelabel_text = FALSE,
                                           modified_nodelabels_text = NULL,
                                           apply_global_nodes = FALSE,
                                           apply_global_edges = FALSE,
                                           apply_global_annotations = FALSE,
                                           which_group = "grViz") {

  apply_modifications <- function(data, modifications, config, mode) {
    if (is.null(modifications) || nrow(modifications) == 0) return(data)
    modified_data <- data

    for (i in seq_len(nrow(modifications))) {
      mod <- modifications[i, ]

      if (mode == 'edge') {
        idx <- which(
          edges_from == mod$lhs &
            edges_to == mod$rhs
        )
      } else if (mode == 'node') {
        idx <- which(
          node_coords$name == mod$text
        )
      }

      #print(idx)
      if (length(idx) == 1) {
        for (col in config$modify_cols) {
          if (col %in% names(mod) && col %in% names(modified_data)) {
            modified_data[idx, col] <- mod[[col]]
          }
        }

        if (!is.null(config$special_case)) {
          modified_data <- config$special_case(modified_data, idx, mod)
        }
      }
    }
    return(modified_data)
  }

  if (inherits(fit, "grViz")) {
    dot_code <- fit$x$diagram
    node_matches <- extract_node_matches(dot_code)

    node_df <- data.frame(
      id = node_matches[, 1],
      attrs = node_matches[, 2],
      stringsAsFactors = FALSE
    ) |>
      dplyr::filter(nchar(attrs) > 0)  |>
      dplyr::mutate(
        attrs = purrr::map(attrs, ~ stringr::str_split(.x, ",\\s*")[[1]] |>
                      discard(~.x == ""))  # Remove empty attributes
      ) |>
      tidyr::unnest(attrs) |>
      dplyr::mutate(
        # Handle cases where '=' might be missing
        attr_val = ifelse(grepl("=", attrs), attrs, paste0(attrs, "=TRUE"))
      ) |>
      tidyr::separate(attr_val, into = c("attr", "value"), sep = "=", extra = "merge")

    edge_df <- dot_code |>
      stringr::str_extract_all("[\"']?(\\w+)[\"']?\\s*->\\s*[\"']?(\\w+)[\"']?", simplify = TRUE) |>
      as.data.frame() |>
      stats::setNames(c("full", "from", "to"))

    svg_data <- DiagrammeRsvg::export_svg(fit)
    svg <- xml2::read_xml(svg_data)
    xml2::xml_ns_strip(svg)
    nodes <- xml2::xml_find_all(svg, "//g[contains(@class, 'node')]")
    edges <- xml2::xml_find_all(svg, "//g[contains(@class, 'edge')]")

    nodes_df0 <- dplyr::bind_rows(lapply(nodes, extract_node_properties, svg))

    edges_df0 <- dplyr::bind_rows(lapply(edges, extract_edge_properties, svg))
    edges_df0 <- edges_df0 |>
      dplyr::mutate(dplyr::across(c(label_x, label_y), ~abs(.x)))

    element_max <- max(c(length(unique(nodes_df0$x)), length(unique(nodes_df0$y))))

    scaled_data <- scale_coordinates_centered(nodes_df0, edges_df0, new_min = -0.15 * element_max, new_max = 0.15 * element_max)

    nodes_df <- scaled_data$nodes
    edges_df <- scaled_data$edges


    node_coords <- data.frame(x = nodes_df$x, y = nodes_df$y)
    node_coords$x <- (node_coords$x - mean(range(node_coords$x))) * relative_x_position + center_x
    node_coords$y <- (node_coords$y - mean(range(node_coords$y))) * relative_y_position + center_y
    node_coords$name <- nodes_df$label

    name_to_label <- stats::setNames(nodes_df$label, nodes_df$node_name)
    edges_df$source <- name_to_label[edges_df$source]
    edges_df$target <- name_to_label[edges_df$target]
    edges_from <- edges_df$source
    edges_to <- edges_df$target

    #print(edges_from)

    node_names <- nodes_df$label

    latent_vars <- node_names[nodes_df$shape == 'oval']
    observed_vars <- node_names[nodes_df$shape == 'rectangle']
    intercept_vars <- node_names[nodes_df$shape == "triangle"]

    latent_idx <- which(nodes_df$shape == 'oval')[[1]]
    observed_idx <- which(nodes_df$shape == 'rectangle')[[1]]

    width_height_ratio_latent <- nodes_df[latent_idx, ]$width / nodes_df[latent_idx, ]$height
    width_height_ratio_observed <- nodes_df[observed_idx, ]$width / nodes_df[observed_idx, ]$height

    node_shapes <- ifelse(node_names %in% intercept_vars, int_shape, # Triangular shape for the intercept node
                          ifelse(node_names %in% latent_vars, latent_shape, observed_shape))
    node_colors <- ifelse(node_names %in% intercept_vars, point_color_int,
                          ifelse(node_names %in% latent_vars, point_color_latent, point_color_observed))
    node_sizes <- ifelse(node_names %in% intercept_vars, point_size_int,
                         ifelse(node_names %in% latent_vars, point_size_latent, point_size_observed))

    node_width_height_ratios <- ifelse(node_names %in% intercept_vars, 1,
                                       ifelse(node_names %in% latent_vars, width_height_ratio_latent, width_height_ratio_observed))

  } else {
    stop("Must be output from 'DiagrammeR' (grViz object class).")
  }

  #print(edges_from)
  #print(edges_to)

  # Apply node position modifications
  if (modify_params_node_xy) {
    node_coords <- apply_modifications(
      node_coords,
      modified_nodes_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0), # No direct column mods
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )

  }

  # Create points dataframe
  points_df <- data.frame(
    x = node_coords$x,
    y = node_coords$y,
    shape = if (apply_global_nodes) node_shapes else nodes_df$shape,
    color = if (apply_global_nodes) node_colors else nodes_df$fill,
    size = node_sizes,
    border_color = if (apply_global_nodes) node_border_color else nodes_df$stroke_color,
    border_width = node_border_width,
    alpha = if (apply_global_nodes) 1 else nodes_df$alpha,
    width_height_ratio = if (apply_global_nodes) node_width_height_ratios else nodes_df$width / nodes_df$height,
    orientation = 0,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  #Apply node modifications
  if (modify_params_node) {
    points_df <- apply_modifications(
      points_df,
      modified_nodes,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "alpha", "shape", "size", "border_color", "border_width", "width_height_ratio"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }

  # Create annotations
  annotations <- data.frame(
    text = node_coords$name,
    x = node_coords$x,
    y = node_coords$y,
    font = ifelse(node_names %in% latent_vars, text_font_latent, text_font_others),
    size = if (apply_global_annotations) ifelse(node_names %in% latent_vars, text_size_latent, text_size_others) else nodes_df$font_size,
    color = if (apply_global_annotations) ifelse(node_names %in% latent_vars, text_color_latent, text_color_others) else nodes_df$text_color,
    fill = NA,
    angle = 0,
    alpha = if (apply_global_annotations) ifelse(node_names %in% latent_vars, text_alpha_latent, text_alpha_others) else 1,
    fontface = if (apply_global_annotations) ifelse(node_names %in% latent_vars, text_fontface_latent, text_fontface_others) else 'plain',
    math_expression = FALSE,
    hjust = 0.5,
    vjust = 0.5,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group_label = FALSE,
    loop_label = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )


  if (modify_params_nodelabel) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "size", "alpha", "angle", "font", "fontface"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }


  if (modify_params_nodelabel_xy) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )
  }

  if (modify_params_nodelabel_text) {
    if (nrow(modified_nodelabels_text) > 0) {
      for (i in seq_len(nrow(modified_nodelabels_text))) {
        mod <- modified_nodelabels_text[i, ]
        node_idx <- which(
          node_coords$name == mod$text
        )
        if (length(node_idx) == 1) {
          annotations$text[[node_idx]] <- mod$nodelabel
        }
      }
    }
  }


  if (length(edges_from) == 0 || length(edges_to) == 0) {
    stop("No edges found in the model. Check the Lavaan syntax.")
  }

  # Create lines dataframe
  lines_df_pre <- data.frame(
    x_start = edges_df$x_start,
    y_start = edges_df$y_start,
    x_end = edges_df$x_end,
    y_end = edges_df$y_end,
    ctrl_x = edges_df$ctrl_x,
    ctrl_y = edges_df$ctrl_y,
    ctrl_x2 = edges_df$ctrl_x2,
    ctrl_y2 = edges_df$ctrl_y2,
    curvature_magnitude = edges_df$curvature,
    rotate_curvature = NA,
    curvature_asymmetry = NA,
    type = ifelse(edges_df$is_directed, "Straight Arrow","Straight Line"),
    color = if (apply_global_edges) edge_color else edges_df$stroke_color,
    end_color = NA,
    color_type = "Single",
    gradient_position = NA,
    width = if (apply_global_edges) line_width else edges_df$stroke_width,
    alpha = if (apply_global_edges) line_alpha else edges_df$alpha,
    arrow = ifelse(edges_df$is_directed, TRUE, NA),
    arrow_type = arrow_type,
    arrow_size = arrow_size,
    two_way = ifelse(edges_df$arrow_config == "bidirectional", TRUE, FALSE),
    lavaan = TRUE,
    network = FALSE,
    line_style = "solid",
    locked = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  # Apply edge modifications
  if (modify_params_edge) {
    lines_df_pre <- apply_modifications(
      lines_df_pre,
      modified_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "width", "alpha", "line_style", "end_color", "gradient_position", "color_type")
      ),
      mode = 'edge'
    )
  }

  # Adjust edge coordinates
  edge_list <- cbind(match(edges_from, node_names), match(edges_to, node_names))
  lines_df <- adjust_edge_coordinates(
    lines_df = lines_df_pre,
    edge_list = edge_list,
    points_df = points_df,
    auto_endpoint_spacing = if (apply_global_edges) line_endpoint_spacing else 1.0
  )

  # Highlight significant paths if requested
  edge_sig_idx <- which(edges_df$sig == TRUE)
  non_edge_sig_idx <- which(edges_df$sig == FALSE)
  if (highlight_sig_path) {
    lines_df$color[edge_sig_idx] <- sig_path_color
    lines_df$color[non_edge_sig_idx] <- non_sig_path_color
  }


  if ("two_way" %in% colnames(lines_df) && any(lines_df$two_way, na.rm = TRUE)) {
    lines_df[lines_df$two_way, c("x_start", "x_end", "y_start", "y_end")] <-
      lines_df[lines_df$two_way, c("x_start", "x_end", "y_start", "y_end")] +
      c(lavaan_curved_x_shift, lavaan_curved_x_shift, lavaan_curved_y_shift, lavaan_curved_y_shift)
  }


  if (any(lines_df$two_way)) {
    two_way_indices <- which(lines_df$two_way)
    control_points <- mapply(
      calculate_control_point,
      x_start = lines_df$x_start[two_way_indices],
      y_start = lines_df$y_start[two_way_indices],
      x_end = lines_df$x_end[two_way_indices],
      y_end = lines_df$y_end[two_way_indices],
      curvature_magnitude = lavaan_curvature_magnitude,
      rotate_curvature = lavaan_rotate_curvature,
      curvature_asymmetry = lavaan_curvature_asymmetry,
      center_x = mean(node_coords$x),
      center_y = mean(node_coords$y),
      SIMPLIFY = FALSE
    )
    lines_df$ctrl_x[two_way_indices] <- sapply(control_points, `[[`, "ctrl_x")
    lines_df$ctrl_y[two_way_indices] <- sapply(control_points, `[[`, "ctrl_y")
    lines_df$ctrl_x2[two_way_indices] <- sapply(control_points, `[[`, "ctrl_x2")
    lines_df$ctrl_y2[two_way_indices] <- sapply(control_points, `[[`, "ctrl_y2")

    lines_df$curvature_magnitude[two_way_indices] <- lavaan_curvature_magnitude
    lines_df$rotate_curvature[two_way_indices] <- lavaan_rotate_curvature
    lines_df$curvature_asymmetry[two_way_indices] <- lavaan_curvature_asymmetry
  }

  # Apply covariance edge modifications

  if (modify_params_cov_edge) {
    lines_df <- apply_modifications(
      lines_df,
      modified_cov_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = NULL,
        special_case = function(data, idx, mod) {
          # Add input validation
          if (length(idx) == 1 &&
              all(c("x_start", "y_start", "x_end", "y_end") %in% names(data)) &&
              all(c("curvature_magnitude", "rotate_curvature", "curvature_asymmetry", "x_shift", "y_shift") %in% names(mod))) {

            data$x_start[idx] <- data$x_start[idx] + mod$x_shift
            data$x_end[idx] <- data$x_end[idx] + mod$x_shift
            data$y_start[idx] <- data$y_start[idx] + mod$y_shift
            data$y_end[idx] <- data$y_end[idx] + mod$y_shift

            cp <- calculate_control_point(
              x_start = data$x_start[idx],
              y_start = data$y_start[idx],
              x_end = data$x_end[idx],
              y_end = data$y_end[idx],
              curvature_magnitude = mod$curvature_magnitude,
              rotate_curvature = mod$rotate_curvature,
              curvature_asymmetry = mod$curvature_asymmetry,
              center_x = mean(node_coords$x),
              center_y = mean(node_coords$y)
            )

            # Safely assign control points
            if (all(c("ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2", "locked") %in% names(data))) {
              data$ctrl_x[idx] <- cp$ctrl_x
              data$ctrl_y[idx] <- cp$ctrl_y
              data$ctrl_x2[idx] <- cp$ctrl_x2
              data$ctrl_y2[idx] <- cp$ctrl_y2
              data$curvature_magnitude[idx] <- mod$curvature_magnitude
              data$rotate_curvature[idx] <- mod$rotate_curvature
              data$curvature_asymmetry[idx] <- mod$curvature_asymmetry
              data$locked[idx] <- FALSE
            }
          }
          return(data)
        }
      ),
      mode = 'edge'
    )
  }

  lines_df$type[lines_df$curvature_magnitude == 0] <- "Straight Arrow"
  lines_df$type[lines_df$curvature_magnitude != 0] <- "Curved Arrow"
  #print(lines_df)

  if (modify_params_edge_xy) {
    lines_df <- apply_modifications(
      lines_df,
      modified_edges_xy,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x_start[idx] <- data$x_start[idx] + mod$start_x_shift
          data$y_start[idx] <- data$y_start[idx] + mod$start_y_shift
          data$x_end[idx] <- data$x_end[idx] + mod$end_x_shift
          data$y_end[idx] <- data$y_end[idx] + mod$end_y_shift

          cp <- calculate_control_point(
            x_start = data$x_start[idx],
            y_start = data$y_start[idx],
            x_end = data$x_end[idx],
            y_end = data$y_end[idx],
            curvature_magnitude = data$curvature_magnitude[idx],
            rotate_curvature = data$rotate_curvature[idx],
            curvature_asymmetry = data$curvature_asymmetry[idx],
            center_x = mean(node_coords$x),
            center_y = mean(node_coords$y)
          )

          data$ctrl_x[idx] <- cp$ctrl_x
          data$ctrl_y[idx] <- cp$ctrl_y
          data$ctrl_x2[idx] <- cp$ctrl_x2
          data$ctrl_y2[idx] <- cp$ctrl_y2

          return(data)
        }
      ),
      mode = 'edge'
    )
  }


  # Handle arrow location
  if (exists("lavaan_arrow_location") && lavaan_arrow_location == "start") {
    temp <- lines_df[, c("x_start", "y_start")]
    lines_df[, c("x_start", "y_start")] <- lines_df[, c("x_end", "y_end")]
    lines_df[, c("x_end", "y_end")] <- temp
  }

  # Prepare edge labels
  edge_labels <- edges_df$label
  lines_df0 <- cbind(lines_df, from = edges_from, to = edges_to, text = edge_labels)
  edgelabels_xy_df <- data.frame(x = numeric(nrow(lines_df0)), y = numeric(nrow(lines_df0)))

  for (i in seq_len(nrow(lines_df0))) {
    intp_points <- if (lines_df0$type[i] == "Curved Arrow") {
      create_bezier_curve(
        x_start = lines_df0$x_start[i], y_start = lines_df0$y_start[i],
        x_end = lines_df0$x_end[i], y_end = lines_df0$y_end[i],
        ctrl_x = lines_df0$ctrl_x[i], ctrl_y = lines_df0$ctrl_y[i],
        ctrl_x2 = lines_df0$ctrl_x2[i], ctrl_y2 = lines_df0$ctrl_y2[i], n_points = 100
      )
    } else {
      interpolate_points(
        x_start = lines_df0$x_start[i], y_start = lines_df0$y_start[i],
        x_end = lines_df0$x_end[i], y_end = lines_df0$y_end[i], n = 100
      )
    }

    mid_idx <- ifelse(lines_df0$type[i] == "Curved Arrow",
                      find_peak_point(
                        intp_points,
                        x_start = lines_df0$x_start[i],
                        y_start = lines_df0$y_start[i],
                        x_end = lines_df0$x_end[i],
                        y_end = lines_df0$y_end[i]
                      ),
                      50)
    #mid_idx <- 50
    edgelabels_xy_df[i, ] <- intp_points[mid_idx, c("x", "y")]
  }

  label_coords <- data.frame(
    text = edges_df$label,
    x = edgelabels_xy_df$x,
    y = edgelabels_xy_df$y,
    font = text_font_edges,
    size = if (apply_global_annotations) text_font_edges else edges_df$label_size,
    color = if (apply_global_annotations) text_color_edges else edges_df$label_color,
    fill = if (apply_global_annotations) text_color_fill else ifelse(edges_df$label == "", NA, text_color_fill),
    angle = 0,
    alpha = if (apply_global_annotations) text_alpha_edges else edges_df$label_alpha,
    fontface = text_fontface_edges,
    math_expression = FALSE,
    hjust = 0.5,
    vjust = 0.5,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group_label = FALSE,
    loop_label = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  # Apply edge label modifications
  if (modify_params_edgelabel) {
    label_coords <- apply_modifications(
      label_coords,
      modified_edgelabels,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "fill", "size", "alpha", "angle", "font", "fontface")
      ),
      mode = 'edge'
    )

  }

  #if (data_file) {
  annotations <- rbind(annotations, label_coords)
  #}

  points_df[c("x", "y")] <- lapply(points_df[c("x", "y")], round, 5)

  line_cols <- c("x_start", "y_start", "x_end", "y_end",
                 "ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2")
  lines_df[line_cols] <- lapply(lines_df[line_cols], round, 5)

  annotations[c("x", "y")] <- lapply(annotations[c("x", "y")], round, 5)

  list(points = points_df, lines = lines_df, annotations = annotations)
}

#' Generate graph data from DiagrammeR grViz objects
#'
#' Cross-package compatibility with outputs from lavaanPlot
#'
#' @return A list containing three data frames:
#'   - `points`: Node data with coordinates, shapes, colors, sizes, and properties
#'   - `lines`: Edge data with coordinates, curvatures, arrows, and styles
#'   - `annotations`: Text labels for nodes and edges with positioning and styling
#'
#' @importFrom dplyr filter mutate across bind_rows
#' @importFrom purrr map discard
#' @importFrom tidyr unnest separate
#' @importFrom stringr str_split str_extract_all
#' @importFrom xml2 read_xml xml_ns_strip xml_find_all
#' @importFrom DiagrammeRsvg export_svg
#' @importFrom stats setNames
#' @importFrom utils head tail
#' @keywords internal
#' @noRd
generate_graph_from_diagrammeR1 <- function(fit, relative_x_position = 25, relative_y_position = 25,
                                            center_x = 0, center_y = 0,
                                            latent_shape = "circle", observed_shape = "square",
                                            int_shape = "triangle",
                                            point_size_latent = 20, point_size_observed = 12,
                                            point_size_int = 10,
                                            width_height_ratio_latent = 1,
                                            width_height_ratio_observed = 1,
                                            width_height_ratio_int = 1,
                                            line_width = 1, line_alpha = 1, text_size_latent = 18, text_font_latent = "sans",
                                            text_color_latent = "#FFFFFF", text_alpha_latent = 1, text_fontface_latent = 'plain',
                                            text_size_others = 16, text_font_others = "sans",
                                            text_color_others = "#FFFFFF", text_alpha_others = 1, text_fontface_others = 'plain',
                                            text_size_edges = 14, text_font_edges = "sans",
                                            text_color_edges =  "#000000", text_color_fill = "#FFFFFF", text_alpha_edges = 1, text_fontface_edges = 'plain',
                                            point_color_latent = "#cc3d3d", point_color_observed = "#1262b3",
                                            point_color_int = "#0f993d",
                                            edge_color = "#000000", line_endpoint_spacing = 0.2,
                                            node_border_color = "#FFFFFF",
                                            node_border_width = 1,
                                            arrow_type = "closed", arrow_size = 0.1,
                                            lavaan_arrow_location = "end",
                                            zoom_factor = 1.2,
                                            lavaan_curvature_magnitude = 0.5,
                                            lavaan_rotate_curvature = FALSE,
                                            lavaan_curvature_asymmetry = 0,
                                            lavaan_curved_x_shift = 0,
                                            lavaan_curved_y_shift = 0,
                                            remove_edgelabels = FALSE,
                                            highlight_free_path = FALSE,
                                            ff_params_edge = NULL,
                                            ff_params_edgelabel = NULL,
                                            highlight_sig_path = FALSE,
                                            sig_path_color = "#000000",
                                            non_sig_path_color = "#000000",
                                            sig_label_fontface = "plain",
                                            non_sig_label_fontface = "plain",
                                            data_file = NULL,
                                            modify_params_edge = FALSE,
                                            modified_edges = NULL,
                                            modify_params_edgelabel = FALSE,
                                            modified_edgelabels = NULL,
                                            modify_params_edgelabel_xy = FALSE,
                                            modified_edgelabels_xy = NULL,
                                            modify_params_edgelabel_text = FALSE,
                                            modified_edgelabels_text = NULL,
                                            modify_params_node = FALSE,
                                            modified_nodes = NULL,
                                            modify_params_node_xy = FALSE,
                                            modified_nodes_xy = NULL,
                                            modify_params_edge_xy = FALSE,
                                            modified_edges_xy = NULL,
                                            modify_params_cov_edge = FALSE,
                                            modified_cov_edges = NULL,
                                            modify_params_nodelabel = FALSE,
                                            modified_nodelabels = NULL,
                                            modify_params_nodelabel_xy = FALSE,
                                            modified_nodelabels_xy = NULL,
                                            modify_params_nodelabel_text = FALSE,
                                            modified_nodelabels_text = NULL,
                                            modify_params_latent_node_xy = FALSE,
                                            modified_latent_nodes_xy = NULL,
                                            modify_params_latent_node_angle = FALSE,
                                            modified_latent_nodes_angle = NULL,
                                            apply_global_nodes = FALSE,
                                            apply_global_edges = FALSE,
                                            apply_global_annotations = FALSE,
                                            flip_layout = FALSE,
                                            flip_direction = NULL,
                                            rotate_layout = FALSE,
                                            rotate_angle = 0,
                                            which_group = "1") {

  apply_modifications <- function(data, modifications, config, mode, batch_process = FALSE) {
    if (is.null(modifications) || nrow(modifications) == 0) return(data)
    modified_data <- data

    if (batch_process && !is.null(config$batch_special_case)) {
      mods <- modifications
      modified_data <- config$batch_special_case(modified_data, mods)
    } else {
      for (i in seq_len(nrow(modifications))) {
        mod <- modifications[i, ]

        if (mode == 'edge') {
          idx <- which(
            edges_from == mod$lhs &
              edges_to == mod$rhs
          )
        } else if (mode == 'node') {
          idx <- which(
            node_coords$name == mod$text
          )
        }

        if (length(idx) > 0) {
          for (col in config$modify_cols) {
            if (col %in% names(mod) && col %in% names(modified_data)) {
              modified_data[idx, col] <- mod[[col]]
            }
          }

          if (!is.null(config$special_case)) {
            modified_data <- config$special_case(modified_data, idx, mod)
          }
        }
      }
    }
    return(modified_data)
  }

  if (inherits(fit, "grViz")) {
    dot_code <- fit$x$diagram
    node_matches <- extract_node_matches(dot_code)

    node_df <- data.frame(
      id = node_matches[, 1],
      attrs = node_matches[, 2],
      stringsAsFactors = FALSE
    ) |>
      dplyr::filter(nchar(attrs) > 0)  |>
      dplyr::mutate(
        attrs = map(attrs, ~ stringr::str_split(.x, ",\\s*")[[1]] |>
                      discard(~.x == ""))  # Remove empty attributes
      ) |>
      tidyr::unnest(attrs) |>
      dplyr::mutate(
        # Handle cases where '=' might be missing
        attr_val = ifelse(grepl("=", attrs), attrs, paste0(attrs, "=TRUE"))
      ) |>
      tidyr::separate(attr_val, into = c("attr", "value"), sep = "=", extra = "merge")

    edge_df <- dot_code |>
      stringr::str_extract_all("[\"']?(\\w+)[\"']?\\s*->\\s*[\"']?(\\w+)[\"']?", simplify = TRUE) |>
      as.data.frame() |>
      setNames(c("full", "from", "to"))

    svg_data <- DiagrammeRsvg::export_svg(fit)
    svg <- xml2::read_xml(svg_data)
    xml2::xml_ns_strip(svg)
    nodes <- xml2::xml_find_all(svg, "//g[contains(@class, 'node')]")
    edges <- xml2::xml_find_all(svg, "//g[contains(@class, 'edge')]")

    nodes_df0 <- dplyr::bind_rows(lapply(nodes, extract_node_properties, svg))

    edges_df0 <- dplyr::bind_rows(lapply(edges, extract_edge_properties, svg))
    edges_df0 <- edges_df0 |>
      dplyr::mutate(dplyr::across(c(label_x, label_y), ~abs(.x)))

    element_max <- max(c(length(unique(nodes_df0$x)), length(unique(nodes_df0$y))))

    scaled_data <- scale_coordinates_centered(nodes_df0, edges_df0, new_min = -0.15 * element_max, new_max = 0.15 * element_max)

    nodes_df <- scaled_data$nodes
    edges_df <- scaled_data$edges

    node_coords <- data.frame(x = nodes_df$x, y = nodes_df$y)

    if (flip_layout) {
      flipped <- flip_around_center(node_coords, flip_direction)
      node_coords$x <- flipped$x
      node_coords$y <- flipped$y
    }

    if (rotate_layout) {
      rotated <- rotate_around_center(node_coords, rotate_angle)
      node_coords$x <- rotated$x
      node_coords$y <- rotated$y
    }

    node_coords$x <- (node_coords$x - mean(range(node_coords$x))) * relative_x_position + center_x
    node_coords$y <- (node_coords$y - mean(range(node_coords$y))) * relative_y_position + center_y
    node_coords$name <- nodes_df$label

    name_to_label <- setNames(nodes_df$label, nodes_df$node_name)

    edges_df$source <- name_to_label[edges_df$source]
    edges_df$target <- name_to_label[edges_df$target]
    edges_from <- edges_df$source
    edges_to <- edges_df$target

    node_names <- nodes_df$label

    latent_vars <- node_names[nodes_df$shape == 'oval']
    observed_vars <- node_names[nodes_df$shape == 'rectangle']
    intercept_vars <- node_names[nodes_df$shape == "triangle"]

    node_shapes <- ifelse(node_names %in% intercept_vars, int_shape, # Triangular shape for the intercept node
                          ifelse(node_names %in% latent_vars, latent_shape, observed_shape))
    node_colors <- ifelse(node_names %in% intercept_vars, point_color_int,
                          ifelse(node_names %in% latent_vars, point_color_latent, point_color_observed))
    node_sizes <- ifelse(node_names %in% intercept_vars, point_size_int,
                         ifelse(node_names %in% latent_vars, point_size_latent, point_size_observed))

    node_width_height_ratios <- ifelse(node_names %in% intercept_vars, width_height_ratio_int,
                                       ifelse(node_names %in% latent_vars, width_height_ratio_latent, width_height_ratio_observed))

  } else {
    stop("Must be output from 'DiagrammeR' (grViz object class).")
  }


  if (modify_params_latent_node_angle) {
    node_coords <- apply_modifications(
      node_coords,
      modified_latent_nodes_angle,
      config = list(
        batch_special_case = function(data, mods) {

          latent_positions <- which(mods$node_type == "latent")

          most_recent_groups <- list()

          for (i in seq_along(latent_positions)) {
            current_latent_pos <- latent_positions[i]

            if (i == length(latent_positions) ||
                (current_latent_pos + 1 < latent_positions[i + 1] &&
                 mods$node_type[current_latent_pos + 1] == "observed")) {

              group_rows <- current_latent_pos
              next_row <- current_latent_pos + 1

              while (next_row <= nrow(mods) && mods$node_type[next_row] == "observed") {
                group_rows <- c(group_rows, next_row)
                next_row <- next_row + 1
              }

              group_data <- mods[group_rows, ]
              latent_name <- group_data$text[1]

              if (is.null(most_recent_groups[[latent_name]]) ||
                  group_rows[1] > most_recent_groups[[latent_name]]$positions[1]) {
                most_recent_groups[[latent_name]] <- list(
                  data = group_data,
                  positions = group_rows
                )
              }
            }
          }

          final_mods_list <- lapply(most_recent_groups, function(x) x$data)

          for (i in  seq_along(final_mods_list)) {
            curr_mods <- final_mods_list[[i]]
            latent_mod <- curr_mods[curr_mods$node_type == "latent", ]
            observed_mods <- curr_mods[curr_mods$node_type == "observed", ]

            latent_node_idx <- which(data$name == latent_mod$text) # idx in node_coords
            obs_node_idx <- which(data$name %in% observed_mods$text)

            if (length(latent_node_idx) > 0 && length(obs_node_idx) > 0) {
              # Calculate centroid (latent node xy)
              center_x <- data$x[latent_node_idx]
              center_y <- data$y[latent_node_idx]

              angle_rad <- curr_mods$angle * pi / 180

              for (obs_idx in obs_node_idx) {
                if(length(obs_idx) > 0) {
                  x_relative <- data$x[obs_idx] - center_x
                  y_relative <- data$y[obs_idx] - center_y

                  new_x <- x_relative * cos(angle_rad) - y_relative * sin(angle_rad)
                  new_y <- x_relative * sin(angle_rad) + y_relative * cos(angle_rad)

                  data$x[obs_idx] <- new_x + center_x
                  data$y[obs_idx] <- new_y + center_y
                }
              }
            }
          }

          return(data)
        }
      ),
      mode = 'node', # ignored
      batch_process = TRUE
    )
  }

  # Apply node position modifications
  if (modify_params_node_xy) {
    node_coords <- apply_modifications(
      node_coords,
      modified_nodes_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0), # No direct column mods
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )
  }

  # Apply latent node group position modifications
  if (modify_params_latent_node_xy) {
    node_coords <- apply_modifications(
      node_coords,
      modified_latent_nodes_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0), # No direct column mods
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )
  }

  # Create points dataframe
  points_df <- data.frame(
    x = node_coords$x,
    y = node_coords$y,
    shape = if (apply_global_nodes) node_shapes else nodes_df$shape,
    color = if (apply_global_nodes) node_colors else nodes_df$fill,
    size = node_sizes,
    border_color = if (apply_global_nodes) node_border_color else nodes_df$stroke_color,
    border_width = node_border_width,
    alpha = if (apply_global_nodes) 1 else nodes_df$alpha,
    width_height_ratio = if (apply_global_nodes) node_width_height_ratios else nodes_df$width / nodes_df$height,
    orientation = 0,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  #Apply node modifications
  if (modify_params_node) {
    points_df <- apply_modifications(
      points_df,
      modified_nodes,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "alpha", "shape", "size", "border_color", "border_width", "width_height_ratio"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }

  # Create annotations
  annotations <- data.frame(
    text = node_coords$name,
    x = node_coords$x,
    y = node_coords$y,
    font = ifelse(node_names %in% latent_vars, text_font_latent, text_font_others),
    size = if (apply_global_annotations) ifelse(node_names %in% latent_vars, text_size_latent, text_size_others) else nodes_df$font_size,
    color = if (apply_global_annotations) ifelse(node_names %in% latent_vars, text_color_latent, text_color_others) else nodes_df$text_color,
    fill = NA,
    angle = 0,
    alpha = if (apply_global_annotations) ifelse(node_names %in% latent_vars, text_alpha_latent, text_alpha_others) else 1,
    fontface = if (apply_global_annotations) ifelse(node_names %in% latent_vars, text_fontface_latent, text_fontface_others) else 'plain',
    math_expression = FALSE,
    hjust = 0.5,
    vjust = 0.5,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group_label = FALSE,
    loop_label = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  if (modify_params_nodelabel) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "size", "alpha", "angle", "font", "fontface"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }


  if (modify_params_nodelabel_xy) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )
  }

  if (modify_params_nodelabel_text) {
    if (nrow(modified_nodelabels_text) > 0) {
      for (i in seq_len(nrow(modified_nodelabels_text))) {
        mod <- modified_nodelabels_text[i, ]
        node_idx <- which(
          node_coords$name == mod$text
        )
        if (length(node_idx) == 1) {
          annotations$text[[node_idx]] <- mod$nodelabel
          annotations$math_expression[[node_idx]] <- mod$math_expression
        }
      }
    }
  }


  if (length(edges_from) == 0 || length(edges_to) == 0) {
    stop("No edges found in the model. Check the Lavaan syntax.")
  }

  # Create lines dataframe
  lines_df_pre <- data.frame(
    x_start = edges_df$x_start,
    y_start = edges_df$y_start,
    x_end = edges_df$x_end,
    y_end = edges_df$y_end,
    ctrl_x = edges_df$ctrl_x,
    ctrl_y = edges_df$ctrl_y,
    ctrl_x2 = edges_df$ctrl_x2,
    ctrl_y2 = edges_df$ctrl_y2,
    curvature_magnitude = edges_df$curvature,
    rotate_curvature = NA,
    curvature_asymmetry = NA,
    type = ifelse(edges_df$is_directed, "Straight Arrow","Straight Line"),
    color = if (apply_global_edges) edge_color else edges_df$stroke_color,
    end_color = NA,
    color_type = "Single",
    gradient_position = NA,
    width = if (apply_global_edges) line_width else edges_df$stroke_width,
    alpha = if (apply_global_edges) line_alpha else edges_df$alpha,
    arrow = ifelse(edges_df$is_directed, TRUE, NA),
    arrow_type = arrow_type,
    arrow_size = arrow_size,
    two_way = ifelse(edges_df$arrow_config == "bidirectional", TRUE, FALSE),
    lavaan = TRUE,
    network = FALSE,
    line_style = "solid",
    locked = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  if (highlight_free_path) {
    lines_df_pre <- apply_modifications(
      lines_df_pre,
      ff_params_edge,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color")
      ),
      mode = 'edge'
    )
  }

  # Highlight significant paths if requested
  edge_sig_idx <- which(edges_df$sig == TRUE)
  non_edge_sig_idx <- which(edges_df$sig == FALSE)
  if (highlight_sig_path) {
    lines_df_pre$color[edge_sig_idx] <- sig_path_color
    lines_df_pre$color[non_edge_sig_idx] <- non_sig_path_color
  }

  # Apply edge modifications
  if (modify_params_edge) {
    lines_df_pre <- apply_modifications(
      lines_df_pre,
      modified_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "width", "alpha", "line_style", "end_color", "gradient_position", "color_type")
      ),
      mode = 'edge'
    )
  }

  # Adjust edge coordinates
  edge_list <- cbind(match(edges_from, node_names), match(edges_to, node_names))
  lines_df <- adjust_edge_coordinates(
    lines_df = lines_df_pre,
    edge_list = edge_list,
    points_df = points_df,
    auto_endpoint_spacing = if (apply_global_edges) line_endpoint_spacing else 1.0
  )

  if ("two_way" %in% colnames(lines_df) && any(lines_df$two_way, na.rm = TRUE)) {
    lines_df[lines_df$two_way, c("x_start", "x_end", "y_start", "y_end")] <-
      lines_df[lines_df$two_way, c("x_start", "x_end", "y_start", "y_end")] +
      c(lavaan_curved_x_shift, lavaan_curved_x_shift, lavaan_curved_y_shift, lavaan_curved_y_shift)
  }


  if (any(lines_df$two_way)) {
    two_way_indices <- which(lines_df$two_way)
    control_points <- mapply(
      calculate_control_point,
      x_start = lines_df$x_start[two_way_indices],
      y_start = lines_df$y_start[two_way_indices],
      x_end = lines_df$x_end[two_way_indices],
      y_end = lines_df$y_end[two_way_indices],
      curvature_magnitude = lavaan_curvature_magnitude,
      rotate_curvature = lavaan_rotate_curvature,
      curvature_asymmetry = lavaan_curvature_asymmetry,
      center_x = mean(node_coords$x),
      center_y = mean(node_coords$y),
      SIMPLIFY = FALSE
    )
    lines_df$ctrl_x[two_way_indices] <- sapply(control_points, `[[`, "ctrl_x")
    lines_df$ctrl_y[two_way_indices] <- sapply(control_points, `[[`, "ctrl_y")
    lines_df$ctrl_x2[two_way_indices] <- sapply(control_points, `[[`, "ctrl_x2")
    lines_df$ctrl_y2[two_way_indices] <- sapply(control_points, `[[`, "ctrl_y2")

    lines_df$curvature_magnitude[two_way_indices] <- lavaan_curvature_magnitude
    lines_df$rotate_curvature[two_way_indices] <- lavaan_rotate_curvature
    lines_df$curvature_asymmetry[two_way_indices] <- lavaan_curvature_asymmetry
  }

  # Apply covariance edge modifications

  if (modify_params_cov_edge) {
    lines_df <- apply_modifications(
      lines_df,
      modified_cov_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = NULL,
        special_case = function(data, idx, mod) {
          # Add input validation
          if (length(idx) == 1 &&
              all(c("x_start", "y_start", "x_end", "y_end") %in% names(data)) &&
              all(c("curvature_magnitude", "rotate_curvature", "curvature_asymmetry", "x_shift", "y_shift") %in% names(mod))) {

            data$x_start[idx] <- data$x_start[idx] + mod$x_shift
            data$x_end[idx] <- data$x_end[idx] + mod$x_shift
            data$y_start[idx] <- data$y_start[idx] + mod$y_shift
            data$y_end[idx] <- data$y_end[idx] + mod$y_shift

            cp <- calculate_control_point(
              x_start = data$x_start[idx],
              y_start = data$y_start[idx],
              x_end = data$x_end[idx],
              y_end = data$y_end[idx],
              curvature_magnitude = mod$curvature_magnitude,
              rotate_curvature = mod$rotate_curvature,
              curvature_asymmetry = mod$curvature_asymmetry,
              center_x = mean(node_coords$x),
              center_y = mean(node_coords$y)
            )

            # Safely assign control points
            if (all(c("ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2", "locked") %in% names(data))) {
              data$ctrl_x[idx] <- cp$ctrl_x
              data$ctrl_y[idx] <- cp$ctrl_y
              data$ctrl_x2[idx] <- cp$ctrl_x2
              data$ctrl_y2[idx] <- cp$ctrl_y2
              data$curvature_magnitude[idx] <- mod$curvature_magnitude
              data$rotate_curvature[idx] <- mod$rotate_curvature
              data$curvature_asymmetry[idx] <- mod$curvature_asymmetry
              data$locked[idx] <- FALSE
            }
          }
          return(data)
        }
      ),
      mode = 'edge'
    )
  }

  lines_df$type[lines_df$curvature_magnitude == 0] <- "Straight Arrow"
  lines_df$type[lines_df$curvature_magnitude != 0] <- "Curved Arrow"


  if (modify_params_edge_xy) {
    lines_df <- apply_modifications(
      lines_df,
      modified_edges_xy,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x_start[idx] <- mod$start_x_shift # data$x_start[idx] + mod$start_x_shift
          data$y_start[idx] <- mod$start_y_shift # data$y_start[idx] + mod$start_y_shift
          data$x_end[idx] <- mod$end_x_shift # data$x_end[idx] + mod$end_x_shift
          data$y_end[idx] <- mod$end_y_shift # data$y_end[idx] + mod$end_y_shift

          if (data$type[idx] %in% c('Curved Line', 'Curved Arrow')) {
            cp <- calculate_control_point(
              x_start = data$x_start[idx],
              y_start = data$y_start[idx],
              x_end = data$x_end[idx],
              y_end = data$y_end[idx],
              curvature_magnitude = data$curvature_magnitude[idx],
              rotate_curvature = data$rotate_curvature[idx],
              curvature_asymmetry = data$curvature_asymmetry[idx],
              center_x = mean(node_coords$x),
              center_y = mean(node_coords$y)
            )

            data$ctrl_x[idx] <- cp$ctrl_x
            data$ctrl_y[idx] <- cp$ctrl_y
            data$ctrl_x2[idx] <- cp$ctrl_x2
            data$ctrl_y2[idx] <- cp$ctrl_y2
          }

          return(data)
        }
      ),
      mode = 'edge'
    )
  }


  # Handle arrow location
  if (exists("lavaan_arrow_location") && lavaan_arrow_location == "start") {
    temp <- lines_df[, c("x_start", "y_start")]
    lines_df[, c("x_start", "y_start")] <- lines_df[, c("x_end", "y_end")]
    lines_df[, c("x_end", "y_end")] <- temp
  }

  edge_labels <- edges_df$label
  # Prepare edge labels
  lines_df0 <- cbind(lines_df, from = edges_from, to = edges_to, text = edge_labels)
  edgelabels_xy_df <- data.frame(x = numeric(nrow(lines_df0)), y = numeric(nrow(lines_df0)))

  for (i in seq_len(nrow(lines_df0))) {
    intp_points <- if (lines_df0$type[i] == "Curved Arrow") {
      create_bezier_curve(
        x_start = lines_df0$x_start[i], y_start = lines_df0$y_start[i],
        x_end = lines_df0$x_end[i], y_end = lines_df0$y_end[i],
        ctrl_x = lines_df0$ctrl_x[i], ctrl_y = lines_df0$ctrl_y[i],
        ctrl_x2 = lines_df0$ctrl_x2[i], ctrl_y2 = lines_df0$ctrl_y2[i], n_points = 100
      )
    } else {
      interpolate_points(
        x_start = lines_df0$x_start[i], y_start = lines_df0$y_start[i],
        x_end = lines_df0$x_end[i], y_end = lines_df0$y_end[i], n = 100
      )
    }

    mid_idx <- ifelse(lines_df0$type[i] == "Curved Arrow",
                      find_peak_point(
                        intp_points,
                        x_start = lines_df0$x_start[i],
                        y_start = lines_df0$y_start[i],
                        x_end = lines_df0$x_end[i],
                        y_end = lines_df0$y_end[i]
                      ),
                      50)
    #mid_idx <- 50
    edgelabels_xy_df[i, ] <- intp_points[mid_idx, c("x", "y")]
  }

  label_coords <- data.frame(
    text = edges_df$label,
    x = edgelabels_xy_df$x,
    y = edgelabels_xy_df$y,
    font = text_font_edges,
    size = if (apply_global_annotations) text_size_edges else edges_df$label_size,
    color = if (apply_global_annotations) text_color_edges else edges_df$label_color,
    fill = if (apply_global_annotations) text_color_fill else ifelse(edges_df$label == "", NA, text_color_fill),
    angle = 0,
    alpha = if (apply_global_annotations) text_alpha_edges else edges_df$label_alpha,
    fontface = text_fontface_edges,
    math_expression = FALSE,
    hjust = 0.5,
    vjust = 0.5,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group_label = FALSE,
    loop_label = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  ) |>
    dplyr::filter(nzchar(trimws(text)))

  # Apply edge label modifications
  if (modify_params_edgelabel) {
    label_coords <- apply_modifications(
      label_coords,
      modified_edgelabels,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "fill", "size", "alpha", "angle", "font", "fontface")
      ),
      mode = 'edge'
    )
  }

  if (highlight_free_path) {
    label_coords <- apply_modifications(
      label_coords,
      ff_params_edgelabel,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "fontface")
      ),
      mode = 'edge'
    )
  }

  if (highlight_sig_path) {
    label_coords$fontface[edge_sig_idx] <- sig_label_fontface
    label_coords$fontface[non_edge_sig_idx] <- non_sig_label_fontface
    label_coords$color[edge_sig_idx] <- sig_path_color
    label_coords$color[non_edge_sig_idx] <- non_sig_path_color
  }


  if (modify_params_edgelabel_xy) {
    label_coords <- apply_modifications(
      label_coords,
      modified_edgelabels_xy,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'edge'
    )
  }


  if (modify_params_edgelabel_text) {
    if (nrow(modified_edgelabels_text) > 0) {
      for (i in seq_len(nrow(modified_edgelabels_text))) {
        mod <- modified_edgelabels_text[i, ]
        edge_idx <- which(
          edges_from == mod$lhs &
            edges_to == mod$rhs
        )
        if (length(edge_idx) > 0) {
          label_coords$text[[edge_idx]] <- mod$text
          label_coords$math_expression[[edge_idx]] <- mod$math_expression
        }
      }
    }
  }

  if (remove_edgelabels) {
    label_coords <- data.frame(
      text = character(),
      x = numeric(),
      y = numeric(),
      font = character(),
      size = numeric(),
      color = character(),
      fill = character(),
      angle = numeric(),
      alpha = numeric(),
      fontface = character(),
      math_expression = logical(),
      hjust = numeric(),
      vjust = numeric(),
      lavaan = logical(),
      network = logical(),
      locked = logical(),
      group_label = logical(),
      loop_label = logical(),
      group = character(),
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(data_file)) {
    if (data_file) {
      annotations <- rbind(annotations, label_coords)
    }
  }

  points_df[c("x", "y")] <- lapply(points_df[c("x", "y")], round, 5)

  line_cols <- c("x_start", "y_start", "x_end", "y_end",
                 "ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2")
  lines_df[line_cols] <- lapply(lines_df[line_cols], round, 5)

  annotations[c("x", "y")] <- lapply(annotations[c("x", "y")], round, 5)

  list(points = points_df, lines = lines_df, annotations = annotations)
}

#' Flip coordinates around center point
#'
#' @param df A data frame containing 'x' and 'y' coordinate columns
#' @param direction Direction of flip: "horizontal", "vertical", or "both"
#'
#' @return A list with flipped 'x' and 'y' coordinates
#'
#' @keywords internal
#' @noRd
flip_around_center <- function(df, direction) {
  center_x <- mean(df$x)
  center_y <- mean(df$y)
  x <- df$x
  y <- df$y

  if (direction == "horizontal") {
    x <- 2 * center_x - x
  } else if (direction == "vertical") {
    y <- 2 * center_y - y
  } else if (direction == "both") {
    x <- 2 * center_x - x
    y <- 2 * center_y - y
  }

  return(list(x = x, y = y))
}

#' Generate graph data from semPaths objects with custom coordinates
#'
#' Enhanced version of generate_graph_from_sempaths that allows custom node coordinates
#' and provides advanced latent variable manipulation features including group rotation,
#' custom width-height ratios, and comprehensive modification parameters.
#' @return A list containing three data frames:
#'   - `points`: Node data with coordinates, shapes, colors, sizes, and properties
#'   - `lines`: Edge data with coordinates, curvatures, arrows, and styles
#'   - `annotations`: Text labels for nodes and edges with positioning and styling
#'   - `loops`: Loop data with coordinates, etc
#'
#' @importFrom dplyr filter mutate select distinct
#' @importFrom stats na.omit
#' @importFrom stats setNames
#' @importFrom utils head tail
#' @importFrom rlang .data
#' @keywords internal
#' @noRd
generate_graph_from_sempaths1 <- function(fit, node_coords,
                                          latent_shape = "circle", observed_shape = "square",
                                          int_shape = "triangle",
                                          point_size_latent = 20, point_size_observed = 12,
                                          point_size_int = 10,
                                          width_height_ratio_latent = 1,
                                          width_height_ratio_observed = 1,
                                          width_height_ratio_int = 1,
                                          line_width = 1, line_alpha = 1, text_size_latent = 18, text_font_latent = "sans",
                                          text_color_latent = "#FFFFFF", text_alpha_latent = 1, text_fontface_latent = 'plain',
                                          text_size_others = 16, text_font_others = "sans",
                                          text_color_others = "#FFFFFF", text_alpha_others = 1, text_fontface_others = 'plain',
                                          text_size_edges = 14, text_font_edges = "sans",
                                          text_color_edges =  "#000000", text_color_fill = "#FFFFFF", text_alpha_edges = 1, text_fontface_edges = 'plain',
                                          point_color_latent = "#cc3d3d", point_color_observed = "#1262b3",
                                          point_color_int = "#0f993d",
                                          edge_color = "#000000", line_endpoint_spacing = 1.2,
                                          node_border_color = "#FFFFFF",
                                          node_border_width = 1,
                                          arrow_type = "closed", arrow_size = 0.1,
                                          lavaan_arrow_location = "end",
                                          zoom_factor = 1.2,
                                          lavaan_curvature_magnitude = 0.5,
                                          lavaan_rotate_curvature = FALSE,
                                          lavaan_curvature_asymmetry = 0,
                                          lavaan_curved_x_shift = 0,
                                          lavaan_curved_y_shift = 0,
                                          remove_edgelabels = FALSE,
                                          highlight_free_path = FALSE,
                                          ff_params_edge = NULL,
                                          ff_params_edgelabel = NULL,
                                          ff_params_loop = NULL,
                                          ff_params_looplabel = NULL,
                                          highlight_free_path_multi_group = FALSE,
                                          ff_params_edge_multi = NULL,
                                          ff_params_edgelabel_multi = NULL,
                                          ff_params_loop_multi = NULL,
                                          ff_params_looplabel_multi = NULL,
                                          highlight_sig_path = FALSE,
                                          sig_path_color = "#000000",
                                          non_sig_path_color = "#000000",
                                          sig_label_fontface = "plain",
                                          non_sig_label_fontface = "plain",
                                          highlight_multi_group = FALSE,
                                          sig_diff_edge = NULL,
                                          sig_diff_edgelabel = NULL,
                                          sig_diff_loop = NULL,
                                          sig_diff_looplabel = NULL,
                                          residuals = FALSE,
                                          residuals_orientation_type = 'Graded',
                                          lavaan_loop_offset = 0.8,
                                          lavaan_radius = 2.5,
                                          lavaan_line_color_loop = "#000000",
                                          lavaan_line_alpha_loop = 1,
                                          lavaan_arrow_type_loop = "closed",
                                          lavaan_arrow_size_loop = 0.08,
                                          lavaan_width_loop = 1,
                                          lavaan_height_loop = 1,
                                          lavaan_gap_size_loop = 0.05,
                                          lavaan_two_way_arrow_loop = TRUE,
                                          data_file = NULL,
                                          modify_params_edge = FALSE,
                                          modified_edges = NULL,
                                          modify_params_edgelabel = FALSE,
                                          modified_edgelabels = NULL,
                                          modify_params_edgelabel_xy = FALSE,
                                          modified_edgelabels_xy = NULL,
                                          modify_params_edgelabel_text = FALSE,
                                          modified_edgelabels_text = NULL,
                                          modify_params_node = FALSE,
                                          modified_nodes = NULL,
                                          modify_params_node_xy = FALSE,
                                          modified_nodes_xy = NULL,
                                          modify_params_edge_xy = FALSE,
                                          modified_edges_xy = NULL,
                                          modify_params_cov_edge = FALSE,
                                          modified_cov_edges = NULL,
                                          modify_params_nodelabel = FALSE,
                                          modified_nodelabels = NULL,
                                          modify_params_nodelabel_xy = FALSE,
                                          modified_nodelabels_xy = NULL,
                                          modify_params_nodelabel_text = FALSE,
                                          modified_nodelabels_text = NULL,
                                          modify_params_latent_node_xy = FALSE,
                                          modified_latent_nodes_xy = NULL,
                                          modify_params_latent_node_angle = FALSE,
                                          modified_latent_nodes_angle = NULL,
                                          modify_params_loop = FALSE,
                                          modified_loops = NULL,
                                          modify_params_loop_xy = FALSE,
                                          modified_loops_xy = NULL,
                                          modify_params_loop_location = FALSE,
                                          modified_loops_location = NULL,
                                          modify_params_looplabel = FALSE,
                                          modified_looplabels = NULL,
                                          modify_params_looplabel_xy = FALSE,
                                          modified_looplabels_xy = NULL,
                                          modify_params_looplabel_text = FALSE,
                                          modified_looplabels_text = NULL,
                                          loop_names_remove = NULL,
                                          which_group = "1") {

  apply_modifications <- function(data, modifications, config, mode, batch_process = FALSE) {
    if (is.null(modifications) || nrow(modifications) == 0) return(data)
    modified_data <- data

    if (batch_process && !is.null(config$batch_special_case)) {
      mods <- modifications
      modified_data <- config$batch_special_case(modified_data, mods)
    } else {

      for (i in seq_len(nrow(modifications))) {
        mod <- modifications[i, ]

        if (mode == 'edge') {
          idx <- which(
            edges_from == mod$lhs &
              edges_to == mod$rhs
          )

        } else if (mode == 'node') {
          idx <- which(
            node_coords$name == mod$text
          )
        } else if (mode == 'loop') {
          idx <- which(
            node_coords$name[!node_coords$name %in% loop_names_remove] == mod$text
          )
        }

        if (length(idx) > 0) {
          for (col in config$modify_cols) {
            if (col %in% names(mod) && col %in% names(modified_data)) {
              modified_data[idx, col] <- mod[[col]]
            }
          }

          if (!is.null(config$special_case)) {
            modified_data <- config$special_case(modified_data, idx, mod)
          }
        }
      }
    }
    return(modified_data)
  }

  if (inherits(fit, "qgraph")) {
    sem_paths <- fit
    node_names <- names(sem_paths$graphAttributes$Nodes$labels)
    if (is.null(node_names)) node_names <- sem_paths$graphAttributes$Nodes$labels
    node_types <- sem_paths$graphAttributes$Nodes$shape

    # Node classification
    latent_vars <- node_names[node_types == "circle"]
    observed_vars <- node_names[node_types == "square"]
    intercept_vars <- node_names[node_types == "triangle"]

    # Process edges
    edges_df0 <- data.frame(
      from = sem_paths$Edgelist$from,
      to = sem_paths$Edgelist$to,
      weight = sem_paths$Edgelist$weight,
      directed = sem_paths$Edgelist$directed,
      bidirectional = sem_paths$Edgelist$bidirectional,
      labels = sem_paths$graphAttributes$Edges$labels,
      sig = ifelse(sem_paths$graphAttributes$Edge$color == "#000000FF", TRUE, FALSE)
    )

    self_loop_indices <- which(edges_df0$from == edges_df0$to)

    if (length(self_loop_indices) > 0) {
      edges_df0$self_loop <- FALSE
      edges_df0$self_loop[self_loop_indices] <- TRUE
    } else {
      edges_df0$self_loop <- FALSE
    }

    edges_df0 <- edges_df0[!duplicated(
      t(apply(edges_df0[c("from", "to")], 1, sort))
    ), ]

    edges_df <- edges_df0[!edges_df0$self_loop, ]
    edges_loop_df <- edges_df0[edges_df0$self_loop,]
    loop_node_names <- node_names[edges_loop_df$from]

    # Handle intercepts
    intercept_indices <- which(node_names == "1")
    intercept_sources <- character(length(intercept_indices))

    for (i in seq_along(intercept_indices)) {
      intercept_idx <- intercept_indices[i]

      connected_edges <- edges_df[edges_df$from == intercept_idx | edges_df$to == intercept_idx, ]

      if (nrow(connected_edges) > 0) {
        target_nodes <- c(connected_edges$from, connected_edges$to)
        target_nodes <- target_nodes[target_nodes != intercept_idx]

        if (length(target_nodes) > 0) {
          target_var <- node_names[target_nodes[1]]
          intercept_sources[i] <- paste0("Intercept_", target_var)
        } else {
          intercept_sources[i] <- paste0("Intercept_", i)
        }
      } else {
        intercept_sources[i] <- paste0("Intercept_", i)
      }
    }

    node_names[intercept_indices] <- intercept_sources

    edges_from <- node_names[edges_df$from]
    edges_to <- node_names[edges_df$to]

    # Edge properties
    edge_op <- ifelse(edges_df$bidirectional, "~~", "~")
    edge_labels <- edges_df$labels
    edge_sig <- edges_df$sig

    # Node properties
    node_shapes <- ifelse(node_names %in% intercept_sources, int_shape,
                          ifelse(node_names %in% latent_vars, latent_shape, observed_shape))
    node_colors <- ifelse(node_names %in% intercept_sources, point_color_int,
                          ifelse(node_names %in% latent_vars, point_color_latent, point_color_observed))
    node_sizes <- ifelse(node_names %in% intercept_sources, point_size_int,
                         ifelse(node_names %in% latent_vars, point_size_latent, point_size_observed))
    node_width_height_ratios <- ifelse(node_names %in% intercept_sources, width_height_ratio_int,
                                       ifelse(node_names %in% latent_vars, width_height_ratio_latent, width_height_ratio_observed))

    node_coords$name <- node_names

  } else {

    stop("Must be output from semPaths with class of 'qgraph'.")

  }


  if (modify_params_latent_node_angle) {
    node_coords <- apply_modifications(
      node_coords,
      modified_latent_nodes_angle,
      config = list(
        batch_special_case = function(data, mods) {

          latent_positions <- which(mods$node_type == "latent")

          most_recent_groups <- list()

          for (i in seq_along(latent_positions)) {
            current_latent_pos <- latent_positions[i]

            if (i == length(latent_positions) ||
                (current_latent_pos + 1 < latent_positions[i + 1] &&
                 (mods$node_type[current_latent_pos + 1] == "observed" ||
                  mods$node_type[current_latent_pos + 1] == "intercept"))) {

              group_rows <- current_latent_pos
              next_row <- current_latent_pos + 1

              # Continue while next rows are observed OR intercept nodes
              while (next_row <= nrow(mods) &&
                     (mods$node_type[next_row] == "observed" ||
                      mods$node_type[next_row] == "intercept")) {
                group_rows <- c(group_rows, next_row)
                next_row <- next_row + 1
              }

              group_data <- mods[group_rows, ]
              latent_name <- group_data$text[1]

              if (is.null(most_recent_groups[[latent_name]]) ||
                  group_rows[1] > most_recent_groups[[latent_name]]$positions[1]) {
                most_recent_groups[[latent_name]] <- list(
                  data = group_data,
                  positions = group_rows
                )
              }
            }
          }

          final_mods_list <- lapply(most_recent_groups, function(x) x$data)

          for (i in seq_along(final_mods_list)) {
            curr_mods <- final_mods_list[[i]]
            latent_mod <- curr_mods[curr_mods$node_type == "latent", ]
            observed_mods <- curr_mods[curr_mods$node_type == "observed", ]
            intercept_mods <- curr_mods[curr_mods$node_type == "intercept", ]

            latent_node_idx <- which(data$name == latent_mod$text) # idx in node_coords
            obs_node_idx <- which(data$name %in% observed_mods$text)
            int_node_idx <- which(data$name %in% intercept_mods$text)

            # Combine observed and intercept nodes for rotation
            all_child_nodes <- c(obs_node_idx, int_node_idx)

            if (length(latent_node_idx) > 0 && length(all_child_nodes) > 0) {
              # Calculate centroid (latent node xy)
              center_x <- data$x[latent_node_idx]
              center_y <- data$y[latent_node_idx]

              angle_rad <- curr_mods$angle * pi / 180

              for (child_idx in all_child_nodes) {
                if(length(child_idx) > 0) {
                  x_relative <- data$x[child_idx] - center_x
                  y_relative <- data$y[child_idx] - center_y

                  new_x <- x_relative * cos(angle_rad) - y_relative * sin(angle_rad)
                  new_y <- x_relative * sin(angle_rad) + y_relative * cos(angle_rad)

                  data$x[child_idx] <- new_x + center_x
                  data$y[child_idx] <- new_y + center_y
                }
              }
            }
          }

          return(data)
        }
      ),
      mode = 'node', # ignored
      batch_process = TRUE
    )
  }

  # Apply node position modifications
  if (modify_params_node_xy) {
    node_coords <- apply_modifications(
      node_coords,
      modified_nodes_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0), # No direct column mods
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )
  }

  # Apply latent node group position modifications
  if (modify_params_latent_node_xy) {
    node_coords <- apply_modifications(
      node_coords,
      modified_latent_nodes_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0), # No direct column mods
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )
  }

  # Create points dataframe
  points_df <- data.frame(
    x = node_coords$x,
    y = node_coords$y,
    shape = node_shapes,
    color = node_colors,
    size = node_sizes,
    border_color = node_border_color,
    border_width = node_border_width,
    alpha = 1,
    width_height_ratio = node_width_height_ratios,
    orientation = 0,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )

  #Apply node modifications
  if (modify_params_node) {
    points_df <- apply_modifications(
      points_df,
      modified_nodes,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "alpha", "shape", "size", "border_color", "border_width", "width_height_ratio"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }

  # Create annotations
  annotations <- data.frame(
    text = node_coords$text,
    x = node_coords$x,
    y = node_coords$y,
    font = ifelse(node_names %in% latent_vars, text_font_latent, text_font_others),
    size = ifelse(node_names %in% latent_vars, text_size_latent, text_size_others),
    color = ifelse(node_names %in% latent_vars, text_color_latent, text_color_others),
    fill = NA,
    angle = 0,
    alpha = ifelse(node_names %in% latent_vars, text_alpha_latent, text_alpha_others),
    fontface = ifelse(node_names %in% latent_vars, text_fontface_latent, text_fontface_others),
    math_expression = FALSE,
    hjust = 0.5,
    vjust = 0.5,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group_label = FALSE,
    loop_label = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )


  if (modify_params_nodelabel) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "size", "alpha", "angle", "font", "fontface"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }


  if (modify_params_nodelabel_xy) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )
  }


  if (modify_params_nodelabel_text) {
    if (nrow(modified_nodelabels_text) > 0) {
      for (i in seq_len(nrow(modified_nodelabels_text))) {
        mod <- modified_nodelabels_text[i, ]
        node_idx <- which(
          node_coords$name == mod$text
        )
        if (length(node_idx) == 1) {
          annotations$text[[node_idx]] <- mod$nodelabel
          annotations$math_expression[[node_idx]] <- mod$math_expression
        }
      }
    }
  }

  if (length(edges_from) == 0 || length(edges_to) == 0) {
    stop("No edges found in the model. Check the Lavaan syntax.")
  }

  # Create lines dataframe
  lines_df_pre <- data.frame(
    x_start = node_coords[match(edges_from, node_names), "x"],
    y_start = node_coords[match(edges_from, node_names), "y"],
    x_end = node_coords[match(edges_to, node_names), "x"],
    y_end = node_coords[match(edges_to, node_names), "y"],
    ctrl_x = NA,
    ctrl_y = NA,
    ctrl_x2 = NA,
    ctrl_y2 = NA,
    curvature_magnitude = NA,
    rotate_curvature = NA,
    curvature_asymmetry = NA,
    type = ifelse(edge_op == "~~", "Curved Arrow", "Straight Arrow"),
    color = edge_color,
    end_color = NA,
    color_type = "Single",
    gradient_position = NA,
    width = line_width,
    alpha = line_alpha,
    arrow = TRUE,
    arrow_type = arrow_type,
    arrow_size = arrow_size,
    two_way = edge_op == "~~",
    lavaan = TRUE,
    network = FALSE,
    line_style = "solid",
    locked = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  )


  # Apply edge modifications

  if (highlight_free_path) {
    lines_df_pre <- apply_modifications(
      lines_df_pre,
      ff_params_edge,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color")
      ),
      mode = 'edge'
    )
  }

  # Highlight significant paths if requested
  edge_sig_idx <- which(edges_df$sig == TRUE)
  non_edge_sig_idx <- which(edges_df$sig == FALSE)

  if (highlight_sig_path) {
    lines_df_pre$color[edge_sig_idx] <- sig_path_color
    lines_df_pre$color[non_edge_sig_idx] <- non_sig_path_color
  }

  if (highlight_free_path_multi_group) {
    lines_df_pre <- apply_modifications(
      lines_df_pre,
      ff_params_edge_multi,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "width")
      ),
      mode = 'edge'
    )
  }

  if (highlight_multi_group) {
    lines_df_pre <- apply_modifications(
      lines_df_pre,
      sig_diff_edge,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "width")
      ),
      mode = 'edge'
    )
  }

  if (modify_params_edge) {
    lines_df_pre <- apply_modifications(
      lines_df_pre,
      modified_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "width", "alpha", "line_style", "end_color", "gradient_position", "color_type")
      ),
      mode = 'edge'
    )
  }

  # Adjust edge coordinates
  edge_list <- cbind(match(edges_from, node_names), match(edges_to, node_names))

  lines_df <- adjust_edge_coordinates(
    lines_df = lines_df_pre,
    edge_list = edge_list,
    points_df = points_df,
    auto_endpoint_spacing = line_endpoint_spacing
  )

  if ("two_way" %in% colnames(lines_df) && any(lines_df$two_way, na.rm = TRUE)) {
    lines_df[lines_df$two_way, c("x_start", "x_end", "y_start", "y_end")] <-
      lines_df[lines_df$two_way, c("x_start", "x_end", "y_start", "y_end")] +
      c(lavaan_curved_x_shift, lavaan_curved_x_shift, lavaan_curved_y_shift, lavaan_curved_y_shift)
  }

  if (any(lines_df$two_way)) {
    two_way_indices <- which(lines_df$two_way)
    control_points <- mapply(
      calculate_control_point,
      x_start = lines_df$x_start[two_way_indices],
      y_start = lines_df$y_start[two_way_indices],
      x_end = lines_df$x_end[two_way_indices],
      y_end = lines_df$y_end[two_way_indices],
      curvature_magnitude = lavaan_curvature_magnitude,
      rotate_curvature = lavaan_rotate_curvature,
      curvature_asymmetry = lavaan_curvature_asymmetry,
      center_x = mean(node_coords$x),
      center_y = mean(node_coords$y),
      SIMPLIFY = FALSE
    )

    lines_df$ctrl_x[two_way_indices] <- sapply(control_points, `[[`, "ctrl_x")
    lines_df$ctrl_y[two_way_indices] <- sapply(control_points, `[[`, "ctrl_y")
    lines_df$ctrl_x2[two_way_indices] <- sapply(control_points, `[[`, "ctrl_x2")
    lines_df$ctrl_y2[two_way_indices] <- sapply(control_points, `[[`, "ctrl_y2")

    lines_df$curvature_magnitude[two_way_indices] <- lavaan_curvature_magnitude
    lines_df$rotate_curvature[two_way_indices] <- lavaan_rotate_curvature
    lines_df$curvature_asymmetry[two_way_indices] <- lavaan_curvature_asymmetry
  }

  # Apply covariance edge modifications
  if (modify_params_cov_edge) {
    lines_df <- apply_modifications(
      lines_df,
      modified_cov_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = NULL,
        special_case = function(data, idx, mod) {
          # Add input validation
          if (length(idx) == 1 &&
              all(c("x_start", "y_start", "x_end", "y_end") %in% names(data)) &&
              all(c("curvature_magnitude", "rotate_curvature", "curvature_asymmetry", "x_shift", "y_shift") %in% names(mod))) {

            data$x_start[idx] <- data$x_start[idx] + mod$x_shift
            data$x_end[idx] <- data$x_end[idx] + mod$x_shift
            data$y_start[idx] <- data$y_start[idx] + mod$y_shift
            data$y_end[idx] <- data$y_end[idx] + mod$y_shift

            cp <- calculate_control_point(
              x_start = data$x_start[idx],
              y_start = data$y_start[idx],
              x_end = data$x_end[idx],
              y_end = data$y_end[idx],
              curvature_magnitude = mod$curvature_magnitude,
              rotate_curvature = mod$rotate_curvature,
              curvature_asymmetry = mod$curvature_asymmetry,
              center_x = mean(node_coords$x),
              center_y = mean(node_coords$y)
            )

            # Safely assign control points
            if (all(c("ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2", "locked") %in% names(data))) {
              data$ctrl_x[idx] <- cp$ctrl_x
              data$ctrl_y[idx] <- cp$ctrl_y
              data$ctrl_x2[idx] <- cp$ctrl_x2
              data$ctrl_y2[idx] <- cp$ctrl_y2
              data$curvature_magnitude[idx] <- mod$curvature_magnitude
              data$rotate_curvature[idx] <- mod$rotate_curvature
              data$curvature_asymmetry[idx] <- mod$curvature_asymmetry
              data$locked[idx] <- FALSE
            }
          }
          return(data)
        }
      ),
      mode = 'edge'
    )
  }

  lines_df$type[lines_df$curvature_magnitude == 0] <- "Straight Arrow"
  lines_df$type[lines_df$curvature_magnitude != 0] <- "Curved Arrow"

  if (modify_params_edge_xy) {
    lines_df <- apply_modifications(
      lines_df,
      modified_edges_xy,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x_start[idx] <- mod$start_x_shift # data$x_start[idx] + mod$start_x_shift
          data$y_start[idx] <- mod$start_y_shift # data$y_start[idx] + mod$start_y_shift
          data$x_end[idx] <- mod$end_x_shift # data$x_end[idx] + mod$end_x_shift
          data$y_end[idx] <- mod$end_y_shift # data$y_end[idx] + mod$end_y_shift

          if (data$type[idx] %in% c('Curved Line', 'Curved Arrow')) {
            cp <- calculate_control_point(
              x_start = data$x_start[idx],
              y_start = data$y_start[idx],
              x_end = data$x_end[idx],
              y_end = data$y_end[idx],
              curvature_magnitude = data$curvature_magnitude[idx],
              rotate_curvature = data$rotate_curvature[idx],
              curvature_asymmetry = data$curvature_asymmetry[idx],
              center_x = mean(node_coords$x),
              center_y = mean(node_coords$y)
            )

            data$ctrl_x[idx] <- cp$ctrl_x
            data$ctrl_y[idx] <- cp$ctrl_y
            data$ctrl_x2[idx] <- cp$ctrl_x2
            data$ctrl_y2[idx] <- cp$ctrl_y2
          }

          return(data)
        }
      ),
      mode = 'edge'
    )
  }


  # Handle arrow location
  if (exists("lavaan_arrow_location") && lavaan_arrow_location == "start") {
    temp <- lines_df[, c("x_start", "y_start")]
    lines_df[, c("x_start", "y_start")] <- lines_df[, c("x_end", "y_end")]
    lines_df[, c("x_end", "y_end")] <- temp
  }

  # Prepare self-loop arrows

  if (!is.null(loop_names_remove)) {
    loop_node_names <- loop_node_names[!loop_node_names %in% loop_names_remove]
  }


  if (residuals) {
    loops_df = data.frame(
      x_center = node_coords[match(loop_node_names, node_names), "x"],
      y_center = node_coords[match(loop_node_names, node_names), "y"],
      radius = lavaan_radius,
      color = lavaan_line_color_loop,
      width = lavaan_width_loop,
      alpha = lavaan_line_alpha_loop,
      arrow_type = lavaan_arrow_type_loop,
      arrow_size = lavaan_arrow_size_loop,
      gap_size = lavaan_gap_size_loop,
      loop_width = lavaan_width_loop,
      loop_height = lavaan_height_loop,
      orientation = 0, # later modified
      lavaan = TRUE,
      two_way = lavaan_two_way_arrow_loop,
      locked = FALSE,
      group = which_group,
      stringsAsFactors = FALSE
    )

    offset_distance <- lavaan_loop_offset

    for (i in 1:nrow(loops_df)) {
      node_name <- loop_node_names[i]
      node_index <- which(node_names == node_name)

      node_x <- points_df$x[node_index]
      node_y <- points_df$y[node_index]
      node_shape <- points_df$shape[node_index]
      node_size <- points_df$size[node_index]
      node_ratio <- points_df$width_height_ratio[node_index]
      node_orientation <- points_df$orientation[node_index]

      connected_edges <- lines_df[lines_df$edges_from == node_name | lines_df$edges_to == node_name, ]

      if (nrow(connected_edges) > 0) {
        edge_vectors <- list()

        for (j in 1:nrow(connected_edges)) {
          if (connected_edges$edges_from[j] == node_name) {
            # Outgoing edge
            dx <- connected_edges$x_end[j] - connected_edges$x_start[j]
            dy <- connected_edges$y_end[j] - connected_edges$y_start[j]
          } else {
            # Incoming edge (reverse direction)
            dx <- connected_edges$x_start[j] - connected_edges$x_end[j]
            dy <- connected_edges$y_start[j] - connected_edges$y_end[j]
          }
          edge_vectors[[j]] <- c(dx, dy)
        }

        avg_dx <- mean(sapply(edge_vectors, function(v) v[1]))
        avg_dy <- mean(sapply(edge_vectors, function(v) v[2]))

        angle_to_connections <- atan2(avg_dy, avg_dx) * 180 / pi
        gap_angle <- (angle_to_connections + 180) %% 360  # Opposite direction

      } else {
        dx_to_center <- mean(node_coords$x) - node_x
        dy_to_center <- mean(node_coords$y) - node_y
        gap_angle <- atan2(dy_to_center, dx_to_center) * 180 / pi
        gap_angle <- ifelse(gap_angle < 0, gap_angle + 360, gap_angle)
      }

      if (residuals_orientation_type == 'Quadratic') {
        quadrant_angles <- c(0, 90, 180, 270)
        angle_differences <- abs(gap_angle - quadrant_angles)
        angle_differences <- pmin(angle_differences, 360 - angle_differences)
        nearest_quadrant <- quadrant_angles[which.min(angle_differences)]
        final_gap_angle <- nearest_quadrant
      } else {
        final_gap_angle <- gap_angle
      }

      loops_df$orientation[i] <- (final_gap_angle - 90) %% 360

      alignment <- detect_local_alignment(node_x, node_y, node_coords$x, node_coords$y)

      if (alignment$type == "horizontal" && alignment$count >= 3) {
        group_dx <- 0
        group_dy <- ifelse(node_y > mean(node_coords$y), 1, -1)  # Outer side
      } else if (alignment$type == "vertical" && alignment$count >= 3) {
        group_dx <- ifelse(node_x > mean(node_coords$x), 1, -1)  # Outer side
        group_dy <- 0
      } else {
        position_angle <- (final_gap_angle + 180) %% 360
        group_dx <- cos(position_angle * pi / 180)
        group_dy <- sin(position_angle * pi / 180)
      }

      boundary_point <- find_intersection(
        x_center = node_x,
        y_center = node_y,
        x_target = node_x + group_dx,
        y_target = node_y + group_dy,
        size = node_size,
        width_height_ratio = node_ratio,
        orientation = node_orientation,
        shape = node_shape
      )

      dx_boundary <- boundary_point$x - node_x
      dy_boundary <- boundary_point$y - node_y
      distance_to_boundary <- sqrt(dx_boundary^2 + dy_boundary^2)

      if (distance_to_boundary > 0) {
        scale <- (distance_to_boundary + offset_distance) / distance_to_boundary
        loops_df$x_center[i] <- node_x + dx_boundary * scale
        loops_df$y_center[i] <- node_y + dy_boundary * scale
      } else {
        loops_df$x_center[i] <- node_x + group_dx * offset_distance
        loops_df$y_center[i] <- node_y + group_dy * offset_distance
      }

      loop_x <- loops_df$x_center[i]
      loop_y <- loops_df$y_center[i]

      dx_loop_to_node <- node_x - loop_x
      dy_loop_to_node <- node_y - loop_y

      angle_loop_to_node <- atan2(dy_loop_to_node, dx_loop_to_node) * 180 / pi
      angle_loop_to_node <- ifelse(angle_loop_to_node < 0, angle_loop_to_node + 360, angle_loop_to_node)

      loops_df$orientation[i] <- (angle_loop_to_node - 90) %% 360
    }

  } else {
    loops_df = data.frame(
      x_center = numeric(), y_center = numeric(), radius = numeric(), color = character(),
      width = numeric(), alpha = numeric(), arrow_type = character(), arrow_size = numeric(),
      gap_size = numeric(), loop_width = numeric(), loop_height = numeric(), orientation = numeric(),
      lavaan = logical(), two_way = logical(), locked = logical(), group = character(), stringsAsFactors = FALSE
    )
  }

  if (residuals) {

    if (highlight_free_path) {
      loops_df <- apply_modifications(
        loops_df,
        ff_params_loop,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = c("color")
        ),
        mode = 'loop'
      )
    }

    loop_sig_idx <- which(edges_loop_df$sig == TRUE)
    non_loop_sig_idx <- which(edges_loop_df$sig == FALSE)

    if (highlight_sig_path) {
      loops_df$color[loop_sig_idx] <- sig_path_color
      loops_df$color[non_loop_sig_idx] <- non_sig_path_color
    }

    if (highlight_free_path_multi_group) {
      loops_df <- apply_modifications(
        loops_df,
        ff_params_loop_multi,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = c("color", "width", "radius")
        ),
        mode = 'loop'
      )
    }

    if (highlight_multi_group) {
      loops_df <- apply_modifications(
        loops_df,
        sig_diff_loop,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = c("color", "width"),
          special_case = NULL
        ),
        mode = 'loop'
      )
    }

    if (modify_params_loop) {
      loops_df <- apply_modifications(
        loops_df,
        modified_loops,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = c("color", "alpha", "radius", "width", "type", "arrow_size", "gap_size", "two_way"),
          special_case = NULL
        ),
        mode = 'loop'
      )
    }

    if (modify_params_loop_xy) {
      loops_df <- apply_modifications(
        loops_df,
        modified_loops_xy,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = character(0), # No direct column mods
          special_case = function(data, idx, mod) {
            data$x_center[idx] <- data$x_center[idx] + mod$x_shift
            data$y_center[idx] <- data$y_center[idx] + mod$y_shift
            return(data)
          }
        ),
        mode = 'loop'
      )
    }

    if (modify_params_loop_location) {
      loops_df <- apply_modifications(
        loops_df,
        modified_loops_location,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = character(0),
          special_case = function(data, idx, mod) {
            # For each modified loop, recompute position and orientation
            for (i in idx) {
              node_name <- loop_node_names[i]
              node_index <- which(node_names == node_name)

              node_x <- points_df$x[node_index]
              node_y <- points_df$y[node_index]
              node_shape <- points_df$shape[node_index]
              node_size <- points_df$size[node_index]
              node_ratio <- points_df$width_height_ratio[node_index]
              node_orientation <- points_df$orientation[node_index]

              loop_angle <- as.numeric(mod$loop_location)

              angle_rad <- loop_angle * pi / 180
              group_dx <- cos(angle_rad)
              group_dy <- sin(angle_rad)

              # Find boundary point and apply offset
              boundary_point <- find_intersection(
                x_center = node_x,
                y_center = node_y,
                x_target = node_x + group_dx,
                y_target = node_y + group_dy,
                size = node_size,
                width_height_ratio = node_ratio,
                orientation = node_orientation,
                shape = node_shape
              )

              offset_distance <- lavaan_loop_offset

              dx_boundary <- boundary_point$x - node_x
              dy_boundary <- boundary_point$y - node_y
              distance_to_boundary <- sqrt(dx_boundary^2 + dy_boundary^2)

              # Update loop position
              if (distance_to_boundary > 0) {
                scale <- (distance_to_boundary + offset_distance) / distance_to_boundary
                data$x_center[i] <- node_x + dx_boundary * scale
                data$y_center[i] <- node_y + dy_boundary * scale
              } else {
                data$x_center[i] <- node_x + group_dx * offset_distance
                data$y_center[i] <- node_y + group_dy * offset_distance
              }

              # Recalculate orientation based on new position
              loop_x <- data$x_center[i]
              loop_y <- data$y_center[i]

              dx_loop_to_node <- node_x - loop_x
              dy_loop_to_node <- node_y - loop_y

              angle_loop_to_node <- atan2(dy_loop_to_node, dx_loop_to_node) * 180 / pi
              angle_loop_to_node <- ifelse(angle_loop_to_node < 0, angle_loop_to_node + 360, angle_loop_to_node)

              data$orientation[i] <- (angle_loop_to_node - 90) %% 360
            }

            return(data)
          }
        ),
        mode = 'loop'
      )
    }
  }

  # Prepare edge labels
  lines_df0 <- cbind(lines_df, from = edges_from, to = edges_to, text = edge_labels)
  edgelabels_xy_df <- data.frame(x = numeric(nrow(lines_df0)), y = numeric(nrow(lines_df0)))

  for (i in seq_len(nrow(lines_df0))) {
    intp_points <- if (lines_df0$type[i] == "Curved Arrow") {
      create_bezier_curve(
        x_start = lines_df0$x_start[i], y_start = lines_df0$y_start[i],
        x_end = lines_df0$x_end[i], y_end = lines_df0$y_end[i],
        ctrl_x = lines_df0$ctrl_x[i], ctrl_y = lines_df0$ctrl_y[i],
        ctrl_x2 = lines_df0$ctrl_x2[i], ctrl_y2 = lines_df0$ctrl_y2[i], n_points = 100
      )
    } else {
      interpolate_points(
        x_start = lines_df0$x_start[i], y_start = lines_df0$y_start[i],
        x_end = lines_df0$x_end[i], y_end = lines_df0$y_end[i], n = 100
      )
    }

    mid_idx <- ifelse(lines_df0$type[i] == "Curved Arrow",
                      find_peak_point(
                        intp_points,
                        x_start = lines_df0$x_start[i],
                        y_start = lines_df0$y_start[i],
                        x_end = lines_df0$x_end[i],
                        y_end = lines_df0$y_end[i]
                      ),
                      50)
    #mid_idx <- 50
    edgelabels_xy_df[i, ] <- intp_points[mid_idx, c("x", "y")]
  }

  pval_idx <- grep("\\*", lines_df0$text)
  label_coords <- data.frame(
    text = as.character(lines_df0$text),
    x = edgelabels_xy_df$x,
    y = edgelabels_xy_df$y,
    font = text_font_edges,
    size = text_size_edges,
    color = text_color_edges,
    fill = ifelse(lines_df0$text == "", NA, text_color_fill),
    angle = 0,
    alpha = text_alpha_edges,
    fontface = text_fontface_edges,
    math_expression = FALSE,
    hjust = 0.5,
    vjust = 0.5,
    lavaan = TRUE,
    network = FALSE,
    locked = FALSE,
    group_label = FALSE,
    loop_label = FALSE,
    group = which_group,
    stringsAsFactors = FALSE
  ) |>
    dplyr::filter(nzchar(trimws(text)))

  edgelabels_sig_idx <- which(edges_df$sig == TRUE)
  non_edgelabels_sig_idx <- which(edges_df$sig == FALSE)

  if (highlight_free_path) {
    label_coords <- apply_modifications(
      label_coords,
      ff_params_edgelabel,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "fontface")
      ),
      mode = 'edge'
    )
  }

  if (highlight_sig_path) {
    label_coords$fontface[edgelabels_sig_idx] <- sig_label_fontface
    label_coords$fontface[non_edgelabels_sig_idx] <- non_sig_label_fontface
    label_coords$color[edgelabels_sig_idx] <- sig_path_color
    label_coords$color[non_edgelabels_sig_idx] <- non_sig_path_color
  }

  if (highlight_free_path_multi_group) {
    label_coords <- apply_modifications(
      label_coords,
      ff_params_edgelabel_multi,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "fontface")
      ),
      mode = 'edge'
    )
  }

  if (highlight_multi_group) {
    label_coords <- apply_modifications(
      label_coords,
      sig_diff_edgelabel,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "fontface")
      ),
      mode = 'edge'
    )
  }
  # Apply edge label modifications

  if (modify_params_edgelabel) {
    label_coords <- apply_modifications(
      label_coords,
      modified_edgelabels,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "fill", "size", "alpha", "angle", "font", "fontface")
      ),
      mode = 'edge'
    )
  }

  if (modify_params_edgelabel_xy) {
    label_coords <- apply_modifications(
      label_coords,
      modified_edgelabels_xy,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'edge'
    )
  }

  if (modify_params_edgelabel_text) {
    if (nrow(modified_edgelabels_text) > 0) {
      for (i in seq_len(nrow(modified_edgelabels_text))) {
        mod <- modified_edgelabels_text[i, ]
        edge_idx <- which(
          edges_from == mod$lhs &
            edges_to == mod$rhs
        )
        if (length(edge_idx) == 1) {
          label_coords$text[[edge_idx]] <- mod$text
          label_coords$math_expression[[edge_idx]] <- mod$math_expression
        }
      }
    }
  }


  if (residuals) {
    # Prepare loop labels
    loop_labels_df <- data.frame(
      x = numeric(nrow(loops_df)),
      y = numeric(nrow(loops_df)),
      text = character(nrow(loops_df)),
      stringsAsFactors = FALSE
    )

    for (i in 1:nrow(loops_df)) {
      node_name <- loop_node_names[i]
      node_index <- which(node_names == node_name)

      loop_label_row <- edges_loop_df[edges_loop_df$from == node_index & edges_loop_df$self_loop == TRUE, ]

      if (nrow(loop_label_row) > 0) {
        loop_labels_df$text[i] <- as.character(loop_label_row$labels[1])
      } else {
        loop_labels_df$text[i] <- ""
      }

      node_x <- loops_df$x_center[i]  # Current loop center (after positioning)
      node_y <- loops_df$y_center[i]
      loop_radius <- loops_df$radius[i]
      loop_orientation <- loops_df$orientation[i]

      gap_angle <- (loop_orientation + 90) %% 360
      label_angle <- (gap_angle + 180) %% 360  # Opposite side

      label_angle_rad <- label_angle * pi / 180

      loop_labels_df$x[i] <- node_x + loop_radius * cos(label_angle_rad)
      loop_labels_df$y[i] <- node_y + loop_radius * sin(label_angle_rad)
    }

    loop_label_coords <- data.frame(
      text = loop_labels_df$text,
      x = loop_labels_df$x,
      y = loop_labels_df$y,
      font = text_font_edges,
      size = text_size_edges,
      color = text_color_edges,
      fill = ifelse(loop_labels_df$text == "", NA, text_color_fill),
      angle = 0,
      alpha = text_alpha_edges,
      fontface = text_fontface_edges,
      math_expression = FALSE,
      hjust = 0.5,
      vjust = 0.5,
      lavaan = TRUE,
      network = FALSE,
      locked = FALSE,
      group_label = FALSE,
      loop_label = TRUE,
      group = which_group,
      stringsAsFactors = FALSE
    ) |>
      dplyr::filter(nzchar(trimws(text)))
  } else {
    loop_label_coords <- data.frame(
      text = character(), x = numeric(), y = numeric(), font = character(), size = numeric(), color = character(), fill = character(), angle = numeric(), alpha = numeric(),
      fontface = character(), math_expression = logical(), hjust = numeric(), vjust = numeric(), lavaan = logical(), network = logical(), locked = logical(), group_label = logical(), loop_label = logical(), group = character(),
      stringsAsFactors = FALSE
    )
  }

  if (residuals) {

    if (highlight_free_path) {
      loop_label_coords <- apply_modifications(
        loop_label_coords,
        ff_params_looplabel,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = c("color", "fontface")
        ),
        mode = 'loop'
      )
    }

    if (highlight_sig_path) {
      loop_label_coords$fontface[loop_sig_idx] <- sig_label_fontface
      loop_label_coords$fontface[loop_sig_idx] <- non_sig_label_fontface
      loop_label_coords$color[loop_sig_idx] <- sig_path_color
      loop_label_coords$color[non_loop_sig_idx] <- non_sig_path_color
    }

    if (highlight_free_path_multi_group) {
      loop_label_coords <- apply_modifications(
        loop_label_coords,
        ff_params_looplabel_multi,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = c("color", "fontface")
        ),
        mode = 'loop'
      )
    }

    if (highlight_multi_group) {
      loop_label_coords <- apply_modifications(
        loop_label_coords,
        sig_diff_looplabel,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = c("color", "fontface")
        ),
        mode = 'loop'
      )
    }

    if (modify_params_looplabel) {
      loop_label_coords <- apply_modifications(
        loop_label_coords,
        modified_looplabels,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = c("color", "fill", "size", "alpha", "angle", "font", "fontface"),
          special_case = NULL
        ),
        mode = 'loop'
      )
    }

    if (modify_params_looplabel_xy) {
      loop_label_coords <- apply_modifications(
        loop_label_coords,
        modified_looplabels_xy,
        config = list(
          match_cols = c(name = "text"),
          modify_cols = character(0),
          special_case = function(data, idx, mod) {
            data$x[idx] <- data$x[idx] + mod$x_shift
            data$y[idx] <- data$y[idx] + mod$y_shift
            return(data)
          }
        ),
        mode = 'loop'
      )
    }

    if (modify_params_looplabel_text) {
      if (nrow(modified_looplabels_text) > 0) {
        for (i in seq_len(nrow(modified_looplabels_text))) {
          mod <- modified_looplabels_text[i, ]
          loop_idx <- which(
            node_coords$name[!node_coords$name %in% loop_names_remove] == mod$text
          )
          if (length(loop_idx) == 1) {
            loop_label_coords$text[[loop_idx]] <- mod$looplabel
            loop_label_coords$math_expression[[loop_idx]] <- mod$math_expression
          }
        }
      }
    }
  }

  if (remove_edgelabels) {
    label_coords <- data.frame(
      text = character(),
      x = numeric(),
      y = numeric(),
      font = character(),
      size = numeric(),
      color = character(),
      fill = character(),
      angle = numeric(),
      alpha = numeric(),
      fontface = character(),
      math_expression = logical(),
      hjust = numeric(),
      vjust = numeric(),
      lavaan = logical(),
      network = logical(),
      locked = logical(),
      group_label = logical(),
      loop_label = logical(),
      group = character(),
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(data_file)) {
    if (data_file) {
      annotations <- rbind(annotations, label_coords, loop_label_coords)
    }
  }

  points_df[c("x", "y")] <- lapply(points_df[c("x", "y")], round, 5)

  line_cols <- c("x_start", "y_start", "x_end", "y_end",
                 "ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2")
  lines_df[line_cols] <- lapply(lines_df[line_cols], round, 5)

  annotations[c("x", "y")] <- lapply(annotations[c("x", "y")], round, 5)

  loops_df[c("x_center", "y_center")] <- lapply(loops_df[c("x_center", "y_center")], round, 5)

  list(points = points_df, lines = lines_df, annotations = annotations, loops = loops_df)
}


#' Generate network layout coordinates
#'
#' Computes node positions for network visualization using various layout algorithms
#' including force-directed layouts, dimensionality reduction methods, and specialized
#' layouts for bipartite networks. Handles multiple network object types and provides
#' fallback options for layout failures.
#'
#' @return A list containing:
#'   - `graph`: The processed graph object (igraph)
#'   - `layout`: Data frame with node coordinates:
#'     - `node`: Character, node identifier
#'     - `x`, `y`: Numeric, node coordinates
#'   - `is_bipartite`: Logical, whether the network is bipartite
#'
#' @details
#' This function provides a unified interface for network layout computation with
#' the following features:
#'
#'
#' @examples
#' \dontrun{
#' # Basic force-directed layout
#' edges <- data.frame(source = c("A", "B", "C"), target = c("B", "C", "A"))
#' nodes <- data.frame(node = c("A", "B", "C"))
#' layout <- generate_network_layout(NULL, edges, nodes, layout_method = "fr")
#'
#' # Dimensionality reduction layout
#' layout <- generate_network_layout(
#'   network_object = my_igraph,
#'   edges = NULL,
#'   nodes = NULL,
#'   layout_method = "dim_reduction",
#'   dim_reduction_method = "umap"
#' )
#'
#' # Circular layout with seed
#' layout <- generate_network_layout(
#'   NULL, edges, nodes,
#'   layout_method = "circle",
#'   random_seed = 123
#' )
#' }
#'
#' @importFrom igraph graph_from_data_frame layout_as_bipartite layout_with_fr
#'   layout_with_kk layout_in_circle layout_on_grid layout_randomly
#'   is_bipartite as_adjacency_matrix
#' @importFrom network network.size is.bipartite is.directed get.vertex.attribute
#' @importFrom network "%n%" "%v%<-" "%e%<-" "%eattr%<-"
#' @importFrom network "%nattr%<-" "%vattr%<-"
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#' @importFrom stats prcomp
#' @importFrom dplyr rename mutate
#' @keywords internal
#' @noRd
generate_network_layout <- function(network_object,
                                    edges,
                                    nodes,
                                    layout_method = "fr",
                                    directed = FALSE,
                                    dim_reduction_method = NULL,
                                    random_seed = NULL,
                                    flip_layout = FALSE,
                                    flip_direction = NULL,
                                    rotate_layout = FALSE,
                                    rotate_angle = 0) {


  if (!is.null(random_seed)) {
    set.seed(random_seed)  # Set the seed if provided
  }

  if (layout_method == 'dim_reduction') {
    use_dim_reduction <- TRUE
  } else use_dim_reduction <- FALSE

  if (!is.null(network_object)) {
    if (inherits(network_object, c("igraph"))) {
      is_bipartite <- igraph::is_bipartite(network_object)
    } else if (inherits(network_object, c("network"))) {
      is_bipartite <- network::is.bipartite(network_object)
    } else {
      is_bipartite <- FALSE
    }
  } else {
    is_bipartite <- FALSE
  }

  if (!is.null(network_object)) {
    if (inherits(network_object, c("igraph"))) {
      graph <- network_object
    } else if (inherits(network_object, c("network"))) {

      network_object %v% "type" <- seq_len(network.size(network_object)) > network_object %n% "bipartite"

      vertex_types <- network::get.vertex.attribute(network_object, "type")  # Must exist for bipartite networks

      if (is_bipartite) {
        graph <- graph_from_data_frame(
          d = edges,
          directed = network::is.directed(network_object),
          vertices = data.frame(
            name = nodes,
            type = vertex_types
          )
        )
      } else {
        graph <- graph_from_data_frame(
          d = edges,
          directed = network::is.directed(network_object),
          vertices = data.frame(
            name = nodes
          )
        )
      }
    }
  } else {
    graph <- graph_from_data_frame(d = edges, vertices = nodes, directed = directed)
  }

  if (is_bipartite == TRUE) {
    # Use bipartite layout if graph is bipartite
    layout <- igraph::layout_as_bipartite(graph) |>
      as.data.frame() |>
      dplyr::rename(x = V1, y = V2) |>
      dplyr::mutate(node = as.character(nodes$node))

    # Flip y-coordinates to make layout more intuitive
    layout$y <- -layout$y

  } else if (use_dim_reduction) {
    adjacency_matrix <- as.matrix(as_adjacency_matrix(graph))
    num_nodes <- nrow(adjacency_matrix)

    if (num_nodes < 3) {
      layout <- layout_with_fr(graph) |>
        as.data.frame() |>
        rename(x = V1, y = V2) |>
        mutate(node = as.character(nodes$node))
    } else {
      layout <- tryCatch({
        if (dim_reduction_method == "tsne") {
          tsne_perplexity <- max(5, min(30, num_nodes - 1))
          Rtsne::Rtsne(adjacency_matrix, perplexity = tsne_perplexity, verbose = FALSE)$Y |>
            as.data.frame() |>
            rename(x = V1, y = V2) |>
            mutate(node = as.character(nodes$node))
        } else if (dim_reduction_method == "umap") {
          umap_neighbors <- max(2, min(15, num_nodes - 1))  # Dynamically set neighbors
          umap::umap(adjacency_matrix, n_neighbors = umap_neighbors)$layout |>
            as.data.frame() |>
            rename(x = V1, y = V2) |>
            mutate(node = as.character(nodes$node))

        } else if (dim_reduction_method == "pca") {
          prcomp(adjacency_matrix, center = TRUE, scale. = TRUE)$x[, 1:2] |>
            as.data.frame() |>
            rename(x = PC1, y = PC2) |>
            mutate(node = as.character(nodes$node))
        } else {
          stop("Invalid dimensionality reduction method selected.")
        }
      }, error = function(e) {
        layout_with_fr(graph) |>  # Fallback to Fruchterman-Reingold
          as.data.frame() |>
          rename(x = V1, y = V2) |>
          mutate(node = as.character(nodes$node))
      })
    }
  } else { # other layout methods
    layout <- switch(layout_method,
                     "fr" = layout_with_fr(graph), # Fruchterman-Reingold
                     "kk" = layout_with_kk(graph), # Kamada-Kawai
                     "circle" = layout_in_circle(graph), # Circular layout
                     "grid" = layout_on_grid(graph), # Grid layout
                     "random" = layout_randomly(graph), # Random layout
                     stop("Invalid layout method specified!")
    ) |>
      as.data.frame() |>
      dplyr::rename(x = V1, y = V2) |>
      dplyr::mutate(node = as.character(nodes$node))
  }

  if (flip_layout) {
    flipped <- flip_around_center(layout, flip_direction)
    layout$x <- flipped$x
    layout$y <- flipped$y
  }

  if (rotate_layout) {
    rotated <- rotate_around_center(layout, rotate_angle)
    layout$x <- rotated$x
    layout$y <- rotated$y
  }

  return(list(graph = graph,
              layout = layout,
              is_bipartite = is_bipartite))
}



#' Generate graph data from pre-computed network layout
#'
#' Creates standardized graph data frames from pre-computed network layouts,
#' providing extensive customization options for visualization including clustering,
#' bezier edges, bipartite network support, and comprehensive modification parameters.
#' This function is designed to work with output from generate_network_layout().
#'
#'
#' @return A list containing three data frames:
#'   - `points`: Node data with coordinates, clustering colors, shapes, and properties
#'   - `lines`: Edge data with coordinates, bezier control points, arrow information, and styling
#'   - `annotations`: Text labels for nodes and edges with positioning and styling
#'
#'
#' @importFrom igraph V E vcount vertex_attr is_connected cluster_louvain
#' @importFrom igraph cluster_leiden cluster_walktrap cluster_fast_greedy
#' @importFrom igraph E<-
#' @importFrom igraph cluster_spinglass cluster_edge_betweenness components membership
#' @importFrom RColorBrewer brewer.pal
#' @importFrom smplot2 sm_palette
#' @importFrom stats setNames
#' @keywords internal
#' @noRd
generate_graph_from_network1 <- function(graph,
                                         layout,
                                         edges,
                                         nodes,
                                         is_bipartite,
                                         directed = TRUE,
                                         layout_width = 30,
                                         layout_height = 30, x_center = 0, y_center = 0,
                                         node_shape = "circle",
                                         node_size = 10,
                                         node_alpha = 1,
                                         node_fill_color = "#1262b3",
                                         node_border_color = "#0f993d", node_border_width = 1,
                                         node_width_height_ratio = 1,
                                         line_width = 1, line_color = "#000000",
                                         line_alpha = 1,
                                         min_edge_width = 0.5, max_edge_width = 3, scale_by_weight = FALSE,
                                         line_endpoint_spacing = 0,
                                         arrow_type = "closed",
                                         arrow_size = 0.1,
                                         node_label_font = "sans", node_label_size = 10,
                                         node_label_color = "#000000",
                                         node_label_alpha = 1, node_label_fontface = "plain",
                                         edge_label_font = "sans", edge_label_size = 10,
                                         edge_label_color = "#000000",
                                         edge_label_fill = "#FFFFFF",
                                         edge_label_alpha = 1, edge_label_fontface = "plain",
                                         zoom_factor = 1.2,
                                         annotate_nodes = TRUE,
                                         annotate_edges = TRUE,
                                         remove_edgelabels = FALSE,
                                         # random_seed = NULL,
                                         use_clustering = FALSE,
                                         clustering_method = "louvain",
                                         cluster_palette = 'rainbow',
                                         bezier_network_edges = FALSE,
                                         network_edges_curvature_magnitude = 0.5,
                                         network_edges_rotate_curvature = FALSE,
                                         network_edges_curvature_asymmetry = 0,
                                         modify_params_edge = FALSE,
                                         modified_edges = NULL,
                                         modify_params_edgelabel = FALSE,
                                         modified_edgelabels = NULL,
                                         modify_params_edgelabel_xy = FALSE,
                                         modified_edgelabels_xy = NULL,
                                         modify_params_node = FALSE,
                                         modified_nodes = NULL,
                                         modify_params_node_xy = FALSE,
                                         modified_nodes_xy = NULL,
                                         modify_params_nodelabel = FALSE,
                                         modified_nodelabels = NULL,
                                         modify_params_nodelabel_xy = FALSE,
                                         modified_nodelabels_xy = NULL,
                                         modify_params_nodelabel_text = FALSE,
                                         modified_nodelabels_text = NULL,
                                         modify_params_bezier_edge = FALSE,
                                         modified_bezier_edges = NULL,
                                         modify_params_edge_xy = FALSE,
                                         modified_edges_xy = NULL,
                                         modify_params_edgelabel_text = FALSE,
                                         modified_edgelabels_text = NULL,
                                         change_bipartite_group = FALSE,
                                         apply_bipartite_nodes = FALSE,
                                         apply_bipartite_edges = FALSE,
                                         apply_bipartite_annotations = FALSE,
                                         last_state = NULL,
                                         which_group = "1") {

  apply_modifications <- function(data, modifications, config, mode) {
    if (is.null(modifications) || nrow(modifications) == 0) return(data)
    modified_data <- data

    for (i in seq_len(nrow(modifications))) {
      mod <- modifications[i, ]

      if (mode == 'edge') {
        idx <- which(
          edges$source == mod$lhs &
            edges$target == mod$rhs
        )
      } else if (mode == 'node') {
        idx <- which(
          layout$node == mod$text
        )
      }

      if (length(idx) == 1) {
        for (col in config$modify_cols) {
          if (col %in% names(mod) && col %in% names(modified_data)) {
            modified_data[idx, col] <- mod[[col]]
          }
        }

        if (!is.null(config$special_case)) {
          modified_data <- config$special_case(modified_data, idx, mod)
        }
      }
    }
    return(modified_data)
  }

  edges$source <- as.character(edges$source)
  edges$target <- as.character(edges$target)


  edge_list <- as.data.frame(edges[, c("source", "target")])


  if (use_clustering) {
    num_clusters <- NULL

    edge_weights <- E(graph)$weight

    if (!is.null(edge_weights) && any(edge_weights < 0, na.rm = TRUE)) {
      n_negative <- sum(edge_weights < 0, na.rm = TRUE)

      E(graph)$weight <- abs(edge_weights)
    }

    if (!is.null(edge_weights) && any(edge_weights == 0, na.rm = TRUE)) {
      # Add small epsilon to avoid division by zero issues
      zero_weights <- which(edge_weights == 0)
      E(graph)$weight[zero_weights] <- 0.0001
    }

    if (!is_connected(graph)) {
    }

    communities <- tryCatch({
      switch(
        clustering_method,
        "louvain" = cluster_louvain(graph, weights = E(graph)$weight),
        "leiden" = cluster_leiden(graph, weights = E(graph)$weight),
        "walktrap" = cluster_walktrap(graph, weights = E(graph)$weight),
        "fast_greedy" = cluster_fast_greedy(graph, weights = E(graph)$weight)
      )
    }, error = function(e) {

      tryCatch({
        cluster_spinglass(graph, weights = if(!is.null(E(graph)$weight)) abs(E(graph)$weight))
      }, error = function(e2) {
        tryCatch({
          cluster_edge_betweenness(graph, weights = if(!is.null(E(graph)$weight)) abs(E(graph)$weight))
        }, error = function(e3) {
          components(graph)$membership
        })
      })
    })

    if (!is.null(communities)) {
      if (inherits(communities, "communities")) {
        nodes$community <- membership(communities)
        num_clusters <- max(nodes$community, na.rm = TRUE)
      } else if (is.numeric(communities) || is.integer(communities)) {
        nodes$community <- communities
        num_clusters <- max(communities, na.rm = TRUE)
      } else {
        nodes$community <- rep(1, vcount(graph))
        num_clusters <- 1
      }
    } else {
      nodes$community <- rep(1, vcount(graph)) # Default to one cluster if clustering fails
      num_clusters <- 1
    }

    if (is.na(num_clusters) || num_clusters <= 0) {
      num_clusters <- 1
      nodes$community <- rep(1, length(nodes$community))
    }

    if (!is.integer(nodes$community)) {
      nodes$community <- as.integer(nodes$community)
    }

    palette_max_colors <- list(
      rainbow = Inf,  # Unlimited
      Set3 = 12,
      Paired = 12,
      Dark2 = 8,
      Accent = 8,
      Pastel1 = 9,
      Pastel2 = 8,
      Spectral = 11,
      YlGnBu = 9,
      RdYlBu = 11,
      smplot2 = 20
    )

    palette_function <- switch(
      cluster_palette,
      "rainbow" = function(n) rainbow(n),
      "Set3" = function(n) RColorBrewer::brewer.pal(min(n, 12), "Set3"),
      "Paired" = function(n) RColorBrewer::brewer.pal(min(n, 12), "Paired"),
      "Dark2" = function(n) RColorBrewer::brewer.pal(min(n, 8), "Dark2"),
      "Accent" = function(n) RColorBrewer::brewer.pal(min(n, 8), "Accent"),
      "Pastel1" = function(n) RColorBrewer::brewer.pal(min(n, 9), "Pastel1"),
      "Pastel2" = function(n) RColorBrewer::brewer.pal(min(n, 8), "Pastel2"),
      "Spectral" = function(n) RColorBrewer::brewer.pal(min(n, 11), "Spectral"),
      "YlGnBu" = function(n) RColorBrewer::brewer.pal(min(n, 9), "YlGnBu"),
      "RdYlBu" = function(n) RColorBrewer::brewer.pal(min(n, 11), "RdYlBu"),
      "smplot2" = function(n) head(smplot2::sm_palette(), n)
    )

    max_colors <- palette_max_colors[[cluster_palette]]

    if (cluster_palette != "rainbow" && num_clusters > max_colors) {
      palette_function <- rainbow
    }

    valid_communities <- nodes$community
    if (any(is.na(valid_communities))) {
      valid_communities[is.na(valid_communities)] <- 1
    }
    if (any(valid_communities <= 0)) {
      valid_communities[valid_communities <= 0] <- 1
    }
    if (any(valid_communities > num_clusters)) {
      valid_communities[valid_communities > num_clusters] <- num_clusters
    }

    if (num_clusters > 0 && all(valid_communities >= 1 & valid_communities <= num_clusters)) {
      node_colors <- palette_function(num_clusters)[valid_communities]
    } else {
      node_colors <- rep(node_fill_color, vcount(graph))
    }

  } else {
    node_colors <- node_fill_color
  }

  layout_min_x <- min(layout$x)
  layout_min_y <- min(layout$y)
  layout_max_x <- max(layout$x)
  layout_max_y <- max(layout$y)

  layout <- layout |>
    mutate(
      x = (x - layout_min_x) / (layout_max_x - layout_min_x) * layout_width + x_center - layout_width / 2,
      y = (y - layout_min_y) / (layout_max_y - layout_min_y) * layout_height + y_center - layout_height / 2
    )



  # Apply node position modifications
  if (modify_params_node_xy) {
    layout <- apply_modifications(
      layout,
      modified_nodes_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0), # No direct column mods
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )

  }

  if (is_bipartite) {
    group_selector <- if (apply_bipartite_nodes) !vertex_attr(graph)$type else vertex_attr(graph)$type
  } else {
    group_selector <- NULL
  }


  points <- layout |>
    dplyr::left_join(nodes, by = c("node" = "node")) |>
    mutate(
      shape = node_shape,
      color = node_colors,
      size =  node_size,
      border_color = node_border_color,
      border_width = node_border_width,
      alpha = node_alpha,
      width_height_ratio = node_width_height_ratio,
      orientation = 0,
      lavaan = FALSE,
      network = TRUE,
      locked = FALSE,
      group = which_group
    ) |>
    select(
      x, y, shape, color, size, border_color, border_width, alpha, width_height_ratio, orientation,
      lavaan, network, locked, group
    )

  if (!is.null(group_selector)) {
    network_points <- which(last_state$points$network == TRUE)
    network_points <- network_points[group_selector]
    last_state$points[, c('x', 'y')] <- points[, c('x', 'y')]
    last_state$points[network_points, ] <- points[group_selector,]
    points <- last_state$points
  }

  if (modify_params_node) {
    points <- apply_modifications(
      points,
      modified_nodes,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "alpha", "shape", "size", "border_color", "border_width", "width_height_ratio"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }


  if (inherits(edges$weight, 'character')) {
    scale_by_weight <- FALSE
  }

  if ("weight" %in% colnames(edges) && scale_by_weight) {
    edges <- edges |>
      mutate(
        scaled_width = rescale_values(weight, to = c(min_edge_width, max_edge_width))
      )
  } else {
    edges <- edges |>
      mutate(scaled_width = line_width)
  }

  # Prepare lines data frame

  lines <- edges |>
    dplyr::left_join(layout, by = c("source" = "node")) |>
    dplyr::rename(x_start = x, y_start = y) |>
    dplyr::left_join(layout, by = c("target" = "node")) |>
    dplyr::rename(x_end = x, y_end = y) |>
    dplyr::mutate(
      ctrl_x = NA,
      ctrl_y = NA,
      ctrl_x2 = NA,
      ctrl_y2 = NA,
      curvature_magnitude = NA,
      rotate_curvature = NA,
      curvature_asymmetry = NA,
      type = ifelse(bezier_network_edges == FALSE, ifelse(directed, "Straight Arrow", "Straight Line"), ifelse(directed, "Curved Arrow", "Curved Line")),
      color = line_color,
      end_color = NA,
      color_type = "Single",
      gradient_position = NA,
      width = scaled_width,
      alpha = line_alpha,
      arrow = ifelse(directed, directed, NA),
      arrow_type = ifelse(directed, arrow_type, NA),
      arrow_size = ifelse(directed, arrow_size, NA),
      two_way = ifelse(two_way, TRUE, FALSE),
      lavaan = FALSE,
      network = TRUE,
      line_style = "solid",
      locked = FALSE,
      group = which_group
    ) |>
    dplyr::select(
      x_start, y_start, x_end, y_end, ctrl_x, ctrl_y, ctrl_x2, ctrl_y2,
      curvature_magnitude, rotate_curvature, curvature_asymmetry, type, color, end_color,
      color_type, gradient_position, width, alpha, arrow, arrow_type,
      arrow_size, two_way, lavaan, network, line_style, locked, group
    )

  node_mapping <- stats::setNames(seq_along(nodes$node), nodes$node)
  numeric_edge_list <- matrix(
    c(node_mapping[edges$source], node_mapping[edges$target]),
    ncol = 2
  )

  if (modify_params_edge) {
    if (nrow(modified_edges) > 0) {
      for (i in seq_len(nrow(modified_edges))) {
        mod <- modified_edges[i, ]

        edge_idx <- which(
          edges$source == mod$lhs &
            edges$target == mod$rhs
        )

        if (length(edge_idx) == 1) {
          lines$color[[edge_idx]] <- mod$color
          lines$width[[edge_idx]] <- mod$width
          lines$alpha[[edge_idx]] <- mod$alpha
          lines$line_style[[edge_idx]] <- mod$line_style
          lines$end_color[[edge_idx]] <- mod$end_color
          lines$gradient_position[[edge_idx]] <- mod$gradient_position
          lines$color_type[[edge_idx]] <- mod$color_type
        }
      }
    }
  }

  lines <- adjust_edge_coordinates(
    lines_df = lines,
    edge_list = numeric_edge_list,
    points_df = points,
    auto_endpoint_spacing = line_endpoint_spacing
  )


  if (bezier_network_edges == TRUE) {
    bezier_indices <- which(lines$type %in% c('Curved Arrow', 'Curved Line'))

    control_points <- mapply(
      calculate_control_point,
      x_start = lines$x_start[bezier_indices],
      y_start = lines$y_start[bezier_indices],
      x_end = lines$x_end[bezier_indices],
      y_end = lines$y_end[bezier_indices],
      curvature_magnitude = network_edges_curvature_magnitude,
      rotate_curvature = network_edges_rotate_curvature,
      curvature_asymmetry = network_edges_curvature_asymmetry,
      SIMPLIFY = FALSE
    )

    # Assign the calculated control points to lines
    lines$ctrl_x[bezier_indices] <- sapply(control_points, `[[`, "ctrl_x")
    lines$ctrl_y[bezier_indices] <- sapply(control_points, `[[`, "ctrl_y")
    lines$ctrl_x2[bezier_indices] <- sapply(control_points, `[[`, "ctrl_x2")
    lines$ctrl_y2[bezier_indices] <- sapply(control_points, `[[`, "ctrl_y2")

    lines$curvature_magnitude[bezier_indices] <- network_edges_curvature_magnitude
    lines$rotate_curvature[bezier_indices] <- network_edges_rotate_curvature
    lines$curvature_asymmetry[bezier_indices] <- network_edges_curvature_asymmetry
    lines$locked[bezier_indices] <- FALSE
  }



  if (modify_params_bezier_edge) {
    lines <- apply_modifications(
      lines,
      modified_bezier_edges,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = NULL,
        special_case = function(data, idx, mod) {
          if (length(idx) == 1 &&
              all(c("x_start", "y_start", "x_end", "y_end") %in% names(data)) &&
              all(c("curvature_magnitude", "rotate_curvature", "curvature_asymmetry", "x_shift", "y_shift") %in% names(mod))) {

            data$x_start[idx] <- data$x_start[idx] + mod$x_shift
            data$x_end[idx] <- data$x_end[idx] + mod$x_shift
            data$y_start[idx] <- data$y_start[idx] + mod$y_shift
            data$y_end[idx] <- data$y_end[idx] + mod$y_shift

            cp <- calculate_control_point(
              x_start = data$x_start[idx],
              y_start = data$y_start[idx],
              x_end = data$x_end[idx],
              y_end = data$y_end[idx],
              curvature_magnitude = mod$curvature_magnitude,
              rotate_curvature = mod$rotate_curvature,
              curvature_asymmetry = mod$curvature_asymmetry
            )

            # Safely assign control points
            if (all(c("ctrl_x", "ctrl_y", "locked") %in% names(data))) {
              data$ctrl_x[idx] <- cp$ctrl_x
              data$ctrl_y[idx] <- cp$ctrl_y
              data$ctrl_x2[idx] <- cp$ctrl_x2
              data$ctrl_y2[idx] <- cp$ctrl_y2
              data$curvature_magnitude[idx] <- mod$curvature_magnitude
              data$rotate_curvature[idx] <- mod$rotate_curvature
              data$curvature_asymmetry[idx] <- mod$curvature_asymmetry
              data$locked[idx] <- FALSE
              data$type[idx] <- ifelse (directed, 'Curved Arrow', 'Curved Line')
            }
          }
          return(data)
        }
      ),
      mode = 'edge'
    )
  }

  lines$type[lines$curvature_magnitude == 0] <- "Straight Arrow"
  lines$type[lines$curvature_magnitude != 0] <- "Curved Arrow"

  if (modify_params_edge_xy) {
    lines <- apply_modifications(
      lines,
      modified_edges_xy,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x_start[idx] <- mod$start_x_shift # data$x_start[idx] + mod$start_x_shift
          data$y_start[idx] <- mod$start_y_shift # data$y_start[idx] + mod$start_y_shift
          data$x_end[idx] <- mod$end_x_shift # data$x_end[idx] + mod$end_x_shift
          data$y_end[idx] <- mod$end_y_shift # data$y_end[idx] + mod$end_y_shift

          if (data$type[idx] %in% c('Curved Line', 'Curved Arrow')) {
            cp <- calculate_control_point(
              x_start = data$x_start[idx],
              y_start = data$y_start[idx],
              x_end = data$x_end[idx],
              y_end = data$y_end[idx],
              curvature_magnitude = data$curvature_magnitude[idx],
              rotate_curvature = data$rotate_curvature[idx],
              curvature_asymmetry = data$curvature_asymmetry[idx]
            )

            data$ctrl_x[idx] <- cp$ctrl_x
            data$ctrl_y[idx] <- cp$ctrl_y
            data$ctrl_x2[idx] <- cp$ctrl_x2
            data$ctrl_y2[idx] <- cp$ctrl_y2
          }

          return(data)
        }
      ),
      mode = 'edge'
    )
  }


  edgelabels_xy_df <- data.frame(x = rep(NA_real_, nrow(lines)),
                                 y = rep(NA_real_, nrow(lines)))

  for (i in 1:nrow(lines)) {

    intp_points <-
      if (lines$type[i] == "Curved Arrow" || lines$type[i] == "Curved Line") {
        create_bezier_curve(
          x_start = lines$x_start[i], y_start = lines$y_start[i],
          x_end = lines$x_end[i], y_end = lines$y_end[i],
          ctrl_x = lines$ctrl_x[i], ctrl_y = lines$ctrl_y[i],
          ctrl_x2 = lines$ctrl_x2[i], ctrl_y2 = lines$ctrl_y2[i], n_points = 100
        )
      } else if (lines$type[i] == "Straight Arrow" || lines$type[i] == "Straight Line") {
        interpolate_points(
          x_start = lines$x_start[i], y_start = lines$y_start[i],
          x_end = lines$x_end[i], y_end = lines$y_end[i], n = 100
        )
      }

    mid_index <- 50
    edgelabels_xy_df$x[i] <- intp_points$x[mid_index]
    edgelabels_xy_df$y[i] <- intp_points$y[mid_index]
  }

  edges$weight <- round(edges$weight, 3) # round

  weight_annotations <- if (remove_edgelabels == FALSE) {
    if (!all(is.na(edges$weight))) {
      lines |>
        mutate(weight = edges$weight) |>
        mutate(
          text = as.character(edges$weight),
          x = edgelabels_xy_df$x,
          y = edgelabels_xy_df$y,
          font = edge_label_font,
          size = edge_label_size,
          color = edge_label_color,
          fill = ifelse(as.character(edges$weight) == "", NA, edge_label_fill),
          angle = 0,
          alpha = edge_label_alpha,
          fontface = edge_label_fontface,
          math_expression = FALSE,
          hjust = 0.5,
          vjust = 0.5,
          lavaan = FALSE,
          network = TRUE,
          locked = FALSE,
          group_label = FALSE,
          loop_label = FALSE,
          group = which_group,
        ) |>
        select(text, x, y, font, size, color, fill, angle, alpha, fontface, math_expression, hjust, vjust, lavaan, network, locked, group_label, loop_label, group)
    } else {
      data.frame(
        text = character(), x = numeric(), y = numeric(), font = character(), size = numeric(),
        color = character(), fill = character(), angle = numeric(), alpha = numeric(), fontface = character(),
        math_expression = logical(), hjust = numeric(), vjust = numeric(), lavaan = logical(), network = logical(), locked = logical(),
        group_label = logical(), loop_label = logical(), group = character(), stringsAsFactors = FALSE
      )
    }
  } else {
    data.frame(
      text = character(), x = numeric(), y = numeric(), font = character(), size = numeric(),
      color = character(), fill = character(), angle = numeric(), alpha = numeric(), fontface = character(),
      math_expression = logical(), hjust = numeric(), vjust = numeric(), lavaan = logical(), network = logical(), locked = logical(),
      group_label = logical(), loop_label = logical(), group = character(), stringsAsFactors = FALSE
    )
  }


  if (modify_params_edgelabel) {
    weight_annotations <- apply_modifications(
      weight_annotations,
      modified_edgelabels,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = c("color", "fill", "size", "alpha", "angle", "font", "fontface")
      ),
      mode = 'edge'
    )
  }

  if (modify_params_edgelabel_xy) {
    weight_annotations <- apply_modifications(
      weight_annotations,
      modified_edgelabels_xy,
      config = list(
        match_cols = c(from = "lhs", to = "rhs"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'edge'
    )
  }

  if (is_bipartite) {
    group_selector <- if (apply_bipartite_annotations) !vertex_attr(graph)$type else vertex_attr(graph)$type
  } else {
    group_selector <- NULL
  }

  if (modify_params_edgelabel_text) {
    if (nrow(modified_edgelabels_text) > 0) {
      for (i in seq_len(nrow(modified_edgelabels_text))) {
        mod <- modified_edgelabels_text[i, ]
        edge_idx <- which(
          edges$source == mod$lhs &
            edges$target == mod$rhs
        )
        if (length(edge_idx) == 1) {
          weight_annotations$text[[edge_idx]] <- mod$text
          weight_annotations$math_expression[[edge_idx]] <- mod$math_expression
        }
      }
    }
  }

  annotations <- points |>
    mutate(
      text = if ("label" %in% colnames(nodes)) nodes$label else if ("node" %in% colnames(layout)) layout$node else NA, # Use node name as default text
      font = node_label_font,
      size = node_label_size,
      color = node_label_color,
      fill = NA,
      angle = 0,
      alpha = node_label_alpha,
      fontface = node_label_fontface,
      math_expression = FALSE,
      hjust = 0.5,
      vjust = 0.5,
      lavaan = FALSE,
      network = TRUE,
      locked = FALSE,
      group_label = FALSE,
      loop_label = FALSE,
      group = which_group,
    ) |> select(text, x, y, font, size, color, fill, angle, alpha, fontface, math_expression, hjust, vjust, lavaan, network, locked, group_label, loop_label, group) |>
    bind_rows(weight_annotations)


  if (!is.null(group_selector)) {
    network_annotations <- which(last_state$annotations$network == TRUE)
    network_annotations <- network_annotations[group_selector]
    last_state$annotations[, c('x', 'y')] <- annotations[, c('x', 'y')]
    last_state$annotations[network_annotations, ] <- annotations[group_selector,]
    annotations <- last_state$annotations
  }


  if (modify_params_nodelabel_xy) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels_xy,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = character(0),
        special_case = function(data, idx, mod) {
          data$x[idx] <- data$x[idx] + mod$x_shift
          data$y[idx] <- data$y[idx] + mod$y_shift
          return(data)
        }
      ),
      mode = 'node'
    )
  }

  if (modify_params_nodelabel_text) {
    if (nrow(modified_nodelabels_text) > 0) {
      for (i in seq_len(nrow(modified_nodelabels_text))) {
        mod <- modified_nodelabels_text[i, ]
        node_idx <- which(
          layout$node == mod$text
        )
        if (length(node_idx) == 1) {
          annotations$text[[node_idx]] <- mod$nodelabel
          annotations$math_expression[[node_idx]] <- mod$math_expression
        }
      }
    }
  }

  if (modify_params_nodelabel) {
    annotations <- apply_modifications(
      annotations,
      modified_nodelabels,
      config = list(
        match_cols = c(name = "text"),
        modify_cols = c("color", "size", "alpha", "angle", "font", "fontface"),
        special_case = NULL
      ),
      mode = 'node'
    )
  }

  points[c("x", "y")] <- lapply(points[c("x", "y")], round, 5)

  line_cols <- c("x_start", "y_start", "x_end", "y_end",
                 "ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2")
  lines[line_cols] <- lapply(lines[line_cols], round, 5)

  annotations[c("x", "y")] <- lapply(annotations[c("x", "y")], round, 5)


  list(points = as.data.frame(points), lines = as.data.frame(lines),
       annotations = as.data.frame(annotations))
}

#' Generate SEM node coordinates from model fit objects
#'
#' Extracts and processes node coordinates from SEM model objects, with options
#' for layout flipping and coordinate normalization.
#'
#' @param fit Model fit object of class 'sem_graph' (from tidySEM) or 'qgraph' (from semPaths)
#' @param relative_x_position Scaling factor for x-axis coordinate normalization
#' @param relative_y_position Scaling factor for y-axis coordinate normalization
#' @param center_x X-coordinate for graph center positioning
#' @param center_y Y-coordinate for graph center positioning
#' @param flip_layout Logical indicating whether to flip the layout
#' @param flip_direction Direction of flip: "horizontal", "vertical", or "both"
#' @param rotate_layout Logical indicating whether to rotate the layout
#' @param rotate_angle Double: angle of layout rotation
#'
#' @return A data frame with processed node coordinates containing columns:
#'   \item{x, y}{Normalized coordinates}
#'   \item{name/text}{Node labels (name for sem_graph, text for qgraph)}
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' # For tidySEM objects
#' node_coords <- generate_sem_node_coords(sem_graph_obj,
#'                                        center_x = 50, center_y = 50,
#'                                        flip_layout = TRUE,
#'                                        flip_direction = "horizontal")
#'
#' # For qgraph objects
#' node_coords <- generate_sem_node_coords(qgraph_obj,
#'                                        relative_x_position = 30,
#'                                        relative_y_position = 30)
#' }
generate_sem_node_coords <- function(fit, relative_x_position = 25, relative_y_position = 25,
                                     center_x = 0, center_y = 0, flip_layout = FALSE,
                                     flip_direction = NULL, rotate_layout = FALSE, rotate_angle = 0) {

  if (is.null(flip_layout)) flip_layout <- FALSE
  if (is.null(rotate_layout)) rotate_layout <- FALSE

  if (inherits(fit, "sem_graph")) {
    node_names <- fit$nodes$name
    node_coords <- as.data.frame(fit$nodes[,c('x','y','name')])

    if (flip_layout) {
      flipped <- flip_around_center(node_coords, flip_direction)
      node_coords$x <- flipped$x
      node_coords$y <- flipped$y
    }

    if (rotate_layout) {
      rotated <- rotate_around_center(node_coords, rotate_angle)
      node_coords$x <- rotated$x
      node_coords$y <- rotated$y
    }

    # Normalize coordinates to center the graph
    node_coords$x <- (node_coords$x - mean(range(node_coords$x))) * relative_x_position + center_x
    node_coords$y <- (node_coords$y - mean(range(node_coords$y))) * relative_y_position + center_y
    node_coords$name <- node_names

  } else if (inherits(fit, "qgraph")) {
    node_coords <- as.data.frame(fit$layout)
    node_names <- names(fit$graphAttributes$Nodes$labels)
    if (is.null(node_names)) node_names <- fit$graphAttributes$Nodes$labels
    colnames(node_coords) <- c("x", "y")

    if (flip_layout) {
      flipped <- flip_around_center(node_coords, flip_direction)
      node_coords$x <- flipped$x
      node_coords$y <- flipped$y
    }

    if (rotate_layout) {
      rotated <- rotate_around_center(node_coords, rotate_angle)
      node_coords$x <- rotated$x
      node_coords$y <- rotated$y
    }

    node_coords$x <- (node_coords$x - mean(range(node_coords$x))) * relative_x_position + center_x
    node_coords$y <- (node_coords$y - mean(range(node_coords$y))) * relative_y_position + center_y
    node_coords$text <- node_names # name is created later

  } else {
    stop("Must be output from tidySEM with class of 'sem_graph' or semPaths with class of 'qgraph'.")
  }

  return(node_coords)
}

#' Rotate coordinates around center point
#'
#' @param df Data frame with 'x' and 'y' coordinate columns
#' @param rotation_angle Rotation angle in degrees
#'
#' @return List with rotated 'x' and 'y' coordinates
#'
#' @keywords internal
#' @noRd
rotate_around_center <- function(df, rotation_angle) {
  center_x <- mean(df$x)
  center_y <- mean(df$y)
  x <- df$x
  y <- df$y
  angle_rad <- rotation_angle * pi / 180

  x_relative <- x - center_x
  y_relative <- y - center_y

  new_x <- x_relative * cos(angle_rad) - y_relative * sin(angle_rad)
  new_y <- x_relative * sin(angle_rad) + y_relative * cos(angle_rad)

  x <- new_x + center_x
  y <- new_y + center_y

  return(list(x = x, y = y))
}


#' Scale values
#'
#'
#' @return Numeric vector
#'
#' @keywords internal
#' @noRd
rescale_values <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
  (x - from[1]) / diff(from) * diff(to) + to[1]
}
