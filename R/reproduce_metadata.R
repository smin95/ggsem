#' Reproduce a network visualization from metadata
#'
#' This function recreates a network visualization using stored metadata from
#' a Shiny app session. It can handle various network object types including
#' igraph, qgraph, ggnet2 objects, and adjacency matrices.
#'
#' @param metadata Either a metadata object or a file path to an RDS file
#'   containing metadata captured from the Shiny app. If NULL, the function
#'   will stop with an error.
#' @param group_id The identifier for the specific network group to reproduce.
#'   This corresponds to the group ID used in the original Shiny app.
#' @param object Optional network object to use instead of the one stored
#'   in metadata. Useful for testing with different network objects.
#' @param zoom_level Zoom factor for the visualization. Default is 1.2.
#'
#' @return A ggplot object containing the reproduced network visualization.
#'
#'
#' @examples
#' \dontrun{
#' # Reproduce network from saved metadata
#' network_plot <- reproduce_network(
#'   metadata = "path/to/metadata.rds",
#'   group_id = "network1"
#' )
#'
#' # Reproduce with custom data
#' network_plot <- reproduce_network(
#'   metadata = metadata_object,
#'   data = "path/to/edges.csv",
#'   group_id = "network1"
#' )
#' }
#' @importFrom utils read.csv
#' @importFrom rlang .data
#' @importFrom dplyr mutate rename
#' @importFrom igraph is_bipartite
#' @importFrom network is.bipartite
#' @export
reproduce_network <- function(metadata = NULL, group_id = NULL, object = NULL, zoom_level = 1.2) {
  if (is.character(metadata) && file.exists(metadata)) {
    metadata <- readRDS(metadata)
  }

  if (is.null(metadata)) {
    stop("No metadata provided. Either provide a metadata file or capture one from the Shiny app.")
  }

  data <-  metadata$network_groups[[group_id]]$data

  if (is.character(data) && file.exists(data)) {
    network_df <- read.csv(data)
    if ("source" %in% colnames(network_df) && "target" %in% colnames(network_df)) {
      # If it's an edge list
      edges <- network_df
    } else if (is.matrix(network_df) || is.data.frame(network_df)) {
      network_df <- read.csv(data, row.names = 1)
      # If it's an adjacency matrix, convert to edge list
      if (is.null(colnames(network_df)) || is.null(rownames(network_df))) {
        stop("Adjacency matrix must have row and column names representing node identifiers.")
      }

      edges <- which(network_df != 0, arr.ind = TRUE) |>
        as.data.frame() |>
        dplyr::rename(source = row, target = col) |>
        dplyr::mutate(
          source = rownames(network_df)[source],
          target = colnames(network_df)[target],
          weight = as.vector(network_df[which(network_df != 0, arr.ind = TRUE)]) # Include weights if matrix is weighted
        )
      if (all(edges$weight == 1)) {
        network_state$weights <- NULL # annotate_edges = FALSE
      }

    } else {
      stop("Invalid input: The input must be either an edge list or adjacency matrix.")
    }
  } else if (is.null(data)) {
    network_df <- metadata$network_groups[[group_id]]$data
  }

  group_storage <- metadata$network_groups[[group_id]]
  modifications_network <- metadata$modifications$network[[group_id]]

  if (is.null(object)) {
    bundleObject <- group_storage$bundleObject
  } else {
    bundleObject <- object
  }

  layout_method <- group_storage$last_layout_method

  is_bipartite <- if (inherits(bundleObject, "igraph")) {
    igraph::is_bipartite(bundleObject)
  } else if (inherits(bundleObject, "network")) {
    network::is.bipartite(bundleObject)
  } else {
    FALSE
  }


  if (inherits(bundleObject, c("qgraph"))) {
    network_graph <- generate_graph_from_qgraph1(
      network_object = bundleObject,
      directed = group_storage$last_is_directed %||% FALSE,
      layout_width = group_storage$last_layout_x %||% 30,
      layout_height = group_storage$last_layout_y %||% 30,
      x_center = group_storage$last_x_center %||% 0,
      y_center = group_storage$last_y_center %||% 0,
      node_shape = group_storage$last_node_shape %||% "circle",
      node_size = group_storage$last_node_size %||% 10,
      node_alpha = group_storage$last_node_alpha %||% 1,
      node_fill_color = group_storage$last_node_fill_color %||% "#1262b3",
      node_border_color = group_storage$last_node_border_color %||% "#0f993d",
      node_border_width = group_storage$last_node_border_width %||% 1,
      node_width_height_ratio = group_storage$last_node_width_height_ratio %||% 1,
      line_width = group_storage$last_line_width %||% 1,
      line_color = group_storage$last_line_color %||% "#000000",
      line_alpha = group_storage$last_line_alpha %||% 1,
      min_edge_width = ifelse(group_storage$last_scale_edge_width, group_storage$last_min_edge_width %||% 0.5, NULL),
      max_edge_width = ifelse(group_storage$last_scale_edge_width, group_storage$last_max_edge_width %||% 3, NULL),
      scale_by_weight = group_storage$last_scale_edge_width %||% FALSE,
      line_endpoint_spacing = group_storage$last_line_endpoint_spacing %||% 0,
      arrow_type = group_storage$last_arrow_type %||% "closed",
      arrow_size = group_storage$last_arrow_size %||% 0.1,
      node_label_font = group_storage$last_node_label_font %||% "sans",
      node_label_size = group_storage$last_node_label_size %||% 15,
      node_label_color = group_storage$last_node_label_color %||% "#000000",
      node_label_alpha = group_storage$last_node_label_alpha %||% 1,
      node_label_fontface = group_storage$last_node_label_fontface %||% "plain",
      edge_label_font = group_storage$last_edge_label_font %||% "sans",
      edge_label_size = group_storage$last_edge_label_size %||% 15,
      edge_label_color = group_storage$last_edge_label_color %||% "#000000",
      edge_label_fill = group_storage$last_edge_label_fill %||% "#FFFFFF",
      edge_label_alpha = group_storage$last_edge_label_alpha %||% 1,
      edge_label_fontface = group_storage$last_edge_label_fontface %||% "plain",
      zoom_factor = zoom_level,
      data_file = NULL,
      bezier_network_edges = group_storage$last_bezier_network_edges %||% FALSE,
      network_edges_curvature_magnitude = group_storage$last_network_edges_curvature_magnitude %||% 0.3,
      network_edges_rotate_curvature = group_storage$last_network_edges_rotate_curvature %||% FALSE,
      network_edges_curvature_asymmetry = group_storage$last_network_edges_curvature_asymmetry %||% 0,
      modify_params_edge = ifelse (!is.null(modifications_network$edge), TRUE, FALSE) %||% FALSE,
      modified_edges = modifications_network$edge,
      modify_params_edgelabel = ifelse (!is.null(modifications_network$edgelabel), TRUE, FALSE) %||% FALSE,
      modified_edgelabels = modifications_network$edgelabel,
      modify_params_edgelabel_xy = ifelse (!is.null(modifications_network$edgelabel_xy), TRUE, FALSE) %||% FALSE,
      modified_edgelabels_xy = modifications_network$edgelabel_xy,
      modify_params_node = ifelse (!is.null(modifications_network$node), TRUE, FALSE) %||% FALSE,
      modified_nodes = modifications_network$node,
      modify_params_node_xy = ifelse (!is.null(modifications_network$node_xy), TRUE, FALSE) %||% FALSE,
      modified_nodes_xy = modifications_network$node_xy,
      modify_params_nodelabel = ifelse (!is.null(modifications_network$nodelabel), TRUE, FALSE) %||% FALSE,
      modified_nodelabels = modifications_network$nodelabel,
      modify_params_nodelabel_xy = ifelse (!is.null(modifications_network$nodelabel_xy), TRUE, FALSE) %||% FALSE,
      modified_nodelabels_xy = modifications_network$nodelabel_xy,
      modify_params_nodelabel_text = ifelse (!is.null(modifications_network$nodelabel_text), TRUE, FALSE) %||% FALSE,
      modified_nodelabels_text = modifications_network$nodelabel_text,
      modify_params_bezier_edge = ifelse (!is.null(modifications_network$edge_curvature), TRUE, FALSE) %||% FALSE,
      modified_bezier_edges = modifications_network$edge_curvature,
      modify_params_edge_xy = ifelse (!is.null(modifications_network$edge_xy), TRUE, FALSE) %||% FALSE,
      modified_edges_xy = modifications_network$edge_xy,
      modify_params_edgelabel_text = ifelse (!is.null(modifications_network$edgelabel_text), TRUE, FALSE) %||% FALSE,
      modified_edgelabels_text = modifications_network$edgelabel_text,
      apply_global_nodes = group_storage$last_apply_global_nodes %||% FALSE,
      apply_global_edges = group_storage$last_apply_global_edges %||% FALSE,
      apply_global_annotations = group_storage$last_apply_global_annotations %||% FALSE,
      which_group = group_id %||% "1"
    )
  } else {
    # igraph or no network object

    network_prep <- generate_network_layout(network_object = bundleObject,
                                            edges = group_storage$edges,
                                            nodes = group_storage$nodes,
                                            layout_method = ifelse(layout_method == 'layout_ai', "fr", layout_method),
                                            directed = group_storage$last_is_directed,
                                            dim_reduction_method = group_storage$last_dim_reduction_method,
                                            random_seed = group_storage$lastrandom_seed)

    if (group_storage$last_layout_method == 'layout_ai') {
      layout <- group_storage$layout # layout by AI
    } else {
      layout <- network_prep$layout
    }


    network_graph <- generate_graph_from_network1(
      graph = network_prep$graph,
      layout = layout,
      is_bipartite = network_prep$is_bipartite,
      edges = group_storage$edges,
      nodes = group_storage$nodes,
      directed = group_storage$last_is_directed %||% TRUE,
      layout_width = group_storage$last_layout_x %||% 30,
      layout_height = group_storage$last_layout_y %||% 30,
      x_center = group_storage$last_x_center %||% 0,
      y_center = group_storage$last_y_center %||% 0,
      node_shape = group_storage$last_node_shape %||% "circle",
      node_size = group_storage$last_node_size %||% 10,
      node_alpha = group_storage$last_node_alpha %||% 1,
      node_fill_color = group_storage$last_node_fill_color %||% "#1262b3",
      node_border_color = group_storage$last_node_border_color %||% "#0f993d",
      node_border_width = group_storage$last_node_border_width %||% 1,
      node_width_height_ratio = group_storage$last_node_width_height_ratio %||% 1,
      line_width = group_storage$last_line_width %||% 1,
      line_color = group_storage$last_line_color %||% "#000000",
      line_alpha = group_storage$last_line_alpha %||% 1,
      min_edge_width = ifelse(group_storage$last_scale_edge_width, group_storage$last_min_edge_width %||% 0.5, NULL),
      max_edge_width = ifelse(group_storage$last_scale_edge_width, group_storage$last_max_edge_width %||% 3, NULL),
      scale_by_weight = group_storage$last_scale_edge_width %||% FALSE,
      line_endpoint_spacing = group_storage$last_line_endpoint_spacing %||% 0,
      arrow_type = group_storage$last_arrow_type %||% "closed",
      arrow_size = group_storage$last_arrow_size %||% 0.1,
      node_label_font = group_storage$last_node_label_font %||% "sans",
      node_label_size = group_storage$last_node_label_size %||% 10,
      node_label_color = group_storage$last_node_label_color %||% "#000000",
      node_label_alpha = group_storage$last_node_label_alpha %||% 1,
      node_label_fontface = group_storage$last_node_label_fontface %||% "plain",
      edge_label_font = group_storage$last_edge_label_font %||% "sans",
      edge_label_size = group_storage$last_edge_label_size %||% 10,
      edge_label_color = group_storage$last_edge_label_color %||% "#000000",
      edge_label_fill = group_storage$last_edge_label_fill %||% "#FFFFFF",
      edge_label_alpha = group_storage$last_edge_label_alpha %||% 1,
      edge_label_fontface = group_storage$last_edge_label_fontface %||% "plain",
      zoom_factor = zoom_level,
      annotate_nodes = TRUE,
      annotate_edges = TRUE,
      use_clustering = group_storage$last_use_clustering %||% FALSE,
      clustering_method = group_storage$last_clustering_method %||% "louvain",
      cluster_palette = group_storage$last_cluster_palette %||% "rainbow",
      bezier_network_edges = group_storage$last_bezier_network_edges %||% FALSE,
      network_edges_curvature_magnitude = group_storage$last_network_edges_curvature_magnitude %||% 0.5,
      network_edges_rotate_curvature = group_storage$last_network_edges_rotate_curvature %||% FALSE,
      network_edges_curvature_asymmetry = group_storage$last_network_edges_curvature_asymmetry %||% 0,
      modify_params_edge = ifelse (!is.null(modifications_network$edge), TRUE, FALSE) %||% FALSE,
      modified_edges = modifications_network$edge,
      modify_params_edgelabel = ifelse (!is.null(modifications_network$edgelabel), TRUE, FALSE) %||% FALSE,
      modified_edgelabels = modifications_network$edgelabel,
      modify_params_edgelabel_xy = ifelse (!is.null(modifications_network$edgelabel_xy), TRUE, FALSE) %||% FALSE,
      modified_edgelabels_xy = modifications_network$edgelabel_xy,
      modify_params_node = ifelse (!is.null(modifications_network$node), TRUE, FALSE) %||% FALSE,
      modified_nodes = modifications_network$node,
      modify_params_node_xy = ifelse (!is.null(modifications_network$node_xy), TRUE, FALSE) %||% FALSE,
      modified_nodes_xy = modifications_network$node_xy,
      modify_params_nodelabel = ifelse (!is.null(modifications_network$nodelabel), TRUE, FALSE) %||% FALSE,
      modified_nodelabels = modifications_network$nodelabel,
      modify_params_nodelabel_xy = ifelse (!is.null(modifications_network$nodelabel_xy), TRUE, FALSE) %||% FALSE,
      modified_nodelabels_xy = modifications_network$nodelabel_xy,
      modify_params_nodelabel_text = ifelse (!is.null(modifications_network$nodelabel_text), TRUE, FALSE) %||% FALSE,
      modified_nodelabels_text = modifications_network$nodelabel_text,
      modify_params_bezier_edge = ifelse (!is.null(modifications_network$edge_curvature), TRUE, FALSE) %||% FALSE,
      modified_bezier_edges = modifications_network$edge_curvature,
      modify_params_edge_xy = ifelse (!is.null(modifications_network$edge_xy), TRUE, FALSE) %||% FALSE,
      modified_edges_xy = modifications_network$edge_xy,
      modify_params_edgelabel_text = ifelse (!is.null(modifications_network$edgelabel_text), TRUE, FALSE) %||% FALSE,
      modified_edgelabels_text = modifications_network$edgelabel_text,
      apply_bipartite_nodes = group_storage$last_apply_bipartite_nodes %||% FALSE,
      apply_bipartite_edges = group_storage$last_apply_bipartite_edges %||% FALSE,
      apply_bipartite_annotations = group_storage$last_apply_bipartite_annotations %||% FALSE,
      last_state = NULL,
      which_group = group_id %||% "1"
    )
  }
  return(network_graph)
}


#' Reproduce a SEM (Structural Equation Modeling) visualization from metadata
#'
#' This function recreates a SEM path diagram using stored metadata from
#' a Shiny app session. It supports various SEM object types including
#' lavaan, semPlot, qgraph, and diagrammeR objects.
#'
#' @param metadata Either a metadata object or a file path to an RDS file
#'   containing metadata captured from the Shiny app. If NULL, the function
#'   will stop with an error.
#' @param lavaan_syntax Optional lavaan syntax string. If NULL, uses the
#'   syntax stored in the metadata.
#' @param group_id The identifier for the specific SEM group to reproduce.
#'   This corresponds to the group ID used in the original Shiny app.
#' @param object Optional SEM object to use instead of the one stored
#'   in metadata. Useful for testing with different SEM objects.
#' @param zoom_level Zoom factor for the visualization. Default is 1.2.
#'
#' @return A ggplot object containing the reproduced SEM path diagram.
#' @importFrom utils read.csv
#' @importFrom lavaan lavaanify parTable
#' @importFrom methods is
#' @importFrom tidySEM prepare_graph
#' @importFrom stats setNames
#' @examples
#' \dontrun{
#' # Reproduce SEM from saved metadata
#' sem_plot <- reproduce_sem(
#'   metadata = "path/to/metadata.rds",
#'   group_id = "sem1"
#' )
#'
#' # Reproduce with custom lavaan syntax
#' sem_plot <- reproduce_sem(
#'   metadata = metadata_object,
#'   lavaan_syntax = "y ~ x1 + x2",
#'   group_id = "sem1"
#' )
#' }
#'
#' @export
reproduce_sem <- function(metadata = NULL,  lavaan_syntax = NULL, group_id = NULL, object = NULL, zoom_level = 1.2) {
  if (is.character(metadata) && file.exists(metadata)) {
    metadata <- readRDS(metadata)
  }

  if (is.null(metadata)) {
    stop("No metadata provided. Either provide a metadata file or capture one from the Shiny app.")
  }

  if (is.null(lavaan_syntax)) {
    lavaan_syntax <- metadata$sem_groups[[group_id]]$last_lavaan_syntax
  }

  data <- metadata$sem_groups[[group_id]]$data

  if (is.null(data)) {
    model <- tryCatch({
      lavaan::lavaanify(lavaan_syntax)
    }, error = function(e) {
      stop("Error parsing lavaan syntax:", e$message)
      return(NULL)
    })

    if (!is.null(model)) {
      observed_vars <- tryCatch({
        unique(setdiff(model$rhs[model$op %in% c("=~", "~", "~~")], model$lhs[model$op == "=~"]))
      }, error = function(e) {
        stop("Error extracting observed variables")
        return(character(0))
      })
    } else {
      observed_vars <- character(0)
    }
  }

  if (!is.null(data) && is.character(data) && file.exists(data)) {
    metadata$sem_groups[[group_id]]$data_file <- TRUE
    data <- read.csv(data, check.names = FALSE)
  } else if (!is.null(data)) {
    metadata$sem_groups[[group_id]]$data_file <- TRUE
    data <- data  # Use the data as provided (could be a data frame)
  } else if (is.null(data) && metadata$sem_groups[[group_id]]$data_file == FALSE) {
    data <- metadata$sem_groups[[group_id]]$data  # synthetic data
  }


  group_storage <- metadata$sem_groups[[group_id]]
  modifications <- metadata$modifications$sem[[group_id]]


  if (is.null(object)) {
    bundleObject <- group_storage$bundleObject
  } else {
    bundleObject <- object
  }

  bundleModelObject <- group_storage$bundleModelObject

  lavaan_layout <- group_storage$last_lavaan_layout
  lavaan_layout_matrix <- group_storage$last_lavaan_layout_matrix

  multi_group_sem_combine_menu <- group_storage$multi_group_sem_combine_menu

  if (!is.null(bundleObject)) {
    # edge_label_file <- TRUE

    if (is(bundleObject)[[1]] == "lavaan") {

      if (!multi_group_sem_combine_menu) {

        layout_algorithm <-
          if (lavaan_layout == 'custom' && !is.null(lavaan_layout_matrix)) {
            lavaan_layout_matrix
          } else if (lavaan_layout == 'layout_ai') {
            "default"
          } else {
            lavaan_layout
          }

        sem_paths <- lavaan_to_sempaths(fit = group_storage$current,
                                        data_file = data,
                                        layout_algorithm = layout_algorithm,
                                        annotate_estimates = group_storage$last_annotate_sem_est,
                                        standardized = group_storage$last_std_est,
                                        unstandardized = group_storage$last_ustd_est,
                                        conf_int = group_storage$last_conf_int,
                                        p_val = group_storage$last_p_val,
                                        multi_group = group_storage$last_multigroup_data_upload %||% FALSE,
                                        group_var = group_storage$last_group_var,
                                        group_level = group_storage$last_group_level %||% NULL,
                                        p_val_alpha = group_storage$last_p_val_alpha,
                                        residuals = group_storage$last_residuals %||% FALSE)

      } else {
        first_group_level <- group_storage$first_group
        second_group_level <- group_storage$second_group

        lavaan_fit <- group_storage$current

        combine <- group_storage$multi_combine_real

        if (!combine) {
          model_first <- group_storage$first_model
          model_second <- group_storage$second_model
          lavaan_fit <- list(model_first, model_second)
        }

        layout_algorithm <-
          if (lavaan_layout == 'custom' && !is.null(lavaan_layout_matrix)) {
            lavaan_layout_matrix
          } else if (lavaan_layout == 'layout_ai') {
            "default"
          } else {
            lavaan_layout
          }

        sem_paths <- lavaan_to_sempaths(fit = lavaan_fit,
                                        layout_algorithm = layout_algorithm,
                                        intercepts = TRUE,
                                        annotate_estimates = group_storage$last_annotate_sem_est,
                                        standardized = group_storage$last_std_est,
                                        unstandardized = group_storage$last_ustd_est,
                                        conf_int = group_storage$last_conf_int,
                                        p_val = group_storage$last_p_val,
                                        combine = combine,
                                        group1 = first_group_level,
                                        group2 = second_group_level,
                                        sep_by = group_storage$last_sep_by,
                                        residuals = group_storage$last_residuals %||% FALSE)

      }


    } else if (is(bundleObject)[[1]] == "blavaan") {

      if (!multi_group_sem_combine_menu) {
        layout_algorithm <-
          if (lavaan_layout == 'custom' && !is.null(lavaan_layout_matrix)) {
            lavaan_layout_matrix
          } else if (lavaan_layout == 'layout_ai') {
            "default"
          } else {
            lavaan_layout
          }

        sem_paths <- blavaan_to_sempaths(fit = group_storage$current,
                                         data_file = group_storage$data,
                                         layout_algorithm = layout_algorithm,
                                         annotate_estimates = group_storage$last_annotate_sem_est,
                                         standardized = group_storage$last_std_est,
                                         unstandardized = group_storage$last_ustd_est,
                                         conf_int = group_storage$last_conf_int,
                                         p_val = group_storage$last_p_val,
                                         multi_group = group_storage$last_multigroup_data_upload %||% FALSE,
                                         group_var = group_storage$last_group_var,
                                         group_level = group_storage$last_group_level %||% NULL,
                                         residuals = group_storage$last_residuals %||% FALSE)

      } else {
        first_group_level <- group_storage$first_group
        second_group_level <- group_storage$second_group

        lavaan_fit <- group_storage$current

        combine <- group_storage$multi_combine_real

        if (!combine) {
          model_first <- group_storage$first_model
          model_second <- group_storage$second_model
          lavaan_fit <- list(model_first, model_second)
        }

        layout_algorithm <-
          if (lavaan_layout == 'custom' && !is.null(lavaan_layout_matrix)) {
            lavaan_layout_matrix
          } else if (lavaan_layout == 'layout_ai') {
            "default"
          } else {
            lavaan_layout
          }

        sem_paths <- blavaan_to_sempaths(fit = lavaan_fit,
                                         layout_algorithm = layout_algorithm,
                                         intercepts = TRUE,
                                         annotate_estimates = group_storage$last_annotate_sem_est,
                                         standardized = group_storage$last_std_est,
                                         unstandardized = group_storage$last_ustd_est,
                                         conf_int = group_storage$last_conf_int,
                                         p_val = group_storage$last_p_val,
                                         combine = combine,
                                         group1 = first_group_level,
                                         group2 = second_group_level,
                                         sep_by = group_storage$last_sep_by,
                                         residuals = group_storage$last_residuals %||% FALSE)
      }

    } else if (inherits(bundleObject, "mplusObject")) {

      layout_algorithm <-
        if (lavaan_layout == 'custom' && !is.null(lavaan_layout_matrix)) {
          lavaan_layout_matrix
        } else if (lavaan_layout == 'layout_ai') {
          "default"
        } else {
          lavaan_layout
        }

      sem_paths <- lavaan_to_sempaths(fit = group_storage$current,
                                      data_file = group_storage$data,
                                      layout_algorithm = layout_algorithm,
                                      annotate_estimates = group_storage$last_annotate_sem_est,
                                      standardized = group_storage$last_std_est,
                                      unstandardized = group_storage$last_ustd_est,
                                      conf_int = group_storage$last_conf_int,
                                      p_val = group_storage$last_p_val,
                                      p_val_alpha = group_storage$last_p_val_alpha,
                                      residuals = group_storage$last_residuals %||% FALSE)

    } else if (inherits(bundleObject, "MxRAMModel")) {

      if (!multi_group_sem_combine_menu) {
        layout_algorithm <-
          if (lavaan_layout == 'custom' && !is.null(lavaan_layout_matrix)) {
            lavaan_layout_matrix
          } else if (lavaan_layout == 'layout_ai') {
            "default"
          } else {
            lavaan_layout
          }

        sem_paths <- lavaan_to_sempaths(fit = group_storage$current,
                                        data_file = group_storage$data,
                                        layout_algorithm = layout_algorithm,
                                        annotate_estimates = group_storage$last_annotate_sem_est,
                                        standardized = group_storage$last_std_est,
                                        unstandardized = group_storage$last_ustd_est,
                                        conf_int = group_storage$last_conf_int,
                                        p_val = group_storage$last_p_val,
                                        p_val_alpha = group_storage$last_p_val_alpha,
                                        residuals = group_storage$last_residuals %||% FALSE)

      } else {
        combine <- group_storage$multi_combine_real

        first_group_level <- group_storage$first_group
        second_group_level <- group_storage$second_group

        if (!combine) {
          model_first <- group_storage$first_model
          model_second <- group_storage$second_model

        }

        layout_algorithm <-
          if (lavaan_layout == 'custom' && !is.null(lavaan_layout_matrix)) {
            lavaan_layout_matrix
          } else if (lavaan_layout == 'layout_ai') {
            "default"
          } else {
            lavaan_layout
          }

        sem_paths <- lavaan_to_sempaths(fit = lavaan_fit,
                                        layout_algorithm = layout_algorithm,
                                        intercepts = TRUE,
                                        annotate_estimates = group_storage$last_annotate_sem_est,
                                        standardized = group_storage$last_std_est,
                                        unstandardized = group_storage$last_ustd_est,
                                        conf_int = group_storage$last_conf_int,
                                        p_val = group_storage$last_p_val,
                                        combine = FALSE,
                                        group1 = first_group_level,
                                        group2 = second_group_level,
                                        sep_by = group_storage$last_sep_by,
                                        residuals = group_storage$last_residuals %||% FALSE)

      }
    } else if (inherits(bundleObject, "qgraph")) { # semPaths
      if (is.null(bundleModelObject)) {
        sem_paths <- bundleObject
      } else {
        if (is(bundleModelObject)[[1]] == "lavaan") {

          if (!multi_group_sem_combine_menu) {

            layout_algorithm <-
              if (lavaan_layout == 'custom' && !is.null(lavaan_layout_matrix)) {
                lavaan_layout_matrix
              } else if (lavaan_layout == 'layout_ai') {
                "default"
              } else {
                lavaan_layout
              }

            sem_paths <- lavaan_to_sempaths(fit = group_storage$current,
                                            data_file = data,
                                            layout_algorithm = layout_algorithm,
                                            annotate_estimates = group_storage$last_annotate_sem_est,
                                            standardized = group_storage$last_std_est,
                                            unstandardized = group_storage$last_ustd_est,
                                            conf_int = group_storage$last_conf_int,
                                            p_val = group_storage$last_p_val,
                                            multi_group = group_storage$last_multigroup_data_upload %||% FALSE,
                                            group_var = group_storage$last_group_var,
                                            group_level = group_storage$last_group_level %||% NULL,
                                            p_val_alpha = group_storage$last_p_val_alpha,
                                            residuals = group_storage$last_residuals %||% FALSE)

          } else {
            first_group_level <- group_storage$first_group
            second_group_level <- group_storage$second_group

            lavaan_fit <- group_storage$current
            combine <- group_storage$multi_combine_real

            if (!combine) {
              model_first <- group_storage$first_model
              model_second <- group_storage$second_model
              lavaan_fit <- list(model_first, model_second)
            }

            layout_algorithm <-
              if (lavaan_layout == 'custom' && !is.null(lavaan_layout_matrix)) {
                lavaan_layout_matrix
              } else if (lavaan_layout == 'layout_ai') {
                "default"
              } else {
                lavaan_layout
              }

            sem_paths <- lavaan_to_sempaths(fit = lavaan_fit,
                                            layout_algorithm = layout_algorithm,
                                            intercepts = TRUE,
                                            annotate_estimates = group_storage$last_annotate_sem_est,
                                            standardized = group_storage$last_std_est,
                                            unstandardized = group_storage$last_ustd_est,
                                            conf_int = group_storage$last_conf_int,
                                            p_val = group_storage$last_p_val,
                                            combine = combine,
                                            group1 = first_group_level,
                                            group2 = second_group_level,
                                            sep_by = group_storage$last_sep_by,
                                            residuals = group_storage$last_residuals %||% FALSE)
          }

        } else if (is(bundleModelObject)[[1]] == "blavaan") {

          if (!multi_group_sem_combine_menu) {
            layout_algorithm <-
              if (lavaan_layout == 'custom' && !is.null(lavaan_layout_matrix)) {
                lavaan_layout_matrix
              } else if (lavaan_layout == 'layout_ai') {
                "default"
              } else {
                lavaan_layout
              }

            sem_paths <- blavaan_to_sempaths(fit = group_storage$current,
                                             data_file = group_storage$data,
                                             layout_algorithm = layout_algorithm,
                                             annotate_estimates = group_storage$last_annotate_sem_est,
                                             standardized = group_storage$last_std_est,
                                             unstandardized = group_storage$last_ustd_est,
                                             conf_int = group_storage$last_conf_int,
                                             p_val = group_storage$last_p_val,
                                             multi_group = group_storage$last_multigroup_data_upload %||% FALSE,
                                             group_var = group_storage$last_group_var,
                                             group_level = group_storage$last_group_level %||% NULL,
                                             residuals = group_storage$last_residuals %||% FALSE)
          } else {
            first_group_level <- group_storage$first_group
            second_group_level <- group_storage$second_group

            lavaan_fit <- group_storage$current
            combine <- group_storage$multi_combine_real

            if (!combine) {
              model_first <- group_storage$first_model
              model_second <- group_storage$second_model
              lavaan_fit <- list(model_first, model_second)
            }

            layout_algorithm <-
              if (lavaan_layout == 'custom' && !is.null(lavaan_layout_matrix)) {
                lavaan_layout_matrix
              } else if (lavaan_layout == 'layout_ai') {
                "default"
              } else {
                lavaan_layout
              }

            sem_paths <- blavaan_to_sempaths(fit = lavaan_fit,
                                             layout_algorithm = layout_algorithm,
                                             intercepts = TRUE,
                                             annotate_estimates = group_storage$last_annotate_sem_est,
                                             standardized = group_storage$last_std_est,
                                             unstandardized = group_storage$last_ustd_est,
                                             conf_int = group_storage$last_conf_int,
                                             p_val = group_storage$last_p_val,
                                             combine = combine,
                                             group1 = first_group_level,
                                             group2 = second_group_level,
                                             sep_by = group_storage$last_sep_by,
                                             residuals = group_storage$last_residuals %||% FALSE)
          }

        }
      }
    } else if (inherits(bundleObject, "sem_graph")) { # tidySEM
      if (!multi_group_sem_combine_menu) {

        fit_delta <- bundleObject
        if (inherits(bundleModelObject, "lavaan")) {
          if (!is.null(group_storage$current) && !is.null(group_storage$original)) {
            if (!identical(lavaan::parTable(group_storage$current), lavaan::parTable(group_storage$original))) {
              fit_delta <- tidySEM::prepare_graph(model = group_storage$current) # in case model parameters are modified
            }
          }
        }

        if (is(bundleModelObject)[[1]] == "lavaan") {
          fit_delta <- update_tidysem_labels(fit_delta, standardized = group_storage$last_std_est, unstandardized = group_storage$last_ustd_est,
                                             p_val = group_storage$last_p_val, conf_int = group_storage$last_conf_int)

          sem_paths <- update_tidysem_labels(bundleObject, standardized = group_storage$last_std_est, unstandardized = group_storage$last_ustd_est,
                                             p_val = group_storage$last_p_val, conf_int = group_storage$last_conf_int)

        } else if (is(bundleModelObject)[[1]] == "blavaan") {
          fit_delta <- update_tidysem_labels_bayes(fit_delta, bundleModelObject, standardized = group_storage$last_std_est, unstandardized = group_storage$last_ustd_est,
                                                   p_val = group_storage$last_p_val, conf_int = group_storage$last_conf_int)

          sem_paths <- update_tidysem_labels_bayes(bundleObject, bundleModelObject, standardized = group_storage$last_std_est, unstandardized = group_storage$last_ustd_est,
                                                   p_val = group_storage$last_p_val, conf_int = group_storage$last_conf_int)
        }

      } else {

        first_group_level <- group_storage$first_group
        second_group_level <- group_storage$second_group

        combine <- group_storage$multi_combine_real

        if (combine) {
          if (is(bundleModelObject)[[1]] == "lavaan") {
            sem_paths <- combine_tidysem_groups(bundleObject, group1 = first_group_level, group2 = second_group_level, sep_by = group_storage$last_sep_by,
                                                standardized = group_storage$last_std_est, unstandardized = group_storage$last_ustd_est, p_val = group_storage$last_p_val, conf_int = group_storage$last_conf_int)
            fit_delta <- combine_tidysem_groups(bundleObject, group1 = first_group_level, group2 = second_group_level, sep_by = group_storage$last_sep_by,
                                                standardized = group_storage$last_std_est, unstandardized = group_storage$last_ustd_est, p_val = group_storage$last_p_val, conf_int = group_storage$last_conf_int)

          } else if (is(bundleModelObject)[[1]] == "blavaan") {
            sem_paths <- combine_tidysem_groups_bayes(bundleObject, bundleModelObject, group1 = first_group_level, group2 = second_group_level, sep_by = group_storage$last_sep_by,
                                                      standardized = group_storage$last_std_est, unstandardized = group_storage$last_ustd_est, p_val = group_storage$last_p_val, conf_int = group_storage$last_conf_int)
            fit_delta <- combine_tidysem_groups_bayes(bundleObject, bundleModelObject, group1 = first_group_level, group2 = second_group_level, sep_by = group_storage$last_sep_by,
                                                      standardized = group_storage$last_std_est, unstandardized = group_storage$last_ustd_est, p_val = group_storage$last_p_val, conf_int = group_storage$last_conf_int)
          }
        } else {
          if (is(bundleModelObject)[[1]] == "lavaan") {
            first_object <- group_storage$first_object
            second_object <- group_storage$second_object

            sem_paths <- combine_tidysem_objects(first_object, second_object, group1 = first_group_level, group2 = second_group_level, sep_by = group_storage$last_sep_by,
                                                 standardized = group_storage$last_std_est, unstandardized = group_storage$last_ustd_est, p_val = group_storage$last_p_val, conf_int = group_storage$last_conf_int)
            fit_delta <- combine_tidysem_objects(first_object, second_object, group1 = first_group_level, group2 = second_group_level, sep_by = group_storage$last_sep_by,
                                                 standardized = group_storage$last_std_est, unstandardized = group_storage$last_ustd_est, p_val = group_storage$last_p_val, conf_int = group_storage$last_conf_int)

          } else if (is(bundleModelObject)[[2]] == "blavaan") {
            first_object <- group_storage$first_object
            second_object <- group_storage$second_object
            first_model <- group_storage$first_model
            second_model <- group_storage$second_model

            sem_paths <- combine_tidysem_objects_bayes(first_object, second_object, first_model, second_model,
                                                       group1 = first_group_level, group2 = second_group_level, sep_by = group_storage$last_sep_by,
                                                       standardized = group_storage$last_std_est, unstandardized = group_storage$last_ustd_est, p_val = group_storage$last_p_val, conf_int = group_storage$last_conf_int)
            fit_delta <- combine_tidysem_objects_bayes(first_object, second_object, first_model, second_model,
                                                       group1 = first_group_level, group2 = second_group_level, sep_by = group_storage$last_sep_by,
                                                       standardized = group_storage$last_std_est, unstandardized = group_storage$last_ustd_est, p_val = group_storage$last_p_val, conf_int = group_storage$last_conf_int)
          }
        }
      }

    } else if (inherits(bundleObject, "grViz")) { # diagrammeR / lavaanPlot
      grviz_object <- bundleObject
    } else {
      stop("Unsupported object type in bundle.")
    }
  } else {

    if (!multi_group_sem_combine_menu) {

      layout_algorithm <-
        if (lavaan_layout == 'custom' && !is.null(lavaan_layout_matrix)) {
          lavaan_layout_matrix
        } else if (lavaan_layout == 'layout_ai') {
          "default"
        } else {
          lavaan_layout
        }

      sem_paths <- lavaan_to_sempaths(fit = group_storage$current,
                                      data_file = group_storage$data,
                                      layout_algorithm = layout_algorithm,
                                      intercepts = group_storage$intercepts,
                                      annotate_estimates = group_storage$last_annotate_sem_est,
                                      standardized = group_storage$last_std_est,
                                      unstandardized = group_storage$last_ustd_est,
                                      conf_int = group_storage$last_conf_int,
                                      p_val = group_storage$last_p_val,
                                      multi_group = group_storage$last_multigroup_data_upload %||% FALSE,
                                      group_var = group_storage$last_group_var,
                                      group_level = group_storage$last_group_level %||% NULL,
                                      p_val_alpha = group_storage$last_p_val_alpha,
                                      residuals = group_storage$last_residuals %||% FALSE)

    } else {

      first_group_level <- group_storage$first_group
      second_group_level <- group_storage$second_group

      lavaan_fit <- group_storage$current

      layout_algorithm <-
        if (lavaan_layout == 'custom' && !is.null(lavaan_layout_matrix)) {
          lavaan_layout_matrix
        } else if (lavaan_layout == 'layout_ai') {
          "default"
        } else {
          lavaan_layout
        }

      sem_paths <- lavaan_to_sempaths(fit = lavaan_fit,
                                      layout_algorithm = layout_algorithm,
                                      intercepts = TRUE,
                                      annotate_estimates = group_storage$last_annotate_sem_est,
                                      standardized = group_storage$last_std_est,
                                      unstandardized = group_storage$last_ustd_est,
                                      p_val = group_storage$last_pval_est,
                                      combine = TRUE,
                                      group1 = first_group_level,
                                      group2 = second_group_level,
                                      sep_by = group_storage$last_sep_by,
                                      residuals = group_storage$last_residuals %||% FALSE)

    }

  }


  if (inherits(bundleObject, c("grViz"))) {
    graph_data <- generate_graph_from_diagrammeR1(grviz_object,
                                                  relative_x_position = group_storage$last_relative_x_position %||% 25,
                                                  relative_y_position = group_storage$last_relative_y_position %||% 25,
                                                  center_x = group_storage$last_center_x_position %||% 0,
                                                  center_y = group_storage$last_center_y_position %||% 0,
                                                  latent_shape = group_storage$last_latent_shape %||% "circle",
                                                  observed_shape = group_storage$last_observed_shape %||% "square",
                                                  int_shape = group_storage$last_int_shape %||% "triangle",
                                                  point_size_latent = group_storage$last_point_size_latent %||% 20,
                                                  point_size_observed = group_storage$last_point_size_observed %||% 12,
                                                  point_size_int = group_storage$last_point_size_int %||% 10,
                                                  width_height_ratio_latent = group_storage$last_width_height_ratio_latent %||% 1,
                                                  width_height_ratio_observed = group_storage$last_width_height_ratio_observed %||% 1,
                                                  width_height_ratio_int = group_storage$last_width_height_ratio_int %||% 1,
                                                  line_width = group_storage$last_line_width %||% 1,
                                                  line_alpha = group_storage$last_line_alpha %||% 1,
                                                  text_size_latent = group_storage$last_text_size_latent %||% 18,
                                                  text_font_latent = group_storage$last_text_font_latent %||% "sans",
                                                  text_color_latent = group_storage$last_text_color_latent %||% "#FFFFFF",
                                                  text_alpha_latent = group_storage$last_text_alpha_latent %||% 1,
                                                  text_fontface_latent = group_storage$last_text_fontface_latent %||% "plain",
                                                  text_size_others = group_storage$last_text_size_others %||% 16,
                                                  text_font_others = group_storage$last_text_font_others %||% "sans",
                                                  text_color_others = group_storage$last_text_color_others %||% "#FFFFFF",
                                                  text_alpha_others = group_storage$last_text_alpha_others %||% 1,
                                                  text_fontface_others = group_storage$last_text_fontface_others %||% "plain",
                                                  text_size_edges = group_storage$last_text_size_edges %||% 14,
                                                  text_font_edges = group_storage$last_text_font_edges %||% "sans",
                                                  text_color_edges = group_storage$last_text_color_edges %||% "#000000",
                                                  text_color_fill = group_storage$last_text_color_fill %||% "#FFFFFF",
                                                  text_alpha_edges = group_storage$last_text_alpha_edges %||% 1,
                                                  text_fontface_edges = group_storage$last_text_fontface_edges %||% "plain",
                                                  point_color_latent = group_storage$last_point_color_latent %||% "#cc3d3d",
                                                  point_color_observed = group_storage$last_point_color_observed %||% "#1262b3",
                                                  point_color_int = group_storage$last_point_color_int %||% "#0f993d",
                                                  edge_color = group_storage$last_edge_color %||% "#000000",
                                                  line_endpoint_spacing = group_storage$last_line_endpoint_spacing %||% 0.2,
                                                  node_border_color = group_storage$last_node_border_color %||% "#FFFFFF",
                                                  node_border_width = group_storage$last_node_border_width %||% 1,
                                                  arrow_type = group_storage$last_arrow_type %||% "closed",
                                                  arrow_size = group_storage$last_arrow_size %||% 0.1,
                                                  lavaan_arrow_location = group_storage$last_lavaan_arrow_location %||% "end",
                                                  zoom_factor = zoom_level, # user's input (keep as is)
                                                  lavaan_curvature_magnitude = group_storage$last_lavaan_curvature_magnitude %||% 0.5,
                                                  lavaan_rotate_curvature = group_storage$last_lavaan_rotate_curvature %||% FALSE,
                                                  lavaan_curvature_asymmetry = group_storage$last_lavaan_curvature_asymmetry %||% 0,
                                                  lavaan_curved_x_shift = group_storage$last_lavaan_curved_x_shift %||% 0,
                                                  lavaan_curved_y_shift = group_storage$last_lavaan_curved_y_shift %||% 0,
                                                  highlight_free_path = group_storage$last_highlight_free_path %||% FALSE,
                                                  ff_params_edge = group_storage$last_ff_params_edge %||% NULL,
                                                  ff_params_edgelabel = group_storage$last_ff_params_edgelabel %||% NULL,
                                                  highlight_sig_path = group_storage$last_highlight_sig_path %||% FALSE,
                                                  sig_path_color = group_storage$last_sig_path_color %||% "#000000",
                                                  non_sig_path_color = group_storage$last_non_sig_path_color %||% "#000000",
                                                  sig_label_fontface = group_storage$last_sig_label_fontface %||% "plain",
                                                  non_sig_label_fontface = group_storage$last_non_sig_label_fontface %||% "plain",
                                                  data_file = group_storage$data_file,
                                                  modify_params_edge = ifelse (!is.null(modifications$edge), TRUE, FALSE) %||% FALSE,
                                                  modified_edges = modifications$edge,
                                                  modify_params_edgelabel = ifelse (!is.null(modifications$edgelabel), TRUE, FALSE) %||% FALSE,
                                                  modified_edgelabels = modifications$edgelabel,
                                                  modify_params_edgelabel_xy = ifelse (!is.null(modifications$edgelabel_xy), TRUE, FALSE) %||% FALSE,
                                                  modified_edgelabels_xy = modifications$edgelabel_xy,
                                                  modify_params_edgelabel_text = ifelse (!is.null(modifications$edgelabel_text), TRUE, FALSE) %||% FALSE,
                                                  modified_edgelabels_text = modifications$edgelabel_text,
                                                  modify_params_node = ifelse (!is.null(modifications$node), TRUE, FALSE) %||% FALSE,
                                                  modified_nodes = modifications$node,
                                                  modify_params_node_xy = ifelse (!is.null(modifications$node_xy), TRUE, FALSE) %||% FALSE,
                                                  modified_nodes_xy = modifications$node_xy,
                                                  modify_params_edge_xy = ifelse (!is.null(modifications$edge_xy), TRUE, FALSE) %||% FALSE,
                                                  modified_edges_xy = modifications$edge_xy,
                                                  modify_params_cov_edge = ifelse (!is.null(modifications$cov_edge), TRUE, FALSE) %||% FALSE,
                                                  modified_cov_edges = modifications$cov_edge,
                                                  modify_params_nodelabel = ifelse (!is.null(modifications$nodelabel), TRUE, FALSE) %||% FALSE,
                                                  modified_nodelabels = modifications$nodelabel,
                                                  modify_params_nodelabel_xy = ifelse (!is.null(modifications$nodelabel_xy), TRUE, FALSE) %||% FALSE,
                                                  modified_nodelabels_xy = modifications$nodelabel_xy,
                                                  modify_params_nodelabel_text = ifelse (!is.null(modifications$nodelabel_text), TRUE, FALSE) %||% FALSE,
                                                  modified_nodelabels_text = modifications$nodelabel_text,
                                                  modify_params_latent_node_xy = ifelse (!is.null(modifications$latent_node_xy), TRUE, FALSE) %||% FALSE,
                                                  modified_latent_nodes_xy = modifications$latent_node_xy,
                                                  modify_params_latent_node_angle = ifelse (!is.null(modifications$latent_node_angle), TRUE, FALSE) %||% FALSE,
                                                  modified_latent_nodes_angle = modifications$latent_node_angle,
                                                  apply_global_nodes = TRUE,
                                                  apply_global_edges = TRUE,
                                                  apply_global_annotations = TRUE,
                                                  flip_layout = group_storage$last_flip_sem_layout %||% FALSE,
                                                  flip_direction = group_storage$last_flip_sem_layout_direction,
                                                  rotate_layout = group_storage$last_rotate_sem_layout %||% FALSE,
                                                  rotate_angle = group_storage$last_rotate_sem_layout_angle %||% 0,
                                                  which_group = group_id %||% "1"
    )
  } else if (inherits(bundleObject, c("sem_graph"))) {
    graph_data <- generate_graph_from_tidySEM1(sem_paths, fit_delta,
                                               relative_x_position = group_storage$last_relative_x_position %||% 25,
                                               relative_y_position = group_storage$last_relative_y_position %||% 25,
                                               center_x = group_storage$last_center_x_position %||% 0,
                                               center_y = group_storage$last_center_y_position %||% 0,
                                               latent_shape = group_storage$last_latent_shape %||% "circle",
                                               observed_shape = group_storage$last_observed_shape %||% "square",
                                               int_shape = group_storage$last_int_shape %||% "triangle",
                                               point_size_latent = group_storage$last_point_size_latent %||% 20,
                                               point_size_observed = group_storage$last_point_size_observed %||% 12,
                                               point_size_int = group_storage$last_point_size_int %||% 10,
                                               width_height_ratio_latent = group_storage$last_width_height_ratio_latent %||% 1,
                                               width_height_ratio_observed = group_storage$last_width_height_ratio_observed %||% 1,
                                               width_height_ratio_int = group_storage$last_width_height_ratio_int %||% 1,
                                               line_width = group_storage$last_line_width %||% 1,
                                               line_alpha = group_storage$last_line_alpha %||% 1,
                                               text_size_latent = group_storage$last_text_size_latent %||% 18,
                                               text_font_latent = group_storage$last_text_font_latent %||% "sans",
                                               text_color_latent = group_storage$last_text_color_latent %||% "#FFFFFF",
                                               text_alpha_latent = group_storage$last_text_alpha_latent %||% 1,
                                               text_fontface_latent = group_storage$last_text_fontface_latent %||% "plain",
                                               text_size_others = group_storage$last_text_size_others %||% 16,
                                               text_font_others = group_storage$last_text_font_others %||% "sans",
                                               text_color_others = group_storage$last_text_color_others %||% "#FFFFFF",
                                               text_alpha_others = group_storage$last_text_alpha_others %||% 1,
                                               text_fontface_others = group_storage$last_text_fontface_others %||% "plain",
                                               text_size_edges = group_storage$last_text_size_edges %||% 14,
                                               text_font_edges = group_storage$last_text_font_edges %||% "sans",
                                               text_color_edges = group_storage$last_text_color_edges %||% "#000000",
                                               text_color_fill = group_storage$last_text_color_fill %||% "#FFFFFF",
                                               text_alpha_edges = group_storage$last_text_alpha_edges %||% 1,
                                               text_fontface_edges = group_storage$last_text_fontface_edges %||% "plain",
                                               point_color_latent = group_storage$last_point_color_latent %||% "#cc3d3d",
                                               point_color_observed = group_storage$last_point_color_observed %||% "#1262b3",
                                               point_color_int = group_storage$last_point_color_int %||% "#0f993d",
                                               edge_color = group_storage$last_edge_color %||% "#000000",
                                               line_endpoint_spacing = group_storage$last_line_endpoint_spacing %||% 0.2,
                                               node_border_color = group_storage$last_node_border_color %||% "#FFFFFF",
                                               node_border_width = group_storage$last_node_border_width %||% 1,
                                               arrow_type = group_storage$last_arrow_type %||% "closed",
                                               arrow_size = group_storage$last_arrow_size %||% 0.1,
                                               lavaan_arrow_location = group_storage$last_lavaan_arrow_location %||% "end",
                                               zoom_factor = zoom_level, # user's input (keep as is)
                                               lavaan_curvature_magnitude = group_storage$last_lavaan_curvature_magnitude %||% 0.5,
                                               lavaan_rotate_curvature = group_storage$last_lavaan_rotate_curvature %||% FALSE,
                                               lavaan_curvature_asymmetry = group_storage$last_lavaan_curvature_asymmetry %||% 0,
                                               lavaan_curved_x_shift = group_storage$last_lavaan_curved_x_shift %||% 0,
                                               lavaan_curved_y_shift = group_storage$last_lavaan_curved_y_shift %||% 0,
                                               highlight_free_path = group_storage$last_highlight_free_path %||% FALSE,
                                               ff_params_edge = group_storage$last_ff_params_edge %||% NULL,
                                               ff_params_edgelabel = group_storage$last_ff_params_edgelabel %||% NULL,
                                               ff_params_loop = group_storage$last_ff_params_loop %||% NULL,
                                               ff_params_looplabel = group_storage$last_ff_params_looplabel %||% NULL,
                                               highlight_free_path_multi_group = group_storage$last_highlight_free_path_multi_group %||% FALSE,
                                               ff_params_edge_multi = group_storage$last_ff_params_edge_multi %||% NULL,
                                               ff_params_edgelabel_multi = group_storage$last_ff_params_edgelabel_multi %||% NULL,
                                               ff_params_loop_multi = group_storage$last_ff_params_loop_multi %||% NULL,
                                               ff_params_looplabel_multi = group_storage$last_ff_params_looplabel_multi %||% NULL,
                                               highlight_sig_path = group_storage$last_highlight_sig_path %||% FALSE,
                                               sig_path_color = group_storage$last_sig_path_color %||% "#000000",
                                               non_sig_path_color = group_storage$last_non_sig_path_color %||% "#000000",
                                               sig_label_fontface = group_storage$last_sig_label_fontface %||% "plain",
                                               non_sig_label_fontface = group_storage$last_non_sig_label_fontface %||% "plain",
                                               highlight_multi_group = group_storage$last_highlight_multi_group %||% FALSE,
                                               sig_diff_edge = group_storage$last_sig_diff_edge,
                                               sig_diff_edgelabel = group_storage$last_sig_diff_edgelabel,
                                               sig_diff_loop = group_storage$last_sig_diff_loop,
                                               sig_diff_looplabel = group_storage$last_sig_diff_looplabel,
                                               residuals = group_storage$last_residuals %||% FALSE,
                                               residuals_orientation_type = group_storage$last_residuals_orientation_type %||% 'Graded',
                                               lavaan_loop_offset = group_storage$last_lavaan_loop_offset %||% 0.8,
                                               lavaan_radius = group_storage$last_lavaan_radius %||% 2.5,
                                               lavaan_line_color_loop = group_storage$last_lavaan_line_color_loop %||% "#000000",
                                               lavaan_line_alpha_loop = group_storage$last_lavaan_line_alpha_loop %||% 1,
                                               lavaan_arrow_type_loop = group_storage$last_lavaan_arrow_type_loop %||% "closed",
                                               lavaan_arrow_size_loop = group_storage$last_lavaan_arrow_size_loop %||% 0.08,
                                               lavaan_width_loop = group_storage$last_lavaan_width_loop %||% 1,
                                               lavaan_height_loop = group_storage$last_lavaan_height_loop %||% 1,
                                               lavaan_gap_size_loop = group_storage$last_lavaan_gap_size_loop %||% 0.05,
                                               lavaan_two_way_arrow_loop = group_storage$last_lavaan_two_way_arrow_loop %||% TRUE,
                                               data_file = group_storage$data_file,
                                               modify_params_edge = ifelse (!is.null(modifications$edge), TRUE, FALSE) %||% FALSE,
                                               modified_edges = modifications$edge,
                                               modify_params_edgelabel = ifelse (!is.null(modifications$edgelabel), TRUE, FALSE) %||% FALSE,
                                               modified_edgelabels = modifications$edgelabel,
                                               modify_params_edgelabel_xy = ifelse (!is.null(modifications$edgelabel_xy), TRUE, FALSE) %||% FALSE,
                                               modified_edgelabels_xy = modifications$edgelabel_xy,
                                               modify_params_edgelabel_text = ifelse (!is.null(modifications$edgelabel_text), TRUE, FALSE) %||% FALSE,
                                               modified_edgelabels_text = modifications$edgelabel_text,
                                               modify_params_node = ifelse (!is.null(modifications$node), TRUE, FALSE) %||% FALSE,
                                               modified_nodes = modifications$node,
                                               modify_params_node_xy = ifelse (!is.null(modifications$node_xy), TRUE, FALSE) %||% FALSE,
                                               modified_nodes_xy = modifications$node_xy,
                                               modify_params_edge_xy = ifelse (!is.null(modifications$edge_xy), TRUE, FALSE) %||% FALSE,
                                               modified_edges_xy = modifications$edge_xy,
                                               modify_params_cov_edge = ifelse (!is.null(modifications$cov_edge), TRUE, FALSE) %||% FALSE,
                                               modified_cov_edges = modifications$cov_edge,
                                               modify_params_nodelabel = ifelse (!is.null(modifications$nodelabel), TRUE, FALSE) %||% FALSE,
                                               modified_nodelabels = modifications$nodelabel,
                                               modify_params_nodelabel_xy = ifelse (!is.null(modifications$nodelabel_xy), TRUE, FALSE) %||% FALSE,
                                               modified_nodelabels_xy = modifications$nodelabel_xy,
                                               modify_params_nodelabel_text = ifelse (!is.null(modifications$nodelabel_text), TRUE, FALSE) %||% FALSE,
                                               modified_nodelabels_text = modifications$nodelabel_text,
                                               modify_params_latent_node_xy = ifelse (!is.null(modifications$latent_node_xy), TRUE, FALSE) %||% FALSE,
                                               modified_latent_nodes_xy = modifications$latent_node_xy,
                                               modify_params_latent_node_angle = ifelse (!is.null(modifications$latent_node_angle), TRUE, FALSE) %||% FALSE,
                                               modified_latent_nodes_angle = modifications$latent_node_angle,
                                               modify_params_loop = ifelse (!is.null(modifications$loop), TRUE, FALSE) %||% FALSE,
                                               modified_loops = modifications$loop,
                                               modify_params_loop_xy = ifelse (!is.null(modifications$loop_xy), TRUE, FALSE) %||% FALSE,
                                               modified_loops_xy = modifications$loop_xy,
                                               modify_params_loop_location = ifelse (!is.null(modifications$loop_location), TRUE, FALSE) %||% FALSE,
                                               modified_loops_location = modifications$loop_location,
                                               modify_params_looplabel = ifelse (!is.null(modifications$looplabel), TRUE, FALSE) %||% FALSE,
                                               modified_looplabels = modifications$looplabel,
                                               modify_params_looplabel_xy = ifelse (!is.null(modifications$looplabel_xy), TRUE, FALSE) %||% FALSE,
                                               modified_looplabels_xy = modifications$looplabel_xy,
                                               modify_params_looplabel_text = ifelse (!is.null(modifications$looplabel_text), TRUE, FALSE) %||% FALSE,
                                               modified_looplabels_text = modifications$looplabel_text,
                                               loop_names_remove = group_storage$last_loop_names_remove_hi %||% NULL,
                                               flip_layout = group_storage$last_flip_sem_layout %||% FALSE,
                                               flip_direction = group_storage$last_flip_sem_layout_direction,
                                               rotate_layout = group_storage$last_rotate_sem_layout %||% FALSE,
                                               rotate_angle = group_storage$last_rotate_sem_layout_angle %||% 0,
                                               which_group = group_id %||% "1",
                                               group_level = group_storage$last_group_level
    )

  } else {

    if (lavaan_layout == 'layout_ai') {

      node_coords <- group_storage$node_coords0 # layout by AI
      node_coords$x <- (node_coords$x - mean(range(node_coords$x))) * group_storage$last_relative_x_position +
        group_storage$last_center_x_position
      node_coords$y <- (node_coords$y - mean(range(node_coords$y))) * group_storage$last_relative_y_position +
        group_storage$last_center_y_position

    } else {
      node_coords <- generate_sem_node_coords(sem_paths, relative_x_position = group_storage$last_relative_x_position,
                                              relative_y_position = group_storage$last_relative_y_position,
                                              center_x = group_storage$last_center_x_position,
                                              center_y = group_storage$last_center_y_position,
                                              flip_layout = group_storage$last_flip_sem_layout,
                                              flip_direction = group_storage$last_flip_sem_layout_direction,
                                              rotate_layout = group_storage$last_rotate_sem_layout,
                                              rotate_angle = group_storage$last_rotate_sem_layout_angle)
    }

    graph_data <- generate_graph_from_sempaths1(sem_paths, node_coords,
                                                latent_shape = group_storage$last_latent_shape %||% "circle",
                                                observed_shape = group_storage$last_observed_shape %||% "square",
                                                int_shape = group_storage$last_int_shape %||% "triangle",
                                                point_size_latent = group_storage$last_point_size_latent %||% 20,
                                                point_size_observed = group_storage$last_point_size_observed %||% 12,
                                                point_size_int = group_storage$last_point_size_int %||% 10,
                                                width_height_ratio_latent = group_storage$last_width_height_ratio_latent %||% 1,
                                                width_height_ratio_observed = group_storage$last_width_height_ratio_observed %||% 1,
                                                width_height_ratio_int = group_storage$last_width_height_ratio_int %||% 1,
                                                line_width = group_storage$last_line_width %||% 1,
                                                line_alpha = group_storage$last_line_alpha %||% 1,
                                                text_size_latent = group_storage$last_text_size_latent %||% 18,
                                                text_font_latent = group_storage$last_text_font_latent %||% "sans",
                                                text_color_latent = group_storage$last_text_color_latent %||% "#FFFFFF",
                                                text_alpha_latent = group_storage$last_text_alpha_latent %||% 1,
                                                text_fontface_latent = group_storage$last_text_fontface_latent %||% "plain",
                                                text_size_others = group_storage$last_text_size_others %||% 16,
                                                text_font_others = group_storage$last_text_font_others %||% "sans",
                                                text_color_others = group_storage$last_text_color_others %||% "#FFFFFF",
                                                text_alpha_others = group_storage$last_text_alpha_others %||% 1,
                                                text_fontface_others = group_storage$last_text_fontface_others %||% "plain",
                                                text_size_edges = group_storage$last_text_size_edges %||% 14,
                                                text_font_edges = group_storage$last_text_font_edges %||% "sans",
                                                text_color_edges = group_storage$last_text_color_edges %||% "#000000",
                                                text_color_fill = group_storage$last_text_color_fill %||% "#FFFFFF",
                                                text_alpha_edges = group_storage$last_text_alpha_edges %||% 1,
                                                text_fontface_edges = group_storage$last_text_fontface_edges %||% "plain",
                                                point_color_latent = group_storage$last_point_color_latent %||% "#cc3d3d",
                                                point_color_observed = group_storage$last_point_color_observed %||% "#1262b3",
                                                point_color_int = group_storage$last_point_color_int %||% "#0f993d",
                                                edge_color = group_storage$last_edge_color %||% "#000000",
                                                line_endpoint_spacing = group_storage$last_line_endpoint_spacing %||% 0.2,
                                                node_border_color = group_storage$last_node_border_color %||% "#FFFFFF",
                                                node_border_width = group_storage$last_node_border_width %||% 1,
                                                arrow_type = group_storage$last_arrow_type %||% "closed",
                                                arrow_size = group_storage$last_arrow_size %||% 0.1,
                                                lavaan_arrow_location = group_storage$last_lavaan_arrow_location %||% "end",
                                                zoom_factor = zoom_level, # user's input (keep as is since it's direct input)
                                                lavaan_curvature_magnitude = group_storage$last_lavaan_curvature_magnitude %||% 0.5,
                                                lavaan_rotate_curvature = group_storage$last_lavaan_rotate_curvature %||% FALSE,
                                                lavaan_curvature_asymmetry = group_storage$last_lavaan_curvature_asymmetry %||% 0,
                                                lavaan_curved_x_shift = group_storage$last_lavaan_curved_x_shift %||% 0,
                                                lavaan_curved_y_shift = group_storage$last_lavaan_curved_y_shift %||% 0,
                                                highlight_free_path = group_storage$last_highlight_free_path %||% FALSE,
                                                ff_params_edge = group_storage$last_ff_params_edge %||% NULL,
                                                ff_params_edgelabel = group_storage$last_ff_params_edgelabel %||% NULL,
                                                ff_params_loop = group_storage$last_ff_params_loop %||% NULL,
                                                ff_params_looplabel = group_storage$last_ff_params_looplabel %||% NULL,
                                                highlight_free_path_multi_group = group_storage$last_highlight_free_path_multi_group %||% FALSE,
                                                ff_params_edge_multi = group_storage$last_ff_params_edge_multi %||% NULL,
                                                ff_params_edgelabel_multi = group_storage$last_ff_params_edgelabel_multi %||% NULL,
                                                ff_params_loop_multi = group_storage$last_ff_params_loop_multi %||% NULL,
                                                ff_params_looplabel_multi = group_storage$last_ff_params_looplabel_multi %||% NULL,
                                                highlight_sig_path = group_storage$last_highlight_sig_path %||% FALSE,
                                                sig_path_color = group_storage$last_sig_path_color %||% "#000000",
                                                non_sig_path_color = group_storage$last_non_sig_path_color %||% "#000000",
                                                sig_label_fontface = group_storage$last_sig_label_fontface %||% "plain",
                                                non_sig_label_fontface = group_storage$last_non_sig_label_fontface %||% "plain",
                                                highlight_multi_group = group_storage$last_highlight_multi_group %||% FALSE,
                                                sig_diff_edge = group_storage$last_sig_diff_edge,
                                                sig_diff_edgelabel = group_storage$last_sig_diff_edgelabel,
                                                sig_diff_loop = group_storage$last_sig_diff_loop,
                                                sig_diff_looplabel = group_storage$last_sig_diff_looplabel,
                                                residuals = group_storage$last_residuals %||% FALSE,
                                                residuals_orientation_type = group_storage$last_residuals_orientation_type %||% 'Graded',
                                                lavaan_loop_offset = group_storage$last_lavaan_loop_offset %||% 0.8,
                                                lavaan_radius = group_storage$last_lavaan_radius %||% 2.5,
                                                lavaan_line_color_loop = group_storage$last_lavaan_line_color_loop %||% "#000000",
                                                lavaan_line_alpha_loop = group_storage$last_lavaan_line_alpha_loop %||% 1,
                                                lavaan_arrow_type_loop = group_storage$last_lavaan_arrow_type_loop %||% "closed",
                                                lavaan_arrow_size_loop = group_storage$last_lavaan_arrow_size_loop %||% 0.08,
                                                lavaan_width_loop = group_storage$last_lavaan_width_loop %||% 1,
                                                lavaan_height_loop = group_storage$last_lavaan_height_loop %||% 1,
                                                lavaan_gap_size_loop = group_storage$last_lavaan_gap_size_loop %||% 0.05,
                                                lavaan_two_way_arrow_loop = group_storage$last_lavaan_two_way_arrow_loop %||% TRUE,
                                                data_file = group_storage$data_file,
                                                modify_params_edge = ifelse (!is.null(modifications$edge), TRUE, FALSE) %||% FALSE,
                                                modified_edges = modifications$edge,
                                                modify_params_edgelabel = ifelse (!is.null(modifications$edgelabel), TRUE, FALSE) %||% FALSE,
                                                modified_edgelabels = modifications$edgelabel,
                                                modify_params_edgelabel_xy = ifelse (!is.null(modifications$edgelabel_xy), TRUE, FALSE) %||% FALSE,
                                                modified_edgelabels_xy = modifications$edgelabel_xy,
                                                modify_params_edgelabel_text = ifelse (!is.null(modifications$edgelabel_text), TRUE, FALSE) %||% FALSE,
                                                modified_edgelabels_text = modifications$edgelabel_text,
                                                modify_params_node = ifelse (!is.null(modifications$node), TRUE, FALSE) %||% FALSE,
                                                modified_nodes = modifications$node,
                                                modify_params_node_xy = ifelse (!is.null(modifications$node_xy), TRUE, FALSE) %||% FALSE,
                                                modified_nodes_xy = modifications$node_xy,
                                                modify_params_edge_xy = ifelse (!is.null(modifications$edge_xy), TRUE, FALSE) %||% FALSE,
                                                modified_edges_xy = modifications$edge_xy,
                                                modify_params_cov_edge = ifelse (!is.null(modifications$cov_edge), TRUE, FALSE) %||% FALSE,
                                                modified_cov_edges = modifications$cov_edge,
                                                modify_params_nodelabel = ifelse (!is.null(modifications$nodelabel), TRUE, FALSE) %||% FALSE,
                                                modified_nodelabels = modifications$nodelabel,
                                                modify_params_nodelabel_xy = ifelse (!is.null(modifications$nodelabel_xy), TRUE, FALSE) %||% FALSE,
                                                modified_nodelabels_xy = modifications$nodelabel_xy,
                                                modify_params_nodelabel_text = ifelse (!is.null(modifications$nodelabel_text), TRUE, FALSE) %||% FALSE,
                                                modified_nodelabels_text = modifications$nodelabel_text,
                                                modify_params_latent_node_xy = ifelse (!is.null(modifications$latent_node_xy), TRUE, FALSE) %||% FALSE,
                                                modified_latent_nodes_xy = modifications$latent_node_xy,
                                                modify_params_latent_node_angle = ifelse (!is.null(modifications$latent_node_angle), TRUE, FALSE) %||% FALSE,
                                                modified_latent_nodes_angle = modifications$latent_node_angle,
                                                modify_params_loop = ifelse (!is.null(modifications$loop), TRUE, FALSE) %||% FALSE,
                                                modified_loops = modifications$loop,
                                                modify_params_loop_xy = ifelse (!is.null(modifications$loop_xy), TRUE, FALSE) %||% FALSE,
                                                modified_loops_xy = modifications$loop_xy,
                                                modify_params_loop_location = ifelse (!is.null(modifications$loop_location), TRUE, FALSE) %||% FALSE,
                                                modified_loops_location = modifications$loop_location,
                                                modify_params_looplabel = ifelse (!is.null(modifications$looplabel), TRUE, FALSE) %||% FALSE,
                                                modified_looplabels = modifications$looplabel,
                                                modify_params_looplabel_xy = ifelse (!is.null(modifications$looplabel_xy), TRUE, FALSE) %||% FALSE,
                                                modified_looplabels_xy = modifications$looplabel_xy,
                                                modify_params_looplabel_text = ifelse (!is.null(modifications$looplabel_text), TRUE, FALSE) %||% FALSE,
                                                modified_looplabels_text = modifications$looplabel_text,
                                                loop_names_remove = group_storage$last_loop_names_remove_hi %||% NULL,
                                                which_group = group_id %||% "1"
    )
  }
  return(graph_data)
}
