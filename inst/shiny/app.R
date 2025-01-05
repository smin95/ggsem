required_packages <- c(
  "shiny", "ggplot2", "igraph", "DT", "colourpicker",
  "grid", "svglite", "grDevices", "lavaan",
  "semPlot", "cowplot", "dplyr", "Rtsne", "umap"
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

library(shiny)
library(ggplot2)
library(igraph)
library(DT)
library(colourpicker)
library(grid)
library(svglite)
library(grDevices)
library(lavaan)
library(semPlot)
library(dplyr)

plot.new()

valid_hex <- function(x) {
  if (grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x)) {
    return(x)
  } else if (is.na(x)) {
    return(NA)
  } else {
    return("#000000") # Default to black or another fallback color
  }
}

valid_line_style <- function(x) {
  valid_styles <- c("dotted", "dashed", "solid")
  if (x %in% valid_styles) {
    return(x)
  } else {
    return("solid") # Default to solid
  }
}

valid_fontface <- function(x) {
  valid_faces <- c("plain", "bold", "italic")
  if (x %in% valid_faces) {
    return(x)
  } else {
    return("plain") # Default to plain
  }
}

valid_font <- function(x) {
  valid_fonts <- c("sans", "mono", "serif")
  if (x %in% valid_fonts) {
    return(x)
  } else {
    return("sans") # Default to sans
  }
}

valid_type <- function(x) {
  valid_types <- c("Straight Line", "Straight Arrow", "Curved Line", "Curved Arrow")
  if (x %in% valid_types) {
    return(x)
  } else {
    return("Straight Line") # Default to "Straight Line"
  }
}

valid_gradient_position <- function(x) {
  x <- as.numeric(x)
  if (!is.na(x) && x >= 0 && x <= 1) {
    return(x)
  } else {
    return(0.5) # Default to midpoint
  }
}

valid_alpha <- function(x) {
  x <- as.numeric(x)
  if (!is.na(x) && x >= 0 && x <= 1) {
    return(x)
  } else {
    return(1) # Default to fully opaque
  }
}

valid_shape <- function(x) {
  valid_shapes <- c("circle", "square", "oval", "triangle", "rectangle", "diamond")
  if (x %in% valid_shapes) {
    return(x)
  } else {
    return("circle") # Default to circle
  }
}

valid_logical <- function(x) {
  x <- toupper(x)
  if (x %in% c("TRUE", "T", "YES", "1")) {
    return(TRUE)
  } else if (x %in% c("FALSE", "F", "NO", "0")) {
    return(FALSE)
  } else {
    return(FALSE) # Default to FALSE
  }
}

adjust_axis_range <- function(plot,
                              x_range = NULL,
                              y_range = NULL,
                              buffer_percent = 0,
                              fixed_aspect_ratio = TRUE) {

  axis_ranges <- get_axis_range(plot)
  current_x_range <- axis_ranges$x_range
  current_y_range <- axis_ranges$y_range

  new_x_range <- if (!is.null(x_range)) x_range else current_x_range
  new_y_range <- if (!is.null(y_range)) y_range else current_y_range

  x_buffer <- (new_x_range[2] - new_x_range[1]) * (buffer_percent / 100)
  y_buffer <- (new_y_range[2] - new_y_range[1]) * (buffer_percent / 100)

  new_x_range <- c(new_x_range[1] - x_buffer, new_x_range[2] + x_buffer)
  new_y_range <- c(new_y_range[1] - y_buffer, new_y_range[2] + y_buffer)

  if (fixed_aspect_ratio) {
    x_width <- diff(new_x_range)
    y_height <- diff(new_y_range)
    aspect_ratio <- y_height / x_width

    if (aspect_ratio > 1) {
      # Adjust x_range to match aspect ratio
      x_center <- mean(new_x_range)
      new_x_range <- c(x_center - y_height / 2, x_center + y_height / 2)
    } else {
      # Adjust y_range to match aspect ratio
      y_center <- mean(new_y_range)
      new_y_range <- c(y_center - x_width / 2, y_center + x_width / 2)
    }
  }

  adjusted_plot <- plot +
    coord_cartesian(xlim = new_x_range, ylim = new_y_range)

  return(adjusted_plot)
}


get_axis_range <- function(plot) {
  plot_build <- ggplot_build(plot)
  y_range <- plot_build$layout$panel_params[[1]]$y.range
  x_range <- plot_build$layout$panel_params[[1]]$x.range
  res <- list(x_range, y_range)
  names(res) <- c('x_range', 'y_range')
  return(res)
}

save_figure <- function(filename, plot, units = "in", dpi = 300, aspect_ratio = NULL,
                        scale_factor = 0.11, ...) {

  axis_ranges <- get_axis_range(plot)

  x_range <- axis_ranges$x_range
  y_range <- axis_ranges$y_range

  x_span <- diff(x_range)
  y_span <- diff(y_range)

  # Determine width and height
  if (!is.null(aspect_ratio)) {
    height <- y_span * scale_factor
    width <- height * aspect_ratio
  } else {
    width <- x_span * scale_factor
    height <- y_span * scale_factor
  }

  ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    ...
  )
}

rescale_values <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
  (x - from[1]) / diff(from) * diff(to) + to[1]
}

generate_graph_from_network <- function(network_data_file,
                                        directed = TRUE,
                                        layout_method = "fr",
                                        layout_width = 1,
                                        layout_height = 1, x_center = 0, y_center = 0,
                                        node_shape = "circle",
                                        node_size = 10, node_fill_color = "#1262b3",
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
                                        edge_label_alpha = 1, edge_label_fontface = "plain",
                                        zoom_factor = 1.2,
                                        annotate_nodes = TRUE,
                                        annotate_edges = TRUE,
                                        existing_points = NULL,
                                        random_seed = NULL,
                                        use_clustering = FALSE,
                                        clustering_method = "louvain",
                                        cluster_palette = 'rainbow',
                                        dim_reduction_method = "tsne") {

  if (!is.null(random_seed)) {
    set.seed(random_seed)  # Set the seed if provided
  }

  if (layout_method == 'dim_reduction') {
    use_dim_reduction <- TRUE
  } else use_dim_reduction <- FALSE

  network_df <- read.csv(network_data_file)
  if ("source" %in% colnames(network_df) && "target" %in% colnames(network_df)) {
    # If it's an edge list
    edges <- network_df
  } else if (is.matrix(network_df) || is.data.frame(network_df)) {
    network_df <- read.csv(network_data_file, row.names = 1)
    # If it's an adjacency matrix, convert to edge list
    if (is.null(colnames(network_df)) || is.null(rownames(network_df))) {
      stop("Adjacency matrix must have row and column names representing node identifiers.")
    }

    edges <- which(network_df != 0, arr.ind = TRUE) %>%
      as.data.frame() %>%
      rename(source = row, target = col) %>%
      mutate(
        source = rownames(network_df)[source],
        target = colnames(network_df)[target],
        weight = as.vector(network_df[which(network_df != 0, arr.ind = TRUE)]) # Include weights if matrix is weighted
      )
    if (all(edges$weight == 1)) {
      annotate_edges <- FALSE
    }

  } else {
    stop("Invalid input: The input must be either an edge list or adjacency matrix.")
  }

  unique_nodes <- unique(c(edges$source, edges$target))
  nodes <- data.frame(node = unique_nodes)

  edge_list <- as.data.frame(edges[, c("source", "target")])

  graph <- graph_from_data_frame(d = edges, vertices = nodes, directed = directed)

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

    sm_colors <-  c('#7f404a', '#5b4080', '#408073', '#8c994d', '#cc9666',
                    '#cc1489', '#1262b3', '#cc3d3d',
                    '#da73e6', '#66b1cc', '#0f993d', '#7f5d0d', '#7b3dcc',
                    '#45e0e6', '#63e617', '#e57717', '#c9b9c6', '#ffe764',
                    '#ffb359', '#9ee1a8')

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
      "smplot2" = function(n) head(sm_colors,n)
    )

    max_colors <- palette_max_colors[[cluster_palette]]

    # Assign colors based on the number of clusters and the chosen palette
    if (cluster_palette != "rainbow" && num_clusters > max_colors) {
      showNotification(
        paste(cluster_palette, "supports a maximum of", max_colors, "colors. Falling back to Rainbow palette."),
        type = "warning",
        duration = 5
      )
      palette_function <- rainbow
    }

    # Generate colors
    node_colors <- palette_function(num_clusters)[nodes$community]
  } else {
    # If clustering is disabled, use default node color
    node_colors <- node_fill_color
  }


  if (is.null(existing_points)) {
    if (use_dim_reduction) {
      adjacency_matrix <- as.matrix(as_adjacency_matrix(graph))
      num_nodes <- nrow(adjacency_matrix)

      if (num_nodes < 3) {
        showNotification(
          "Not enough nodes for dimensionality reduction. Falling back to Fruchterman-Reingold layout.",
          type = "warning",
          duration = 5
        )
        layout <- layout_with_fr(graph) %>%
          as.data.frame() %>%
          rename(x = V1, y = V2) %>%
          mutate(node = V(graph)$name)
      } else {
        layout <- tryCatch({
          if (dim_reduction_method == "tsne") {
            tsne_perplexity <- max(5, min(30, num_nodes - 1))
            Rtsne::Rtsne(adjacency_matrix, perplexity = tsne_perplexity, verbose = FALSE)$Y %>%
              as.data.frame() %>%
              rename(x = V1, y = V2) %>%
              mutate(node = V(graph)$name)
          } else if (dim_reduction_method == "umap") {
            umap_neighbors <- max(2, min(15, num_nodes - 1))  # Dynamically set neighbors
            umap::umap(adjacency_matrix, n_neighbors = umap_neighbors)$layout %>%
              as.data.frame() %>%
              rename(x = V1, y = V2) %>%
              mutate(node = V(graph)$name)

          } else if (dim_reduction_method == "pca") {
            prcomp(adjacency_matrix, center = TRUE, scale. = TRUE)$x[, 1:2] %>%
              as.data.frame() %>%
              rename(x = PC1, y = PC2) %>%
              mutate(node = V(graph)$name)
          } else {
            stop("Invalid dimensionality reduction method selected.")
          }
        }, error = function(e) {
          showNotification(
            paste("Dimensionality reduction failed:", e$message, "Falling back to Fruchterman-Reingold layout."),
            type = "warning",
            duration = 5
          )
          layout_with_fr(graph) %>%  # Fallback to Fruchterman-Reingold
            as.data.frame() %>%
            rename(x = V1, y = V2) %>%
            mutate(node = V(graph)$name)
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
      ) %>%
        as.data.frame() %>%
        rename(x = V1, y = V2) %>%
        mutate(node = V(graph)$name)
    }
    #print('hello world')
    layout_min_x <- min(layout$x)
    layout_min_y <- min(layout$y)
    layout_max_x <- max(layout$x)
    layout_max_y <- max(layout$y)

    layout <- layout %>%
      mutate(
        x = (x - layout_min_x) / (layout_max_x - layout_min_x) * layout_width + x_center - layout_width / 2,
        y = (y - layout_min_y) / (layout_max_y - layout_min_y) * layout_height + y_center - layout_height / 2
      )
  } else {
    layout <- existing_points %>%
      select(x, y) %>%
      mutate(node = nodes$node)
  }

  points <- layout %>%
    left_join(nodes, by = c("node" = "node")) %>%
    mutate(
      shape = node_shape,
      color = if (!is.null(nodes$color)) nodes$color else node_colors,
      size = if (!is.null(nodes$size)) nodes$size else node_size,
      border_color = if (!is.null(nodes$border_color)) nodes$border_color else node_border_color,
      border_width = if (!is.null(nodes$border_width)) nodes$border_width else node_border_width,
      alpha = 1,
      width_height_ratio = node_width_height_ratio,
      orientation = 0,
      lavaan = FALSE,
      network = TRUE,
      locked = TRUE
    ) %>%
    select(
      x, y, shape, color, size, border_color, border_width, alpha, width_height_ratio, orientation,
      lavaan, network, locked
    )


  edges <- edges %>%
    mutate(
      edge_id = pmin(source, target),
      edge_pair = pmax(source, target)
    ) %>%
    group_by(edge_id, edge_pair) %>%
    summarise(
      source = first(source),
      target = first(target),
      weight = mean(weight, na.rm = TRUE),
      two_way = n() > 1,
      .groups = "drop"
    )


  if ("weight" %in% colnames(edges) && scale_by_weight) {
    edges <- edges %>%
      mutate(
        scaled_width = rescale_values(weight, to = c(min_edge_width, max_edge_width))
      )
  } else {
    edges <- edges %>%
      mutate(scaled_width = line_width)
  }

  # Prepare lines data frame

  lines <- edges %>%
    left_join(layout, by = c("source" = "node")) %>%
    rename(x_start = x, y_start = y) %>%
    left_join(layout, by = c("target" = "node")) %>%
    rename(x_end = x, y_end = y) %>%
    mutate(
      ctrl_x = NA,
      ctrl_y = NA,
      type = ifelse(directed, "Straight Arrow", "Straight Line"),
      color = line_color,
      end_color = NA, # Default end color for gradients
      color_type = "Single", # Default color type
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
      locked = TRUE
    ) %>%
    select(
      x_start, y_start, x_end, y_end, ctrl_x, ctrl_y, type, color, end_color,
      color_type, gradient_position, width, alpha, arrow, arrow_type,
      arrow_size, two_way, lavaan, network, line_style, locked
    )

  node_mapping <- setNames(seq_along(nodes$node), nodes$node)
  numeric_edge_list <- matrix(
    c(node_mapping[edges$source], node_mapping[edges$target]),
    ncol = 2
  )

  lines <- adjust_edge_coordinates(
    lines_df = lines,
    edge_list = numeric_edge_list,
    points_df = points,
    auto_endpoint_spacing = line_endpoint_spacing,
    zoom_factor = zoom_factor
  )

  weight_annotations <- if (annotate_edges == TRUE) {
    if ("weight" %in% colnames(edges)) {
      lines %>%
        mutate(weight = edges$weight) %>%
        mutate(
          text = as.character(edges$weight),
          x = (x_start + x_end) / 2,
          y = (y_start + y_end) / 2,
          font = edge_label_font,
          size = edge_label_size,
          color = edge_label_color,
          angle = 0,
          alpha = edge_label_alpha,
          fontface = edge_label_fontface,
          math_expression = FALSE,
          lavaan = FALSE,
          network = TRUE,
          locked = FALSE
        ) %>%
        select(text, x, y, font, size, color, angle, alpha, fontface, math_expression, lavaan, network, locked)
    } else {
      data.frame(
        text = character(), x = numeric(), y = numeric(), font = character(), size = numeric(),
        color = character(), angle = numeric(), alpha = numeric(), fontface = character(),
        math_expression = logical(), lavaan = logical(), network = logical(), locked = logical(),
        stringsAsFactors = FALSE
      )
    }
  } else {
    data.frame(
      text = character(), x = numeric(), y = numeric(), font = character(), size = numeric(),
      color = character(), angle = numeric(), alpha = numeric(), fontface = character(),
      math_expression = logical(), lavaan = logical(), network = logical(), locked = logical(),
      stringsAsFactors = FALSE
    )
  }

  if (annotate_nodes == TRUE) {
    annotations <- points %>%
      mutate(
        text = if ("label" %in% colnames(nodes)) nodes$label else if ("node" %in% colnames(layout)) layout$node else NA, # Use node name as default text
        font = node_label_font,
        size = node_label_size,
        color = node_label_color,
        angle = 0,
        alpha = node_label_alpha,
        fontface = node_label_fontface,
        math_expression = FALSE,
        lavaan = FALSE,
        network = TRUE,
        locked = TRUE
      ) %>%
      select(text, x, y, font, size, color, angle, alpha, fontface, math_expression, lavaan, network, locked) %>%
      bind_rows(weight_annotations)
  } else {
    data.frame(
      text = character(), x = numeric(), y = numeric(), font = character(), size = numeric(),
      color = character(), angle = numeric(), alpha = numeric(), fontface = character(),
      math_expression = logical(), lavaan = logical(), network = logical(), locked = logical(),
      stringsAsFactors = FALSE
    )
  }


  list(points = as.data.frame(points), lines = as.data.frame(lines),
       annotations = as.data.frame(annotations))
}


adjust_edge_coordinates <- function(lines_df, edge_list, points_df, auto_endpoint_spacing = 0, zoom_factor = 1) {
  for (i in 1:nrow(lines_df)) {
    start_index <- edge_list[i, 1]
    end_index <- edge_list[i, 2]

    # cat("Processing edge:", i, "Start Index:", start_index, "End Index:", end_index, "\n")

    if (start_index == end_index) {
      warning(paste("Skipping self-loop at row", i, "Node Index:", start_index))
      next
    }

    start_shape <- points_df$shape[start_index]
    end_shape <- points_df$shape[end_index]

    start_x <- points_df$x[start_index]
    start_y <- points_df$y[start_index]
    end_x <- points_df$x[end_index]
    end_y <- points_df$y[end_index]

    start_size <- points_df$size[start_index]
    end_size <- points_df$size[end_index]

    start_width_height_ratio <- points_df$width_height_ratio[start_index] %||% 1
    end_width_height_ratio <- points_df$width_height_ratio[end_index] %||% 1

    start_orientation <- points_df$orientation[start_index]
    end_orientation <- points_df$orientation[end_index]

    start_dimensions <- list(size = start_size, width_height_ratio = start_width_height_ratio)
    end_dimensions <- list(size = end_size, width_height_ratio = end_width_height_ratio)

    # Adjust the endpoint coordinates
    adjusted_coords <- adjust_endpoint(
      x1 = start_x, y1 = start_y,
      x2 = end_x, y2 = end_y,
      spacing = auto_endpoint_spacing,
      shape1 = start_shape, shape2 = end_shape,
      dimensions1 = start_dimensions,
      dimensions2 = end_dimensions,
      orientation1 = start_orientation, orientation2 = end_orientation
    )

    # Update lines_df with adjusted coordinates
    lines_df$x_start[i] <- adjusted_coords$x_start
    lines_df$y_start[i] <- adjusted_coords$y_start
    lines_df$x_end[i] <- adjusted_coords$x_end
    lines_df$y_end[i] <- adjusted_coords$y_end
  }
  return(lines_df)
}

#
find_intersection <- function(x_center, y_center, x_target, y_target,
                              size, width_height_ratio = 1, orientation = 0,
                              shape = "circle", zoom_factor = 1) {
  dx <- x_target - x_center
  dy <- y_target - y_center

  width <- (size / 3) / zoom_factor
  min_size_factor <- 0.25
  scale_factor <- sqrt(2)

  if (shape == "circle") {
    width <- size * min_size_factor # / scale_factor
    height <- width
  } else if (shape == "square") {
    width <- size * min_size_factor # scale_factor
    height <- width
  } else if (shape == "triangle") {
    width <- size * sqrt(4 / sqrt(3)) * min_size_factor / scale_factor
    height <- width * sqrt(3) / 2
  } else if (shape == "rectangle") {
    height <- size * min_size_factor
    width <- height * width_height_ratio # * 0.65 # line longer
  } else if (shape == "diamond") {
    height <- size * 1.4 * sqrt(1.5) * min_size_factor / scale_factor
    width <- height * width_height_ratio
  } else if (shape == "oval") {
    height <- size * min_size_factor # / scale_factor
    width <- height * width_height_ratio
  }

  angle <- -orientation * pi / 180
  dx_rot <- cos(angle) * dx - sin(angle) * dy
  dy_rot <- sin(angle) * dx + cos(angle) * dy

  scale <- max(abs(dx_rot / (width / 2)), abs(dy_rot / (height / 2)))

  # Calculate intersection points
  x_intersect_rot <- dx_rot / scale
  y_intersect_rot <- dy_rot / scale

  x_intersect <- cos(-angle) * x_intersect_rot - sin(-angle) * y_intersect_rot
  y_intersect <- sin(-angle) * x_intersect_rot + cos(-angle) * y_intersect_rot

  # Translate back to the original center
  return(list(x = x_intersect + x_center, y = y_intersect + y_center))
}

adjust_endpoint <- function(x1, y1, x2, y2, spacing = 0,
                            shape1 = "circle", shape2 = "circle",
                            dimensions1 = list(width_height_ratio = 1, size = 1),
                            dimensions2 = list(width_height_ratio = 1, size = 1),
                            orientation1 = 0, orientation2 = 0, zoom_factor = 1) {
  # Find intersections
  start_intersect <- find_intersection(
    x_center = x1, y_center = y1,
    x_target = x2, y_target = y2,
    size = dimensions1$size, width_height_ratio = dimensions1$width_height_ratio,
    orientation = orientation1, shape = shape1,
    zoom_factor = zoom_factor
  )

  x1 <- start_intersect$x
  y1 <- start_intersect$y

  end_intersect <- find_intersection(
    x_center = x2, y_center = y2,
    x_target = x1, y_target = y1,
    size = dimensions2$size, width_height_ratio = dimensions2$width_height_ratio,
    orientation = orientation2, shape = shape2,
    zoom_factor = zoom_factor
  )

  x2 <- end_intersect$x
  y2 <- end_intersect$y

  # cat("Start Intersection: (", x1, ",", y1, ")\n")
  # cat("End Intersection: (", x2, ",", y2, ")\n")

  # Apply spacing adjustment
  if (spacing > 0) {
    dx <- x2 - x1
    dy <- y2 - y1
    distance <- sqrt(dx^2 + dy^2)
    if (distance > 0) {
      dx <- dx / distance
      dy <- dy / distance
      x1 <- x1 + spacing * dx
      y1 <- y1 + spacing * dy
      x2 <- x2 - spacing * dx
      y2 <- y2 - spacing * dy
    }
  }

  # cat("Adjusted Start: (", x1, ",", y1, ")\n")
  # cat("Adjusted End: (", x2, ",", y2, ")\n")

  return(list(x_start = x1, y_start = y1, x_end = x2, y_end = y2))
}

interpolate_points <- function(x_start, y_start, x_end, y_end, n = 100) {
  t <- seq(0, 1, length.out = n)
  x <- (1 - t) * x_start + t * x_end
  y <- (1 - t) * y_start + t * y_end
  data.frame(x = x, y = y)
}

rotate_coords <- function(x, y, angle, cx = 0, cy = 0) {
  angle_rad <- angle * pi / 180
  x_rot <- cos(angle_rad) * (x - cx) - sin(angle_rad) * (y - cy) + cx
  y_rot <- sin(angle_rad) * (x - cx) + cos(angle_rad) * (y - cy) + cy
  list(x = x_rot, y = y_rot)
}


default_control_point <- function(x_start, y_start, x_end, y_end, offset_ratio = 0.3) {
  mid_x <- (x_start + x_end) / 2
  mid_y <- (y_start + y_end) / 2

  dx <- x_end - x_start
  dy <- y_end - y_start
  offset_x <- -dy * offset_ratio
  offset_y <- dx * offset_ratio

  list(ctrl_x = mid_x + offset_x, ctrl_y = mid_y + offset_y)
}

create_bezier_curve <- function(x_start, y_start, x_end, y_end, ctrl_x, ctrl_y, n_points = 100) {
  t <- seq(0, 1, length.out = n_points)

  bezier_x <- (1 - t)^2 * x_start + 2 * (1 - t) * t * ctrl_x + t^2 * x_end
  bezier_y <- (1 - t)^2 * y_start + 2 * (1 - t) * t * ctrl_y + t^2 * y_end

  data.frame(x = bezier_x, y = bezier_y)
}


generate_graph_from_lavaan <- function(lavaan_string, sem_code = NULL, data_file = NULL, relative_x_position = 1, relative_y_position = 1,
                                       center_x = 0, center_y = 0,
                                       latent_shape = "circle", observed_shape = "square",
                                       int_shape = "triangle",
                                       point_size_latent = 40, point_size_observed = 40,
                                       point_size_int = 30,
                                       line_width = 1, text_size_latent = 20, text_font_latent = "serif",
                                       text_color_latent = 'black', text_alpha_latent = 1, text_fontface_latent = 'plain',
                                       text_size_others = 20, text_font_others = "serif",
                                       text_color_others = 'black', text_alpha_others = 1, text_fontface_others = 'plain',
                                       text_size_edges = 20, text_font_edges = "serif",
                                       text_color_edges = 'black', text_alpha_edges = 1, text_fontface_edges = 'plain',
                                       point_color_latent = "#000000", point_color_observed = "#000000",
                                       point_color_int = "b#000000",
                                       edge_color = "#000000", line_endpoint_spacing = 0,
                                       node_border_color = "white",
                                       node_border_width = 1, fontface = "plain",
                                       arrow_type = "open", arrow_size = 0.2,
                                       layout_algorithm = "tree", data = NULL,
                                       lavaan_arrow_location = "end",
                                       zoom_factor = 1.2,
                                       lavaan_curvature_magnitude = 1,
                                       lavaan_rotate_curvature = 0.5,
                                       data_format = 'df') {

  model <- lavaan::lavaanify(lavaan_string)
  latent_vars <- unique(model$lhs[model$op == "=~"])    # Latent variables
  observed_vars <- unique(setdiff(model$rhs[model$op %in% c("=~", "~", "~~")], model$lhs[model$op == "=~"]))

  if (!is.null(data_file) && file.exists(data_file)) {
    if (data_format == "matrix") {
      data <- as.matrix(read.csv(data_file, row.names = 1, check.names = FALSE))
      if (!all(apply(data, c(1, 2), is.numeric))) {
        stop("Matrix data must contain numeric values only.")
      }
      # Ensure the matrix is symmetric
      if (!isSymmetric(data)) {
        stop("The matrix must be symmetric for lavaan compatibility.")
      }
    } else {
      data <- read.csv(data_file, check.names = FALSE)
    }

  } else {
    # Generate synthetic data
    data <- as.data.frame(matrix(rnorm(100 * length(observed_vars)), nrow = 100))
    colnames(data) <- observed_vars
  }

  if (!is.null(sem_code) && (!grepl("lavaan_string", sem_code) || !grepl("data", sem_code))) {
    stop("Custom SEM code must include `lavaan_string` and `data`.")
  }

  allowed_functions <- c("sem", "cfa", "growth", "efa")
  if (!any(sapply(allowed_functions, grepl, sem_code))) {
    stop("Custom SEM code must use a function from the `lavaan` package.")
  }

  fit <- tryCatch({
    if (!is.null(sem_code) && sem_code != "") {
      if (!grepl("lavaan_string", sem_code) || !grepl("data", sem_code)) {
        stop("Custom SEM code must include `lavaan_string` and `data`.")
      }
      eval(parse(text = sem_code))
    } else {
      sem(lavaan_string, data = data)
    }
  }, error = function(e) {
    stop("Error in SEM model: ", e$message)
  })

  param_est <- lavaan::parameterEstimates(fit)
  edge_params <- param_est[param_est$op %in% c("=~", "~", "~~"), ]

  intercepts <- c()
  is_growth_model <- any(sapply(allowed_functions, grepl, sem_code)) && grepl("growth", sem_code)
  # print(param_est)

  # print(intercepts) # debug
  sem_paths <- semPlot::semPaths(fit, layout = layout_algorithm, what = "paths", plot = FALSE)

  # Extract node coordinates and node names
  node_coords <- as.data.frame(sem_paths$layout)
  colnames(node_coords) <- c("x", "y")

  # Normalize coordinates to center the graph
  node_coords$x <- (node_coords$x - mean(range(node_coords$x))) * relative_x_position + center_x
  node_coords$y <- (node_coords$y - mean(range(node_coords$y))) * relative_y_position + center_y

  node_names <- names(sem_paths$graphAttributes$Nodes$labels)
  node_coords$name <- node_names
  #print(node_names)

  edges_from <- model$lhs[model$op %in% c("=~", "~", "~~")]
  edges_to <- model$rhs[model$op %in% c("=~", "~", "~~")]
  edge_op <- model$op[model$op %in% c("=~", "~", "~~")]

  intercept_sources <- character(0)
  if (is_growth_model) {
    intercepts <- unique(param_est$lhs[param_est$op == "~1"])   # Detect intercepts for growth models
    intercept_indices <- which(node_names == "1")
    intercept_targets <- param_est$lhs[param_est$op == "~1"]
    intercept_sources <- paste0("Intercept_", seq_along(intercept_indices))
    node_names[intercept_indices] <- intercept_sources

    if (length(intercept_targets) > 0) {
      edges_from <- c(edges_from, intercept_sources)
      edges_to <- c(edges_to, intercept_targets)
      edge_op <- c(edge_op, rep("~1", length(intercept_targets)))
    }
  }

  node_shapes <- ifelse(node_names %in% intercept_sources, int_shape, # Triangular shape for the intercept node
                        # ifelse(node_names %in% intercepts, int_shape,
                        ifelse(node_names %in% latent_vars, latent_shape, observed_shape)
  )
  node_colors <- ifelse(node_names %in% intercept_sources, point_color_int,
                        ifelse(node_names %in% latent_vars, point_color_latent, point_color_observed)
  )
  node_sizes <- ifelse(node_names %in% intercept_sources, point_size_int,
                       ifelse(node_names %in% latent_vars, point_size_latent, point_size_observed)
  )

  node_width_height_ratios <- ifelse(node_shapes %in% c("rectangle", "oval"), 1.6, 1)

  # Create the data frames for points, lines and texts

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
    locked = TRUE,
    stringsAsFactors = FALSE
  )

  # Create annotations data frame
  annotations <- data.frame(
    text = node_coords$name,
    x = node_coords$x,
    y = node_coords$y,
    font = ifelse(node_names %in% latent_vars, text_font_latent, text_font_others),
    size = ifelse(node_names %in% latent_vars, text_size_latent, text_size_others),
    color = ifelse(node_names %in% latent_vars, text_color_latent, text_color_others),
    angle = 0,
    alpha = ifelse(node_names %in% latent_vars, text_alpha_latent, text_alpha_others),
    fontface = ifelse(node_names %in% latent_vars, text_fontface_latent, text_fontface_others),
    math_expression = FALSE,
    lavaan = TRUE,
    network = FALSE,
    locked = TRUE,
    stringsAsFactors = FALSE
  )

  if (length(edges_from) == 0 || length(edges_to) == 0) {
    stop("No edges found in the model. Check the Lavaan syntax.")
  }

  lines_df_pre <- data.frame( # pre-adjustment
    x_start = node_coords[match(edges_from, node_names), "x"],
    y_start = node_coords[match(edges_from, node_names), "y"],
    x_end = node_coords[match(edges_to, node_names), "x"],
    y_end = node_coords[match(edges_to, node_names), "y"],
    ctrl_x = NA,
    ctrl_y = NA,
    type = ifelse(edge_op == "~~", "Curved Arrow", "Straight Arrow"), # Determine type dynamically
    color = edge_color,
    end_color = NA,
    color_type = "Single",
    gradient_position = NA,
    width = line_width,
    alpha = 1,
    arrow = TRUE,
    arrow_type = arrow_type,
    arrow_size = arrow_size,
    two_way = edge_op == "~~", # Boolean flag for bidirectional edges
    lavaan = TRUE,
    network = FALSE,
    line_style = "solid",
    locked = TRUE,
    stringsAsFactors = FALSE
  )
  # print(lines_df)
  edge_list <- cbind(match(edges_from, node_names), match(edges_to, node_names))
  # print(edge_list)
  lines_df <- adjust_edge_coordinates(
    lines_df = lines_df_pre,
    edge_list = edge_list,
    points_df = points_df, # `points_df` contains the node attributes
    auto_endpoint_spacing = line_endpoint_spacing,
    zoom_factor = zoom_factor
  )

  # print(lines_df$two_way)
  if (any(lines_df$two_way)) {
    two_way_indices <- which(lines_df$two_way)

    control_points <- mapply(
      function(x_start, y_start, x_end, y_end) {
        mid_x <- (x_start + x_end) / 2
        mid_y <- (y_start + y_end) / 2
        dx <- x_end - x_start
        dy <- y_end - y_start

        offset_x <- -dy * lavaan_curvature_magnitude
        offset_y <- dx * lavaan_curvature_magnitude

        ctrl_x <- mid_x + offset_x
        ctrl_y <- mid_y + offset_y

        # Apply 180° rotation if the option is selected
        if (lavaan_rotate_curvature == TRUE) {
          ctrl_x <- 2 * mid_x - ctrl_x
          ctrl_y <- 2 * mid_y - ctrl_y
        }

        list(ctrl_x = ctrl_x, ctrl_y = ctrl_y)
      },
      x_start = lines_df$x_start[two_way_indices],
      y_start = lines_df$y_start[two_way_indices],
      x_end = lines_df$x_end[two_way_indices],
      y_end = lines_df$y_end[two_way_indices],
      SIMPLIFY = FALSE
    )

    # Assign the calculated control points to lines_df
    lines_df$ctrl_x[two_way_indices] <- sapply(control_points, `[[`, "ctrl_x")
    lines_df$ctrl_y[two_way_indices] <- sapply(control_points, `[[`, "ctrl_y")
    lines_df$locked[two_way_indices] <- FALSE
  }


  if (exists("lavaan_arrow_location") && lavaan_arrow_location == "start") {
    for (i in seq_len(nrow(lines_df))) {
      # Swap start and end coordinates for each edge
      temp_x <- lines_df$x_start[i]
      temp_y <- lines_df$y_start[i]
      lines_df$x_start[i] <- lines_df$x_end[i]
      lines_df$y_start[i] <- lines_df$y_end[i]
      lines_df$x_end[i] <- temp_x
      lines_df$y_end[i] <- temp_y
    }
  }

  lines_df0 <- lines_df # make a copy for edge labels

  if (!is.null(data_file) && file.exists(data_file)) {
    lines_df0$from <- edges_from
    lines_df0$to <- edges_to

    edge_labels <- edge_params$est
    edge_params$key <- paste0(edge_params$lhs, "_", edge_params$rhs)

    # self-loop removal (variance/error terms)
    edge_params$est <- ifelse(edge_params$lhs == edge_params$rhs, "", edge_params$est)
    # print(edge_params)
    edge_params$pvalue <- ifelse(edge_params$lhs == edge_params$rhs, "", edge_params$pvalue)

    lines_df0$key <- paste0(lines_df0$from, "_", lines_df0$to)
    lines_df0$text <- edge_params$est[match(lines_df0$key, edge_params$key)]
    lines_df0$text[is.na(lines_df0$text)] <- ""

    lines_df0$text <- paste0(
      round(as.double(edge_params$est[match(lines_df0$key, edge_params$key)]), 3), # Round estimates
      ifelse(
        !is.na(edge_params$pvalue[match(lines_df0$key, edge_params$key)]) &
          edge_params$pvalue[match(lines_df0$key, edge_params$key)] < 0.05,
        "*",
        "" # Append "*" if p-value < 0.05
      )
    )
    lines_df0$text <- ifelse(grepl("NA", lines_df0$text), "", lines_df0$text)
    lines_df0$text <- ifelse(lines_df0$text == "1", "", lines_df0$text) # remove label for fixed parameter
    # print(lines_df0)

    filtered_lines_df0 <- lines_df0[lines_df0$text != "" & !is.na(lines_df0$text), ]
    label_coords <- data.frame(
      x = (filtered_lines_df0$x_start + filtered_lines_df0$x_end) / 2,
      y = (filtered_lines_df0$y_start + filtered_lines_df0$y_end) / 2,
      text = filtered_lines_df0$text,
      font = text_font_edges,
      size = text_size_edges,
      color = text_color_edges,
      angle = 0,
      alpha = text_alpha_edges,
      fontface = text_fontface_edges,
      math_expression = FALSE,
      lavaan = TRUE,
      network = FALSE,
      locked = FALSE,
      stringsAsFactors = FALSE
    )
    # print(label_coords)
    annotations <- rbind(annotations, label_coords)
  }
  start_indices <- edge_list[, 1]
  end_indices <- edge_list[, 2]

  # Remove rows where start_index equals end_index (self-loop lines)
  valid_indices <- start_indices != end_indices
  lines_df <- lines_df[valid_indices, ]
  edge_list <- edge_list[valid_indices, ]

  return(list(points = points_df, lines = lines_df, annotations = annotations))
}

auto_generate_edges <- function(points_data, layout_type = "fully_connected", line_color = "#000000",
                                line_width = 2, line_alpha = 1, line_style = "solid", random_prob = 0.1, particular_node = NULL,
                                auto_endpoint_spacing = 0, zoom_factor = 1.2, random_seed = NULL) {

  if (!is.null(random_seed)) {
    set.seed(random_seed)  # Set the seed if provided
  }

  # Filter out locked nodes
  unlocked_points <- points_data[
    !points_data$locked & !points_data$lavaan,
    c("x", "y", "shape", "size", "width_height_ratio", "orientation")
  ]

  if (nrow(unlocked_points) < 2) {
    return(NULL)
  }

  # Extract coordinates for unlocked points
  coord_matrix <- as.matrix(unlocked_points[, c("x", "y")])

  edge_list <- NULL

  # Fully connected layout
  if (layout_type == "fully_connected") {
    g <- make_full_graph(nrow(unlocked_points))
    edge_list <- as_edgelist(g)
  } else if (layout_type == "nearest_neighbor") {
    g <- make_empty_graph(n = nrow(unlocked_points))
    dist_matrix <- as.matrix(dist(coord_matrix))
    for (i in 1:nrow(dist_matrix)) {
      dist_matrix[i, i] <- Inf
      nearest_neighbor <- which.min(dist_matrix[i, ])
      if (!is.na(nearest_neighbor)) {
        g <- add_edges(g, c(i, nearest_neighbor))
      }
    }
    edge_list <- as_edgelist(g)
  } else if (layout_type == "connect_to_central_node") {
    center_point <- colMeans(coord_matrix)
    dist_to_center <- apply(coord_matrix, 1, function(row) sqrt(sum((row - center_point)^2)))
    central_node_index <- which.min(dist_to_center)
    edge_list <- cbind(central_node_index, setdiff(1:nrow(coord_matrix), central_node_index))
  } else if (layout_type == "connect_to_particular_node") {
    if (!is.null(particular_node)) {
      selected_node <- which(rownames(unlocked_points) == particular_node)
      if (length(selected_node) == 0) {
        return(NULL) # If selected node = invalid, exit
      }

      edge_list <- cbind(selected_node, setdiff(1:nrow(unlocked_points), selected_node))
    }
  } else if (layout_type == "random_graph") {
    g <- erdos.renyi.game(nrow(unlocked_points), p.or.m = random_prob, directed = FALSE)
    edge_list <- as_edgelist(g)
  }

  # Check if edge_list is valid and has proper indices
  if (!is.null(edge_list) && nrow(edge_list) > 0 && all(edge_list[, 1] <= nrow(coord_matrix)) && all(edge_list[, 2] <= nrow(coord_matrix))) {
    lines_df <- data.frame(
      x_start = coord_matrix[edge_list[, 1], 1],
      y_start = coord_matrix[edge_list[, 1], 2],
      x_end = coord_matrix[edge_list[, 2], 1],
      y_end = coord_matrix[edge_list[, 2], 2],
      ctrl_x = NA,
      ctrl_y = NA,
      type = "Auto-generated",
      color = line_color,
      end_color = NA,
      color_type = "Single",
      gradient_position = NA,
      width = line_width,
      alpha = line_alpha,
      arrow = FALSE,
      arrow_type = NA,
      arrow_size = NA,
      two_way = FALSE,
      lavaan = FALSE,
      network = FALSE,
      line_style = line_style,
      locked = FALSE,
      stringsAsFactors = FALSE
    )

    lines_df <- adjust_edge_coordinates(lines_df, edge_list, unlocked_points, auto_endpoint_spacing, zoom_factor)

    return(lines_df)
  } else {
    return(NULL)
  }
}


auto_layout_points <- function(points_data, layout_type = "layout_in_circle", distance = 1,
                               center_x = 0, center_y = 0, orientation = 0,
                               random_seed = NULL) {

  if (!is.null(random_seed)) {
    set.seed(random_seed)  # Set the seed if provided
  }

  if (!"locked" %in% names(points_data)) {
    points_data$locked <- FALSE
  }

  unlocked_points <- points_data[!points_data$locked & !points_data$lavaan, ]

  if (layout_type == "straight_line") {
    n <- nrow(unlocked_points)
    angle_rad <- orientation * pi / 180
    dx <- cos(angle_rad)
    dy <- sin(angle_rad)

    unlocked_points$x <- seq(center_x - distance * (n - 1) / 2 * dx,
                             center_x + distance * (n - 1) / 2 * dx,
                             length.out = n
    )
    unlocked_points$y <- seq(center_y - distance * (n - 1) / 2 * dy,
                             center_y + distance * (n - 1) / 2 * dy,
                             length.out = n
    )
  } else {
    g <- make_empty_graph(n = nrow(unlocked_points))
    layout_fun <- match.fun(layout_type)
    layout_coords <- layout_fun(g) * distance

    angle_rad <- orientation * pi / 180
    rotated_coords <- data.frame(
      x = layout_coords[, 1] * cos(angle_rad) - layout_coords[, 2] * sin(angle_rad),
      y = layout_coords[, 1] * sin(angle_rad) + layout_coords[, 2] * cos(angle_rad)
    )
    unlocked_points$x <- rotated_coords$x + center_x
    unlocked_points$y <- rotated_coords$y + center_y
  }

  points_data[!points_data$locked & !points_data$lavaan, ] <- unlocked_points
  return(points_data)
}


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .sidebar {
        height: 100vh;
        overflow-y: auto;
        padding: 15px;
        background-color: #eff1f3;
      }
      .conditional-panel {
      background-color: #eff1f3; /* Light blue-gray background */
      color: #333; /* Dark text */
      border: none;
      border-radius: 10px; /* Rounded corners */
      padding: 20px; /* Internal spacing */
      }
      /* Sidebar headers */
      .sidebar h4, .sidebar h5 {
        color: black; /* Lighter text color for headers */
        font-weight: bold;
      }
      .scrollable-tables {
      height: 400px;
      overflow-y: auto;
      border: 1px solid #ddd;
      padding: 10px;
      background-color: white;
      font-size: 14px;
    }
    .scrollable-tables h4 {
      margin-top: 20px;
    }
    /* Style for toggle buttons */
      .toggle-button {
        cursor: pointer;
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 5px 0;
      }
      .toggle-button:hover {
        text-decoration: underline;
      }
      /* Chevron rotation */
      .toggle-button .fas {
        transition: transform 0.2s;
      }
      .toggle-button.collapsed .fas {
        transform: rotate(-90deg);
      }
      .redo-button {
      background-color: #f0f1f3;
      color: black;
      border: none;
      border-radius: 5px;
      padding: 10px;
      font-size: 16px;
      cursor: pointer;
      }
      .redo-button:hover {
      background-color: #d4d4d4; /* Darker on hover */
      color: black;
      border: none;
      border-radius: 5px;
      padding: 10px;
      font-size: 17px;
      cursor: pointer;
      transition: all 0.3s ease;
      }
      .redo-button-main {
      background-color: #f0f1f3;
      color: black;
      border: none;
      border-radius: 5px;
      padding: 10px;
      font-size: 18px;
      cursor: pointer;
      }
      .redo-button-main:hover {
      background-color: #d4d4d4; /* Darker on hover */
      color: black;
      border: none;
      border-radius: 5px;
      padding: 10px;
      font-size: 19px;
      cursor: pointer;
      transition: all 0.3s ease;
      }
      .redo-button0 {
      background-color: white;
      color: black;
      border: none;
      border-radius: 5px;
      padding: 10px;
      font-size: 14px;
      cursor: pointer;
      }
      .redo-button0:hover {
      background-color: #f0f1f3; /* Darker on hover */
      color: black;
      border: none;
      border-radius: 5px;
      padding: 10px;
      font-size: 14px;
      cursor: pointer;
      transition: all 0.3s ease;
      }
      .custom-select .selectize-control {
      background-color: #eff0f3;
      border: 1px solid #d1d5d8;
      border-radius: 4px;
      padding: 2px;
      box-shadow: none;
      }

      /* Input field inside the dropdown */
      .custom-select .selectize-input {
      background-color: white;
      color: #333333;
      font-size: 14px;
      border: none;
      box-shadow: none;
      padding: 8px;
      }

      /* Dropdown menu items */
      .custom-select .selectize-dropdown {
      background-color: white;
      border: 1px solid #d1d5d8;
      border-radius: 4px;
      box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
      }

      /* Hover effect for dropdown items */
      .custom-select .selectize-dropdown-content .option:hover {
      background-color: #eff1f3;
      color: #000000;
      }

      /* Tab headers hover effect */
      .nav-tabs > li > a:hover {
        background-color: #e2e8f0;
        color: #1a202c;
        border-color: #cbd5e0;
        transition: all 0.3s ease;
      }

      /* Active tab header style */
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover {
        background-color: #eff1f3;
        color: #333333;
        border: 1px solid #d1d5d8;
      }

      /* Adjust tab panel hover (content area inside tabs) */
      .tab-content:hover {
        background-color: #f8fafc;
        transition: background-color 0.3s ease;
      }

      /* Table row hover inside tabs */
      .tab-content table.dataTable tbody tr:hover {
        background-color: #eff1f3;
        color: #1a202c;
        transition: background-color 0.3s ease, color 0.3s ease;
      }

      /* Table header hover inside tabs */
      .tab-content table.dataTable thead th:hover {
        background-color: #d9dee1;
        color: #000; /* Darker text color */
        transition: background-color 0.3s ease, color 0.3s ease;
      }
    "))
  ),
  tags$head(
    tags$title("ggsem: Interactive & Reproducible Visualizations of SEM Diagrams")
  ),
  titlePanel(
    tags$a(
      href = "https://smin95.github.io/ggsem",
      target = "_blank",
      style = "text-decoration: none; color: inherit; display: block; text-align: center; margin: 20px 0;",
      HTML(
        paste(
          "<div style='font-size: 28px; font-weight: bold; text-transform: lowercase; color: #4B5563; '>
          ggsem:
        </div>",
          "<div style='font-size: 18px; color: #1F2937; letter-spacing: 2px;'>
          Interactive ",
          as.character(icon("mouse-pointer", style = "margin-left: 6px; color: #1F2937;")),
          " and Reproducible ",
          as.character(icon("sync-alt", style = "margin-left: 6px; color: #4B5563;")),
          "</div>",
          "<div style='font-size: 20px; background: linear-gradient(to right, #6B7280, #4B5563); -webkit-background-clip: text; color: transparent;'>
          Visualizations of Networks and SEM Diagrams
        </div>",
          as.character(icon("circle-nodes", style = "margin-top: 10px; color: #6B7280; font-size: 24px;")),
          as.character(icon("project-diagram", style = "margin-top: 10px; color: #6B7280; font-size: 24px;"))
        )
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar", # Apply the custom CSS class
      width = 4,
      h4(
        tagList(
          icon("mouse-pointer", style = "margin-right: 8px;"),
          "Element Selection"
        )
      ),
      div(
        class = "custom-select", # Apply custom class
        selectInput(
          "element_type",
          "Choose Element Type:",
          choices = c("Point", "Line", "Text Annotation", "Self-loop Arrow", "SEM Data", "Network Data")
        )
      ),

      # Layer ordering elements
      selectInput(
        "layer_order",
        label = tagList(
          icon("layer-group", style = "margin-right: 8px;"),
          "Select Layer Order:"
        ),
        choices = list(
          "Points in front" = "points_front",
          "Lines in front" = "lines_front",
          "Annotations in front" = "annotations_front",
          "Self-loop Arrows in front" = "loops_front"
        ),
        selected = "points_front"
      ),
      div(style = "margin-top: 10px;"),
      # Zoom control slider
      fluidRow(
        column(
          4,
          sliderInput(
            "zoom",
            label = HTML(paste(
              icon("search-minus", style = "margin-right: 6px;"), "Zoom Out:"
            )),
            min = .8, max = 3.2, value = 1.2, step = 0.1
          )
        ),
        column(
          4,
          sliderInput(
            "horizontal_shift",
            label = HTML(paste(
              icon("arrows-alt-h", style = "margin-right: 8px;"), "X-Level:"
            )),
            min = -50, max = 50, value = 0, step = 2
          )
        ),
        column(
          4,
          sliderInput(
            "vertical_shift",
            label = HTML(paste(
              icon("arrows-alt-v", style = "margin-right: 8px;"), "Y-Level:"
            )),
            min = -50, max = 50, value = 0, step = 2
          )
        )
      ),
      fluidRow(
        column(
          12, # Make the column span the full width
          div(
            class = "text-center", # Bootstrap class for centering content
            actionButton("undo_button", class = "redo-button", label = tagList(icon("undo"), "Undo")),
            actionButton("redo_button", class = "redo-button", label = tagList(icon("redo"), "Redo"))
          )
        )
      ),
      #div(style = "margin-top: 10px;"),
      conditionalPanel(
        condition = "input.element_type == 'Point'",
        #shiny::wellPanel(
        class = "conditional-panel",
        tags$div(
          class = "panel-group",
          style = "margin: 0; padding: 0;",
          tags$div(
            class = "toggle-button collapsed",
            `data-toggle` = "collapse",
            `data-target` = "#subPointInputs",
            `aria-expanded` = "false",
            `aria-controls` = "subPointInputs",
            tags$h4(
              tagList(
                tags$span(icon("plus-circle", style = "margin-right: 8px;"),
                          title = "Add points before drawing networks or node connections."),
                h5(HTML("<b style='font-size: 16px;'>Draw Individual Points</b>")),
                tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
              )
            )
          ),
          tags$div(
            id = "subPointInputs",
            class = "panel-collapse collapse",
            fluidRow(
              column(6, textInput("x_coord", "X Coordinate:", "0")),
              column(6, textInput("y_coord", "Y Coordinate:", "0"))
            ),
            shiny::wellPanel(
              fluidRow(
                column(6, colourInput("point_color", "Point Color:", value = "#000000")),
                column(6, div(class = "custom-select", selectInput("shape",
                                                                   HTML(paste(
                                                                     icon("shapes", style = "margin-right: 6px;"), # icon for shapes
                                                                     "Select Shape"
                                                                   )),
                                                                   choices = c("circle", "square", "rectangle", "oval", "triangle", "diamond")
                )))
              ),
              fluidRow(
                column(6, numericInput("point_size", "Point Size:", value = 15, min = 1)),
                column(6, numericInput("border_width", "Border Width:", value = 1, min = 0))
              ),
              fluidRow(
                column(6, colourInput("border_color", "Border Color:", value = "white")),
                column(6, numericInput("point_alpha", "Point Alpha:", value = 1, min = 0, max = 1, step = 0.1)) # Alpha input for points
              ),
              fluidRow(
                column(
                  6,
                  numericInput(
                    "point_orientation",
                    label = HTML(paste(
                      icon("sync-alt", style = "margin-right: 8px;"), "Orientation (Degrees):"
                    )),
                    value = 0,
                    min = 0,
                    max = 360,
                    step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.shape == 'rectangle' || input.shape == 'oval' || input.shape == 'diamond'",
                  column(
                    6,
                    div(
                      numericInput(
                        "width_height_ratio",
                        label = HTML(paste(
                          icon("ruler-combined", style = "margin-right: 8px;"), "Width/Height Ratio"
                        )),
                        value = 1.6,
                        min = 0.1,
                        step = 0.1
                      ),
                      tags$span(
                        icon("question-circle"),
                        title = "Adjust the ratio of width to height for rectangle, oval, and diamond shapes.",
                        style = "cursor: help; margin-left: 6px; color: #007bff;"
                      ),
                      style = "display: flex; align-items: center;"
                    )
                  )
                )
              ),
              tags$div(
                style = "position: relative;",
                #style = "position: absolute; bottom: 10px; right: 10px; font-size: 12px; color: #007bff;",
                tags$span(
                  icon("info-circle", style = "margin-right: 6px;"),
                  "These inputs support aesthetic grouping for unlocked points."
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            12,
            div(
              actionButton(
                "add_point",
                class = "redo-button-main",
                label = tagList(icon("plus-circle"), HTML("&nbsp;Add Point"))
              ),
              style = "text-align: center;" # Center the button
            )
          )
        ),
        div(style = "margin-top: 10px;"),
        tags$div(
          class = "panel-group",
          style = "margin: 0; padding: 0;",
          tags$div(
            class = "toggle-button collapsed",
            `data-toggle` = "collapse",
            `data-target` = "#subDrawNetworks",
            `aria-expanded` = "false",
            `aria-controls` = "subDrawNetworks",
            tags$h4(
              tagList(
                tags$span(icon("project-diagram", style = "margin-right: 8px;"),
                          title = "Points need to be drawn before using the network layout feature."),
                h5(HTML("<b style='font-size: 16px;'>Sort Points in Layout</b>")),
                tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
              )
            )
          ),
          tags$div(
            id = "subDrawNetworks",
            class = "panel-collapse collapse",
            fluidRow(
              column(12, selectInput("layout_type",
                                     HTML(paste(
                                       icon("project-diagram", style = "margin-right: 6px;"),
                                       "Layout Type"
                                     )),
                                     choices = c(
                                       "Circle" = "layout_in_circle",
                                       "Grid" = "layout_on_grid",
                                       "Random" = "layout_randomly",
                                       "Star" = "layout_as_star",
                                       "Fruchterman-Reingold" = "layout_with_fr",
                                       "Kamada-Kawai" = "layout_with_kk",
                                       "Straight Line" = "straight_line"
                                     )
              ))
            ),
            fluidRow(
              column(
                6,
                numericInput(
                  "point_distance",
                  HTML(paste(
                    icon("ruler-horizontal", style = "margin-right: 6px;"),
                    "Point Distance:"
                  )),
                  value = 10,
                  min = 0.1,
                  step = 0.1
                )
              ),
              column(
                6,
                numericInput(
                  "layout_orientation",
                  HTML(paste(
                    icon("sync-alt", style = "margin-right: 6px;"),
                    "Orientation (Degrees):"
                  )),
                  min = 0,
                  max = 360,
                  value = 0,
                  step = 1
                )
              )
            ),
            fluidRow(
              column(6, numericInput("center_x", "Center X Position:", value = 0)),
              column(6, numericInput("center_y", "Center Y Position:", value = 0))
            ),
            fluidRow(
              column(6, colourInput("grad_start_color", "Gradient Start Color:", value = "blue")),
              column(6, colourInput("grad_end_color", "Gradient End Color:", value = "red"))
            )
          )
        ),
        fluidRow(
          column(
            12,
            div(
              actionButton(
                "auto_layout",
                class = "redo-button-main",
                label = tags$span(icon("vector-square"), HTML("&nbsp;Auto-layout Points"), title = "Automatically position unlocked points into a selected layout type.")
              ),
              style = "display: flex; align-items: center; justify-content: center;" # Center horizontally
            )
          )
        ),
        fluidRow(
          column(
            12,
            div(
              actionButton(
                "apply_gradient",
                class = "redo-button-main",
                label = tags$span(icon("palette"), HTML("&nbsp;Apply Gradient"), title = "Apply a gradient color effect on unlocked points based on the selected start and end colors.")
              ),
              style = "display: flex; align-items: center; justify-content: center; gap: 10px;" # Center horizontally with gap
            )
          )
        ),
        fluidRow(
          column(
            12,
            div(
              actionButton(
                "lock_points",
                class = "redo-button-main",
                label = tags$span(icon("lock"), HTML("&nbsp;Lock Points"), title = "Prevent points from being moved or modified in the layout or forming automatic edges")
              ),
              style = "display: flex; align-items: center; justify-content: center; gap: 10px;" # Ensures alignment and spacing
            )
          )
        ),
        tags$div(
          class = "panel-group",
          style = "margin: 0; padding: 0;",
          tags$div(
            class = "toggle-button collapsed",
            `data-toggle` = "collapse",
            `data-target` = "#subAestheticGrouping",
            `aria-expanded` = "false",
            `aria-controls` = "subAestheticGrouping",
            tags$h4(
              tagList(
                tags$span(icon("object-group", style = "margin-right: 8px;"), title = "Apply changes in the positions (and aesthetics) of multiple unlocked points at once."),
                h5(HTML("<b style='font-size: 16px;'>Aesthetic Grouping</b>")),
                tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
              )
            )
          ),
          tags$div(
            id = "subAestheticGrouping",
            class = "panel-collapse collapse",
            fluidRow(
              column(6, checkboxInput(
                "bulk_shift_point_only",
                HTML(paste(
                  icon("map-marker-alt", title = "If checked, XY positions of unlocked points will also be shifted in group.",
                       style = "margin-right: 6px;"),
                  "Shift XY Only"
                )),
                value = TRUE
              )),
              column(6, checkboxInput(
                "bulk_aesthetics_point_only",
                HTML(paste(
                  icon("paint-brush", title = "If checked, aesthetics of unlocked points will be adjusted in group.",
                       style = "margin-right: 6px;"),
                  "Aesthetics Only"
                )),
                value = FALSE
              )),
            column(
              6,
              numericInput(
                "bulk_shift_x",
                label = HTML(paste(icon("arrows-alt-h", style = "margin-right: 6px;"), "Shift X")),
                value = 0,
                step = 0.1
              )
            ),
            column(
              6,
              numericInput(
                "bulk_shift_y",
                label = HTML(paste(icon("arrows-alt-v", style = "margin-right: 6px;"), "Shift Y")),
                value = 0,
                step = 0.1
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            actionButton(
              "apply_point_bulk_shift",
              class = "redo-button-main",
              label = tagList(icon("check-circle"), HTML("&nbsp;Apply Changes"))
            ),
            style = "display: flex; justify-content: center;"
          )
        )
      )
      #)
    ),
    # Line Inputs in conditionalPanel
    conditionalPanel(
      condition = "input.element_type == 'Line'",
      #shiny::wellPanel(
      class = "conditional-panel",
      tags$div(
        tags$div(
          class = "toggle-button collapsed",
          `data-toggle` = "collapse",
          `data-target` = "#subLineInputs",
          `aria-expanded` = "false",
          `aria-controls` = "subLineInputs",
          tags$h4(
            tagList(
              tags$span(icon("grip-lines", style = "margin-right: 8px;"), title = "Manually draw each line element."),
              h5(HTML("<b style='font-size: 16px;'>Draw Individual Lines</b>")),
              tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
            )
          )
        ),
        tags$div(
          id = "subLineInputs",
          class = "panel-collapse collapse",
          fluidRow(
            column(6, textInput("x_start", "Start X Coordinate:", "0")),
            column(6, textInput("y_start", "Start Y Coordinate:", "0"))
          ),
          fluidRow(
            column(6, textInput("x_end", "End X Coordinate:", "5")),
            column(6, textInput("y_end", "End Y Coordinate:", "5"))
          ),
          shiny::wellPanel(
            fluidRow(
              column(6, colourInput("line_color", "Start Color:", value = "#000000")),
              column(6, selectInput("color_type", "Line Color Type:", choices = c("Single", "Gradient")))
            ),
            conditionalPanel(
              condition = "input.color_type == 'Gradient'",
              fluidRow(
                column(6, colourInput("end_color", "End Color:", value = "white")),
                column(6, sliderInput("gradient_position", "Gradient Intersection:", min = 0.01, max = 0.99, value = 0.5, step = 0.01),
                       tags$span(
                         icon("question-circle"),
                         title = "The close to 0, the more gradient favors the end color.",
                         style = "cursor: help; margin-left: 6px; color: #007bff;"
                       ),
                       style = "display: flex; align-items: center;"
                )
              )
            ),
            fluidRow(
              column(6, numericInput("line_width", "Line Width:", value = 1, min = 1)),
              column(6, numericInput("line_alpha", "Line Alpha:", value = 1, min = 0, max = 1, step = 0.1))
            ),
            fluidRow(
              column(6, selectInput("line_type", "Line Type:", choices = c("Straight Line", "Straight Arrow", "Curved Line", "Curved Arrow"))),
              conditionalPanel(
                condition = "input.color_type == 'Single'",
                column(6, selectInput("line_style", "Line Style:", choices = c("solid", "dashed", "dotted")))
              )
            ),

            # Conditional display for curved lines
            conditionalPanel(
              condition = "input.line_type == 'Curved Line' || input.line_type == 'Curved Arrow'",
              fluidRow(
                column(6,
                       numericInput("ctrl_x", "Control Point X", value = 0),
                       tags$span(
                         icon("question-circle"),
                         title = "Do not modify this input unless you are adjusting Bezier curves manually.",
                         style = "cursor: help; margin-left: 6px; color: #007bff;"
                       ),
                       style = "display: flex; align-items: center;"
                ),
                column(6,
                       numericInput("ctrl_y", "Control Point Y", value = 0),
                       tags$span(
                         icon("question-circle"),
                         title = "Do not modify this input unless you are adjusting Bezier curves manually.",
                         style = "cursor: help; margin-left: 6px; color: #007bff;"
                       ),
                       style = "display: flex; align-items: center;"
                )
              ),
              fluidRow(
                column(6, sliderInput("curvature_magnitude", "Curvature Magnitude:", min = 0, max = 2, value = 0.3, step = 0.01)),
                column(6, checkboxInput("rotate_curvature", "Rotate Curvature 180°", value = FALSE))
              )
            ),

            # Conditional display for arrows
            conditionalPanel(
              condition = "input.line_type == 'Straight Arrow' || input.line_type == 'Curved Arrow'",
              fluidRow(
                column(6, selectInput("arrow_type", "Arrow Type:", choices = c("open", "closed"))),
                column(6, numericInput("arrow_size", "Arrow Size:", value = 0.2, min = 0.1, step = 0.1))
              ),
              fluidRow(
                column(6, checkboxInput("two_way_arrow", "Two-way Arrow", value = FALSE)) # two-way arrows checkbox
              )
            ),
            tags$div(
              style = "position: relative;",
              tags$span(
                icon("info-circle", style = "margin-right: 6px;"),
                "These inputs support aesthetic grouping for unlocked lines."
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            actionButton(
              "add_line",
              label = tagList(icon("arrows-alt-h"), HTML("&nbsp;&nbsp;Add Line")),
              class = "redo-button-main"
            ),
            style = "text-align: center;"
          )
        )
      ),
      #br(),
      tags$div(
        tags$div(
          class = "panel-group",
          style = "margin: 0; padding: 0;",
          # Header with toggle button
          tags$div(
            class = "toggle-button collapsed",
            `data-toggle` = "collapse",
            `data-target` = "#subAutoEdges",
            `aria-expanded` = "false",
            `aria-controls` = "subAutoEdges",
            tags$h4(
              tagList(
                tags$span(icon("project-diagram", style = "margin-right: 8px;"),
                          title =  "Define how nodes are connected. Ensure points (unlocked) are added before connecting them."),
                h5(HTML("<b style='font-size: 16px;'>Auto-generate Edges to Connect Nodes</b>")),
                tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
              )
            )
          )),
        tags$div(
          id = "subAutoEdges",
          class = "panel-collapse collapse",
          selectInput("edge_type", "Edge Type:", choices = c("Line", "Arrow"), selected = "Line"),
          selectInput("connection_type", "Choose Edge Connection Type:",
                      choices = c(
                        "Fully Connected" = "fully_connected",
                        "Nearest Neighbor" = "nearest_neighbor",
                        "Connect to Central Node" = "connect_to_central_node",
                        "Connect to Particular Node" = "connect_to_particular_node",
                        "Random Graph" = "random_graph"
                      ),
                      selected = "connect_to_central_node"
          ),
          conditionalPanel(
            condition = "input.connection_type == 'connect_to_particular_node'",
            selectInput("particular_node", "Select Central Node:", choices = NULL)
          ),
          fluidRow(
            column(6, colourInput("auto_line_color", "Edge Color:", value = "#000000")),
            column(6, div(
              numericInput("auto_endpoint_spacing", "Edge Spacing:", value = 0, min = 0, step = 0.1),
              tags$span(
                icon("question-circle"),
                title = "Adjusts the spacing between the endpoints of lines and their connected nodes.",
                style = "cursor: help; margin-left: 6px; color: #007bff;"
              ),
              style = "display: flex; align-items: right;"
            ))
          ),
          fluidRow(
            column(6, numericInput("auto_line_width", "Edge Width:", value = 1, min = 0.1, step = 0.1)),
            column(6, numericInput("auto_line_alpha", "Edge Alpha:", value = 1, min = 0, max = 1, step = 0.1))
          ),
          fluidRow(
            column(6, selectInput("auto_line_style", "Edge Line Style:",
                                  choices = c("solid", "dashed", "dotted"),
                                  selected = "solid"
            )),
            column(4, actionButton("lock_lines_button", label = HTML(paste(icon("lock"), "Lock Lines"))))
          ),
          conditionalPanel(
            condition = "input.edge_type == 'Arrow'",
            fluidRow(
              column(6, numericInput("arrow_size", "Arrow Size:", value = 0.2, min = 0.1, step = 0.1)),
              column(6, selectInput("arrow_type", "Arrow Type:", choices = c("open", "closed")))
            ),
            fluidRow(
              column(6, checkboxInput("two_way_arrow", "Two-way Arrow", value = FALSE)),
              column(6, selectInput("arrow_location", "Arrowhead Location:",
                                    choices = c("start", "end"),
                                    selected = "end"
              ))
            )
          ),
        )
      ),
      div(
        actionButton(
          "auto_generate_edges_button",
          class = "redo-button-main",
          label = tags$span(icon("project-diagram"), "Auto-generate Edges", title = "Automatically generate edges between unlocked points with a specific layout (but not locked points).")
        ),
        style = "display: flex; align-items: center; justify-content: center;" # Ensures spacing and centering
      ),
      tags$div(
        class = "panel-group",
        style = "margin: 0; padding: 0;",
        tags$div(
          class = "toggle-button collapsed",
          `data-toggle` = "collapse",
          `data-target` = "#subLineGrouping",
          `aria-expanded` = "false",
          `aria-controls` = "subLineGrouping",
          tags$h4(
            tagList(
              tags$span(icon("object-group", style = "margin-right: 8px;"), title = "Apply changes in the positions (and aesthetics) of multiple unlocked lines at once."),
              h5(HTML("<b style='font-size: 16px;'>Aesthetic Grouping</b>")),
              tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
            )
          )
        ),
        tags$div(
          id = "subLineGrouping",
          class = "panel-collapse collapse",
          fluidRow(
            column(6, checkboxInput(
              "bulk_shift_line_only",
              HTML(paste(
                icon("map-marker-alt", title = "If checked, XY positions of unlocked lines will be shifted in group.",
                     style = "margin-right: 6px;"),
                "Shift XY Only"
              )),
              value = TRUE
            )),
            column(6, checkboxInput(
              "bulk_aesthetics_line_only",
              HTML(paste(
                icon("palette", title = "If checked, aesthetics of unlocked lines such as width, color, and alpha will be adjusted.",
                     style = "margin-right: 6px;"),
                "Aesthetics Only"
              )),
              value = FALSE)
            ),
            column(
              6,
              numericInput(
                "line_bulk_shift_x",
                label = HTML(paste(icon("arrows-alt-h", style = "margin-right: 6px;"), "Shift X")),
                value = 0,
                step = 0.1
              )
            ),
            column(
              6,
              numericInput(
                "line_bulk_shift_y",
                label = HTML(paste(icon("arrows-alt-v", style = "margin-right: 6px;"), "Shift Y")),
                value = 0,
                step = 0.1
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            actionButton(
              "apply_line_bulk_shift",
              label = tagList(icon("check-circle"), HTML("&nbsp;Apply Changes")),
              class = "redo-button-main"
            ),
            style = "display: flex; justify-content: center;"
          )
        )
      )
      # )
    ),

    # Text Annotation Inputs
    conditionalPanel(
      condition = "input.element_type == 'Text Annotation'",
      #shiny::wellPanel(
      class = "conditional-panel",
      tags$div(
        class = "panel-group",
        style = "margin: 0; padding: 0;",
        tags$div(
          class = "toggle-button collapsed",
          `data-toggle` = "collapse",
          `data-target` = "#subTextAdd",
          `aria-expanded` = "false",
          `aria-controls` = "subTextAdd",
          tags$h4(
            tagList(
              icon("pencil-alt", style = "margin-right: 8px;"),
              h5(HTML("<b style='font-size: 16px;'>Draw Individual Annotations</b>")),
              tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
            )
          )
        ),
        tags$div(
          id = "subTextAdd",
          class = "panel-collapse collapse",
          fluidRow(
            column(12, textInput("annotation_text", "Text:", "Sample Text")),
          ),
          fluidRow(
            column(
              12,
              checkboxInput(
                "math_expression",
                HTML(paste(
                  icon("square-root-alt", style = "margin-right: 6px;"),
                  "Use Math Expression"
                )),
                value = FALSE
              ),
              tags$span(
                icon("question-circle"),
                title = "The syntax should follow the parse() function.",
                style = "cursor: help; margin-right: 6px; color: #007bff;"
              ),
              style = "display: flex; align-items: right;"
            )
          ),
          fluidRow(
            column(6, textInput("annotation_x", "X Coordinate:", "0")),
            column(6, textInput("annotation_y", "Y Coordinate:", "0"))
          ),
          shiny::wellPanel(
            fluidRow(
              column(6, selectInput("font_family", "Font:",
                                    choices = c("sans", "serif", "mono"),
                                    selected = "sans"
              )),
              column(6, numericInput("text_size", "Text Size:", value = 20, min = 1))
            ),
            fluidRow(
              column(6, colourInput("text_color", "Color:", value = "#000000")),
              column(6, numericInput("text_angle", "Angle (deg):", value = 0))
            ),
            fluidRow(
              column(6, numericInput("text_alpha", "Text Alpha:", value = 1, min = 0, max = 1, step = 0.1)),
              column(6, selectInput("text_typeface", "Fontface::", choices = c("Plain", "Bold", "Italic")))
            ),
            tags$div(
              style = "position: relative;",
              #style = "position: absolute; bottom: 10px; right: 10px; font-size: 12px; color: #007bff;",
              tags$span(
                icon("info-circle", style = "margin-right: 6px;"),
                "These inputs support aesthetic grouping for unlocked annotations."
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            actionButton(
              "add_annotation",
              label = tagList(icon("font"), HTML("&nbsp;&nbsp;Add Annotation")),
              class = "redo-button-main"
            ),
            style = "display: flex; justify-content: center;"
          )
        )
      ),
      #h4("Text Annotation Inputs"),
      tags$div(
        class = "panel-group",
        style = "margin: 0; padding: 0;",
        tags$div(
          class = "toggle-button collapsed",
          `data-toggle` = "collapse",
          `data-target` = "#subTextGroup",
          `aria-expanded` = "false",
          `aria-controls` = "subTextGroup",
          tags$h4(
            tagList(
              tags$span(icon("object-group", style = "margin-right: 8px;"),
                        title = "Apply changes in the positions (and aesthetics) of multiple unlocked annotations at once."),
              h5(HTML("<b style='font-size: 16px;'>Aesthetic Grouping</b>")),
              tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
            )
          )
        ),
        tags$div(
          id = "subTextGroup",
          class = "panel-collapse collapse",
          fluidRow(
            column(6, div(
              style = "display: flex; align-items: center;",
              checkboxInput(
                "bulk_shift_annotation_only",
                HTML(paste(
                  icon("map-marker-alt", title = "If checked, XY positions of unlocked annotations will be shifted in group.",
                       style = "margin-right: 6px;"),
                  "Shift XY Only"
                )),
                value = TRUE
              ))),
            column(6, div(
              style = "display: flex; align-items: center;",
              checkboxInput(
                "bulk_aesthetics_annotation_only",
                HTML(paste(
                  icon("palette", title = "If checked, aesthetics of unlocked annotations will be adjusted in group.",
                       style = "margin-right: 6px;"),
                  "Aesthetics Only"
                )),
                value = FALSE
              )
            )
            ),
            column(
              6,
              numericInput(
                "annotation_bulk_shift_x",
                label = HTML(paste(icon("arrows-alt-h", style = "margin-right: 6px;"), "Shift X")),
                value = 0,
                step = 0.1
              )
            ),
            column(
              6,
              numericInput(
                "annotation_bulk_shift_y",
                label = HTML(paste(icon("arrows-alt-v", style = "margin-right: 6px;"), "Shift Y")),
                value = 0,
                step = 0.1
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            actionButton(
              "apply_annotation_changes",
              class = "redo-button-main",
              label = tagList(icon("check-circle"), HTML("&nbsp;Apply Changes"))
            ),
            style = "display: flex; justify-content: center;" # Center-align the button
          )
        ),
        column(
          12,
          div(
            div(
              actionButton(
                "lock_annotations_button",
                class = "redo-button-main",
                label = tags$span(icon("lock"), HTML("&nbsp;Lock Annotations"), title = "Prevent annotations from being moved or modified in group")
              ),
              style = "display: flex; align-items: center; justify-content: center; gap: 6px;" # Center-align button and icon
            )
          )
        )
      )
      #)
    ),

    # Self-loop Arrow Inputs
    conditionalPanel(
      condition = "input.element_type == 'Self-loop Arrow'",
      #shiny::wellPanel(
      class = "conditional-panel",
      tags$div(
        class = "panel-group",
        style = "margin: 0; padding: 0;",
        tags$div(
          class = "toggle-button collapsed",
          `data-toggle` = "collapse",
          `data-target` = "#subSelfLoops",
          `aria-expanded` = "false",
          `aria-controls` = "subSelfLoops",
          tags$h4(
            tagList(
              icon("fas fa-redo-alt", style = "margin-right: 8px;"),
              h5(HTML("<b style='font-size: 16px;'>Draw Self-loop Arrows</b>")),
              tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
            )
          )
        ),
        tags$div(
          id = "subSelfLoops",
          class = "panel-collapse collapse",
          fluidRow(
            column(6, textInput("x_center", "X Coordinate (Center):", "0")),
            column(6, textInput("y_center", "Y Coordinate (Center):", "0"))
          ),
          fluidRow(
            column(6, numericInput("radius", "Radius:", value = 5, min = 0.1)),
            column(6, numericInput("line_width_loop", "Line Width:", value = 1, min = 0.1))
          ),
          fluidRow(
            column(6, colourInput("line_color_loop", "Line Color:", value = "#000000")),
            column(6, numericInput("line_alpha_loop", "Line Alpha:", value = 1, min = 0, max = 1, step = 0.1))
          ),
          fluidRow(
            column(6, selectInput("arrow_type_loop", "Arrow Type:", choices = c("open", "closed"))),
            column(6, numericInput("arrow_size_loop", "Arrow Size:", value = 0.2, min = 0.1, step = 0.1))
          ),
          fluidRow(
            column(6, numericInput("width_loop", "Loop Width:", value = 1, min = 0.1)),
            column(6, numericInput("height_loop", "Loop Height:", value = 1, min = 0.1))
          ),
          fluidRow(
            column(12, checkboxInput("two_way_arrow_loop", "Two-way Arrow", value = FALSE)) # checkbox for two-way self-loop arrows
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            actionButton(
              "add_loop",
              label = tagList(
                tags$i(
                  class = "fas fa-redo-alt",
                  style = "transform: rotate(135deg); margin-right: 5px;"
                ),
                "Add Self-loop Arrow"
              ),
              class = 'redo-button-main'
            ),
            style = "display: flex; justify-content: center;"
          )
        )
      ),
      #h4("Self-loop Arrow Inputs"),
      tags$div(
        class = "panel-group",
        style = "margin: 0; padding: 0;",
        tags$div(
          class = "toggle-button collapsed",
          `data-toggle` = "collapse",
          `data-target` = "#subLoopChange",
          `aria-expanded` = "false",
          `aria-controls` = "subLoopChange",
          tags$h4(
            tagList(
              tags$span(icon("cogs", style = "margin-right: 8px;"),
                        title = "Modify existing self-loop's arrow. Only works in unlocked state. Modifies gap size and orientation only."),
              h5(HTML("<b style='font-size: 16px;'>Change Configurations</b>")),
              tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
            )
          )
        ),
        tags$div(
          id = "subLoopChange",
          class = "panel-collapse collapse",
          fluidRow(
            column(6, numericInput("gap_size_loop", "Gap Size:", value = 0.2, min = 0, max = 1, step = 0.05)),
            column(6, numericInput("orientation_loop", "Orientation (deg):", value = 0, min = -180, max = 180))
          )
        ),
      ),
      fluidRow(
        column(
          12,
          div(
            actionButton(
              "apply_loop_changes",
              label = tagList(icon("check-circle"), HTML("&nbsp;Apply Changes")),
              class = "redo-button-main"
            ),
            style = "display: flex; justify-content: center;"
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            actionButton(
              "lock_loops",
              label = tagList(icon("lock"), HTML("&nbsp;Lock Self-loop Arrows")),
              value = FALSE,
              class = "redo-button-main"
            ),
            style = "display: flex; justify-content: center;" # Centers the button horizontally
          )
        )
      ),
      #div(style = "margin-top: 15px;"),
      # h4(
      #   "Change Configurations",
      #   tags$span(
      #     icon("question-circle"),
      #     title = "Modify existing self-loop's arrow. Only works in unlocked state. Modifies gap size and orientation only.",
      #     style = "cursor: help; margin-left: 6px; color: #007bff;"
      #   ),
      # ),
      #)
    ),

    # Conditional panel for SEM Data
    conditionalPanel(
      condition = "input.element_type == 'SEM Data'",
      #h5(HTML("<b style='font-size: 20px; text-align: center; display: block; '>Path Diagram in SEM</b>")),
      #shiny::wellPanel(
      class = "conditional-panel",
      tags$div(
        class = "panel-group",
        tags$div(
          class = "toggle-button collapsed",
          `data-toggle` = "collapse",
          `data-target` = "#subLavaanData",
          `aria-expanded` = "false",
          `aria-controls` = "subLavaanData",
          tags$h4(
            tagList(
              tags$span(icon("database", style = "margin-right: 8px;"),
                        title = "Produces SEM diagram with a model in lavaan syntax (without quotations) and/or data."),
              h5(HTML("<b style='font-size: 16px;'>Data and Model Specifics&nbsp</b>")),
              tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;"),
            )
          )
        ),
        tags$div(
          id = "subLavaanData",
          class = "panel-collapse collapse",
          radioButtons(
            "data_format",
            "Interpret as:",
            choices = c("Data frame" = "df", "Matrix" = "matrix"),
            selected = "df",
            inline = TRUE
          ),
          fileInput("edge_label_file", "Upload Data (CSV, Optional):"),
          # Lavaan syntax input
          textAreaInput("lavaan_syntax", "Lavaan Syntax", value = "
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
visual ~~ speed
    ", width = "100%", height = "200px"),
          tagList(
            tags$span(
              icon("question-circle"),
              title = "Customize the sem() function call or others (e.g., cfa()). `lavaan_string` and `data` are fixed variables and must remain unchanged.",
              style = "cursor: help; margin-left: 6px; color: #007bff;"
            ),
            textAreaInput(
              "sem_code",
              "Custom SEM Code",
              value = "sem(lavaan_string, data = data)",
              width = "100%",
              height = "100px"
            ),
          )
        ),
      ),
      tags$div(
        class = "panel-group",
        tags$div(
          class = "toggle-button collapsed",
          `data-toggle` = "collapse",
          `data-target` = "#subSEMlayouts",
          `aria-expanded` = "false",
          `aria-controls` = "subSEMlayouts",
          tags$h4(
            tagList(
              icon("project-diagram", style = "margin-right: 8px;"),
              h5(HTML("<b style='font-size: 16px;'>SEM Layout Settings</b>")),
              tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
            )
          )
        ),
        tags$div(
          id = "subSEMlayouts",
          class = "panel-collapse collapse",
          selectInput("lavaan_layout", "Choose Layout Algorithm:",
                      choices = c(
                        "Tree" = "tree",
                        "Circle" = "circle",
                        "Spring" = "spring",
                        "Tree2" = "tree2",
                        "Circle2" = "circle2",
                        "Default" = "default"
                      ),
                      selected = "default"
          ),
          fluidRow(
            column(6, numericInput("center_x_position", "Center X:", value = 0, step = 1)),
            column(6, numericInput("center_y_position", "Center Y:", value = 0, step = 1))
          ),
          fluidRow(
            column(
              6,
              numericInput(
                "relative_x_position",
                HTML(paste(icon("ruler-horizontal"), "&nbsp;Width X:")),
                value = 25,
                min = 0.1,
                step = 0.1
              )
            ),
            column(
              6,
              numericInput(
                "relative_y_position",
                HTML(paste(icon("ruler-vertical"), "&nbsp;Height Y:")),
                value = 25,
                min = 0.1,
                step = 0.1
              )
            )
          )
        )
      ),
      tags$div(
        class = "panel-group",
        # Header with toggle button
        tags$div(
          class = "toggle-button collapsed",
          `data-toggle` = "collapse",
          `data-target` = "#subNodeSettings",
          `aria-expanded` = "false",
          `aria-controls` = "subNodeSettings",
          tags$h4(
            tagList(
              icon("shapes", style = "margin-right: 8px;"),
              h5(HTML("<b style='font-size: 16px;'>Node Settings</b>")),
              tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
            )
          )
        ),
        tags$div(
          id = "subNodeSettings",
          class = "panel-collapse collapse",
          fluidRow(
            column(6, selectInput("latent_shape", "Latent Node Shape:",
                                  choices = c("circle", "square", "rectangle", "oval", "triangle", "diamond"),
                                  selected = "circle"
            )),
            column(6, colourInput("latent_color_input", "Latent Node Color:", value = "#cc3d3d")),
            column(6, selectInput("observed_shape", "Observed Node Shape:",
                                  choices = c("circle", "square", "rectangle", "oval", "triangle", "diamond"),
                                  selected = "square"
            )),
            column(6, colourInput("observed_color_input", "Observed Node Color:", value = "#1262b3")),
            column(6, selectInput("int_shape", "Intercept Node Shape:",
                                  choices = c("circle", "square", "rectangle", "oval", "triangle", "diamond"),
                                  selected = "triangle"
            )),
            column(6, colourInput("int_color_input", "Intercept Node Color:", value = "#0f993d")),
            column(6, numericInput("latent_size_input", "Latent Node Size:", value = 20, min = 1)),
            column(6, numericInput("observed_size_input", "Observed Node Size:", value = 12, min = 1)),
            column(6, numericInput("int_size_input", "Intercept Node Size:", value = 10, min = 1)),
            column(6, colourInput("node_border_color", "Node Border Color:", value = "white")),
            column(6, numericInput("node_border_width", "Border Width:", value = 1, min = 0.1, step = 0.1))
          )
        )
      ),
      tags$div(
        class = "panel-group",
        tags$div(
          class = "toggle-button collapsed",
          `data-toggle` = "collapse",
          `data-target` = "#subEdgeSettings",
          `aria-expanded` = "false",
          `aria-controls` = "subEdgeSettings",
          tags$h4(
            tagList(
              icon("arrows-alt-h", style = "margin-right: 8px;"),
              h5(HTML("<b style='font-size: 16px;'>Edge Settings</b>")),
              tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
            )
          )
        ),
        tags$div(
          id = "subEdgeSettings",
          class = "panel-collapse collapse",
          fluidRow(
            column(6, selectInput("lavaan_arrow_type", "Arrow Type:", choices = c("open", "closed"), selected = "closed")),
            column(6, numericInput("lavaan_arrow_size", "Arrow Size:", value = 0.1, min = 0.1, step = 0.1)),
            column(6, numericInput("line_endpoint_spacing",
                                   "Line Endpoint Spacing:",
                                   value = 1.2, min = 0, step = 0.1
            )),
            column(6, selectInput("lavaan_arrow_location", "Arrowhead Location:",
                                  choices = c("start", "end"), selected = "end"
            )),
            column(6, colourInput("edge_color_input", "Edge Color:", value = "#000000")),
            column(6, numericInput("line_width_input",
                                   "Linewidth:",
                                   value = 1, min = 0.1, step = 0.1)
            )
          ),
          h5(HTML("<b style='font-size: 16px;'>Covariance (Two-way) Lines</b>")),
          fluidRow(
            column(
              6,
              tags$span(
                icon("question-circle"),
                title = "Control the curvature magnitude of two-way (covariance) unlocked arrow(s).).",
                style = "cursor: help; margin-left: 6px; color: #007bff;"
              ),
              sliderInput(
                "lavaan_curvature_magnitude",
                "Curvature Size:",
                min = 0,
                max = 2,
                value = 0.5,
                step = 0.01
              ),
            ),
            column(
              6,
              tags$span(
                icon("question-circle"),
                title = "Rotate the orientation of the two-way (covariance) unlocked arrow(s) by 180 degrees.",
                style = "cursor: help; margin-left: 6px; color: #007bff;"
              ),
              checkboxInput(
                "lavaan_rotate_curvature",
                "Rotate Curvature 180°",
                value = FALSE
              )
            )
          )
        )
      ),
      tags$div(
        class = "panel-group",
        tags$div(
          class = "toggle-button collapsed",
          `data-toggle` = "collapse",
          `data-target` = "#subAnnotationSettings",
          `aria-expanded` = "false",
          `aria-controls` = "subAnnotationSettings",
          tags$h4(
            tagList(
              icon("text-width", style = "margin-right: 8px;"),
              h5(HTML("<b style='font-size: 16px;'>Annotation Settings</b>")),
              tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
            )
          )
        ),
        tags$div(
          id = "subAnnotationSettings",
          class = "panel-collapse collapse",
          h5(HTML("<b style='font-size: 16px;'>Latent Variable Labels</b>")),
          fluidRow(
            column(6, numericInput("text_size_input",
                                   "Text Size:",
                                   value = 18, min = 5, step = 1
            )),
            column(6, selectInput("text_font_input", "Text Font:",
                                  choices = c("sans", "serif", "mono"), selected = "sans"
            )),
            column(6, colourInput("text_color_input", "Text Color:", value = "#FFFFFF")),
            column(6, numericInput("text_alpha_input", "Text Alpha:", value = 1, min = 0, max = 1, step = 0.1)),
            column(6, selectInput("text_fontface_input", "Fontface:", choices = c("Plain", "Bold", "Italic")))
          ),
          h5(HTML("<b style='font-size: 16px;'>Other Variable Labels</b>")),
          fluidRow(
            column(6, numericInput("text_size_others",
                                   "Text Size:",
                                   value = 16, min = 5, step = 1
            )),
            column(6, selectInput("text_font_others", "Text Font:",
                                  choices = c("sans", "serif", "mono"), selected = "sans"
            )),
            column(6, selectInput("text_fontface_others", "Fontface:", choices = c("Plain", "Bold", "Italic"))),
            column(6, colourInput("text_color_others", "Text Color:", value = "#FFFFFF")),
            column(6, numericInput("text_alpha_others", "Text Alpha:", value = 1, min = 0, max = 1, step = 0.1))),
          h5(HTML("<b style='font-size: 16px;'>Edge Labels</b>")),
          fluidRow(
            column(6, numericInput("text_size_edges",
                                   "Text Size:",
                                   value = 14, min = 5, step = 1
            )),
            column(6, selectInput("text_font_edges", "Text Font:",
                                  choices = c("sans", "serif", "mono"), selected = "sans"
            )),
            column(6, selectInput("text_fontface_edges", "Fontface:", choices = c("Plain", "Bold", "Italic"))),
            column(6, colourInput("text_color_edges", "Text Color:", value = "#000000")),
            column(6, numericInput("text_alpha_edges", "Text Alpha:", value = 1, min = 0, max = 1, step = 0.1))
          )
        ),
      ),
      fluidRow(
        column(
          12,
          div(
            actionButton(
              "generate_graph",
              label = tags$span(icon("project-diagram"), HTML("&nbsp;Draw a SEM"), title = "Click to generate the SEM graph from the lavaan model."),
              class = "redo-button-main"
            ),
            style = "display: flex; align-items: center; justify-content: center; gap: 10px;" # Centers and spaces the button and help icon
          )
        ),
        column(
          12,
          div(
            actionButton(
              "apply_changes_lavaan",
              label = tags$span(icon("check-circle"), HTML("&nbsp;Apply Changes"), title = "Apply the changes made to an existing (unlocked) SEM diagram (all aesthetic parameters). It is only applicable for SEM diagrams created within the app, not with CSVs from previous sessions."),
              class = "redo-button-main"
            ),
            style = "display: flex; align-items: center; justify-content: center; gap: 10px;" # Centers and spaces the button and help icon
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            actionButton(
              "lock_lavaan",
              label = tags$span(icon("lock"), HTML("&nbsp;Finalize a SEM"), title = "Finalize the SEM diagram to prevent further changes."),
              value = FALSE,
              class = "redo-button-main"
            ),
            style = "display: flex; align-items: center; justify-content: center; gap: 10px;" # Centers button and icon with spacing
          )
        )
      ),
      #),
    ),
    conditionalPanel(
      condition = "input.element_type == 'Network Data'",
      #h5(HTML("<b style='font-size: 20px; text-align: center; display: block; '>Network Diagram</b>")),
      #shiny::wellPanel(
      class = "conditional-panel",
      tagList(
        tags$span(
          icon("question-circle"),
          title = "Edge List or Adjacency Matrix file is required.",
          style = "cursor: help; margin-right: 6px; color: #007bff;"
        ),
        fileInput("network_file", "Upload Network CSV File", accept = ".csv")
      ),
      tags$div(
        class = "panel-group",
        tags$div(
          class = "toggle-button collapsed",
          `data-toggle` = "collapse",
          `data-target` = "#subnetLayouts",
          `aria-expanded` = "false",
          `aria-controls` = "subnetLayouts",
          tags$h4(
            tagList(
              icon("sitemap", style = "margin-right: 8px;"),
              h5(HTML("<b style='font-size: 16px;'>Network Layout Settings</b>")),
              tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
            )
          )
        ),
        tags$div(
          id = "subnetLayouts",
          class = "panel-collapse collapse",
          fluidRow(
            column(12, selectInput(
              "layout_method",
              "Choose Layout Method:",
              choices = c(
                "Fruchterman-Reingold" = "fr",
                "Kamada-Kawai" = "kk",
                "Circular Layout" = "circle",
                "Grid Layout" = "grid",
                "Random Layout" = "random",
                "Dimensionality Reduction" = "dim_reduction"
              ),
              selected = "fr")
            ),
            column(6, checkboxInput("is_directed", "Directed Network", value = TRUE)),

            column(6, div(style = "display: flex; align-items: center;",
                          numericInput("random_seed", "Set Random Seed",
                                       value = as.numeric(format(Sys.time(), "%OS3")) * 1000, min = 1, step = 1),
                          tags$span(
                            title = "This only applies when a graph is generated, not when changes are applied.",
                            tags$i(class = "fa fa-question-circle", style = "color: #007bff; margin-left: 5px; cursor: pointer;")
                          )
            )
            ),
            column(6, numericInput("layout_x_net", "Layout Width (X):", value = 20, min = 0.1, step = 0.1)),
            column(6, numericInput("layout_y_net", "Layout Height (Y):", value = 20, min = 0.1, step = 0.1)),
            column(6, numericInput("x_center_net", "X Center:", value = 0, step = 1)),
            column(6, numericInput("y_center_net", "Y Center:", value = 0, step = 1)),
            conditionalPanel(
              condition = "input.layout_method == 'dim_reduction'",
              column(12, selectInput(
                "dim_reduction_method",
                "Dimensionality Reduction Method:",
                choices = c(
                  "t-SNE" = "tsne",
                  "UMAP" = "umap",
                  "PCA" = "pca"
                ),
                selected = "tsne"
              ))
            ),
            hr(),
            column(12, checkboxInput("use_clustering", "Enable Clustering", value = FALSE)),
            conditionalPanel(
              condition = "input.use_clustering == true",
              column(12, selectInput(
                "clustering_method",
                "Clustering Method:",
                choices = c(
                  "Louvain (undirected)" = "louvain",
                  "Leiden (undirected)" = "leiden",
                  "Walktrap (directed/undirected)" = "walktrap",
                  "Fast Greedy (undirected)" = "fast_greedy"
                ),
                selected = "louvain"
              )),
              column(12, selectInput(
                "cluster_palette",
                "Select Color Palette:",
                choices = c(
                  "Rainbow" = "rainbow",
                  "Set1" = "Set1",
                  "Paired" = "Paired",
                  "Dark2" = "Dark2",
                  "Set3" = "Set3",
                  "Pastel1" = "Pastel1",
                  "Pastel2" = "Pastel2",
                  "Spectral" = "Spectral",
                  "YlGnBu" = "YlGnBu",
                  "RdYlBu" = "RdYlBu",
                  "smplot2" = "smplot2"
                ),
                selected = "rainbow"
              ))
            )
          )
        )
      ),
      tags$div(
        class = "panel-group",
        tags$div(
          class = "toggle-button collapsed",
          `data-toggle` = "collapse",
          `data-target` = "#subnodeSettings",
          `aria-expanded` = "false",
          `aria-controls` = "subnodeSettings",
          tags$h4(
            tagList(
              icon("shapes", style = "margin-right: 8px;"),
              h5(HTML("<b style='font-size: 16px;'>Node Settings</b>")),
              tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
            )
          )
        ),
        tags$div(
          id = "subnodeSettings",
          class = "panel-collapse collapse",
          fluidRow(
            column(6, selectInput("node_shape_net",
                                  HTML(paste(
                                    icon("shapes", style = "margin-right: 6px;"), # icon for shapes
                                    "Select Shape"
                                  )),
                                  choices = c("circle", "square", "rectangle", "oval", "triangle", "diamond")
            )),
            conditionalPanel(
              condition = "input.node_shape_net == 'rectangle' || input.node_shape_net == 'oval' || input.node_shape_net == 'diamond'",
              column(
                6,
                div(
                  numericInput(
                    "node_width_height_ratio_net",
                    label = HTML(paste(
                      icon("ruler-combined", style = "margin-right: 8px;"), "Width/Height Ratio"
                    )),
                    value = 1.6,
                    min = 0.1,
                    step = 0.1
                  ),
                  tags$span(
                    icon("question-circle"),
                    title = "Adjust the ratio of width to height for rectangle, oval, and diamond shapes.",
                    style = "cursor: help; margin-left: 6px; color: #007bff;"
                  ),
                  style = "display: flex; align-items: center;"
                )
              )
            ),
            column(6, numericInput("node_size_net", "Node Size:", value = 15, step = 1)),
            column(6, colourInput("node_fill_color_net", "Node Fill Color:", value = "#1262b3")),
            column(6, colourInput("node_border_color_net", "Node Border Color:", value = "#FFFFFF")),
            column(6, numericInput("node_border_width_net", "Node Border Width:", value = 1, step = 0.1))
          )
        )
      ),
      tags$div(
        class = "panel-group",
        tags$div(
          class = "toggle-button collapsed",
          `data-toggle` = "collapse",
          `data-target` = "#subedgeSettings",
          `aria-expanded` = "false",
          `aria-controls` = "subedgeSettings",
          tags$h4(
            tagList(
              icon("long-arrow-alt-up", style = "margin-right: 8px;"),
              h5(HTML("<b style='font-size: 16px;'>Edge Settings</b>")),
              tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
            )
          )
        ),
        tags$div(
          id = "subedgeSettings",
          class = "panel-collapse collapse",
          fluidRow(
            column(6, numericInput("line_width_net", "Edge Width:", value = 1, step = 0.1)),
            column(6, colourInput("line_color_net", "Edge Color:", value = "#000000"))
          ),
          fluidRow(
            column(6, numericInput("line_alpha_net", "Edge Alpha:", value = 1, min = 0, max = 1, step = 0.1)),
            column(6, numericInput("line_endpoint_spacing_net", "Line Endpoint Spacing:", value = 1, min = 0, step = 0.1))
          ),
          conditionalPanel(
            condition = "input.is_directed",
            fluidRow(
              column(6, selectInput("arrow_type_net", "Arrow Type:", choices = c("open", "closed"), selected = "closed")),
              column(6, numericInput("arrow_size_net", "Arrow Size:", value = 0.1, min = 0.1, step = 0.1))
            )
          ),
          h5(HTML("<b style='font-size: 16px;'>Other Edge Width Settings</b>")),
          checkboxInput("scale_edge_width", "Scale Edge Width by Weight", value = FALSE),
          conditionalPanel(
            condition = "input.scale_edge_width == true",
            fluidRow(
              column(6, numericInput("min_edge_width", "Min. Edge Width:", value = 0.5, min = 0.1, step = 0.1)),
              column(6, numericInput("max_edge_width", "Max. Edge Width:", value = 3, min = 0.1, step = 0.1))
            )
          ),
        ),
      ),
      tags$div(
        class = "panel-group",
        tags$div(
          class = "toggle-button collapsed",
          `data-toggle` = "collapse",
          `data-target` = "#subTextSettings",
          `aria-expanded` = "false",
          `aria-controls` = "subTextSettings",
          tags$h4(
            tagList(
              icon("text-height", style = "margin-right: 8px;"),
              h5(HTML("<b style='font-size: 16px;'>Annotation Settings</b>")),
              tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
            )
          )
        ),
        tags$div(
          id = "subTextSettings",
          class = "panel-collapse collapse",
          h5(HTML("<b style='font-size: 16px;'>Node Labels</b>")),
          fluidRow(
            column(6, selectInput("node_label_font", "Text Font:", choices = c("sans", "serif", "mono"), selected = "sans")),
            column(6, numericInput("node_label_size", "Text Size:", value = 15, min = 1, step = 1))
          ),
          fluidRow(
            column(6, colourInput("node_label_color", "Text Color:", value = "#FFFFFF")),
            column(6, numericInput("node_label_alpha", "Text Alpha:", value = 1, min = 0, max = 1, step = 0.1)),
            column(6, selectInput("node_label_fontface", "Fontface:", choices = c("Plain", "Bold", "Italic")))
          ),
          h5(HTML("<b style='font-size: 16px;'>Edge Labels</b>")),
          # fluidRow(
          #   column(12, checkboxInput("annotate_edges", "Show Edge Labels", value = TRUE)),
          # ),
          fluidRow(
            column(6, selectInput("edge_label_font", "Text Font:", choices = c("sans", "serif", "mono"), selected = "sans")),
            column(6, numericInput("edge_label_size", "Text Size:", value = 15, min = 1, step = 1))
          ),
          fluidRow(
            column(6, colourInput("edge_label_color", "Text Color:", value = "#000000")),
            column(6, numericInput("edge_label_alpha", "Text Alpha:", value = 1, min = 0, max = 1, step = 0.1)),
            column(6, selectInput("edge_label_fontface", "Fontface:", choices = c("Plain", "Bold", "Italic")))
          )
        ),
      ),
      fluidRow(
        column(
          12,
          div(
            actionButton(
              "generate_network",
              label = tags$span(icon("project-diagram"), HTML("&nbsp;Draw a Network"), title = "Click to generate the network diagram from a CSV file."),
              class = "redo-button-main"
            ),
            style = "display: flex; align-items: center; justify-content: center; gap: 10px;" # Centered alignment and spacing
          )
        ),
        column(
          12,
          div(
            actionButton(
              "apply_changes_network",
              label = tags$span(icon("check-circle"), HTML("&nbsp;Apply Changes"), title = "Apply the changes made to an existing (unlocked) network diagram (all aesthetic parameters)."),
              class = "redo-button-main"
            ),
            style = "display: flex; align-items: center; justify-content: center; gap: 10px;" # Centered alignment and spacing
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            actionButton(
              "lock_network",
              label = tags$span(icon("lock"), HTML("&nbsp;Finalize a Network"), title = "Finalize the network diagram to prevent further changes."),
              value = FALSE,
              class = "redo-button-main"
            ),
            style = "display: flex; align-items: center; justify-content: center; gap: 10px;" # Ensures proper alignment and spacing
          )
        )
      ),
      #)
    ),
    fluidRow(
      column(
        12, # Make the column span the full width
        div(
          class = "text-center", # Bootstrap class for centering content
          actionButton("undo_button", class = "redo-button", label = tagList(icon("undo"), "Undo")),
          actionButton("redo_button", class = "redo-button", label = tagList(icon("redo"), "Redo"))
        )
      )
    ),
    div(style = "margin-top: 10px;"),
    tags$div(
      class = "panel-group",
      style = "margin: 0; padding: 0;",
      tags$div(
        class = "toggle-button collapsed",
        `data-toggle` = "collapse",
        `data-target` = "#loadCSV",
        `aria-expanded` = "false",
        `aria-controls` = "loadCSV",
        tags$h4(
          tagList(
            icon("file-upload", style = "margin-right: 8px;"),
            h5(HTML("<b style='font-size: 16px;'>Upload CSV Files</b>")),
            tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
          )
        )
      ),
      tags$div(
        id = "loadCSV",
        class = "panel-collapse collapse",
        fileInput("points_file", "Upload Points CSV"),
        fileInput("lines_file", "Upload Lines CSV"),
        fileInput("annotations_file", "Upload Annotations CSV"),
        fileInput("self_loop_file", "Upload Self Loop Arrows CSV"),
      ),
    ),
    # Download CSV dropdown menu
    tags$div(
      class = "panel-group",
      style = "margin: 0; padding: 0;",
      tags$div(tags$div(
        class = "toggle-button collapsed",
        `data-toggle` = "collapse",
        `data-target` = "#exportCSV",
        `aria-expanded` = "false",
        `aria-controls` = "exportCSV",
        tags$h4(
          tagList(
            icon("image", style = "margin-right: 8px;"),
            h5(HTML("<b style='font-size: 16px;'>Export Visualizations</b>")),
            tags$i(class = "fas fa-chevron-down", style = "margin-left: auto;")
          )
        )),
        tags$div(
          id = "exportCSV",
          class = "panel-collapse collapse",
          selectInput(
            "csv_type",
            "Choose CSV to Download:",
            choices = c("Points CSV", "Lines CSV", "Annotations CSV", "Self-loop Arrows CSV")
          ),
          downloadButton(
            "download_selected_csv",
            "Download Selected CSV",
            class = "redo-button"
          ),
          div(style = "margin-top: 10px;"),
          selectInput(
            "export_format",
            "Choose Export Format:",
            choices = c("PNG", "JPEG", "PDF", "SVG")
          ),
          div(
            style = "margin-top: 10px;",
            checkboxInput("use_x_range", HTML("Specify X Range <i class='fa fa-question-circle' style='color: #007bff; cursor: pointer;' title='Define the X-axis range to customize the view of your plot.'></i>"),
                          value = FALSE),
            conditionalPanel(
              condition = "input.use_x_range == true",
              fluidRow(
                column(6, numericInput("x_range_min", "X Range Min:", value = NA, step = 1)),
                column(6,  numericInput("x_range_max", "X Range Max:", value = NA, step = 1))
              ),
            ),
            checkboxInput("use_y_range", HTML("Specify Y Range <i class='fa fa-question-circle' style='color: #007bff; cursor: pointer;' title='Define the Y-axis range to customize the view of your plot.'></i>"),
                          value = FALSE),

            conditionalPanel(
              condition = "input.use_y_range == true",
              fluidRow(
                column(6, numericInput("y_range_min", "Y Range Min:", value = NA, step = 1)),
                column(6, numericInput("y_range_max", "Y Range Max:", value = NA, step = 1))
              )
            )
          ),
          downloadButton("download_plot", "Save the Figure", class = "redo-button"),
          textOutput("instruction")
        )
      )
    )
  ),

  # Main panel for plot and data tables
  mainPanel(
    div(
      style = "border: 2px solid dimgray; padding: 5px;",
      plotOutput("plot", hover = hoverOpts(id = "plot_hover"), height = "700px", width = "100%")
    ),
    textOutput("hover_info"),
    br(),
    fluidRow(
      column(12, h4(
        tagList(
          icon("table"), # Replace with a suitable icon, e.g., "table"
          HTML("&nbsp;"), # Add space between the icon and text
          "Output Tables"
        )
      )),

      # Scrollable container for all tables
      fluidRow(
        column(
          12,
          tabsetPanel(
            # Points Table Tab
            tabPanel(
              title = tagList(icon("plus-circle"), "Points Table"),
              div(
                class = "scrollable-tables",
                fluidRow(
                  column(
                    4,
                    actionButton(
                      "delete_selected_point",
                      label = tagList(icon("trash-alt"), HTML("&nbsp;&nbsp;Delete Selected Point(s)")),
                      class = "redo-button0"
                    )
                  ),
                  column(
                    4,
                    actionButton(
                      "unlock_selected_point",
                      label = tagList(icon("unlock"), HTML("&nbsp;&nbsp;Unlock Selected Point(s)")),
                      class = "redo-button0"
                    )
                  ),
                  column(
                    4,
                    actionButton(
                      "lock_selected_point",
                      label = tagList(icon("lock"), HTML("&nbsp;Lock Selected Point(s)")),
                      class = "redo-button0"
                    )
                  )
                ),
                fluidRow(
                  column(
                    4,
                    actionButton(
                      "delete_all_points",
                      label = tagList(icon("trash"), HTML("&nbsp;&nbsp;Delete All Points")),
                      class = "redo-button0"
                    )
                  ),
                  column(
                    4,
                    actionButton(
                      "unlock_points",
                      label = tagList(icon("lock"), HTML("&nbsp;Unlock All Points")),
                      class = "redo-button0"
                    )
                  ),
                  column(
                    4,
                    actionButton(
                      "lock_points",
                      label = tagList(icon("lock"), HTML("&nbsp;Lock All Points")),
                      class = "redo-button0"
                    )
                  )
                ),
                tags$div(style = "height: 7.5px;"),
                DTOutput("data_table")
              )
            ),
            # Lines Table Tab
            tabPanel(
              title = tagList(icon("arrows-alt-h"), "Lines Table"),
              div(
                class = "scrollable-tables",
                fluidRow(
                  column(
                    4,
                    actionButton(
                      "delete_selected_line",
                      label = tagList(icon("trash-alt"), HTML("&nbsp;&nbsp;Delete Selected Line(s)")),
                      class = "redo-button0"
                    )
                  ),
                  column(
                    4,
                    actionButton(
                      "unlock_selected_lines",
                      label = tagList(icon("unlock"), HTML("&nbsp;&nbsp;Unlock Selected Line(s)")),
                      class = "redo-button0"
                    )
                  ),
                  column(
                    4,
                    actionButton(
                      "lock_selected_lines",
                      label = tagList(icon("lock"), HTML("&nbsp;Lock Selected Line(s)")),
                      class = "redo-button0"
                    )
                  )
                ),
                fluidRow(
                  column(
                    4,
                    actionButton(
                      "delete_all_lines",
                      label = tagList(icon("trash"), HTML("&nbsp;&nbsp;Delete All Lines")),
                      class = "redo-button0"
                    )
                  ),
                  column(
                    4,
                    actionButton(
                      "unlock_lines_button",
                      label = tagList(icon("lock"), HTML("&nbsp;Unlock All Lines")),
                      class = "redo-button0"
                    )
                  ),
                  column(
                    4,
                    actionButton(
                      "lock_lines_button",
                      label = tagList(icon("lock"), HTML("&nbsp;Lock All Lines")),
                      class = "redo-button0"
                    )
                  )
                ),
                tags$div(style = "height: 7.5px;"),
                DTOutput("line_table")
              )
            ),
            # Annotations Table Tab
            tabPanel(
              title = tagList(icon("pencil-alt"), "Annotations Table"),
              div(
                class = "scrollable-tables",
                fluidRow(
                  column(
                    4,
                    actionButton(
                      "delete_selected_annotation",
                      label = tagList(icon("trash-alt"), HTML("&nbsp;&nbsp;Delete Selected Annotation(s)")),
                      class = "redo-button0"
                    )
                  ),
                  column(
                    4,
                    actionButton(
                      "unlock_selected_annotation",
                      label = tagList(icon("unlock"), HTML("&nbsp;&nbsp;Unlock Selected Annotation(s)")),
                      class = "redo-button0"
                    )
                  ),
                  column(
                    4,
                    actionButton(
                      "lock_selected_annotation",
                      label = tagList(icon("lock"), HTML("&nbsp;Lock Selected Annotation(s)")),
                      class = "redo-button0"
                    )
                  )
                ),
                fluidRow(
                  column(
                    4,
                    actionButton(
                      "delete_all_annotations",
                      label = tagList(icon("trash"), HTML("&nbsp;&nbsp;Delete All Annotations")),
                      class = "redo-button0"
                    )
                  ),
                  column(
                    4,
                    actionButton(
                      "unlock_annotations_button",
                      label = tagList(icon("lock"), HTML("&nbsp;Unlock All Annotations")),
                      class = "redo-button0"
                    )
                  ),
                  column(
                    4,
                    actionButton(
                      "lock_annotations_button",
                      label = tagList(icon("lock"), HTML("&nbsp;Lock All Annotations")),
                      class = "redo-button0"
                    )
                  )
                ),
                tags$div(style = "height: 7.5px;"),
                DTOutput("annotation_table")
              )
            ),
            # Self-loop Arrows Table Tab
            tabPanel(
              title = tagList(tags$i(class = "fa fa-redo", style = "transform: rotate(135deg);"), "Self-loop Arrows Table"),
              div(
                class = "scrollable-tables",
                fluidRow(
                  column(
                    4,
                    actionButton(
                      "delete_selected_loop",
                      label = tagList(icon("trash-alt"), HTML("&nbsp;&nbsp;Delete Selected Self-loop Arrow(s)")),
                      class = "redo-button0"
                    )
                  ),
                  column(
                    4,
                    actionButton(
                      "unlock_selected_loop",
                      label = tagList(icon("unlock"), HTML("&nbsp;&nbsp;Unlock Selected Self-loop Arrow(s)")),
                      class = "redo-button0"
                    )
                  ),
                  column(
                    4,
                    actionButton(
                      "delete_all_loops",
                      label = tagList(icon("trash"), HTML("&nbsp;&nbsp;Delete All Self-loop Arrows")),
                      class = "redo-button0"
                    )
                  )
                ),
                tags$div(style = "height: 7.5px;"),
                DTOutput("loop_table")
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(12, textOutput("axis_info")) # hover -> XY coord
    ),
  )
)
)

server <- function(input, output, session) {
  options(warn = -1)
  # For undo/redo history
  values <- reactiveValues(
    points = data.frame(
      x = numeric(), y = numeric(), shape = character(), color = character(), size = numeric(),
      border_color = character(), border_width = numeric(), alpha = numeric(), width_height_ratio = numeric(),
      orientation = numeric(), lavaan = logical(), network = logical(), locked = logical(), stringsAsFactors = FALSE
    ),
    lines = data.frame(
      x_start = numeric(), y_start = numeric(), x_end = numeric(), y_end = numeric(),
      ctrl_x = numeric(), ctrl_y = numeric(), type = character(), color = character(), end_color = character(), color_type = character(),
      gradient_position = numeric(), width = numeric(), alpha = numeric(), arrow = logical(), arrow_type = character(),
      arrow_size = numeric(), two_way = logical(), lavaan = logical(), network = logical(), line_style = character(), locked = logical(), stringsAsFactors = FALSE
    ),
    annotations = data.frame(
      text = character(), x = numeric(), y = numeric(), font = character(), size = numeric(), color = character(), angle = numeric(), alpha = numeric(),
      fontface = character(), math_expression = logical(), lavaan = logical(), network = logical(), locked = logical(), stringsAsFactors = FALSE
    ),
    loops = data.frame(
      x_center = numeric(), y_center = numeric(), radius = numeric(), color = character(),
      width = numeric(), alpha = numeric(), arrow_type = character(), arrow_size = numeric(),
      gap_size = numeric(), loop_width = numeric(), loop_height = numeric(), orientation = numeric(),
      two_way = logical(), locked = logical(), stringsAsFactors = FALSE
    ),
    undo_stack = list(), # Stack for undo
    redo_stack = list() # Stack for redo
  )
  uploaded_data <- reactiveVal(NULL)
  # Reactive value to store the last valid hover coordinates
  last_hover <- reactiveVal(NULL)
  debounced_hover <- debounce(reactive(input$plot_hover), 10)

  output$hover_info <- renderText({
    hover <- input$plot_hover
    if (!is.null(hover)) {
      last_hover(paste("Hovered at: X =", round(hover$x, 2), "Y =", round(hover$y, 2)))
    }
    last_hover() %||% "Hover over the plot to see X and Y coordinates"
  })

  save_state <- function() {
    values$undo_stack <- append(values$undo_stack, list(list(
      points = values$points,
      lines = values$lines,
      annotations = values$annotations,
      loops = values$loops
    )))
    values$redo_stack <- list()
  }

  undo <- function() {
    if (length(values$undo_stack) > 0) {
      values$redo_stack <- append(values$redo_stack, list(list(
        points = values$points,
        lines = values$lines,
        annotations = values$annotations,
        loops = values$loops
      )))

      last_state <- tail(values$undo_stack, 1)[[1]]
      values$undo_stack <- values$undo_stack[-length(values$undo_stack)]
      values$points <- last_state$points
      values$lines <- last_state$lines
      values$annotations <- last_state$annotations
      values$loops <- last_state$loops
    }
  }


  redo <- function() {
    if (length(values$redo_stack) > 0) {
      values$undo_stack <- append(values$undo_stack, list(list(
        points = values$points,
        lines = values$lines,
        annotations = values$annotations,
        loops = values$loops
      )))
      last_state <- tail(values$redo_stack, 1)[[1]]
      values$redo_stack <- values$redo_stack[-length(values$redo_stack)]
      values$points <- last_state$points
      values$lines <- last_state$lines
      values$annotations <- last_state$annotations
      values$loops <- last_state$loops
    }
  }

  add_new_line <- function(new_line_data) {
    expected_columns <- c(
      "x_start", "y_start", "x_end", "y_end", "ctrl_x", "ctrl_y", "type",
      "color", "end_color", "color_type", "gradient_position", "width",
      "alpha", "arrow", "arrow_type", "arrow_size", "two_way", "lavaan", "network", "line_style", "locked"
    )

    missing_columns <- setdiff(expected_columns, colnames(new_line_data))
    if (length(missing_columns) > 0) {
      for (col in missing_columns) {
        new_line_data[[col]] <- NA
      }
    }

    new_line_data <- new_line_data[expected_columns]
    values$lines <- rbind(values$lines, new_line_data)
  }


  data_table_proxy <- dataTableProxy("data_table")

  observeEvent(input$unlock_points, {
    if (any(values$points$locked)) {
      save_state()
      values$points$locked <- FALSE
      showNotification("All points have been unlocked.", type = "message")
    } else {
      showNotification("No locked points to unlock.", type = "warning")
    }
  })

  observeEvent(input$lock_points, {
    if (any(!values$points$locked)) {
      save_state()
      values$points$locked <- TRUE
      showNotification("All points have been locked.", type = "message")
    } else {
      showNotification("No unlocked points to lock.", type = "warning")
    }
  })

  observeEvent(input$unlock_selected_point, {
    selected_row <- input$data_table_rows_selected

    if (!is.null(selected_row)) {
      save_state()
      values$points$locked[selected_row] <- FALSE
      showNotification(
        paste("Points at rows", paste(selected_row, collapse = ", "), "have been unlocked."),
        type = "message"
      )
    } else {
      showNotification("No point selected. Please select a point to unlock.", type = "warning")
    }
  })

  observeEvent(input$lock_selected_point, {
    selected_row <- input$data_table_rows_selected

    if (!is.null(selected_row)) {
      save_state()
      values$points$locked[selected_row] <- TRUE
      showNotification(
        paste("Points at rows", paste(selected_row, collapse = ", "), "have been locked."),
        type = "message"
      )
    } else {
      showNotification("No point selected. Please select a point to unlock.", type = "warning")
    }
  })

  observeEvent(input$unlock_annotations_button, {
    if (any(values$annotations$locked)) {
      save_state()
      values$annotations$locked <- FALSE
      showNotification("All annotations have been unlocked.", type = "message")
    } else {
      showNotification("No locked annotations to unlock.", type = "warning")
    }
  })

  observeEvent(input$lock_annotations_button, {
    if (any(!values$annotations$locked)) {
      save_state()
      values$annotations$locked <- TRUE # Locks all annotations
      showNotification("All annotations have been locked.", type = "message")
    } else {
      showNotification("No unlocked annotations to lock.", type = "warning")
    }
  })

  observeEvent(input$unlock_lines_button, {
    if (any(values$lines$locked)) {
      save_state()
      values$lines$locked <- FALSE
      showNotification("All lines have been unlocked.", type = "message")
    } else {
      showNotification("No locked lines to unlock.", type = "warning")
    }
  })

  observeEvent(input$lock_lines_button, {
    if (any(!values$lines$locked)) {
      save_state()
      values$lines$locked <- TRUE
      showNotification("All lines have been locked.", type = "message")
    } else {
      showNotification("No unlocked lines to lock.", type = "warning")
    }
  })


  observeEvent(input$lock_loops, {
    if (any(!values$loops$locked)) {
      save_state()
      values$loops$locked <- TRUE
      showNotification("All self-loop arrows have been locked.", type = "message")
    } else {
      showNotification("No unlocked self-loop arrows to lock.", type = "warning")
    }
  })

  observeEvent(input$unlock_selected_loop, {
    selected_row <- input$loop_table_rows_selected

    if (!is.null(selected_row)) {
      save_state()
      values$loops$locked[selected_row] <- FALSE
      showNotification(paste("Self-loop arrow at row", selected_row, "has been unlocked."), type = "message")
    } else {
      showNotification("No self-loop arrow selected. Please select a self-loop arrow to unlock.", type = "warning")
    }
  })


  observeEvent(input$lock_network, {
    save_state()

    # Lock Network elements
    values$points$network[values$points$network == TRUE] <- FALSE
    values$lines$network[values$lines$network == TRUE] <- FALSE
    values$annotations$network[values$annotations$network == TRUE] <- FALSE

    showNotification("Network changes finalized 'Apply Changes' will not affect these elements.", type = "warning")
  })

  observeEvent(input$lock_lavaan, {
    save_state()

    # Lock SEM elements
    values$points$lavaan[values$points$lavaan == TRUE] <- FALSE
    values$lines$lavaan[values$lines$lavaan == TRUE] <- FALSE
    values$annotations$lavaan[values$annotations$lavaan == TRUE] <- FALSE

    showNotification("SEM changes finalized 'Apply Changes' will not affect these elements.", type = "warning")
  })

  observeEvent(input$undo_button, {
    undo()
  })

  observeEvent(input$redo_button, {
    redo()
  })


  observeEvent( # curved line
    {
      input$x_start
      input$y_start
      input$x_end
      input$y_end
    },
    {
      req(input$x_start, input$y_start, input$x_end, input$y_end)

      default_ctrl <- default_control_point(as.numeric(input$x_start), as.numeric(input$y_start), as.numeric(input$x_end), as.numeric(input$y_end))

      updateNumericInput(session, "ctrl_x", value = default_ctrl$ctrl_x)
      updateNumericInput(session, "ctrl_y", value = default_ctrl$ctrl_y)
    }
  )

  # Add point
  observeEvent(input$add_point, {
    tryCatch({
      req(input$x_coord, input$y_coord)
      save_state() # Save the state before making changes
      new_point <- data.frame(
        x = as.numeric(input$x_coord),
        y = as.numeric(input$y_coord),
        shape = input$shape,
        color = input$point_color,
        size = input$point_size,
        border_color = input$border_color,
        border_width = input$border_width,
        alpha = input$point_alpha,
        width_height_ratio = ifelse(input$shape %in% c("rectangle", "oval", "diamond"),
                                    as.numeric(input$width_height_ratio), 1
        ),
        orientation = as.numeric(input$point_orientation),
        lavaan = FALSE,
        network = FALSE,
        locked = FALSE,
        stringsAsFactors = FALSE
      )
      values$points <- rbind(values$points, new_point)
    }, error = function(e) {
      showNotification(
        paste("Error adding point:", e$message),
        type = "error",
        duration = 5
      )
    })
  })


  # Auto layout points
  observeEvent(input$auto_layout, {
    tryCatch({
      req(nrow(values$points) > 0)
      save_state()

      values$points <- auto_layout_points(
        values$points,
        layout_type = input$layout_type,
        distance = input$point_distance,
        center_x = input$center_x,
        center_y = input$center_y,
        orientation = input$layout_orientation,
        random_seed = input$random_seed
      )
    }, error = function(e) {
      showNotification(
        paste("Error applying auto layout:", e$message),
        type = "error",
        duration = 5
      )
    })
  })

  observeEvent(input$apply_point_bulk_shift, {
    tryCatch({
      save_state()

      # Filter unlocked points
      unlocked_points <- values$points[!values$points$locked, ]

      shift_x <- if (!is.null(input$bulk_shift_x) && input$bulk_shift_x != "" && !is.na(as.numeric(input$bulk_shift_x))) {
        as.numeric(input$bulk_shift_x)
      } else {
        0
      }

      shift_y <- if (!is.null(input$bulk_shift_y) && input$bulk_shift_y != "" && !is.na(as.numeric(input$bulk_shift_y))) {
        as.numeric(input$bulk_shift_y)
      } else {
        0
      }

      if (input$bulk_shift_point_only) {
        if (nrow(unlocked_points) > 0) {
          values$points[!values$points$locked, "x"] <- unlocked_points$x + shift_x
          values$points[!values$points$locked, "y"] <- unlocked_points$y + shift_y
          showNotification("XY shifts applied to unlocked points.", type = "message")
        } else {
          showNotification("No unlocked points found to apply XY shifts.", type = "warning")
        }
      }

      if (input$bulk_aesthetics_point_only) {
        if (nrow(unlocked_points) > 0) {
          values$points[!values$points$locked, "color"] <- input$point_color
          values$points[!values$points$locked, "shape"] <- input$shape
          values$points[!values$points$locked, "size"] <- input$point_size
          values$points[!values$points$locked, "border_width"] <- input$border_width
          values$points[!values$points$locked, "border_color"] <- input$border_color
          values$points[!values$points$locked, "alpha"] <- input$point_alpha
          values$points[!values$points$locked, "orientation"] <- input$point_orientation

          if (input$shape %in% c('rectangle', 'oval', 'diamond')) {
            values$points[!values$points$locked, "width_height_ratio"] <- input$width_height_ratio
          } else {
            values$points[!values$points$locked, "width_height_ratio"] <- 1
          }


          showNotification("Aesthetic changes applied to unlocked points.", type = "message")
        } else {
          showNotification("No unlocked points found to apply aesthetic changes.", type = "warning")
        }
      }

      output$plot <- renderPlot({
        recreate_plot()
      })
    }, error = function(e) {
      showNotification(
        paste("Error applying bulk shift:", e$message),
        type = "error",
        duration = 5
      )
    })
  })


  # Add line
  observeEvent(input$add_line, {
    tryCatch({
      req(input$x_start, input$y_start, input$x_end, input$y_end)
      save_state()

      new_line <- data.frame(
        x_start = as.numeric(input$x_start),
        y_start = as.numeric(input$y_start),
        x_end = as.numeric(input$x_end),
        y_end = as.numeric(input$y_end),
        ctrl_x = if (input$line_type %in% c("Curved Line", "Curved Arrow")) as.numeric(input$ctrl_x) else NA,
        ctrl_y = if (input$line_type %in% c("Curved Line", "Curved Arrow")) as.numeric(input$ctrl_y) else NA,
        type = input$line_type,
        color = input$line_color,
        end_color = if (input$color_type == "Gradient") input$end_color else NA,
        color_type = input$color_type,
        gradient_position = if (input$color_type == "Gradient") input$gradient_position else NA,
        width = input$line_width,
        alpha = input$line_alpha,
        arrow = input$line_type %in% c("Straight Arrow", "Curved Arrow"),
        arrow_type = if (input$line_type %in% c("Straight Arrow", "Curved Arrow")) input$arrow_type else NA,
        arrow_size = if (input$line_type %in% c("Straight Arrow", "Curved Arrow")) input$arrow_size else NA,
        two_way = input$two_way_arrow,
        lavaan = FALSE,
        network = FALSE,
        line_style = input$line_style,
        locked = FALSE,
        stringsAsFactors = FALSE
      )
      add_new_line(new_line)
      # values$lines <- rbind(values$lines, new_line)
    }, error = function(e) {
      showNotification(
        paste("Error adding line:", e$message),
        type = "error",
        duration = 5
      )
    })
  })

  observeEvent(input$lock_selected_lines, {
    tryCatch({
      selected_rows <- input$line_table_rows_selected

      if (!is.null(selected_rows) && length(selected_rows) > 0) {
        save_state()
        values$lines$locked[selected_rows] <- TRUE # Lock the selected lines
        showNotification(paste(length(selected_rows), "line(s) locked."), type = "message")
      } else {
        showNotification("No line selected. Please select line(s) to lock.", type = "warning")
      }
    }, error = function(e) {
      showNotification(
        paste("Error locking selected lines:", e$message),
        type = "error",
        duration = 5
      )
    })
  })


  observeEvent(input$unlock_selected_lines, {
    tryCatch({
      selected_rows <- input$line_table_rows_selected

      if (!is.null(selected_rows) && length(selected_rows) > 0) {
        save_state()
        values$lines$locked[selected_rows] <- FALSE # Unlock the selected lines
        showNotification(paste(length(selected_rows), "line(s) unlocked."), type = "message")
      } else {
        showNotification("No line selected. Please select line(s) to unlock.", type = "warning")
      }
    }, error = function(e) {
      showNotification(
        paste("Error unlocking selected lines:", e$message),
        type = "error",
        duration = 5
      )
    })
  })


  observeEvent(input$lock_selected_annotation, {
    tryCatch({
      selected_rows <- input$annotation_table_rows_selected

      if (!is.null(selected_rows) && length(selected_rows) > 0) {
        save_state()
        values$annotations$locked[selected_rows] <- TRUE # Lock the selected annotations
        showNotification(paste(length(selected_rows), "annotation(s) locked."), type = "message")
      } else {
        showNotification("No annotation selected. Please select annotation(s) to lock.", type = "warning")
      }
    }, error = function(e) {
      showNotification(
        paste("Error locking selected annotations:", e$message),
        type = "error",
        duration = 5
      )
    })
  })


  observeEvent(input$unlock_selected_annotation, {
    tryCatch({
      selected_rows <- input$annotation_table_rows_selected

      if (!is.null(selected_rows) && length(selected_rows) > 0) {
        save_state()
        values$annotations$locked[selected_rows] <- FALSE # Unlock the selected annotations
        showNotification(paste(length(selected_rows), "annotation(s) unlocked."), type = "message")
      } else {
        showNotification("No annotation selected. Please select annotation(s) to unlock.", type = "warning")
      }
    }, error = function(e) {
      showNotification(
        paste("Error unlocking selected annotations:", e$message),
        type = "error",
        duration = 5
      )
    })
  })

  # Auto-generate edges
  observeEvent(input$auto_generate_edges_button, {
    tryCatch({
      req(nrow(values$points) > 1)

      save_state()

      connection_type <- input$connection_type

      new_edges <- auto_generate_edges(
        points_data = values$points,
        layout_type = connection_type,
        line_color = input$auto_line_color,
        line_width = input$auto_line_width,
        line_alpha = input$auto_line_alpha,
        line_style = input$auto_line_style,
        particular_node = input$particular_node,
        auto_endpoint_spacing = input$auto_endpoint_spacing,
        zoom_factor = input$zoom,
        random_seed = input$random_seed
      )

      if (!is.null(new_edges)) {
        for (i in 1:nrow(new_edges)) {
          if (input$arrow_location == "start") {
            # Swap start and end coordinates
            temp_x <- new_edges$x_start[i]
            temp_y <- new_edges$y_start[i]
            new_edges$x_start[i] <- new_edges$x_end[i]
            new_edges$y_start[i] <- new_edges$y_end[i]
            new_edges$x_end[i] <- temp_x
            new_edges$y_end[i] <- temp_y
          }
        }

        new_edges$width <- input$auto_line_width
        new_edges$alpha <- input$auto_line_alpha
        new_edges$color <- input$auto_line_color

        # Arrow properties
        new_edges$arrow <- (input$edge_type == "Arrow")
        new_edges$arrow_type <- if (input$edge_type == "Arrow") input$arrow_type else NA
        new_edges$arrow_size <- if (input$edge_type == "Arrow") input$arrow_size else NA
        new_edges$two_way <- if (input$edge_type == "Arrow") input$two_way_arrow else FALSE

        values$lines <- rbind(values$lines, new_edges)
      } else {
        showModal(modalDialog(
          title = "Error",
          "Not enough unlocked points to generate edges.",
          easyClose = TRUE
        ))
      }
    }, error = function(e) {
      showNotification(
        paste("Error auto-generating edges:", e$message),
        type = "error",
        duration = 5
      )
    })
  })

  # observe({
  #   if (nrow(values$points) > 0) {
  #     unlocked_points <- values$points[!values$points$locked, ]
  #
  #     point_choices <- rownames(unlocked_points)
  #
  #     # point_choices <- seq_len(nrow(values$points))
  #     updateSelectInput(session, "particular_node", choices = point_choices)
  #   }
  # })

  observe({
    req(input$x_start, input$y_start, input$x_end, input$y_end)

    mid_x <- (as.numeric(input$x_start) + as.numeric(input$x_end)) / 2
    mid_y <- (as.numeric(input$y_start) + as.numeric(input$y_end)) / 2

    dx <- as.numeric(input$x_end) - as.numeric(input$x_start)
    dy <- as.numeric(input$y_end) - as.numeric(input$y_start)

    offset_x <- -dy
    offset_y <- dx

    magnitude <- input$curvature_magnitude
    ctrl_x <- mid_x + offset_x * magnitude
    ctrl_y <- mid_y + offset_y * magnitude

    if (input$rotate_curvature) {
      ctrl_x <- 2 * mid_x - ctrl_x
      ctrl_y <- 2 * mid_y - ctrl_y
    }

    updateNumericInput(session, "ctrl_x", value = ctrl_x)
    updateNumericInput(session, "ctrl_y", value = ctrl_y)
  })

  observe({
    req(input$lavaan_curvature_magnitude)

    # Update curvature control points for two-way edges
    if (any(values$lines$two_way & values$lines$lavaan)) {
      two_way_indices <- which(values$lines$two_way & values$lines$lavaan)

      control_points <- mapply(
        function(x_start, y_start, x_end, y_end) {
          mid_x <- (x_start + x_end) / 2
          mid_y <- (y_start + y_end) / 2
          dx <- x_end - x_start
          dy <- y_end - y_start

          offset_x <- -dy * input$lavaan_curvature_magnitude
          offset_y <- dx * input$lavaan_curvature_magnitude

          ctrl_x <- mid_x + offset_x
          ctrl_y <- mid_y + offset_y

          # Apply 180° rotation if the option is selected
          if (isTRUE(input$lavaan_rotate_curvature)) {
            ctrl_x <- 2 * mid_x - ctrl_x
            ctrl_y <- 2 * mid_y - ctrl_y
          }

          list(ctrl_x = ctrl_x, ctrl_y = ctrl_y)
        },
        x_start = values$lines$x_start[two_way_indices],
        y_start = values$lines$y_start[two_way_indices],
        x_end = values$lines$x_end[two_way_indices],
        y_end = values$lines$y_end[two_way_indices],
        SIMPLIFY = FALSE
      )

      # Update the control points in the `values$lines` data frame
      values$lines$ctrl_x[two_way_indices] <- sapply(control_points, `[[`, "ctrl_x")
      values$lines$ctrl_y[two_way_indices] <- sapply(control_points, `[[`, "ctrl_y")
    }
  })

  observeEvent(input$apply_line_bulk_shift, {
    tryCatch({
      save_state()

      # Filter unlocked lines
      unlocked_lines <- values$lines[!values$lines$locked, ]

      line_shift_x <- if (!is.null(input$line_bulk_shift_x) && input$line_bulk_shift_x != "" && !is.na(as.numeric(input$line_bulk_shift_x))) {
        as.numeric(input$line_bulk_shift_x)
      } else {
        0
      }

      line_shift_y <- if (!is.null(input$line_bulk_shift_y) && input$line_bulk_shift_y != "" && !is.na(as.numeric(input$line_bulk_shift_y))) {
        as.numeric(input$line_bulk_shift_y)
      } else {
        0
      }
      #print('hello world')
      #print(unlocked_lines$x_start)
      #print(unlocked_lines$x_start)
      #print(line_shift_x)
      unlocked_lines$x_start + line_shift_x
      if (input$bulk_shift_line_only) {
        if (nrow(unlocked_lines) > 0) {
          values$lines[!values$lines$locked, "x_start"] <- unlocked_lines$x_start + line_shift_x
          values$lines[!values$lines$locked, "y_start"] <- unlocked_lines$y_start + line_shift_y
          values$lines[!values$lines$locked, "x_end"] <- unlocked_lines$x_end + line_shift_x
          values$lines[!values$lines$locked, "y_end"] <- unlocked_lines$y_end + line_shift_y

          showNotification("XY shifts applied to unlocked lines.", type = "message")
        } else {
          showNotification("No unlocked lines found to apply XY shifts.", type = "warning")
        }
      }

      if (input$bulk_aesthetics_line_only) {
        if (nrow(unlocked_lines) > 0) {
          # Update styling attributes for unlocked lines
          values$lines[!values$lines$locked, "color"] <- input$line_color
          values$lines[!values$lines$locked, "color_type"] <- input$color_type
          values$lines[!values$lines$locked, "width"] <- input$line_width
          values$lines[!values$lines$locked, "alpha"] <- input$line_alpha
          values$lines[!values$lines$locked, "line_style"] <- input$line_style
          values$lines[!values$lines$locked, "line_type"] <- input$line_type


          values$lines[!values$lines$locked, "two_way"] <- input$two_way_arrow

          if (input$line_type %in% c("Straight Arrow", "Curved Arrow")) {
            values$lines[!values$lines$locked, "arrow_size"] <- input$arrow_size
            values$lines[!values$lines$locked, "arrow_type"] <- input$arrow_type
          } else {
            values$lines[!values$lines$locked, "arrow_size"] <- NA
            values$lines[!values$lines$locked, "arrow_type"] <- NA
          }

          if (input$color_type == "Gradient") {
            values$lines[!values$lines$locked, "end_color"] <- input$end_color
            values$lines[!values$lines$locked, "gradient_position"] <- input$gradient_position
          }

          showNotification("Aesthetic changes applied to unlocked lines.", type = "message")
        } else {
          showNotification("No unlocked lines found to apply aesthetic changes.", type = "warning")
        }
      }

      output$plot <- renderPlot({
        recreate_plot()
      })
    }, error = function(e) {
      showNotification(
        paste("Error applying line bulk changes:", e$message),
        type = "error",
        duration = 5
      )
    })
  })



  observeEvent(input$apply_annotation_changes, {
    tryCatch({
      save_state()

      # Check for unlocked annotations
      unlocked_annotations <- !values$annotations$locked

      # Safely handle input values for bulk shift
      annotation_shift_x <- if (!is.null(input$annotation_bulk_shift_x) && input$annotation_bulk_shift_x != "" && !is.na(as.numeric(input$annotation_bulk_shift_x))) {
        as.numeric(input$annotation_bulk_shift_x)
      } else {
        0
      }

      annotation_shift_y <- if (!is.null(input$annotation_bulk_shift_y) && input$annotation_bulk_shift_y != "" && !is.na(as.numeric(input$annotation_bulk_shift_y))) {
        as.numeric(input$annotation_bulk_shift_y)
      } else {
        0
      }

      if (input$bulk_shift_annotation_only) {
        if (any(unlocked_annotations)) {
          values$annotations$x[unlocked_annotations] <- values$annotations$x[unlocked_annotations] + annotation_shift_x
          values$annotations$y[unlocked_annotations] <- values$annotations$y[unlocked_annotations] + annotation_shift_y

          showNotification("XY shifts applied to unlocked annotations.", type = "message")
        } else {
          showNotification("No unlocked annotations found to apply XY shifts.", type = "warning")
        }
      }
      if (input$bulk_aesthetics_annotation_only) {
        if (any(unlocked_annotations)) {
          # Update styling attributes for unlocked annotations
          values$annotations$font[unlocked_annotations] <- input$font_family
          values$annotations$size[unlocked_annotations] <- input$text_size
          values$annotations$color[unlocked_annotations] <- input$text_color
          values$annotations$fontface[unlocked_annotations] <- switch(input$text_typeface,
                                                                      "Bold" = "bold",
                                                                      "Italic" = "italic",
                                                                      "Plain" = "plain"
          )
          values$annotations$alpha[unlocked_annotations] <- input$text_alpha
          values$annotations$angle[unlocked_annotations] <- input$text_angle

          showNotification("Aesthetic changes applied to unlocked annotations.", type = "message")
        } else {
          showNotification("No unlocked annotations found to apply aesthetic changes.", type = "warning")
        }
      }

      # Trigger a plot update
      output$plot <- renderPlot({
        recreate_plot()
      })
    }, error = function(e) {
      showNotification(
        paste("Error applying annotation changes:", e$message),
        type = "error",
        duration = 5
      )
    })
  })



  # Add annotation
  observeEvent(input$add_annotation, {
    tryCatch({
      req(input$annotation_text, input$annotation_x, input$annotation_y)
      save_state()

      fontface <- switch(input$text_typeface,
                         "Bold" = "bold",
                         "Italic" = "italic",
                         "Plain" = "plain"
      )

      new_annotation <- data.frame(
        text = input$annotation_text,
        x = as.numeric(input$annotation_x),
        y = as.numeric(input$annotation_y),
        font = input$font_family,
        size = input$text_size,
        color = input$text_color,
        angle = input$text_angle,
        alpha = input$text_alpha,
        fontface = fontface,
        math_expression = input$math_expression,
        lavaan = FALSE,
        network = FALSE,
        locked = FALSE,
        stringsAsFactors = FALSE
      )

      values$annotations <- rbind(values$annotations, new_annotation)
    }, error = function(e) {
      showNotification(
        paste("Error adding annotation:", e$message),
        type = "error",
        duration = 5
      )
    })
  })


  # Add self-loop arrow
  observeEvent(input$add_loop, {
    tryCatch({
      req(input$x_center, input$y_center, input$radius, input$gap_size_loop, input$width_loop, input$height_loop, input$orientation_loop)
      save_state()

      t <- seq(0, 2 * pi, length.out = 100)
      gap_angle <- input$gap_size_loop * pi
      loop_start <- t[t < (2 * pi - gap_angle)]

      x_ellipse <- as.numeric(input$x_center) + input$width_loop * input$radius * cos(loop_start)
      y_ellipse <- as.numeric(input$y_center) + input$height_loop * input$radius * sin(loop_start)

      theta <- input$orientation_loop * pi / 180
      x_rotated <- cos(theta) * (x_ellipse - as.numeric(input$x_center)) - sin(theta) * (y_ellipse - as.numeric(input$y_center)) + as.numeric(input$x_center)
      y_rotated <- sin(theta) * (x_ellipse - as.numeric(input$x_center)) + cos(theta) * (y_ellipse - as.numeric(input$y_center)) + as.numeric(input$y_center)

      circle_data <- data.frame(x = x_rotated, y = y_rotated)

      arrow_type <- if (input$arrow_type_loop == "closed") arrow(type = "closed", length = unit(input$arrow_size_loop, "inches")) else arrow(type = "open", length = unit(input$arrow_size_loop, "inches"))

      new_loop <- data.frame(
        x_center = as.numeric(input$x_center),
        y_center = as.numeric(input$y_center),
        radius = input$radius,
        color = input$line_color_loop,
        width = input$line_width_loop,
        alpha = input$line_alpha_loop,
        arrow_type = input$arrow_type_loop,
        arrow_size = input$arrow_size_loop,
        gap_size = input$gap_size_loop,
        loop_width = input$width_loop,
        loop_height = input$height_loop,
        orientation = input$orientation_loop,
        two_way = input$two_way_arrow_loop,
        locked = FALSE,
        stringsAsFactors = FALSE
      )

      values$loops <- rbind(values$loops, new_loop)
    }, error = function(e) {
      showNotification(
        paste("Error adding loop:", e$message),
        type = "error",
        duration = 5
      )
    })
  })


  observeEvent(input$apply_loop_changes, {
    req(nrow(values$loops) > 0)

    save_state()

    for (i in 1:nrow(values$loops)) {
      if (!values$loops$locked[i]) {
        values$loops$orientation[i] <- input$orientation_loop
        values$loops$gap_size[i] <- input$gap_size_loop
      }
    }
  })

  last_valid_syntax <- reactiveVal(NULL)

  observeEvent(input$generate_graph, {
    req(input$lavaan_syntax)

    save_state()

    fontface_latent <- switch(input$text_fontface_input,
                              "Bold" = "bold",
                              "Italic" = "italic",
                              "Plain" = "plain"
    )

    fontface_others <- switch(input$text_fontface_others,
                              "Bold" = "bold",
                              "Italic" = "italic",
                              "Plain" = "plain"
    )

    fontface_edges <- switch(input$text_fontface_edges,
                             "Bold" = "bold",
                             "Italic" = "italic",
                             "Plain" = "plain"
    )



    if (!grepl("lavaan_string", input$sem_code) || !grepl("data", input$sem_code)) {
      showNotification(
        "Custom SEM code must include `lavaan_string` and `data` as-is.",
        type = "error"
      )
      return(NULL)
    }

    updateSelectInput(session, "layer_order", selected = "annotations_front")

    tryCatch(
      {
        data_file <- if (!is.null(input$edge_label_file) && file.exists(input$edge_label_file$datapath)) {
          input$edge_label_file$datapath
        } else {
          NULL
        }

        graph_data <- generate_graph_from_lavaan(input$lavaan_syntax,
                                                 sem_code = input$sem_code,
                                                 data_file = data_file,
                                                 relative_x_position = input$relative_x_position,
                                                 relative_y_position = input$relative_y_position,
                                                 center_x = input$center_x_position,
                                                 center_y = input$center_y_position,
                                                 latent_shape = input$latent_shape,
                                                 observed_shape = input$observed_shape,
                                                 int_shape = input$int_shape,
                                                 point_size_latent = input$latent_size_input,
                                                 point_size_observed = input$observed_size_input,
                                                 point_size_int = input$int_size_input,
                                                 line_width = input$line_width_input,
                                                 text_size_latent = input$text_size_input,
                                                 text_font_latent = input$text_font_input,
                                                 text_color_latent = input$text_color_input,
                                                 text_alpha_latent = input$text_alpha_input,
                                                 text_fontface_latent = fontface_latent,
                                                 text_size_others = input$text_size_others,
                                                 text_font_others = input$text_font_others,
                                                 text_color_others = input$text_color_others,
                                                 text_alpha_others = input$text_alpha_others,
                                                 text_fontface_others = fontface_others,
                                                 text_size_edges = input$text_size_edges,
                                                 text_font_edges = input$text_font_edges,
                                                 text_color_edges = input$text_color_edges,
                                                 text_alpha_edges = input$text_alpha_edges,
                                                 text_fontface_edges = fontface_edges,
                                                 point_color_latent = input$latent_color_input,
                                                 point_color_observed = input$observed_color_input,
                                                 point_color_int = input$int_color_input,
                                                 edge_color = input$edge_color_input,
                                                 line_endpoint_spacing = input$line_endpoint_spacing,
                                                 node_border_color = input$node_border_color,
                                                 node_border_width = input$node_border_width,
                                                 fontface = fontface,
                                                 arrow_type = input$lavaan_arrow_type,
                                                 arrow_size = input$lavaan_arrow_size,
                                                 layout_algorithm = input$lavaan_layout,
                                                 lavaan_arrow_location = input$lavaan_arrow_location,
                                                 zoom_factor = input$zoom,
                                                 lavaan_curvature_magnitude = input$lavaan_curvature_magnitude,
                                                 lavaan_rotate_curvature = input$lavaan_rotate_curvature,
                                                 data_format = input$data_format
        )

        values$last_lavaan_layout <- input$lavaan_layout
        values$last_relative_x_position <- input$relative_x_position
        values$last_relative_y_position <- input$relative_y_position
        values$last_center_x_position <- input$center_x_position
        values$last_center_y_position <- input$center_y_position

        last_valid_syntax(input$lavaan_syntax)
        values$points <- rbind(values$points, graph_data$points)
        values$lines <- rbind(values$lines, graph_data$lines)
        values$annotations <- rbind(values$annotations, graph_data$annotations)



        output$plot <- renderPlot({
          recreate_plot()
        })

        # Show notification
        if (!is.null(data_file)) {
          showNotification("Graph generated successfully using the uploaded CSV file.", type = "message")
        } else {
          showNotification("Graph generated using no data (no edge labels).", type = "message")
        }
      },
      error = function(e) {
        showNotification(paste("Error in generating SEM from lavaan model:", e$message), type = "error")
      }
    )
  })


  observeEvent(input$apply_changes_lavaan, {
    tryCatch({
      req(input$lavaan_syntax)

      if (is.null(last_valid_syntax())) {
        showNotification(
          "To apply changes, SEM graph must be generated first (not from CSVs with grahpical info). Unlock selected elements to perform aesthetic grouping.",
          type = "error"
        )
        return()
      } else if (input$lavaan_syntax != last_valid_syntax()) {
        showNotification(
          "Cannot apply changes: Model syntax has been modified. Please regenerate the graph.",
          type = "error"
        )
        return() # Stop execution
      }

      save_state()
      lavaan_points0 <- values$points[values$points$lavaan, ]

      if (nrow(lavaan_points0) == 0) {
        showNotification("No lavaan points to apply changes.", type = "warning")
      } else {
        fontface_latent <- switch(input$text_fontface_input,
                                  "Bold" = "bold",
                                  "Italic" = "italic",
                                  "Plain" = "plain"
        )

        fontface_others <- switch(input$text_fontface_others,
                                  "Bold" = "bold",
                                  "Italic" = "italic",
                                  "Plain" = "plain"
        )

        fontface_edges <- switch(input$text_fontface_edges,
                                 "Bold" = "bold",
                                 "Italic" = "italic",
                                 "Plain" = "plain"
        )

        data_file <- if (!is.null(input$edge_label_file) && file.exists(input$edge_label_file$datapath)) {
          input$edge_label_file$datapath
        } else {
          NULL
        }

        layout_changed <- is.null(values$last_lavaan_layout) ||
          input$lavaan_layout != values$last_lavaan_layout

        scaling_changed <- input$relative_x_position != values$last_relative_x_position || input$relative_y_position != values$last_relative_y_position

        positional_shift <- input$center_x_position != values$last_center_x_position || input$center_y_position != values$last_center_y_position

        if (layout_changed) {
          values$last_lavaan_layout <- input$lavaan_layout
        }

        graph_data <- generate_graph_from_lavaan(input$lavaan_syntax,
                                                 sem_code = input$sem_code,
                                                 data_file = data_file,
                                                 relative_x_position = input$relative_x_position,
                                                 relative_y_position = input$relative_y_position,
                                                 center_x = input$center_x_position,
                                                 center_y = input$center_y_position,
                                                 latent_shape = input$latent_shape,
                                                 observed_shape = input$observed_shape,
                                                 int_shape = input$int_shape,
                                                 point_size_latent = input$latent_size_input,
                                                 point_size_observed = input$observed_size_input,
                                                 point_size_int = input$int_size_input,
                                                 line_width = input$line_width_input,
                                                 text_size_latent = input$text_size_input,
                                                 text_font_latent = input$text_font_input,
                                                 text_color_latent = input$text_color_input,
                                                 text_alpha_latent = input$text_alpha_input,
                                                 text_fontface_latent = fontface_latent,
                                                 text_size_others = input$text_size_others,
                                                 text_font_others = input$text_font_others,
                                                 text_color_others = input$text_color_others,
                                                 text_alpha_others = input$text_alpha_others,
                                                 text_fontface_others = fontface_others,
                                                 text_size_edges = input$text_size_edges,
                                                 text_font_edges = input$text_font_edges,
                                                 text_color_edges = input$text_color_edges,
                                                 text_alpha_edges = input$text_alpha_edges,
                                                 text_fontface_edges = fontface_edges,
                                                 point_color_latent = input$latent_color_input,
                                                 point_color_observed = input$observed_color_input,
                                                 point_color_int = input$int_color_input,
                                                 edge_color = input$edge_color_input,
                                                 line_endpoint_spacing = input$line_endpoint_spacing,
                                                 node_border_color = input$node_border_color,
                                                 node_border_width = input$node_border_width,
                                                 fontface = fontface,
                                                 arrow_type = input$lavaan_arrow_type,
                                                 arrow_size = input$lavaan_arrow_size,
                                                 layout_algorithm = input$lavaan_layout,
                                                 lavaan_arrow_location = input$lavaan_arrow_location,
                                                 zoom_factor = input$zoom,
                                                 lavaan_curvature_magnitude = input$lavaan_curvature_magnitude,
                                                 lavaan_rotate_curvature = input$lavaan_rotate_curvature,
                                                 data_format = input$data_format
        )

        lavaan_points <- which(values$points$lavaan == TRUE)
        if (length(lavaan_points) > 0) {
          values$points[lavaan_points, ] <- graph_data$points
        }

        lavaan_lines <- which(values$lines$lavaan == TRUE)
        if (length(lavaan_lines) > 0) {
          values$lines[lavaan_lines, ] <- graph_data$lines
        }

        lavaan_annotations <- which(values$annotations$lavaan == TRUE)
        lavaan_annotations_xy0 <- values$annotations[lavaan_annotations, c('x', 'y')]
        if (length(lavaan_annotations) > 0) {
          values$annotations[lavaan_annotations, ] <- graph_data$annotations
          if (!layout_changed && !scaling_changed && !positional_shift) {
            values$annotations[lavaan_annotations, c('x', 'y')] <- lavaan_annotations_xy0
          }
        }

        output$plot <- renderPlot({
          recreate_plot()
        })

        showNotification("Graph updated successfully.", type = "message")
      }
    }, error = function(e) {
      showNotification(
        paste("Error applying lavaan changes:", e$message),
        type = "error",
        duration = 5
      )
    })
  })

  observeEvent(input$apply_loop_changes, {
    tryCatch({
      req(nrow(values$loops) > 0)

      save_state()

      for (i in 1:nrow(values$loops)) {
        if (!values$loops$locked[i]) {
          values$loops$gap_size[i] <- input$gap_size_loop
          values$loops$orientation[i] <- input$orientation_loop
        }
      }

      output$plot <- renderPlot({
        recreate_plot()
      })

      showNotification("Loop changes applied successfully.", type = "message")
    }, error = function(e) {
      showNotification(
        paste("Error applying loop changes:", e$message),
        type = "error",
        duration = 5
      )
    })
  })


  observeEvent(input$rotate_curvature, {
    req(input$x_start, input$y_start, input$x_end, input$y_end, input$ctrl_x, input$ctrl_y)

    mid_x <- (as.numeric(input$x_start) + as.numeric(input$x_end)) / 2
    mid_y <- (as.numeric(input$y_start) + as.numeric(input$y_end)) / 2

    ctrl_x_new <- 2 * mid_x - as.numeric(input$ctrl_x)
    ctrl_y_new <- 2 * mid_y - as.numeric(input$ctrl_y)

    updateNumericInput(session, "ctrl_x", value = ctrl_x_new)
    updateNumericInput(session, "ctrl_y", value = ctrl_y_new)
  })

  observeEvent(input$apply_gradient, {
    tryCatch({
      unlocked_points <- values$points[!values$points$locked & !values$points$lavaan, ]

      if (nrow(unlocked_points) > 1) {
        grad_start_color <- input$grad_start_color
        grad_end_color <- input$grad_end_color
        gradient_colors_layout <- colorRampPalette(c(grad_start_color, grad_end_color))(nrow(unlocked_points))

        values$points[!values$points$locked & !values$points$lavaan, "color"] <- gradient_colors_layout

        output$plot <- renderPlot({
          recreate_plot()
        })

        showNotification("Gradient applied successfully.", type = "message")
      } else {
        showNotification("Insufficient unlocked points to apply gradient.", type = "warning")
      }
    }, error = function(e) {
      showNotification(
        paste("Error applying gradient:", e$message),
        type = "error",
        duration = 5
      )
    })
  })



  last_valid_network_data <- reactiveVal(NULL)

  observeEvent(input$generate_network, {
    tryCatch({
      if (is.null(input$network_file$datapath)) {
        showNotification("Network data file CSV required.", type = "error")
        return()
      }

      req(input$network_file$datapath)
      save_state()

      node_label_fontface <- switch(input$node_label_fontface,
                                    "Bold" = "bold",
                                    "Italic" = "italic",
                                    "Plain" = "plain"
      )

      edge_label_fontface <- switch(input$edge_label_fontface,
                                    "Bold" = "bold",
                                    "Italic" = "italic",
                                    "Plain" = "plain"
      )

      # Process network data
      network_graph <- generate_graph_from_network(
        network_data_file = input$network_file$datapath,
        directed = input$is_directed,
        layout_method = input$layout_method,
        layout_width = input$layout_x_net,
        layout_height = input$layout_y_net,
        x_center = input$x_center_net,
        y_center = input$y_center_net,
        node_shape = input$node_shape_net,
        node_size = input$node_size_net,
        node_fill_color = input$node_fill_color_net,
        node_border_color = input$node_border_color_net,
        node_border_width = input$node_border_width_net,
        node_width_height_ratio = input$node_width_height_ratio_net,
        line_width = input$line_width_net,
        line_color = input$line_color_net,
        line_alpha = input$line_alpha_net,
        min_edge_width = ifelse(input$scale_edge_width, input$min_edge_width, NULL),
        max_edge_width = ifelse(input$scale_edge_width, input$max_edge_width, NULL),
        scale_by_weight = input$scale_edge_width,
        line_endpoint_spacing = input$line_endpoint_spacing_net,
        arrow_type = input$arrow_type_net,
        arrow_size = input$arrow_size_net,
        node_label_font = input$node_label_font,
        node_label_size = input$node_label_size,
        node_label_color = input$node_label_color,
        node_label_alpha = input$node_label_alpha,
        node_label_fontface = node_label_fontface,
        edge_label_font = input$edge_label_font,
        edge_label_size = input$edge_label_size,
        edge_label_color = input$edge_label_color,
        edge_label_alpha = input$edge_label_alpha,
        edge_label_fontface = edge_label_fontface,
        zoom_factor = input$zoom,
        annotate_nodes = TRUE,
        annotate_edges = TRUE,
        random_seed = input$random_seed,
        use_clustering = input$use_clustering,
        clustering_method = input$clustering_method,
        cluster_palette = input$cluster_palette,
        dim_reduction_method = input$dim_reduction_method
      )

      last_valid_network_data(input$network_file$datapath)

      updateSelectInput(session, "layer_order", selected = "annotations_front")

      values$last_layout_method <- input$layout_method
      if (input$layout_method == "dim_reduction") {
        values$last_dim_reduction_method <- input$dim_reduction_method
      } else {
        values$last_dim_reduction_method <- NULL
      }

      values$last_layout_x <- input$layout_x_net
      values$last_layout_y <- input$layout_y_net
      values$last_x_center <- input$x_center_net
      values$last_y_center <- input$y_center_net
      values$last_annotate_edges <- input$annotate_edges
      values$last_random_seed <- input$random_seed

      # Update reactive values
      values$points <- rbind(values$points, network_graph$points)
      values$lines <- rbind(values$lines, network_graph$lines)
      values$annotations <- rbind(values$annotations, network_graph$annotations)

      # Trigger plot redraw
      output$plot <- renderPlot({
        recreate_plot()
      })

      # Notify user
      showNotification("Network diagram generated successfully.", type = "message")
    }, error = function(e) {
      showNotification(
        paste("Error generating network:", e$message),
        type = "error",
        duration = 5
      )
    })
  })

  observeEvent(input$apply_changes_network, {
    tryCatch({
      req(input$network_file$datapath)

      if (input$network_file$datapath != last_valid_network_data()) {
        showNotification(
          "Cannot apply changes: Data file (and/or path) has been modified. Please regenerate the graph.",
          type = "error"
        )
        return() # Stop further execution
      }

      save_state()

      network_lines0 <- values$lines[values$lines$network, ]
      network_points0 <- values$points[values$points$network, ]

      if (nrow(network_lines0) == 0 && nrow(network_points0) == 0) {
        showNotification("No network nodes or edges to apply changes.", type = "warning")
      } else {
        node_label_fontface <- switch(input$node_label_fontface,
                                      "Bold" = "bold",
                                      "Italic" = "italic",
                                      "Plain" = "plain"
        )

        edge_label_fontface <- switch(input$edge_label_fontface,
                                      "Bold" = "bold",
                                      "Italic" = "italic",
                                      "Plain" = "plain"
        )

        network_data <- read.csv(input$network_file$datapath)

        # print(values$last_dim_reduction_method)
        layout_changed <- if (values$last_layout_method == "dim_reduction") {
          input$layout_method != values$last_layout_method || input$dim_reduction_method != values$last_dim_reduction_method
        } else {
          input$layout_method != values$last_layout_method
        }

        scaling_changed <- input$layout_x_net != values$last_layout_x || input$layout_y_net != values$last_layout_y
        positional_shift <- input$x_center_net != values$last_x_center || input$y_center_net != values$last_y_center

        if (layout_changed) {
          values$last_layout_method <- input$layout_method

          if (input$layout_method == "dim_reduction") {
            values$last_dim_reduction_method <- input$dim_reduction_method
            showNotification(
              paste("Layout changed to Dimensionality Reduction using", input$dim_reduction_method, "method."),
              type = "message",
              duration = 5
            )
          } else {
            values$last_dim_reduction_method <- NULL
            showNotification(
              paste("Layout changed to", input$layout_method, "method."),
              type = "message",
              duration = 5
            )
          }
        }


        if (scaling_changed || positional_shift) {
          scale_x <- input$layout_x_net / values$last_layout_x
          scale_y <- input$layout_y_net / values$last_layout_y
          shift_x <- input$x_center_net - values$last_x_center
          shift_y <- input$y_center_net - values$last_y_center

          values$points <- values$points %>%
            mutate(
              x = x * ifelse(scaling_changed, scale_x, 1) + shift_x,
              y = y * ifelse(scaling_changed, scale_y, 1) + shift_y
            )

          values$lines <- values$lines %>%
            mutate(
              x_start = x_start * ifelse(scaling_changed, scale_x, 1) + shift_x,
              y_start = y_start * ifelse(scaling_changed, scale_y, 1) + shift_y,
              x_end = x_end * ifelse(scaling_changed, scale_x, 1) + shift_x,
              y_end = y_end * ifelse(scaling_changed, scale_y, 1) + shift_y
            )

          values$annotations <- values$annotations %>%
            mutate(
              x = x * ifelse(scaling_changed, scale_x, 1) + shift_x,
              y = y * ifelse(scaling_changed, scale_y, 1) + shift_y
            )

          values$last_layout_x <- input$layout_x_net
          values$last_layout_y <- input$layout_y_net
          values$last_x_center <- input$x_center_net
          values$last_y_center <- input$y_center_net
        }

        # if (layout_changed) {
        #   values$last_layout_method <- input$layout_method
        #   if (input$layout_method == "dim_reduction") {
        #     values$last_dim_reduction_method <- input$dim_reduction_method
        #   }
        # }

        existing_points <- if (layout_changed) NULL else {
          values$points %>% filter(network == TRUE)
        }

        network_graph <- generate_graph_from_network(
          network_data_file = input$network_file$datapath,
          directed = input$is_directed,
          layout_method = input$layout_method,
          layout_width = input$layout_x_net,
          layout_height = input$layout_y_net,
          x_center = input$x_center_net,
          y_center = input$y_center_net,
          node_shape = input$node_shape_net,
          node_size = input$node_size_net,
          node_fill_color = input$node_fill_color_net,
          node_border_color = input$node_border_color_net,
          node_border_width = input$node_border_width_net,
          node_width_height_ratio = input$node_width_height_ratio_net,
          line_width = input$line_width_net,
          line_color = input$line_color_net,
          line_alpha = input$line_alpha_net,
          min_edge_width = ifelse(input$scale_edge_width, input$min_edge_width, NULL),
          max_edge_width = ifelse(input$scale_edge_width, input$max_edge_width, NULL),
          scale_by_weight = input$scale_edge_width,
          line_endpoint_spacing = input$line_endpoint_spacing_net,
          arrow_type = input$arrow_type_net,
          arrow_size = input$arrow_size_net,
          node_label_font = input$node_label_font,
          node_label_size = input$node_label_size,
          node_label_color = input$node_label_color,
          node_label_alpha = input$node_label_alpha,
          node_label_fontface = node_label_fontface,
          edge_label_font = input$edge_label_font,
          edge_label_size = input$edge_label_size,
          edge_label_color = input$edge_label_color,
          edge_label_alpha = input$edge_label_alpha,
          edge_label_fontface = edge_label_fontface,
          zoom_factor = input$zoom,
          annotate_nodes = TRUE,
          annotate_edges = TRUE,
          existing_points = existing_points,
          random_seed = input$random_seed,
          use_clustering = input$use_clustering,
          clustering_method = input$clustering_method,
          cluster_palette = input$cluster_palette,
          dim_reduction_method = input$dim_reduction_method
        )

        updateSelectInput(session, "layer_order", selected = "annotations_front")

        network_points <- which(values$points$network == TRUE)
        if (length(network_points) > 0) {
          values$points[network_points, ] <- network_graph$points
        }

        network_lines <- which(values$lines$network == TRUE)
        if (length(network_lines) > 0) {
          values$lines[network_lines, ] <- network_graph$lines
        }

        network_annotations <- which(values$annotations$network == TRUE)
        network_annotations_xy0 <- values$annotations[network_annotations, c('x', 'y')]
        if (length(network_annotations) > 0) {
          values$annotations[network_annotations, ] <- network_graph$annotations
          if (!layout_changed && !scaling_changed && !positional_shift) {
            values$annotations[network_annotations, c('x', 'y')] <- network_annotations_xy0
          }
        }

        output$plot <- renderPlot({
          recreate_plot()
        })

        showNotification("Graph updated successfully.", type = "message")
      }
    }, error = function(e) {
      showNotification(
        paste("Error applying network changes:", e$message),
        type = "error",
        duration = 5
      )
    })
  })

  observeEvent(input$delete_all_points, {
    save_state()
    values$points <- data.frame(
      x = numeric(), y = numeric(), shape = character(), color = character(), size = numeric(),
      border_color = character(), border_width = numeric(), alpha = numeric(), width_height_ratio = numeric(),
      orientation = numeric(), lavaan = logical(), network = logical(), locked = logical(), stringsAsFactors = FALSE
    )
  })

  observeEvent(input$delete_selected_point, {
    selected_row <- input$data_table_rows_selected
    if (!is.null(selected_row)) {
      save_state()
      values$points <- values$points[-selected_row, ]
      showNotification(
        paste("Points at rows", paste(selected_row, collapse = ", "), "have been deleted."),
        type = "message"
      )
    }
  })


  observeEvent(input$delete_selected_line, {
    selected_row <- input$line_table_rows_selected
    if (!is.null(selected_row)) {
      save_state()
      values$lines <- values$lines[-selected_row, ]
      showNotification(
        paste("Lines at rows", paste(selected_row, collapse = ", "), "have been deleted."),
        type = "message"
      )
    }
  })

  observeEvent(input$delete_all_lines, {
    save_state()
    values$lines <- data.frame(
      x_start = numeric(), y_start = numeric(), x_end = numeric(), y_end = numeric(),
      ctrl_x = numeric(), ctrl_y = numeric(), type = character(), color = character(), end_color = character(), color_type = character(),
      gradient_position = numeric(), width = numeric(), alpha = numeric(), arrow = logical(), arrow_type = character(),
      arrow_size = numeric(), two_way = logical(), lavaan = logical(), network = logical(), line_style = character(), locked = logical(), stringsAsFactors = FALSE
    )
  })

  observeEvent(input$delete_selected_annotation, {
    selected_row <- input$annotation_table_rows_selected
    if (!is.null(selected_row)) {
      save_state()
      values$annotations <- values$annotations[-selected_row, ]
      showNotification(
        paste("Annotations at rows", paste(selected_row, collapse = ", "), "have been deleted."),
        type = "message"
      )
    }
  })

  observeEvent(input$lavaan_file, {
    req(input$lavaan_file)
    user_data <- tryCatch(
      {
        read.csv(input$lavaan_file$datapath)
      },
      error = function(e) {
        showNotification("Error reading CSV file. Please upload a valid CSV.", type = "error")
        return(NULL)
      }
    )

    if (!is.null(user_data)) {
      uploaded_data(user_data)
    }
  })


  observeEvent(input$delete_all_annotations, {
    save_state()
    values$annotations <- data.frame(
      text = character(), x = numeric(), y = numeric(), font = character(),
      size = numeric(), color = character(), angle = numeric(), alpha = numeric(),
      fontface = character(), math_expression = logical(), lavaan = logical(),
      network = logical(), locked = logical(), stringsAsFactors = FALSE
    )
  })

  observeEvent(input$delete_selected_loop, {
    selected_row <- input$loop_table_rows_selected
    if (!is.null(selected_row)) {
      save_state()
      values$loops <- values$loops[-selected_row, ]
      showNotification(
        paste("Loops at rows", paste(selected_row, collapse = ", "), "have been deleted."),
        type = "message"
      )
    }
  })

  observeEvent(input$delete_all_loops, {
    save_state()
    values$loops <- data.frame(
      x_center = numeric(), y_center = numeric(), radius = numeric(), color = character(),
      width = numeric(), alpha = numeric(), arrow_type = character(), arrow_size = numeric(),
      gap_size = numeric(), loop_width = numeric(), loop_height = numeric(), orientation = numeric(),
      two_way = logical(), locked = logical(), stringsAsFactors = FALSE
    )
  })

  # Create the plot output on the plotting space
  recreate_plot <- function() {
    tryCatch({
      zoom_factor <- input$zoom
      horizontal_shift <- input$horizontal_shift
      vertical_shift <- input$vertical_shift

      x_limits <- c(-40, 40) + horizontal_shift
      y_limits <- c(-40, 40) + vertical_shift

      p <- ggplot() +
        coord_fixed(ratio = 1, xlim = x_limits, ylim = y_limits, expand = FALSE, clip = "off") + # Ensure square plotting space
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none"
        )
      # scale_x_continuous(breaks = seq(x_limits[[1]], x_limits[[2]], by = 10)) +
      # scale_y_continuous(breaks = seq(y_limits[[1]], y_limits[[2]], by = 10))

      layer_order <- input$layer_order

      draw_points <- function(p, zoom_factor = 1) {
        if (nrow(values$points) > 0) {
          values$points$color <- sapply(values$points$color, valid_hex)
          values$points$border_color <- sapply(values$points$border_color, valid_hex)
          values$points$shape <- sapply(values$points$shape, valid_shape)
          values$points$lavaan <- sapply(values$points$lavaan, valid_logical)
          values$points$network <- sapply(values$points$network, valid_logical)
          values$points$locked <- sapply(values$points$locked, valid_logical)
          values$points$alpha <- sapply(values$points$alpha, valid_alpha)

          if (length(values$points$color) != nrow(values$points)) {
            values$points$color <- rep(values$points$color[1], nrow(values$points))
          }

          if (length(values$points$border_width) != nrow(values$points)) {
            values$points$border_width <- rep(values$points$border_width[1], nrow(values$points))
          }

          for (i in 1:nrow(values$points)) {
            shape <- values$points$shape[i]
            adjusted_stroke <- values$points$border_width[i] / zoom_factor
            adjusted_width <- (values$points$size[i] / 3)
            adjusted_height <- adjusted_width / ifelse(!is.null(values$points$width_height_ratio[i]),
                                                       values$points$width_height_ratio[i], 1
            )

            min_size_factor <- 0.25
            scale_factor <- sqrt(2)
            if (shape %in% c("circle")) {
              adjusted_width <- values$points$size[i] / scale_factor* min_size_factor
              adjusted_height <- adjusted_width
            } else if (shape == "square") {
              adjusted_width <- values$points$size[i] * sqrt(2) * min_size_factor
              adjusted_height <- adjusted_width
            } else if (shape == "triangle") {
              adjusted_width <- values$points$size[i] * sqrt(4 / sqrt(3))  * min_size_factor
              adjusted_height <- adjusted_width * sqrt(3) / 2
            } else if (shape == "rectangle") {
              width_height_ratio <- ifelse(!is.null(values$points$width_height_ratio[i]),
                                           values$points$width_height_ratio[i], 1
              )
              adjusted_height <- values$points$size[i]  * min_size_factor
              adjusted_width <- adjusted_height * width_height_ratio
            } else if (shape == "diamond") {
              width_height_ratio <- ifelse(!is.null(values$points$width_height_ratio[i]),
                                           values$points$width_height_ratio[i], 1
              )

              adjusted_height <- values$points$size[i] * 1.4 * sqrt(1.5) * min_size_factor
              adjusted_width <- adjusted_height * width_height_ratio
            } else if (shape == "oval") {
              width_height_ratio <- ifelse(!is.null(values$points$width_height_ratio[i]),
                                           values$points$width_height_ratio[i], 1
              )
              adjusted_height <- values$points$size[i] * min_size_factor / scale_factor
              adjusted_width <- adjusted_height * width_height_ratio
            }

            if (shape == "circle") {
              t <- seq(0, 2 * pi, length.out = 100)
              circle_coords <- data.frame(
                x = values$points$x[i] + adjusted_width * cos(t),
                y = values$points$y[i] + adjusted_height * sin(t)
              )
              p <- p + annotate(
                "polygon",
                x = circle_coords$x,
                y = circle_coords$y,
                fill = values$points$color[i],
                colour = values$points$border_color[i],
                size = adjusted_stroke,
                alpha = values$points$alpha[i]
              )
            } else if (shape == "triangle") {
              triangle_coords <- data.frame(
                x = c(
                  values$points$x[i],
                  values$points$x[i] - adjusted_width / 2,
                  values$points$x[i] + adjusted_width / 2
                ),
                y = c(
                  values$points$y[i] + adjusted_height / 2,
                  values$points$y[i] - adjusted_height / 2,
                  values$points$y[i] - adjusted_height / 2
                )
              )
              rotated_coords <- rotate_coords(triangle_coords$x, triangle_coords$y, values$points$orientation[i],
                                              cx = values$points$x[i], cy = values$points$y[i]
              )
              p <- p + annotate(
                "polygon",
                x = rotated_coords$x,
                y = rotated_coords$y,
                fill = values$points$color[i],
                colour = values$points$border_color[i],
                size = adjusted_stroke,
                alpha = values$points$alpha[i]
              )
            } else if (shape == "square") {
              rect_coords <- data.frame(
                x = c(
                  values$points$x[i] - adjusted_width / 2,
                  values$points$x[i] + adjusted_width / 2,
                  values$points$x[i] + adjusted_width / 2,
                  values$points$x[i] - adjusted_width / 2
                ),
                y = c(
                  values$points$y[i] - adjusted_height / 2,
                  values$points$y[i] - adjusted_height / 2,
                  values$points$y[i] + adjusted_height / 2,
                  values$points$y[i] + adjusted_height / 2
                )
              )
              rotated_coords <- rotate_coords(rect_coords$x, rect_coords$y, values$points$orientation[i],
                                              cx = values$points$x[i], cy = values$points$y[i]
              )
              p <- p + annotate(
                "polygon",
                x = rotated_coords$x,
                y = rotated_coords$y,
                fill = values$points$color[i],
                colour = values$points$border_color[i],
                size = adjusted_stroke,
                alpha = values$points$alpha[i]
              )
            } else if (shape == "oval") {
              t <- seq(0, 2 * pi, length.out = 100)
              oval_coords <- data.frame(
                x = values$points$x[i] + adjusted_width * cos(t),
                y = values$points$y[i] + adjusted_height * sin(t)
              )
              rotated_coords <- rotate_coords(oval_coords$x, oval_coords$y, values$points$orientation[i],
                                              cx = values$points$x[i], cy = values$points$y[i]
              )
              p <- p + annotate(
                "polygon",
                x = rotated_coords$x,
                y = rotated_coords$y,
                fill = values$points$color[i],
                colour = values$points$border_color[i],
                size = adjusted_stroke,
                alpha = values$points$alpha[i]
              )
            } else if (shape == "rectangle") {
              rect_coords <- data.frame(
                x = c(
                  values$points$x[i] - adjusted_width / 2,
                  values$points$x[i] + adjusted_width / 2,
                  values$points$x[i] + adjusted_width / 2,
                  values$points$x[i] - adjusted_width / 2
                ),
                y = c(
                  values$points$y[i] - adjusted_height / 2,
                  values$points$y[i] - adjusted_height / 2,
                  values$points$y[i] + adjusted_height / 2,
                  values$points$y[i] + adjusted_height / 2
                )
              )

              rotated_coords <- rotate_coords(rect_coords$x, rect_coords$y, values$points$orientation[i],
                                              cx = values$points$x[i], cy = values$points$y[i]
              )

              p <- p + annotate(
                "polygon",
                x = rotated_coords$x,
                y = rotated_coords$y,
                fill = values$points$color[i],
                colour = values$points$border_color[i],
                size = adjusted_stroke,
                alpha = values$points$alpha[i]
              )
            } else {
              # diamond
              diamond_coords <- data.frame(
                x = c(
                  values$points$x[i],
                  values$points$x[i] - adjusted_width / 2,
                  values$points$x[i],
                  values$points$x[i] + adjusted_width / 2
                ),
                y = c(
                  values$points$y[i] + adjusted_height / 2,
                  values$points$y[i],
                  values$points$y[i] - adjusted_height / 2,
                  values$points$y[i]
                )
              )
              rotated_coords <- rotate_coords(diamond_coords$x, diamond_coords$y, values$points$orientation[i],
                                              cx = values$points$x[i], cy = values$points$y[i]
              )
              p <- p + annotate(
                "polygon",
                x = rotated_coords$x,
                y = rotated_coords$y,
                fill = values$points$color[i],
                colour = values$points$border_color[i],
                size = adjusted_stroke,
                alpha = values$points$alpha[i]
              )
            }
          }
        }
        return(p)
      }


      draw_lines <- function(p, zoom_factor = 1) {
        if (nrow(values$lines) > 0) {
          values$lines$color <- sapply(values$lines$color, valid_hex)
          values$lines$end_color <- sapply(values$lines$end_color, valid_hex)
          values$lines$lavaan <- sapply(values$lines$lavaan, valid_logical)
          values$lines$network <- sapply(values$lines$network, valid_logical)
          values$lines$locked <- sapply(values$lines$locked, valid_logical)
          values$lines$line_style <- sapply(values$lines$line_style, valid_line_style)
          values$lines$alpha <- sapply(values$lines$alpha, valid_alpha)
          values$lines$gradient_position <- sapply(values$lines$gradient_position, valid_gradient_position)
          values$lines$type <- sapply(values$lines$type, valid_type)

          for (i in 1:nrow(values$lines)) {
            line_type <- values$lines$type[i]
            start_color <- values$lines$color[i]
            end_color <- if (length(values$lines$color_type[i]) > 0 && values$lines$color_type[i] == "Gradient") {
              values$lines$end_color[i]
            } else {
              start_color
            }
            gradient_position <- if (!is.null(values$lines$gradient_position[i]) && length(values$lines$gradient_position[i]) > 0) {
              values$lines$gradient_position[i]
            } else {
              1
            }

            if (!is.null(line_type) && length(line_type) > 0) {
              adjusted_line_width <- values$lines$width[i] / zoom_factor
              adjusted_arrow_size <- if (!is.na(values$lines$arrow_size[i])) values$lines$arrow_size[i] / zoom_factor else NA


              if (line_type == "Straight Line" || line_type == "Straight Arrow" || line_type == "Auto-generated") {
                if (!is.null(values$lines$x_start[i]) && !is.null(values$lines$x_end[i])) {
                  if (values$lines$color_type[i] == "Gradient") {
                    straight_points <- interpolate_points(
                      x_start = values$lines$x_start[i], y_start = values$lines$y_start[i],
                      x_end = values$lines$x_end[i], y_end = values$lines$y_end[i]
                    )

                    n_points <- nrow(straight_points)
                    split_index <- round(gradient_position * n_points)

                    color_interpolator <- colorRampPalette(c(start_color, end_color))
                    intermediate_color <- color_interpolator(3)[2]

                    gradient_colors_start <- colorRampPalette(c(start_color, intermediate_color))(split_index)
                    gradient_colors_end <- colorRampPalette(c(intermediate_color, end_color))(n_points - split_index + 1)


                    # Draw the line segment by segment with interpolated colors
                    for (j in 1:(split_index - 1)) {
                      p <- p + annotate("segment",
                                        x = straight_points$x[j], y = straight_points$y[j],
                                        xend = straight_points$x[j + 1], yend = straight_points$y[j + 1],
                                        color = gradient_colors_start[j],
                                        size = adjusted_line_width, alpha = values$lines$alpha[i]
                      )
                    }

                    # Draw the second segment with the end color gradient
                    for (j in split_index:(n_points - 1)) {
                      p <- p + annotate("segment",
                                        x = straight_points$x[j], y = straight_points$y[j],
                                        xend = straight_points$x[j + 1], yend = straight_points$y[j + 1],
                                        color = gradient_colors_end[j - split_index + 1],
                                        size = adjusted_line_width, alpha = values$lines$alpha[i]
                      )
                    }
                  } else {
                    # For single-color straight lines, use annotate("segment")
                    p <- p + annotate("segment",
                                      x = values$lines$x_start[i], y = values$lines$y_start[i],
                                      xend = values$lines$x_end[i], yend = values$lines$y_end[i],
                                      color = start_color,
                                      size = adjusted_line_width, alpha = values$lines$alpha[i],
                                      linetype = values$lines$line_style[i]
                    )
                  }

                  # Add arrowhead if necessary
                  arrow_type <- values$lines$arrow_type[i]
                  if (!is.null(arrow_type) && !is.na(adjusted_arrow_size)) {
                    offset_factor <- 0.01

                    dx <- values$lines$x_end[i] - values$lines$x_start[i]
                    dy <- values$lines$y_end[i] - values$lines$y_start[i]
                    norm <- sqrt(dx^2 + dy^2)


                    x_adjust_start <- values$lines$x_start[i] + offset_factor * dx / norm
                    y_adjust_start <- values$lines$y_start[i] + offset_factor * dy / norm

                    x_adjust_end <- values$lines$x_end[i] - offset_factor * dx / norm
                    y_adjust_end <- values$lines$y_end[i] - offset_factor * dy / norm

                    if (values$lines$two_way[i]) {
                      # Two-way arrow logic
                      p <- p + annotate("segment",
                                        x = x_adjust_start, y = y_adjust_start,
                                        xend = values$lines$x_start[i], yend = values$lines$y_start[i],
                                        size = adjusted_line_width, alpha = values$lines$alpha[i],
                                        arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                                        color = start_color
                      ) +
                        annotate("segment",
                                 x = x_adjust_end, y = y_adjust_end,
                                 xend = values$lines$x_end[i], yend = values$lines$y_end[i],
                                 size = adjusted_line_width, alpha = values$lines$alpha[i],
                                 arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                                 color = end_color
                        )
                    } else {
                      # One-way arrow logic
                      p <- p + annotate("segment",
                                        x = x_adjust_end, y = y_adjust_end,
                                        xend = values$lines$x_end[i], yend = values$lines$y_end[i],
                                        size = adjusted_line_width, alpha = values$lines$alpha[i],
                                        arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                                        color = end_color
                      ) # Use solid end color for arrowhead
                    }
                  }
                }
              }

              # For curved lines and arrows
              if (line_type == "Curved Line" || line_type == "Curved Arrow") {
                if (!is.null(values$lines$ctrl_x[i]) && !is.null(values$lines$ctrl_y[i])) {
                  # Use create_bezier_curve for curved lines
                  bezier_points <- create_bezier_curve(
                    x_start = values$lines$x_start[i], y_start = values$lines$y_start[i],
                    x_end = values$lines$x_end[i], y_end = values$lines$y_end[i],
                    ctrl_x = values$lines$ctrl_x[i], ctrl_y = values$lines$ctrl_y[i]
                  )

                  if (values$lines$color_type[i] == "Gradient") {
                    # Interpolate gradient colors along the Bezier curve
                    n_points <- nrow(bezier_points)
                    split_index <- round(gradient_position * n_points)
                    color_interpolator <- colorRampPalette(c(start_color, end_color))
                    intermediate_color <- color_interpolator(3)[2]

                    gradient_colors_start <- colorRampPalette(c(start_color, intermediate_color))(split_index)
                    gradient_colors_end <- colorRampPalette(c(intermediate_color, end_color))(n_points - split_index + 1)

                    p <- p + annotate("path",
                                      x = bezier_points$x,
                                      y = bezier_points$y,
                                      color = start_color,
                                      size = adjusted_line_width,
                                      alpha = values$lines$alpha[i],
                                      linetype = values$lines$line_style[i]
                    )

                    for (j in 1:(split_index - 1)) {
                      p <- p + annotate("path",
                                        x = bezier_points$x[j:(j + 1)],
                                        y = bezier_points$y[j:(j + 1)],
                                        color = gradient_colors_start[j],
                                        size = adjusted_line_width, alpha = values$lines$alpha[i]
                      )
                    }

                    for (j in split_index:(n_points - 1)) {
                      p <- p + annotate("path",
                                        x = bezier_points$x[j:(j + 1)],
                                        y = bezier_points$y[j:(j + 1)],
                                        color = gradient_colors_end[j - split_index + 1],
                                        size = adjusted_line_width, alpha = values$lines$alpha[i]
                      )
                    }
                  } else {
                    p <- p + annotate("path",
                                      x = bezier_points$x,
                                      y = bezier_points$y,
                                      color = start_color,
                                      size = adjusted_line_width, alpha = values$lines$alpha[i],
                                      linetype = values$lines$line_style[i]
                    )
                  }

                  # Add arrowhead for curved lines if necessary
                  arrow_type <- values$lines$arrow_type[i]
                  if (line_type == "Curved Arrow" && !is.null(arrow_type) && !is.na(adjusted_arrow_size)) {
                    if (values$lines$two_way[i]) {
                      dx_start <- bezier_points$x[2] - bezier_points$x[1]
                      dy_start <- bezier_points$y[2] - bezier_points$y[1]

                      dx_end <- bezier_points$x[nrow(bezier_points)] - bezier_points$x[nrow(bezier_points) - 1]
                      dy_end <- bezier_points$y[nrow(bezier_points)] - bezier_points$y[nrow(bezier_points) - 1]

                      norm_start <- sqrt(dx_start^2 + dy_start^2)
                      norm_end <- sqrt(dx_end^2 + dy_end^2)

                      p <- p + annotate("segment",
                                        x = bezier_points$x[1], y = bezier_points$y[1],
                                        xend = bezier_points$x[1] - dx_start / norm_start * 1e-5,
                                        yend = bezier_points$y[1] - dy_start / norm_start * 1e-5,
                                        size = adjusted_line_width,
                                        arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                                        color = start_color
                      ) +
                        annotate("segment",
                                 x = bezier_points$x[nrow(bezier_points)], y = bezier_points$y[nrow(bezier_points)],
                                 xend = bezier_points$x[nrow(bezier_points)] + dx_end / norm_end * 1e-5,
                                 yend = bezier_points$y[nrow(bezier_points)] + dy_end / norm_end * 1e-5,
                                 size = adjusted_line_width,
                                 arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                                 color = end_color
                        )
                    } else {
                      dx_end <- bezier_points$x[nrow(bezier_points)] - bezier_points$x[nrow(bezier_points) - 1]
                      dy_end <- bezier_points$y[nrow(bezier_points)] - bezier_points$y[nrow(bezier_points) - 1]
                      norm_end <- sqrt(dx_end^2 + dy_end^2)

                      p <- p + annotate("segment",
                                        x = bezier_points$x[nrow(bezier_points)], y = bezier_points$y[nrow(bezier_points)],
                                        xend = bezier_points$x[nrow(bezier_points)] + dx_end / norm_end * 1e-5,
                                        yend = bezier_points$y[nrow(bezier_points)] + dy_end / norm_end * 1e-5,
                                        size = adjusted_line_width,
                                        arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                                        color = end_color
                      )
                    }
                  }
                }
              }
            }
          }
        }
        return(p)
      }



      draw_annotations <- function(p, zoom_factor = 1) {
        if (nrow(values$annotations) > 0) {
          values$annotations$color <- sapply(values$annotations$color, valid_hex)
          values$annotations$lavaan <- sapply(values$annotations$lavaan, valid_logical)
          values$annotations$network <- sapply(values$annotations$network, valid_logical)
          values$annotations$locked <- sapply(values$annotations$locked, valid_logical)
          values$annotations$alpha <- sapply(values$annotations$alpha, valid_alpha)
          values$annotations$fontface <- sapply(values$annotations$fontface, valid_fontface)
          values$annotations$font <- sapply(values$annotations$font, valid_font)

          for (i in 1:nrow(values$annotations)) {
            # mathematical annotations (logical)
            annotation_text <- if (input$math_expression) {
              suppressWarnings(tryCatch(parse(text = values$annotations$text[i]), error = function(e) values$annotations$text[i]))
            } else {
              values$annotations$text[i]
            }

            adjusted_size <- (values$annotations$size[i] / 3) / zoom_factor

            # Add annotation to the plot
            p <- p + annotate("text",
                              x = values$annotations$x[i],
                              y = values$annotations$y[i],
                              label = annotation_text,
                              size = adjusted_size,
                              color = values$annotations$color[i],
                              alpha = values$annotations$alpha[i],
                              angle = values$annotations$angle[i],
                              family = values$annotations$font[i],
                              fontface = values$annotations$fontface[i]
            )
          }
        }
        return(p)
      }


      draw_loops <- function(p, zoom_factor = 1) {
        if (!is.null(values$loops) && nrow(values$loops) > 0) {
          values$loops$color <- sapply(values$loops$color, valid_hex)
          values$loops$locked <- sapply(values$loops$locked, valid_logical)
          values$loops$alpha <- sapply(values$loops$alpha, valid_alpha)

          for (i in 1:nrow(values$loops)) {
            t <- seq(0, 2 * pi, length.out = 100)
            gap_angle <- values$loops$gap_size[i] * pi
            loop_start <- t[t < (2 * pi - gap_angle)]

            x_ellipse <- values$loops$x_center[i] + (values$loops$loop_width[i]) * values$loops$radius[i] * cos(loop_start)
            y_ellipse <- values$loops$y_center[i] + (values$loops$loop_height[i]) * values$loops$radius[i] * sin(loop_start)

            theta <- values$loops$orientation[i] * pi / 180
            x_rotated <- cos(theta) * (x_ellipse - values$loops$x_center[i]) - sin(theta) * (y_ellipse - values$loops$y_center[i]) + values$loops$x_center[i]
            y_rotated <- sin(theta) * (x_ellipse - values$loops$x_center[i]) + cos(theta) * (y_ellipse - values$loops$y_center[i]) + values$loops$y_center[i]

            # Add arrowhead


            arrow_type <- if (values$loops$arrow_type[i] == "closed") {
              arrow(type = "closed", length = unit(values$loops$arrow_size[i] / zoom_factor, "inches"))
            } else {
              arrow(type = "open", length = unit(values$loops$arrow_size[i] / zoom_factor, "inches"))
            }

            p <- p + annotate("path",
                              x = x_rotated,
                              y = y_rotated,
                              color = values$loops$color[i],
                              linewidth = values$loops$width[i] / zoom_factor,
                              alpha = values$loops$alpha[i],
                              arrow = arrow_type
            )

            # two-way arrows
            if (values$loops$two_way[i]) {
              x_rotated_rev <- rev(x_rotated)
              y_rotated_rev <- rev(y_rotated)

              # reverse loop
              p <- p + annotate("path",
                                x = x_rotated_rev,
                                y = y_rotated_rev,
                                color = values$loops$color[i],
                                linewidth = values$loops$width[i] / zoom_factor,
                                alpha = values$loops$alpha[i],
                                arrow = arrow_type
              )
            }
          }
        }
        return(p)
      }



      # Order of the elements based on the user's input
      if (input$layer_order == "points_front") {
        # Draw points, lines, annotations, and self-loop arrows
        p <- draw_annotations(p, zoom_factor)
        p <- draw_loops(p, zoom_factor)
        p <- draw_lines(p, zoom_factor)
        p <- draw_points(p, zoom_factor)
      } else if (input$layer_order == "lines_front") {
        # Draw lines, annotations, points, and self-loop arrows
        p <- draw_loops(p, zoom_factor)
        p <- draw_points(p, zoom_factor)
        p <- draw_annotations(p, zoom_factor)
        p <- draw_lines(p, zoom_factor)
      } else if (input$layer_order == "annotations_front") {
        # Draw annotations, points, lines, and self-loop arrows
        p <- draw_loops(p, zoom_factor)
        p <- draw_lines(p, zoom_factor)
        p <- draw_points(p, zoom_factor)
        p <- draw_annotations(p, zoom_factor)
      } else if (input$layer_order == "loops_front") {
        # Draw self-loop arrows, points, lines, and annotations
        p <- draw_points(p, zoom_factor)
        p <- draw_lines(p, zoom_factor)
        p <- draw_annotations(p, zoom_factor)
        p <- draw_loops(p, zoom_factor)
      }

      adjusted_x_range <- c(-40, 40) * zoom_factor + horizontal_shift
      adjusted_y_range <- c(-40, 40) * zoom_factor + vertical_shift

      adjusted_axis_ranges <- list(x_range = adjusted_x_range, y_range = adjusted_y_range)
      #print(adjusted_axis_ranges)

      zoomed_plot <- cowplot::ggdraw(xlim = adjusted_x_range, ylim = adjusted_y_range) +
        cowplot::draw_plot(
          p,
          x = mean(adjusted_x_range) - 0.5 * diff(adjusted_x_range) / zoom_factor,
          y = mean(adjusted_y_range) - 0.5 * diff(adjusted_y_range) / zoom_factor ,
          width = diff(adjusted_x_range) / zoom_factor,
          height = diff(adjusted_y_range) / zoom_factor,
        )

      return(zoomed_plot)
    }, error = function(e) {
      showNotification(
        paste("Error recreating the plot:", e$message),
        type = "error",
        duration = 5
      )
      # Return a blank plot in case of error
      ggplot() +
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none"
        ) +
        ggtitle("Error: Unable to plot")
    })
  }

  # Render the plot using ggplot engine

  suppressWarnings({
    output$plot <- renderPlot({
      recreate_plot()
    })
  })


  output$data_table <- renderDT({
    datatable(
      values$points,
      selection = "multiple",
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = "ftip",
        paging = TRUE,
        stateSave = TRUE,
        rowCallback = JS(
          "function(row, data, index) {",
          "  if (data[13] == true) {",
          "    $('td', row).css('background-color', '#f4cccc');", # Light red for locked points
          "  } else {",
          "    $('td', row).css('background-color', '');", # Reset color for unlocked points
          "  }",
          "}"
        )
      ),
      escape = FALSE,
      editable = TRUE
    )
  })

  output$uploaded_data_preview <- DT::renderDataTable({
    req(uploaded_data())
    head(uploaded_data(), 10)
  })

  output$processing_indicator <- renderUI({
    if (is.null(uploaded_data())) {
      "No data uploaded. Using default dataset."
    } else {
      "Processing uploaded dataset..."
    }
  })


  # Render line table with clickable rows
  output$line_table <- renderDT({
    datatable(values$lines,
              selection = "multiple",
              options = list(
                pageLength = 10, autoWidth = TRUE,
                dom = "ftip",
                paging = TRUE,
                stateSave = TRUE,
                rowCallback = JS(
                  "function(row, data, index) {",
                  "  if (data[21] == true) {", # 'locked' column index (adjust if needed)
                  "    $('td', row).css('background-color', '#f4cccc');", # Light red for locked rows
                  "  } else {",
                  "    $('td', row).css('background-color', '');", # Reset for unlocked rows
                  "  }",
                  "}"
                )
              ),
              escape = FALSE, editable = TRUE
    )
  })


  output$annotation_table <- renderDT({
    datatable(values$annotations,
              selection = "multiple",
              options = list(
                pageLength = 10, autoWidth = TRUE,
                dom = "ftip",
                paging = TRUE,
                stateSave = TRUE,
                rowCallback = JS(
                  "function(row, data, index) {",
                  "  if (data[13] == true) {",
                  "    $('td', row).css('background-color', '#f4cccc');", # Light red for locked points
                  "  } else {",
                  "    $('td', row).css('background-color', '');", # Reset color for unlocked points
                  "  }",
                  "}"
                )
              ),
              escape = FALSE, editable = TRUE
    )
  })


  output$loop_table <- renderDT({
    datatable(values$loops,
              selection = "multiple",
              options = list(
                pageLength = 10, autoWidth = TRUE,
                dom = "ftip",
                paging = TRUE,
                stateSave = TRUE,
                rowCallback = JS(
                  "function(row, data, index) {",
                  "  if (data[12] == true) {", # Assuming 'locked' is the 12th column in the table
                  "    $('td', row).css('background-color', '#f4cccc');", # Light red for locked rows
                  "  } else {",
                  "    $('td', row).css('background-color', '');", # Reset color for unlocked rows
                  "  }",
                  "}"
                )
              ),
              escape = FALSE, editable = TRUE
    )
  })
  observeEvent(input$data_table_cell_edit, {
    info <- input$data_table_cell_edit
    save_state()

    # Check the column being edited and update accordingly
    if (info$col %in% c("x", "y", "size", "border_width", "alpha", "width_height_ratio", "orientation")) {
      # Ensure numeric values
      values$points[info$row, info$col] <- as.numeric(info$value)
    } else if (info$col %in% c("color", "border_color")) {
      # Validate hex color code
      if (grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", info$value)) {
        values$points[info$row, info$col] <- as.character(info$value)
      } else {
        showNotification("Invalid color input. Update skipped.", type = "error")
        return() # Skip invalid color updates
      }
    } else if (info$col == "shape") {
      # Safeguard for valid shapes
      valid_shapes <- c("circle", "square", "oval", "triangle", "rectangle", "diamond")
      if (info$value %in% valid_shapes) {
        values$points[info$row, info$col] <- as.character(info$value)
      } else {
        showNotification(
          paste("Invalid shape input. Must be one of:", paste(valid_shapes, collapse = ", ")),
          type = "error"
        )
        return()
      }
    } else if (info$col %in% c("lavaan", "network", "locked")) {
      # Convert logical inputs to uppercase and validate
      logical_value <- toupper(info$value)
      if (logical_value %in% c("TRUE", "FALSE")) {
        values$points[info$row, info$col] <- as.logical(logical_value)
      } else {
        showNotification("Invalid logical input. Must be TRUE or FALSE.", type = "error")
        return() # Skip invalid logical updates
      }
    } else {
      # Default case for other string values
      values$points[info$row, info$col] <- info$value
    }

    # Re-render the plot with updated data
    output$plot <- renderPlot({
      recreate_plot()
    })
  })


  get_current_page <- function(proxy, session) {
    as.numeric(session$clientData[[paste0("output_", proxy$id, "_state")]]$page) + 1
  }


  observeEvent(input$line_table_cell_edit, {
    info <- input$line_table_cell_edit
    save_state()

    if (info$col %in% c("x_start", "y_start", "x_end", "y_end", "width", "alpha", "ctrl_x", "ctrl_y", "gradient_position", "arrow_size")) {
      values$lines[info$row, info$col] <- as.numeric(info$value)
    } else if (info$col == "line_style") {
      valid_line_styles <- c("solid", "dashed", "dotted")
      if (info$value %in% valid_line_styles) {
        values$lines[info$row, info$col] <- as.character(info$value)
      } else {
        showNotification(
          paste("Invalid line style input. Must be one of:", paste(valid_line_styles, collapse = ", ")),
          type = "error"
        )
        return()
      }
    } else if (info$col %in% c("color", "end_color")) {
      if (grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", info$value)) {
        values$lines[info$row, info$col] <- as.character(info$value)
      } else {
        showNotification("Invalid color input. Update skipped.", type = "error")
        return()
      }
    } else if (info$col == "arrow_type") {
      valid_arrow_types <- c("open", "closed")
      if (info$value %in% valid_arrow_types) {
        values$lines[info$row, info$col] <- as.character(info$value)
      } else {
        showNotification(
          paste("Invalid arrow type input. Must be one of:", paste(valid_arrow_types, collapse = ", ")),
          type = "error"
        )
        return()
      }
    } else if (info$col %in% c("arrow", "two_way", "lavaan", "network", "locked")) {
      logical_value <- toupper(info$value)
      if (logical_value %in% c("TRUE", "FALSE")) {
        values$lines[info$row, info$col] <- as.logical(logical_value)
      } else {
        showNotification("Invalid logical input. Must be TRUE or FALSE.", type = "error")
        return()
      }
    } else {
      values$lines[info$row, info$col] <- info$value
    }

    output$plot <- renderPlot({
      recreate_plot()
    })
  })



  observeEvent(input$annotation_table_cell_edit, {
    info <- input$annotation_table_cell_edit
    save_state()

    if (info$col %in% c("x", "y", "size", "angle", "alpha")) {
      values$annotations[info$row, info$col] <- as.numeric(info$value)
    } else if (info$col == "color") {
      if (grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", info$value)) {
        values$annotations[info$row, info$col] <- as.character(info$value)
      } else {
        showNotification("Invalid color input. Update skipped.", type = "error")
        return() # Skip invalid color updates
      }
    } else if (info$col == "fontface") {
      valid_fontfaces <- c("plain", "bold", "italic")
      if (info$value %in% valid_fontfaces) {
        values$annotations[info$row, info$col] <- as.character(info$value)
      } else {
        showNotification(
          paste("Invalid fontface input. Must be one of:", paste(valid_fontfaces, collapse = ", ")),
          type = "error"
        )
        return()
      }
    } else if (info$col %in% c("math_expression", "lavaan", "network", "locked")) {
      logical_value <- toupper(info$value)
      if (logical_value %in% c("TRUE", "FALSE")) {
        values$annotations[info$row, info$col] <- as.logical(logical_value)
      } else {
        showNotification("Invalid logical input. Must be TRUE or FALSE.", type = "error")
        return()
      }
    } else {
      values$annotations[info$row, info$col] <- info$value
    }
    output$plot <- renderPlot({
      recreate_plot()
    })
  })


  observeEvent(input$loop_table_cell_edit, {
    info <- input$loop_table_cell_edit
    save_state()

    if (info$col %in% c("x_center", "y_center", "radius", "width", "alpha", "gap_size", "loop_width", "loop_height", "orientation", "arrow_size")) {
      values$loops[info$row, info$col] <- as.numeric(info$value)
    } else if (info$col == "color") {
      if (grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", info$value)) {
        values$loops[info$row, info$col] <- as.character(info$value)
      } else {
        showNotification("Invalid color input. Update skipped.", type = "error")
        return()
      }
    } else if (info$col == "arrow_type") {
      valid_arrow_types <- c("open", "closed")
      if (info$value %in% valid_arrow_types) {
        values$loops[info$row, info$col] <- as.character(info$value)
      } else {
        showNotification(
          paste("Invalid arrow type input. Must be one of:", paste(valid_arrow_types, collapse = ", ")),
          type = "error"
        )
        return()
      }
    } else if (info$col %in% c("two_way", "locked")) {
      logical_value <- toupper(info$value)
      if (logical_value %in% c("TRUE", "FALSE")) {
        values$loops[info$row, info$col] <- as.logical(logical_value)
      } else {
        showNotification("Invalid logical input. Must be TRUE or FALSE.", type = "error")
        return() # Skip invalid logical updates
      }
    } else {
      values$loops[info$row, info$col] <- info$value
    }

    output$plot <- renderPlot({
      recreate_plot()
    })
  })



  output$download_plot <- downloadHandler(
    filename = function() {
      paste("ggsem_figure-", Sys.Date(), ".", tolower(input$export_format), sep = "")
    },
    content = function(file) {

      x_range <- if (input$use_x_range && !is.na(input$x_range_min) && !is.na(input$x_range_max)) {
        c(input$x_range_min, input$x_range_max)
      } else {
        NULL
      }

      y_range <- if (input$use_y_range && !is.na(input$y_range_min) && !is.na(input$y_range_max)) {
        c(input$y_range_min, input$y_range_max)
      } else {
        NULL
      }

      pp <- recreate_plot()

      pp1 <- adjust_axis_range(pp,
                               x_range = x_range,
                               y_range = y_range)
      save_figure(
        file,
        plot = pp1,
        device = switch(input$export_format,
                        "PNG" = "png",
                        "JPEG" = "jpeg",
                        "PDF" = cairo_pdf,
                        "SVG" = svglite
        )
      )
    }
  )


  output$download_selected_csv <- downloadHandler(
    filename = function() {
      paste0(tolower(input$csv_type), "-", Sys.Date(), ".csv")
    },
    content = function(file) {
      data_to_save <- switch(input$csv_type,
                             "Points CSV" = values$points,
                             "Lines CSV" = values$lines,
                             "Annotations CSV" = values$annotations,
                             "Self-loop Arrows CSV" = values$loops
      )

      write.csv(data_to_save, file, row.names = FALSE)
    }
  )


  # message about axis limits
  output$axis_info <- renderText({
    "Adjust Zoom Level / X-Level / Y-Level to change the limits of x and y axes."
  })

  # Download CSV for points
  output$download_points_csv <- downloadHandler(
    filename = function() {
      paste("points_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$points, file, row.names = FALSE)
    }
  )

  # Download CSV for lines
  output$download_lines_csv <- downloadHandler(
    filename = function() {
      paste("lines_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$lines, file, row.names = FALSE)
    }
  )

  # Download CSV for annotations
  output$download_annotations_csv <- downloadHandler(
    filename = function() {
      paste("annotations_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$annotations, file, row.names = FALSE)
    }
  )

  # Download CSV for self-loop arrows
  output$download_loops_csv <- downloadHandler(
    filename = function() {
      paste("loops_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$loops, file, row.names = FALSE)
    }
  )

  values <- reactiveValues(
    points = data.frame(),
    lines = data.frame(),
    annotations = data.frame(),
    loops = data.frame()
  )

  observeEvent(input$points_file, {
    tryCatch({
      req(input$points_file)
      points_data <- read.csv(input$points_file$datapath)

      # Validate required columns for points
      required_columns <- c("x", "y", "shape", "color", "size", "orientation", "width_height_ratio")
      if (!all(required_columns %in% colnames(points_data))) {
        stop("Points file is missing required columns: ", paste(setdiff(required_columns, colnames(points_data)), collapse = ", "))
      }

      values$points <- points_data
      showNotification("Points file loaded successfully.", type = "message")
    }, error = function(e) {
      showNotification(
        paste("Error loading points file:", e$message),
        type = "error"
      )
    })
  })

  observeEvent(input$lines_file, {
    tryCatch({
      req(input$lines_file)
      lines_data <- read.csv(input$lines_file$datapath)

      # Validate required columns for lines
      required_columns <- c("x_start", "y_start", "x_end", "y_end", "type", "color")
      if (!all(required_columns %in% colnames(lines_data))) {
        stop("Lines file is missing required columns: ", paste(setdiff(required_columns, colnames(lines_data)), collapse = ", "))
      }

      values$lines <- lines_data
      showNotification("Lines file loaded successfully.", type = "message")
    }, error = function(e) {
      showNotification(
        paste("Error loading lines file:", e$message),
        type = "error"
      )
    })
  })
  observeEvent(input$annotations_file, {
    tryCatch({
      req(input$annotations_file)
      annotations_data <- read.csv(input$annotations_file$datapath)

      # Validate required columns for annotations
      required_columns <- c("text", "x", "y", "color", "size")
      if (!all(required_columns %in% colnames(annotations_data))) {
        stop("Annotations file is missing required columns: ", paste(setdiff(required_columns, colnames(annotations_data)), collapse = ", "))
      }

      values$annotations <- annotations_data
      showNotification("Annotations file loaded successfully.", type = "message")
    }, error = function(e) {
      showNotification(
        paste("Error loading annotations file:", e$message),
        type = "error"
      )
    })
  })

  observeEvent(input$self_loop_file, {
    tryCatch({
      req(input$self_loop_file)
      self_loop_data <- read.csv(input$self_loop_file$datapath)

      # Validate required columns for loops
      required_columns <- c("x_center", "y_center", "radius", "color", "width")
      if (!all(required_columns %in% colnames(self_loop_data))) {
        stop("Self-loop file is missing required columns: ", paste(setdiff(required_columns, colnames(self_loop_data)), collapse = ", "))
      }

      values$loops <- self_loop_data
      showNotification("Self-loop file loaded successfully.", type = "message")
    }, error = function(e) {
      showNotification(
        paste("Error loading self-loop file:", e$message),
        type = "error"
      )
    })
  })
}

shinyApp(ui = ui, server = server)
