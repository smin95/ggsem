#' Calculate straight line length
#'
#' @keywords internal
#' @noRd
#'
calculate_straight_length <- function(x_start, y_start, x_end, y_end) {
  sqrt((x_end - x_start)^2 + (y_end - y_start)^2)
}

#' Calculate approximate length of a Bezier curve
#' @keywords internal
#' @noRd
calculate_curve_length <- function(curve_data) {
  sum(sqrt(diff(curve_data$x)^2 + diff(curve_data$y)^2))  # Sum of distances between consecutive points
}

#' Interpolate points along a straight line for gradient
#' @keywords internal
#' @noRd
interpolate_points <- function(x_start, y_start, x_end, y_end, n = 100) {
  t <- seq(0, 1, length.out = n)
  x <- (1 - t) * x_start + t * x_end
  y <- (1 - t) * y_start + t * y_end
  data.frame(x = x, y = y)
}

#' Calculate where the curvy line curves (for default)
#' @keywords internal
#' @noRd
default_control_point <- function(x_start, y_start, x_end, y_end, offset_ratio = 0.3) {
  mid_x <- (x_start + x_end) / 2
  mid_y <- (y_start + y_end) / 2

  dx <- x_end - x_start
  dy <- y_end - y_start
  offset_x <- -dy * offset_ratio
  offset_y <- dx * offset_ratio

  list(ctrl_x = mid_x + offset_x, ctrl_y = mid_y + offset_y)
}

#' Create curved lines (using Bezier approximation)
#' @keywords internal
#' @noRd
create_bezier_curve <- function(x_start, y_start, x_end, y_end, ctrl_x, ctrl_y, n_points = 100) {
  t <- seq(0, 1, length.out = n_points)

  bezier_x <- (1 - t)^2 * x_start + 2 * (1 - t) * t * ctrl_x + t^2 * x_end
  bezier_y <- (1 - t)^2 * y_start + 2 * (1 - t) * t * ctrl_y + t^2 * y_end

  data.frame(x = bezier_x, y = bezier_y)
}


#' Get XY coordinates from lavaan syntax using igraph
#' @keywords internal
#' @importFrom lavaan lavaanify
#' @importFrom igraph graph.empty add_vertices add_edges layout_with_sugiyama as_edgelist
#' @noRd
extract_coords_from_lavaan_igraph <- function(lavaan_string) {

  model <- lavaan::lavaanify(lavaan_string)

  g <- graph.empty(directed = TRUE)

  vars <- unique(c(model$lhs, model$rhs))
  g <- add_vertices(g, length(vars), name = vars)

  for (i in 1:nrow(model)) {
    if (model$op[i] == "=~" || model$op[i] == "~") {
      g <- add_edges(g, c(model$rhs[i], model$lhs[i]))
    }
    if (model$op[i] == "~~") {
      g <- add_edges(g, c(model$rhs[i], model$lhs[i], model$lhs[i], model$rhs[i]))
    }
  }

  layout <- layout_with_sugiyama(g)
  node_coords <- as.data.frame(layout$layout)
  colnames(node_coords) <- c("x", "y")
  node_coords$name <- V(g)$name  # Assign variable names

  edges <- as_edgelist(g)
  edge_coords <- data.frame(
    x_start = node_coords[match(edges[, 1], node_coords$name), "x"],
    y_start = node_coords[match(edges[, 1], node_coords$name), "y"],
    x_end = node_coords[match(edges[, 2], node_coords$name), "x"],
    y_end = node_coords[match(edges[, 2], node_coords$name), "y"],
    stringsAsFactors = FALSE
  )

  return(list(points = node_coords, lines = edge_coords))
}


#' Generate a data frame to render lavaan in the Shiny app
#' @keywords internal
#' @noRd
#' @importFrom lavaan lavaanify
#' @importFrom igraph graph.empty add_vertices add_edges layout_with_sugiyama as_edgelist
generate_graph_from_lavaan <- function(lavaan_string, relative_position = 1, point_size = 40,
                                       line_width = 1, text_size = 20, text_font = "serif",
                                       point_color = "black", edge_color = "black", line_endpoint_spacing = 0,
                                       node_border_color = "white",
                                       node_border_width = 1, fontface = "plain",
                                       arrow_type = "open", arrow_size = 0.2,
                                       layout_algorithm = layout_with_sugiyama) {

  model <- lavaanify(lavaan_string)

  g <- graph.empty(directed = TRUE)

  vars <- unique(c(model$lhs, model$rhs)) # lhs = left-hand side
  g <- add_vertices(g, length(vars), name = vars)

  lines_df <- data.frame(x_start = numeric(), y_start = numeric(), x_end = numeric(), y_end = numeric(),
                         ctrl_x = numeric(), ctrl_y = numeric(), type = character(),
                         color = character(), end_color = character(), color_type = character(),
                         gradient_position = numeric(), width = numeric(), alpha = numeric(),
                         arrow = logical(), arrow_type = character(), arrow_size = numeric(),
                         stringsAsFactors = FALSE)

  latent_vars <- unique(model$lhs[model$op == "=~"])

  for (i in 1:nrow(model)) {
    if (model$op[i] == "=~" || model$op[i] == "~") { # regression
      g <- add_edges(g, c(model$rhs[i], model$lhs[i]))
    } else if (model$op[i] == "~~" && model$lhs[i] != model$rhs[i]) { # covariance
      g <- add_edges(g, c(model$lhs[i], model$rhs[i]))
      g <- add_edges(g, c(model$rhs[i], model$lhs[i]))
    }
  }

  layout <- layout_algorithm(g)

  if (identical(layout_algorithm, layout_with_sugiyama)) {
    node_coords <- as.data.frame(layout$layout)
    colnames(node_coords) <- c("x", "y")
  } else {
    if (is.matrix(layout)) {
      node_coords <- as.data.frame(layout)
      colnames(node_coords) <- c("x", "y")
    } else {
      stop("Unexpected layout format: not a matrix")
    }
  }

  node_names <- V(g)$name

  # Calculate the center of the graph
  center_x <- mean(node_coords$x)
  center_y <- mean(node_coords$y)

  # Scaling relative to center
  node_coords$x <- (node_coords$x - center_x) * relative_position + center_x
  node_coords$y <- (node_coords$y - center_y) * relative_position + center_y

  adjust_endpoint <- function(x1, y1, x2, y2, spacing = 0) {

    dx <- x2 - x1
    dy <- y2 - y1
    distance <- sqrt(dx^2 + dy^2)

    # Adjust the start and end points based on spacing
    if (distance > 0) {
      x1 <- x1 + (spacing * dx / distance)
      y1 <- y1 + (spacing * dy / distance)
      x2 <- x2 - (spacing * dx / distance)
      y2 <- y2 - (spacing * dy / distance)
    }

    return(list(x_start = x1, y_start = y1, x_end = x2, y_end = y2))
  }


  # Convert edges (arrows) to lines data frame
  edges <- as_edgelist(g)
  for (i in 1:nrow(edges)) {
    start_node <- edges[i, 1]
    end_node <- edges[i, 2]

    x_start <- node_coords[node_names == start_node, "x"]
    y_start <- node_coords[node_names == start_node, "y"]
    x_end <- node_coords[node_names == end_node, "x"]
    y_end <- node_coords[node_names == end_node, "y"]

    adjusted_coords <- adjust_endpoint(x_start, y_start, x_end, y_end, line_endpoint_spacing)


    ctrl_x <- NA
    ctrl_y <- NA

    # Add arrow and color information using user input for edge color
    arrow <- TRUE  # All lines will be straight arrows for lavaan generated graph
    arrow_type <- arrow_type
    arrow_size <- arrow_size
    color <- edge_color
    end_color <- edge_color
    color_type <- "Single"
    gradient_position <- NA

    # Add the new line to the lines_df
    new_line <- data.frame(
      x_start = adjusted_coords$x_start,
      y_start = adjusted_coords$y_start,
      x_end = adjusted_coords$x_end,
      y_end = adjusted_coords$y_end,
      ctrl_x = ctrl_x,
      ctrl_y = ctrl_y,
      type = "Lavaan",
      color = color,
      end_color = end_color,
      color_type = color_type,
      gradient_position = gradient_position,
      width = line_width,
      alpha = 1,
      arrow = TRUE,
      arrow_type = arrow_type,
      arrow_size = arrow_size,
      stringsAsFactors = FALSE
    )


    lines_df <- rbind(lines_df, new_line)
  }

  # Create points (nodes) data frame and assign shapes based on latent/observed distinction
  points_df <- node_coords
  points_df$shape <- ifelse(node_names %in% latent_vars, "circle", "square")  # Latent variables as circles, observed variables as squares
  points_df$color <- point_color
  points_df$size <- point_size
  points_df$border_color <- node_border_color
  points_df$border_width <- node_border_width
  points_df$alpha <- 1
  points_df$locked <- FALSE     # Not locked

  # Create annotations data frame from node names
  annotations <- data.frame(
    text = node_names,
    x = node_coords$x,
    y = node_coords$y,
    font = text_font,
    size = text_size,
    color = "black",
    angle = 0,
    alpha = 1,
    fontface = fontface,
    math_expression = FALSE,
    stringsAsFactors = FALSE
  )

  return(list(points = points_df, lines = lines_df, annotations = annotations))
}

#' Auto-generate lines to "unlocked" points
#' @keywords internal
#' @noRd
#' @importFrom igraph graph.empty add_vertices add_edges layout_with_sugiyama as_edgelist
auto_generate_edges <- function(points_data, layout_type = "fully_connected", line_color = "black", line_width = 2, line_alpha = 1) {
  # Filter out locked nodes
  unlocked_points <- points_data[!points_data$locked, ]

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
      stringsAsFactors = FALSE
    )
    return(lines_df)
  } else {
    return(NULL)
  }
}


#' Auto-layout "unlocked" points using a specific algorithm (igraph)
#' @keywords internal
#' @noRd
#' @importFrom igraph graph.empty add_vertices add_edges layout_with_sugiyama as_edgelist
auto_layout_points <- function(points_data, layout_type = "layout_in_circle", distance = 1, center_x = 0, center_y = 0) {
  if (!"locked" %in% names(points_data)) {
    points_data$locked <- FALSE
  }

  unlocked_points <- points_data[!points_data$locked, ]

  if (layout_type == "horizontal_straight") {
    n <- nrow(unlocked_points)
    unlocked_points$x <- seq(center_x - distance * (n - 1) / 2, center_x + distance * (n - 1) / 2, length.out = n)
    unlocked_points$y <- rep(center_y, n)
  } else if (layout_type == "vertical_straight") {
    n <- nrow(unlocked_points)
    unlocked_points$y <- seq(center_y - distance * (n - 1) / 2, center_y + distance * (n - 1) / 2, length.out = n)
    unlocked_points$x <- rep(center_x, n)
  } else {
    g <- make_empty_graph(n = nrow(unlocked_points))
    layout_fun <- match.fun(layout_type)
    layout_coords <- layout_fun(g) * distance
    unlocked_points$x <- layout_coords[, 1] + center_x
    unlocked_points$y <- layout_coords[, 2] + center_y
  }

  points_data[!points_data$locked, ] <- unlocked_points
  return(points_data)
}
