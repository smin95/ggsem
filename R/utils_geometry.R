#' Interpolate points along a straight line
#'
#' Generates evenly spaced points along a straight line between two points,
#' primarily used for creating gradient effects.
#'
#' @param x_start Numeric, x-coordinate where line begins
#' @param y_start Numeric, y-coordinate where line begins
#' @param x_end Numeric, x-coordinate where line ends
#' @param y_end Numeric, y-coordinate where line ends
#' @param n Integer, number of points to interpolate (default: 500)
#'
#' @return A data frame with columns `x` and `y` containing the interpolated points
#' @keywords internal
#' @noRd
interpolate_points <- function(x_start, y_start, x_end, y_end, n = 500) {
  # n = 100 in the shiny app to make it faster
  t <- seq(0, 1, length.out = n)
  x <- (1 - t) * x_start + t * x_end
  y <- (1 - t) * y_start + t * y_end
  data.frame(x = x, y = y)
}

#' Find peak point of Bezier curve
#'
#' Calculates the point on a Bezier curve that is farthest from the straight line
#' connecting the start and end points, useful for determining curve height.
#'
#' @param bezier_points Data frame containing Bezier curve points with columns `x` and `y`
#' @param x_start Numeric, x-coordinate of line start point
#' @param y_start Numeric, y-coordinate of line start point
#' @param x_end Numeric, x-coordinate of line end point
#' @param y_end Numeric, y-coordinate of line end point
#'
#' @return Integer index of the peak point in `bezier_points`
#' @keywords internal
#' @noRd
find_peak_point <- function(bezier_points, x_start, y_start, x_end, y_end) {
  A <- y_end - y_start
  B <- x_start - x_end
  C <- (x_end * y_start) - (x_start * y_end)

  distances <- abs(A * bezier_points$x + B * bezier_points$y + C) / sqrt(A^2 + B^2)

  peak_idx <- which.max(distances)

  return(peak_idx)
}


#' Create cubic Bezier curve
#'
#' Generates points along a cubic Bezier curve defined by start, end, and two control points.
#'
#' @param x_start Numeric, x-coordinate where the curve begins
#' @param y_start Numeric, y-coordinate where the curve begins
#' @param x_end Numeric, x-coordinate where the curve ends
#' @param y_end Numeric, y-coordinate where the curve ends
#' @param ctrl_x Numeric, x-coordinate of first control point
#' @param ctrl_y Numeric, y-coordinate of first control point
#' @param ctrl_x2 Numeric, x-coordinate of second control point
#' @param ctrl_y2 Numeric, y-coordinate of second control point
#' @param n_points Integer, number of points to generate along the curve (default: 100)
#'
#' @return Data frame with columns `x` and `y` containing the curve points
#' @keywords internal
#' @noRd
create_bezier_curve <- function(x_start, y_start,
                                x_end, y_end,
                                ctrl_x, ctrl_y,
                                ctrl_x2, ctrl_y2,
                                n_points = 100) {

  t <- seq(0, 1, length.out = n_points)

  bezier_x <- (1-t)^3*x_start + 3*(1-t)^2*t*ctrl_x + 3*(1-t)*t^2*ctrl_x2 + t^3*x_end
  bezier_y <- (1-t)^3*y_start + 3*(1-t)^2*t*ctrl_y + 3*(1-t)*t^2*ctrl_y2 + t^3*y_end

  data.frame(x = bezier_x, y = bezier_y)
}

#' Validate hexadecimal color code
#'
#' Checks if a string represents a valid hexadecimal color code and returns
#' a fallback color if invalid.
#'
#' @param x Character string representing a hex color code
#'
#' @return Valid hex color code string, or "#000000" (black) if invalid
#' @keywords internal
#' @noRd
valid_hex <- function(x) {
  if (grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x)) {
    return(x)
  } else if (is.na(x)) {
    return(NA)
  } else {
    return("#000000")  # Default to black or another fallback color
  }
}

#' Rotate coordinates around a center point
#'
#' Applies rotation transformation to coordinates around a specified center point.
#'
#' @param x Numeric vector of x-coordinates to rotate
#' @param y Numeric vector of y-coordinates to rotate
#' @param angle Numeric, rotation angle in degrees (positive = counterclockwise)
#' @param cx Numeric, x-coordinate of rotation center (default: 0)
#' @param cy Numeric, y-coordinate of rotation center (default: 0)
#'
#' @return List with components:
#'   - `x`: Numeric vector of rotated x-coordinates
#'   - `y`: Numeric vector of rotated y-coordinates
#' @keywords internal
#' @noRd
rotate_coords <- function(x, y, angle, cx = 0, cy = 0) {
  angle_rad <- angle * pi / 180
  x_rot <- cos(angle_rad) * (x - cx) - sin(angle_rad) * (y - cy) + cx
  y_rot <- sin(angle_rad) * (x - cx) + cos(angle_rad) * (y - cy) + cy
  list(x = x_rot, y = y_rot)
}



#' Validate line style
#'
#' Ensures line style is one of the supported values with fallback to "solid".
#'
#' @param x Character string specifying line style
#'
#' @return Valid line style: "solid", "dotted", or "dashed"
#' @keywords internal
#' @noRd
valid_line_style <- function(x) {
  valid_styles <- c("dotted", "dashed", "solid")
  if (x %in% valid_styles) {
    return(x)
  } else {
    return("solid") # Default to solid
  }
}

#' Validate font face
#'
#' Ensures font face is one of the supported values with fallback to "plain".
#'
#' @param x Character string specifying font face
#'
#' @return Valid font face: "plain", "bold", or "italic"
#' @keywords internal
#' @noRd
valid_fontface <- function(x) {
  valid_faces <- c("plain", "bold", "italic")
  if (x %in% valid_faces) {
    return(x)
  } else {
    return("plain") # Default to plain
  }
}

#' Validate font family
#'
#' Ensures font family is one of the supported values with fallback to "sans".
#'
#' @param x Character string specifying font family
#'
#' @return Valid font family: "sans", "mono", or "serif"
#' @keywords internal
#' @noRd
valid_font <- function(x) {
  valid_fonts <- c("sans", "mono", "serif")
  if (x %in% valid_fonts) {
    return(x)
  } else {
    return("sans") # Default to sans
  }
}

#' Validate line or arrow type
#'
#' Ensures type is one of the supported values with fallback to "Straight Line".
#'
#' @param x Character string specifying type
#'
#' @return Valid type: "Straight Line", "Straight Arrow", "Curved Line", or "Curved Arrow"
#' @keywords internal
#' @noRd
valid_type <- function(x) {
  valid_types <- c("Straight Line", "Straight Arrow", "Curved Line", "Curved Arrow")
  if (x %in% valid_types) {
    return(x)
  } else {
    return("Straight Line") # Default to "Straight Line"
  }
}

#' Validate gradient position
#'
#' Ensures gradient position is numeric between 0 and 1 with fallback to 0.5.
#'
#' @param x Numeric value specifying gradient position
#'
#' @return Valid gradient position between 0 and 1
#' @keywords internal
#' @noRd
valid_gradient_position <- function(x) {
  x <- as.numeric(x)
  if (!is.na(x) && x >= 0 && x <= 1) {
    return(x)
  } else {
    return(0.5) # Default to midpoint
  }
}

#' Validate alpha transparency value
#'
#' Ensures alpha value is numeric between 0 and 1 with fallback to 1 (opaque).
#'
#' @param x Numeric value specifying transparency (0 = transparent, 1 = opaque)
#'
#' @return Valid alpha value between 0 and 1
#' @keywords internal
#' @noRd
valid_alpha <- function(x) {
  x <- as.numeric(x)
  if (!is.na(x) && x >= 0 && x <= 1) {
    return(x)
  } else {
    return(1) # Default to fully opaque
  }
}

#' Validate shape type
#'
#' Ensures shape is one of the supported values with fallback to "circle".
#'
#' @param x Character string specifying shape
#'
#' @return Valid shape: "circle", "square", "oval", "triangle", "rectangle", or "diamond"
#' @keywords internal
#' @noRd
valid_shape <- function(x) {
  valid_shapes <- c("circle", "square", "oval", "triangle", "rectangle", "diamond")
  if (x %in% valid_shapes) {
    return(x)
  } else {
    return("circle") # Default to circle
  }
}

#' Validate logical input
#'
#' Converts various logical representations to TRUE/FALSE with fallback to FALSE.
#'
#' @param x Value to be interpreted as logical (accepts TRUE/T/YES/1 or FALSE/F/NO/0)
#'
#' @return Logical value (TRUE or FALSE)
#' @keywords internal
#' @noRd
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



#' Adjust edge coordinates for node boundaries
#'
#' Adjusts the start and end coordinates of edges to account for node boundaries,
#' ensuring that lines/arrows properly connect to the edges of node shapes rather
#' than their centers. Handles different node shapes, sizes, and orientations.
#'
#' @param lines_df Data frame containing line coordinates with columns:
#'   `x_start`, `y_start`, `x_end`, `y_end`
#' @param edge_list Matrix or data frame with two columns specifying start and
#'   end node indices for each edge
#' @param points_df Data frame containing node information with columns:
#'   `x`, `y`, `shape`, `size`, `width_height_ratio`, `orientation`
#' @param auto_endpoint_spacing Numeric, additional spacing to apply beyond
#'   node boundaries (default: 0)
#'
#' @return Updated lines data frame with adjusted coordinates that account for
#'   node boundaries
#' @keywords internal
#' @noRd
adjust_edge_coordinates <- function(lines_df, edge_list, points_df, auto_endpoint_spacing = 0) {
  for (i in 1:nrow(lines_df)) {
    start_index <- edge_list[i, 1]
    end_index <- edge_list[i, 2]

    if (start_index == end_index) {
      #warning(paste("Skipping self-loop at row", i, "Node Index:", start_index))
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


#' Adjust endpoints for node shape boundaries
#'
#' Calculates the intersection points between a line connecting two nodes and
#' the boundaries of their respective shapes. Applies additional spacing if needed.
#'
#' @param x1 Numeric, x-coordinate of first node center
#' @param y1 Numeric, y-coordinate of first node center
#' @param x2 Numeric, x-coordinate of second node center
#' @param y2 Numeric, y-coordinate of second node center
#' @param spacing Numeric, additional spacing beyond node boundaries (default: 0)
#' @param shape1 Character, shape of first node (default: "circle")
#' @param shape2 Character, shape of second node (default: "circle")
#' @param dimensions1 List containing size and width_height_ratio for first node
#' @param dimensions2 List containing size and width_height_ratio for second node
#' @param orientation1 Numeric, orientation angle in degrees for first node (default: 0)
#' @param orientation2 Numeric, orientation angle in degrees for second node (default: 0)
#'
#' @return List with adjusted coordinates:
#'   - `x_start`, `y_start`: Adjusted start coordinates
#'   - `x_end`, `y_end`: Adjusted end coordinates
#' @keywords internal
#' @noRd
adjust_endpoint <- function(x1, y1, x2, y2, spacing = 0,
                            shape1 = "circle", shape2 = "circle",
                            dimensions1 = list(width_height_ratio = 1, size = 1),
                            dimensions2 = list(width_height_ratio = 1, size = 1),
                            orientation1 = 0, orientation2 = 0) {
  # Find intersections
  start_intersect <- find_intersection(
    x_center = x1, y_center = y1,
    x_target = x2, y_target = y2,
    size = dimensions1$size, width_height_ratio = dimensions1$width_height_ratio,
    orientation = orientation1, shape = shape1
  )

  x1 <- start_intersect$x
  y1 <- start_intersect$y

  end_intersect <- find_intersection(
    x_center = x2, y_center = y2,
    x_target = x1, y_target = y1,
    size = dimensions2$size, width_height_ratio = dimensions2$width_height_ratio,
    orientation = orientation2, shape = shape2
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

#' Find intersection point with node boundary
#'
#' Calculates the intersection point between a line from node center to target
#' point and the boundary of the node shape. Handles various shapes including
#' circles, squares, triangles, rectangles, diamonds, and ovals.
#'
#' @param x_center Numeric, x-coordinate of node center
#' @param y_center Numeric, y-coordinate of node center
#' @param x_target Numeric, x-coordinate of target point
#' @param y_target Numeric, y-coordinate of target point
#' @param size Numeric, size parameter of the node
#' @param width_height_ratio Numeric, width to height ratio (default: 1)
#' @param orientation Numeric, orientation angle in degrees (default: 0)
#' @param shape Character, shape type: "circle", "square", "triangle",
#'   "rectangle", "diamond", or "oval" (default: "circle")
#'
#' @return List with intersection coordinates:
#'   - `x`: x-coordinate of intersection point
#'   - `y`: y-coordinate of intersection point
#' @keywords internal
#' @noRd
find_intersection <- function(x_center, y_center, x_target, y_target,
                              size, width_height_ratio = 1, orientation = 0,
                              shape = "circle") {

  # Calculate the direction vector from center to target
  dx <- x_target - x_center
  dy <- y_target - y_center

  # Normalize the direction vector
  distance <- sqrt(dx^2 + dy^2)
  if (distance == 0) {
    return(list(x = x_center, y = y_center))
  }

  dx_norm <- dx / distance
  dy_norm <- dy / distance

  # Use the EXACT same calculations as draw_points
  min_size_factor <- 0.25
  scale_factor <- sqrt(2)

  # Calculate adjusted dimensions - EXACTLY as in draw_points
  adjusted_width <- switch(shape,
                           "circle" = size / scale_factor * min_size_factor,
                           "square" = size * sqrt(2) * min_size_factor,
                           "triangle" = size * sqrt(4 / sqrt(3)) * min_size_factor,
                           "rectangle" = size * min_size_factor * width_height_ratio,
                           "diamond" = size * 1.4 * sqrt(1.5) * min_size_factor * width_height_ratio,
                           "oval" = size * min_size_factor / scale_factor * width_height_ratio,
                           size / 3  # default
  )

  adjusted_height <- switch(shape,
                            "circle" = adjusted_width,
                            "square" = adjusted_width,
                            "triangle" = adjusted_width * sqrt(3) / 2,
                            "rectangle" = size * min_size_factor,
                            "diamond" = size * 1.4 * sqrt(1.5) * min_size_factor,
                            "oval" = size * min_size_factor / scale_factor,
                            adjusted_width / width_height_ratio  # default
  )

  coords <- switch(shape,
                   "circle" = {
                     t <- seq(0, 2 * pi, length.out = 100)
                     data.frame(
                       x = adjusted_width * cos(t),
                       y = adjusted_height * sin(t)
                     )
                   },
                   "square" = data.frame(
                     x = c(-adjusted_width/2, adjusted_width/2, adjusted_width/2, -adjusted_width/2),
                     y = c(-adjusted_height/2, -adjusted_height/2, adjusted_height/2, adjusted_height/2)
                   ),
                   "triangle" = data.frame(
                     x = c(0, -adjusted_width/2, adjusted_width/2),
                     y = c(-adjusted_height/2, adjusted_height/2, adjusted_height/2)
                   ),
                   "rectangle" = data.frame(
                     x = c(-adjusted_width/2, adjusted_width/2, adjusted_width/2, -adjusted_width/2),
                     y = c(-adjusted_height/2, -adjusted_height/2, adjusted_height/2, adjusted_height/2)
                   ),
                   "diamond" = data.frame(
                     x = c(0, -adjusted_width/2, 0, adjusted_width/2),
                     y = c(adjusted_height/2, 0, -adjusted_height/2, 0)
                   ),
                   "oval" = {
                     t <- seq(0, 2 * pi, length.out = 100)
                     data.frame(
                       x = adjusted_width * cos(t),
                       y = adjusted_height * sin(t)
                     )
                   },
                   # Default to circle
                   {
                     t <- seq(0, 2 * pi, length.out = 100)
                     data.frame(
                       x = adjusted_width * cos(t),
                       y = adjusted_height * sin(t)
                     )
                   }
  )

  if (orientation != 0) {
    angle <- -orientation * pi / 180
    coords <- rotate_coords(coords$x, coords$y, orientation, cx = 0, cy = 0)
  }

  if (shape %in% c("circle", "oval")) {
    if (abs(dx_norm) > 0 || abs(dy_norm) > 0) {
      t <- 1 / sqrt((dx_norm/adjusted_width)^2 + (dy_norm/adjusted_height)^2)
      x_intersect_rot <- t * dx_norm
      y_intersect_rot <- t * dy_norm
    } else {
      x_intersect_rot <- 0
      y_intersect_rot <- 0
    }
  } else if (shape == "triangle") {
    # Simple edge intersection for triangle
    n_vertices <- 3
    t_values <- c()

    for (i in 1:n_vertices) {
      j <- ifelse(i == n_vertices, 1, i + 1)

      x1 <- coords$x[i]
      y1 <- coords$y[i]
      x2 <- coords$x[j]
      y2 <- coords$y[j]

      # Line-line intersection between ray and edge
      denom <- (x2 - x1) * dy_norm - (y2 - y1) * dx_norm
      if (abs(denom) < 1e-10) next  # Parallel lines

      t <- (x1 * (y2 - y1) - y1 * (x2 - x1)) / denom
      if (t <= 1e-10) next  # Intersection behind ray origin or too close

      # Check if intersection is within segment using barycentric coordinates
      intersect_x <- t * dx_norm
      intersect_y <- t * dy_norm

      # Vector from edge start to intersection point
      edge_vec <- c(x2 - x1, y2 - y1)
      point_vec <- c(intersect_x - x1, intersect_y - y1)

      # Project point onto edge
      edge_length_sq <- edge_vec[1]^2 + edge_vec[2]^2
      if (edge_length_sq < 1e-10) next

      dot_product <- point_vec[1] * edge_vec[1] + point_vec[2] * edge_vec[2]
      u <- dot_product / edge_length_sq

      if (u >= 0 && u <= 1) {
        t_values <- c(t_values, t)
      }
    }

    if (length(t_values) > 0) {
      t <- min(t_values)
      x_intersect_rot <- t * dx_norm
      y_intersect_rot <- t * dy_norm
    } else {
      # Final fallback: use bounding circle of the actual triangle bounds
      max_x <- max(abs(coords$x))
      max_y <- max(abs(coords$y))
      max_dim <- max(max_x, max_y)
      x_intersect_rot <- max_dim * dx_norm
      y_intersect_rot <- max_dim * dy_norm
    }
  } else {
    # For polygons, find intersection with edges
    n_vertices <- nrow(coords)
    t_values <- c()

    for (i in 1:n_vertices) {
      j <- ifelse(i == n_vertices, 1, i + 1)

      x1 <- coords$x[i]
      y1 <- coords$y[i]
      x2 <- coords$x[j]
      y2 <- coords$y[j]

      # Line-line intersection between ray and edge
      denom <- (x2 - x1) * dy_norm - (y2 - y1) * dx_norm
      if (abs(denom) < 1e-10) next  # Parallel lines

      t <- (x1 * (y2 - y1) - y1 * (x2 - x1)) / denom
      if (t <= 0) next  # Intersection behind ray origin

      # Check if intersection is within segment
      u <- ((t * dx_norm - x1) * (x2 - x1) + (t * dy_norm - y1) * (y2 - y1)) /
        ((x2 - x1)^2 + (y2 - y1)^2)

      if (u >= 0 && u <= 1) {
        t_values <- c(t_values, t)
      }
    }

    if (length(t_values) > 0) {
      t <- min(t_values)  # Closest intersection
      x_intersect_rot <- t * dx_norm
      y_intersect_rot <- t * dy_norm
    } else {
      # Fallback: use bounding circle
      max_dim <- max(adjusted_width, adjusted_height)
      x_intersect_rot <- (max_dim / 2) * dx_norm
      y_intersect_rot <- (max_dim / 2) * dy_norm
    }
  }

  if (orientation != 0) {
    angle <- orientation * pi / 180
    x_temp <- x_intersect_rot
    y_temp <- y_intersect_rot
    x_intersect_rot <- cos(angle) * x_temp - sin(angle) * y_temp
    y_intersect_rot <- sin(angle) * x_temp + cos(angle) * y_temp
  }

  x_intersect <- x_center + x_intersect_rot
  y_intersect <- y_center + y_intersect_rot

  return(list(x = x_intersect, y = y_intersect))
}



#' Safe which function with null handling
#'
#' A wrapper around `which()` that returns NULL instead of integer(0) when
#' no elements satisfy the condition, preventing empty integer vector issues.
#'
#' @param condition Logical vector or expression to evaluate
#'
#' @return Integer vector of indices where condition is TRUE, or NULL if
#'   no elements satisfy the condition
#' @keywords internal
#' @noRd
safe_which <- function(condition) {
  result <- which(condition)
  if(length(result) > 0) result else NULL
}


#' Identify and classify ggplot2 layers
#'
#' Analyzes a ggplot2 object to identify and classify different layer types
#' (edges, nodes, texts) and extracts their data with proper indexing.
#' Handles arrow detection and two-way arrow identification.
#'
#' @param ggplot2_obj A ggplot2 object to analyze
#'
#' @return List containing classified data frames:
#'   - `lines`: Edge/line data with arrow information
#'   - `points`: Node/point data
#'   - `texts`: Text label data
#'   - `other`: Other geom types
#' @keywords internal
#' @noRd
identify_layers <- function(ggplot2_obj) {
  built_plot <- ggplot_build(ggplot2_obj)

  layer_types <- sapply(built_plot$plot$layers, function(layer) {
    class(layer$geom)[1]
  })

  component_map <- data.frame(
    layer_index = seq_along(built_plot$plot$layers),
    type = ifelse(layer_types == "GeomSegment", "edges",
                  ifelse(layer_types == "GeomPoint", "nodes",
                         ifelse(layer_types == "GeomText", "texts", "other"))),
    aes = sapply(built_plot$plot$layers, function(x) {
      paste(names(x$mapping), collapse = ", ")
    })
  )

  # Apply to all cases
  valid_layers <- list(
    lines = safe_which(component_map$type == "edges"),
    points = safe_which(component_map$type == "nodes"),
    texts = safe_which(component_map$type == "texts")
  )

  valid_layers1 <- Filter(Negate(is.null), valid_layers)
  layer_indices <- unlist(valid_layers1)
  ordered_names <- names(layer_indices)[order(layer_indices)]

  names(built_plot$data) <- ordered_names

  is_zero_length <- function(x) {
    if (inherits(x, "unit")) {
      as.numeric(x) == 0
    } else {
      x == 0
    }
  }

  if (!is.null(built_plot$data[['lines']])) {
    arrow_params <- built_plot$plot$layers[[valid_layers1$lines]]$geom_params$arrow
    has_arrows <- !is.null(arrow_params) &&
      !is_zero_length(arrow_params$length)
    built_plot$data[[valid_layers1$lines]]$arrow <- has_arrows
    built_plot$data[[valid_layers1$lines]]$arrow_type <- if (has_arrows) {
      if (arrow_params$type == 1) "open" else if (arrow_params$type == 2) "closed" else NA
    } else {
      NA
    }
    built_plot$data[[valid_layers1$lines]]$two_way <- if (arrow_params$ends == 3) TRUE else FALSE
  }

  ggnet2_data <- built_plot$data
  new_ggnet2_data <- list()
  patterns <- c("lines", "texts", "points", "other")
  for (pattern in patterns) {
    matching_elements <- ggnet2_data[grep(pattern, names(ggnet2_data))]
    if (length(matching_elements) > 0) {
      new_ggnet2_data[[pattern]] <- do.call(rbind, matching_elements)
    }
  }

  return(new_ggnet2_data)
}


#' Convert color name to hexadecimal format
#'
#' Converts R color names to hexadecimal color codes. Returns hex codes unchanged.
#'
#' @param color Character string, either a color name or hex code
#'
#' @return Character string with hexadecimal color code
#' @importFrom grDevices col2rgb colorRampPalette colors rainbow rgb
#' @keywords internal
#' @noRd
to_hex <- function(color) {
  if (grepl("^#[0-9A-Fa-f]{6}$", color, ignore.case = TRUE)) {
    return(color)
  }
  else if (color %in% colors()) {
    rgb_matrix <- col2rgb(color)
    return(rgb(rgb_matrix[1,], rgb_matrix[2,], rgb_matrix[3,], maxColorValue = 255))
  }
  else {
    return(color)
  }
}

#' Convert color to hexadecimal format with alpha handling
#'
#' Converts R color names to hexadecimal color codes, handling both 6-digit
#' and 8-digit (with alpha) hex codes. For 8-digit codes, strips alpha channel.
#'
#' @param color Character string, either a color name or hex code
#'
#' @return Character string with 6-digit hexadecimal color code (alpha removed)
#' @importFrom grDevices col2rgb colorRampPalette colors rainbow rgb
#' @keywords internal
#' @noRd
to_hex2 <- function(color) {
  if (grepl("^#[0-9A-Fa-f]{6}$", color, ignore.case = TRUE)) {
    return(color)
  }
  else if (grepl("^#[0-9A-Fa-f]{8}$", color, ignore.case = TRUE)) {
    return(substr(color, 1, 7))
  }
  else if (color %in% colors()) {
    rgb_matrix <- col2rgb(color)
    return(rgb(rgb_matrix[1,], rgb_matrix[2,], rgb_matrix[3,], maxColorValue = 255))
  }
  else {
    return(color)
  }
}


#' Extract alpha value from hexadecimal color
#'
#' Extracts the alpha (transparency) value from 8-digit or 4-digit hexadecimal
#' color codes. Returns 1 (fully opaque) for colors without alpha channel.
#'
#' @param hex_color Character string with hexadecimal color code
#'
#' @return Numeric alpha value between 0 (transparent) and 1 (opaque)
#' @keywords internal
#' @noRd
get_alpha <- function(hex_color) {
  if (grepl("^#[0-9A-Fa-f]{8}$", hex_color, ignore.case = TRUE)) {
    alpha_hex <- substr(hex_color, 8, 9)  # Extract last 2 digits (AA)
  }
  else if (grepl("^#[0-9A-Fa-f]{4}$", hex_color, ignore.case = TRUE)) {
    alpha_hex <- paste0(
      substr(hex_color, 4, 4),  # Short hex (RGBA) doubles digits (A -> AA)
      substr(hex_color, 4, 4)
    )
  }
  else {
    return(1)  # Default to opaque (1) if no alpha channel exists
  }

  alpha_decimal <- strtoi(paste0("0x", alpha_hex)) / 255
  return(alpha_decimal)
}


#' Calculate control points for bezier curves
#'
#' Computes control points for cubic bezier curves used in network visualizations.
#' Generates control points that create smooth curved edges between nodes with
#' customizable curvature magnitude, rotation, and asymmetry.
#'
#' @param x_start Numeric, x-coordinate of the starting point
#' @param y_start Numeric, y-coordinate of the starting point
#' @param x_end Numeric, x-coordinate of the ending point
#' @param y_end Numeric, y-coordinate of the ending point
#' @param curvature_magnitude Numeric, magnitude of the curvature (default: 0.3).
#'   Higher values create more pronounced curves. Typical range: 0.1 to 0.5.
#' @param rotate_curvature Logical, whether to rotate the curvature direction
#'   (default: FALSE). When TRUE, curves in the opposite direction.
#' @param curvature_asymmetry Numeric, asymmetry factor for the curve (default: 0).
#'   Positive values shift curvature toward the start, negative toward the end.
#'   Range: -1 to 1.
#' @param center_x center x coordinate of nodes
#' @param center_y center y coordinate of nodes
#'
#' @return A list containing four control point coordinates:
#'   - `ctrl_x`: x-coordinate of first control point
#'   - `ctrl_y`: y-coordinate of first control point
#'   - `ctrl_x2`: x-coordinate of second control point
#'   - `ctrl_y2`: y-coordinate of second control point
#'
#' @details
#' This function implements a cubic bezier curve calculation where:
#' \itemize{
#'   \item Control points are positioned at 1/3 and 2/3 along the line segment
#'   \item Curvature is applied perpendicular to the line direction
#'   \item Asymmetry adjusts the relative curvature strength at each control point
#'   \item Rotation flips the curve direction by mirroring control points
#' }
#'
#' The algorithm:
#' \enumerate{
#'   \item Calculates midpoint and segment properties
#'   \item Computes perpendicular direction vector
#'   \item Applies curvature magnitude with asymmetry adjustment
#'   \item Optionally rotates curvature direction
#'   \item Returns control points for cubic bezier interpolation
#' }
#'
#' @examples
#' \dontrun{
#' # Simple symmetric curve
#' cp <- calculate_control_point(0, 0, 10, 0, curvature_magnitude = 0.3)
#'
#' # Asymmetric curve favoring the start point
#' cp <- calculate_control_point(0, 0, 10, 0, curvature_magnitude = 0.3,
#'                              curvature_asymmetry = 0.5)
#'
#' # Rotated curve (opposite direction)
#' cp <- calculate_control_point(0, 0, 10, 0, curvature_magnitude = 0.3,
#'                              rotate_curvature = TRUE)
#' }
#'
#' @seealso \code{\link{create_bezier_curve}} for generating bezier curves from control points
#' @keywords internal
#' @noRd
calculate_control_point <- function(x_start, y_start, x_end, y_end,
                                    curvature_magnitude = 0.3,
                                    rotate_curvature = FALSE,
                                    curvature_asymmetry = 0,
                                    center_x = NULL, center_y = NULL) {
  mid_x <- (x_start + x_end) / 2
  mid_y <- (y_start + y_end) / 2
  dx <- x_end - x_start
  dy <- y_end - y_start
  seg_length <- sqrt(dx^2 + dy^2)

  if (is.null(center_x) || is.null(center_y)) {
    perp_x <- -dy/seg_length
    perp_y <- dx/seg_length
  } else {
    center_to_mid_x <- mid_x - center_x
    center_to_mid_y <- mid_y - center_y
    center_to_mid_length <- sqrt(center_to_mid_x^2 + center_to_mid_y^2)

    if (center_to_mid_length > 0) {
      outward_x <- center_to_mid_x / center_to_mid_length
      outward_y <- center_to_mid_y / center_to_mid_length

      perp_x <- -dy/seg_length
      perp_y <- dx/seg_length

      dot_product <- perp_x * outward_x + perp_y * outward_y

      if (dot_product < 0) {
        perp_x <- -perp_x
        perp_y <- -perp_y
      }
    } else {
      perp_x <- -dy/seg_length
      perp_y <- dx/seg_length
    }
  }

  ctrl1_x <- x_start + dx/3 + perp_x * curvature_magnitude * seg_length * (1 + curvature_asymmetry)
  ctrl1_y <- y_start + dy/3 + perp_y * curvature_magnitude * seg_length * (1 + curvature_asymmetry)
  ctrl2_x <- x_end - dx/3 + perp_x * curvature_magnitude * seg_length * (1 - curvature_asymmetry)
  ctrl2_y <- y_end - dy/3 + perp_y * curvature_magnitude * seg_length * (1 - curvature_asymmetry)

  if (rotate_curvature) {
    ctrl1_x <- 2 * mid_x - ctrl1_x
    ctrl1_y <- 2 * mid_y - ctrl1_y
    ctrl2_x <- 2 * mid_x - ctrl2_x
    ctrl2_y <- 2 * mid_y - ctrl2_y

    temp_x <- ctrl1_x
    temp_y <- ctrl1_y
    ctrl1_x <- ctrl2_x
    ctrl1_y <- ctrl2_y
    ctrl2_x <- temp_x
    ctrl2_y <- temp_y
  }

  list(
    ctrl_x = ctrl1_x,
    ctrl_y = ctrl1_y,
    ctrl_x2 = ctrl2_x,
    ctrl_y2 = ctrl2_y
  )
}


#' Scale and center graph coordinates
#'
#' Transforms node and edge coordinates to a standardized range while preserving
#' the relative layout structure. Centers the graph at the origin (0,0) and
#' scales coordinates to fit within specified bounds, maintaining aspect ratio.
#'
#' @param nodes_df Data frame containing node coordinates with columns:
#'   - `x`, `y`: Numeric, node center coordinates
#'   - `text_x`, `text_y`: Numeric, node label coordinates (optional)
#' @param edges_df Data frame containing edge coordinates with columns:
#'   - `x_start`, `y_start`: Numeric, edge start coordinates
#'   - `x_end`, `y_end`: Numeric, edge end coordinates
#'   - `label_x`, `label_y`: Numeric, edge label coordinates (optional)
#'   Default: NULL (only scale nodes)
#' @param new_min Numeric, minimum value for scaled coordinate range (default: -2)
#' @param new_max Numeric, maximum value for scaled coordinate range (default: 2)
#'
#' @return A list containing:
#'   - `nodes`: Scaled and centered nodes data frame
#'   - `edges`: Scaled and centered edges data frame (if edges_df provided)
#'
#'
#' @examples
#' \dontrun{
#' # Scale nodes only
#' nodes <- data.frame(x = c(0, 10, 5), y = c(0, 8, 4))
#' scaled <- scale_coordinates_centered(nodes)
#'
#' # Scale both nodes and edges
#' nodes <- data.frame(x = c(0, 10), y = c(0, 8))
#' edges <- data.frame(x_start = c(0), y_start = c(0),
#'                    x_end = c(10), y_end = c(8))
#' scaled <- scale_coordinates_centered(nodes, edges, -1, 1)
#' }
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @keywords internal
#' @noRd
scale_coordinates_centered <- function(nodes_df, edges_df = NULL, new_min = -2, new_max = 2) {
  # Calculate scaling parameters from nodes_df
  x_center <- (min(nodes_df$x, na.rm = TRUE) + max(nodes_df$x, na.rm = TRUE)) / 2
  y_center <- (min(nodes_df$y, na.rm = TRUE) + max(nodes_df$y, na.rm = TRUE)) / 2
  x_range <- max(nodes_df$x, na.rm = TRUE) - min(nodes_df$x, na.rm = TRUE)
  y_range <- max(nodes_df$y, na.rm = TRUE) - min(nodes_df$y, na.rm = TRUE)

  max_range <- max(x_range, y_range)
  scale_factor <- (new_max - new_min) / max_range

  nodes_df_scaled <- nodes_df |>
    mutate(
      x = (x - x_center) * scale_factor,
      y = (y - y_center) * scale_factor,
      text_x = (text_x - x_center) * scale_factor,
      text_y = (text_y - y_center) * scale_factor
    )

  if (!is.null(edges_df)) {
    edges_df_scaled <- edges_df |>
      mutate(
        x_start = (x_start - x_center) * scale_factor,
        y_start = (y_start - y_center) * scale_factor,
        x_end= (x_end - x_center) * scale_factor,
        y_end = (y_end - y_center) * scale_factor,
        label_x = (label_x - x_center) * scale_factor,
        label_y = (label_y - y_center) * scale_factor
      )

    return(list(nodes = nodes_df_scaled, edges = edges_df_scaled))
  }

  return(list(nodes = nodes_df_scaled, edges = NULL))
}

#' Detect local alignment patterns among nodes
#'
#' Identifies whether nodes are aligned horizontally or vertically within a
#' specified tolerance. This function helps in detecting structured layout
#' patterns in graph visualizations, which is useful for optimizing edge
#' routing and improving overall readability.
#'
#' @param node_x Numeric x-coordinate of the reference node
#' @param node_y Numeric y-coordinate of the reference node
#' @param all_x Numeric vector containing x-coordinates of all nodes
#' @param all_y Numeric vector containing y-coordinates of all nodes
#' @param tolerance Numeric tolerance for considering nodes as aligned.
#'   Nodes are considered aligned if their coordinates differ by less than
#'   this value. Default is 0.1.
#'
#' @return A list containing alignment information with the following elements:
#'   - `type`: Character string indicating the alignment type:
#'     - "horizontal": Nodes are horizontally aligned (same y-coordinate)
#'     - "vertical": Nodes are vertically aligned (same x-coordinate)
#'     - "scattered": No significant alignment pattern detected
#'   - `count`: Integer count of nodes sharing the alignment (including the
#'     reference node)
#'   - `y`: (For horizontal alignment) The y-coordinate shared by aligned nodes
#'   - `x`: (For vertical alignment) The x-coordinate shared by aligned nodes
#'
#'
#' @examples
#' \dontrun{
#' # Example 1: Horizontal alignment
#' x_coords <- c(1, 2, 3, 1.5, 2.5)
#' y_coords <- c(2, 2.05, 1.95, 5, 5)
#' result <- detect_local_alignment(1, 2, x_coords, y_coords, tolerance = 0.1)
#' # Returns: list(type = "horizontal", count = 3, y = 2)
#'
#' # Example 2: Vertical alignment
#' x_coords <- c(1, 1.05, 0.95, 3, 4)
#' y_coords <- c(2, 3, 4, 3, 4)
#' result <- detect_local_alignment(1, 2, x_coords, y_coords, tolerance = 0.1)
#' # Returns: list(type = "vertical", count = 3, x = 1)
#'
#' # Example 3: Scattered nodes
#' x_coords <- c(1, 3, 5, 7, 9)
#' y_coords <- c(2, 4, 6, 8, 10)
#' result <- detect_local_alignment(1, 2, x_coords, y_coords, tolerance = 0.1)
#' # Returns: list(type = "scattered", count = 1)
#' }
#'
#' @keywords internal
#' @noRd
detect_local_alignment <- function(node_x, node_y, all_x, all_y, tolerance = 0.1) {
  # Find nodes that are horizontally aligned (same Y within tolerance)
  horizontally_aligned <- abs(all_y - node_y) < tolerance
  horizontal_count <- sum(horizontally_aligned)

  # Find nodes that are vertically aligned (same X within tolerance)
  vertically_aligned <- abs(all_x - node_x) < tolerance
  vertical_count <- sum(vertically_aligned)

  # Return the strongest alignment pattern
  if (horizontal_count >= 2 && horizontal_count >= vertical_count) {
    return(list(type = "horizontal", count = horizontal_count, y = node_y))
  } else if (vertical_count >= 2 && vertical_count >= horizontal_count) {
    return(list(type = "vertical", count = vertical_count, x = node_x))
  } else {
    return(list(type = "scattered", count = 1))
  }
}

#' Flip coordinates around the center point
#'
#' Performs reflection/flip transformations on coordinate data around the
#' geometric center. This function is useful for layout manipulation in
#' graph visualizations, allowing horizontal, vertical, or combined flips.
#'
#' @param df Data frame containing coordinates to flip. Must contain columns:
#'   - `x`: Numeric x-coordinates
#'   - `y`: Numeric y-coordinates
#' @param direction Character string specifying the flip direction:
#'   - "horizontal": Flip across vertical axis (mirror left-right)
#'   - "vertical": Flip across horizontal axis (mirror top-bottom)
#'   - "both": Flip across both axes simultaneously (180-degree rotation)
#'
#' @return A list containing the transformed coordinates:
#'   - `x`: Numeric vector of transformed x-coordinates
#'   - `y`: Numeric vector of transformed y-coordinates
#'
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' df <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3))
#'
#' # Horizontal flip (mirror left-right)
#' flipped_h <- flip_around_center(df, "horizontal")
#' # Points (1,1), (2,2), (3,3) become (3,1), (2,2), (1,3)
#'
#' # Vertical flip (mirror top-bottom)
#' flipped_v <- flip_around_center(df, "vertical")
#' # Points (1,1), (2,2), (3,3) become (1,3), (2,2), (3,1)
#'
#' # Both flips (180-degree rotation)
#' flipped_both <- flip_around_center(df, "both")
#' # Points (1,1), (2,2), (3,3) become (3,3), (2,2), (1,1)
#' }
#'
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

#' Rotate coordinates around the center point
#'
#' Performs rotation transformations on coordinate data around the geometric
#' center. This function applies 2D rotation using standard trigonometric
#' transformations, useful for reorienting graph layouts.
#'
#' @param df Data frame containing coordinates to rotate. Must contain columns:
#'   - `x`: Numeric x-coordinates
#'   - `y`: Numeric y-coordinates
#' @param rotation_angle Numeric angle in degrees for the rotation.
#'   Positive angles rotate counter-clockwise, negative angles rotate clockwise.
#'
#' @return A list containing the transformed coordinates:
#'   - `x`: Numeric vector of transformed x-coordinates
#'   - `y`: Numeric vector of transformed y-coordinates
#'
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' df <- data.frame(x = c(1, 2, 1), y = c(1, 1, 2))
#'
#' # 90-degree counter-clockwise rotation
#' rotated_90 <- rotate_around_center(df, 90)
#' # Points (1,1), (2,1), (1,2) rotate around center (4/3, 4/3)
#'
#' # 180-degree rotation
#' rotated_180 <- rotate_around_center(df, 180)
#' # Equivalent to flip_around_center(df, "both")
#'
#' # -45-degree rotation (45 degrees clockwise)
#' rotated_neg45 <- rotate_around_center(df, -45)
#' }
#'
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
