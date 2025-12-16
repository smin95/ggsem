#' Extract SVG dimensions from XML document
#'
#' Extracts width and height dimensions from an SVG XML document,
#' first attempting to use the viewBox attribute and falling back
#' to explicit width/height attributes if viewBox is not available.
#'
#' @param svg An XML document object representing the SVG
#'
#' @return A list containing:
#'   - `width`: Numeric width of the SVG
#'   - `height`: Numeric height of the SVG
#'
#' @importFrom xml2 xml_attr
#' @keywords internal
#' @noRd
get_svg_dimensions <- function(svg) {
  view_box <- xml_attr(svg, "viewBox")
  if (!is.na(view_box)) {
    dims <- as.numeric(strsplit(view_box, " ")[[1]])
    return(list(width = dims[3], height = dims[4]))
  }
  list(width = as.numeric(gsub("pt", "", xml_attr(svg, "width"))),
       height = as.numeric(gsub("pt", "", xml_attr(svg, "height"))))
}

#' Extract node properties from SVG XML
#'
#' Parses SVG XML nodes to extract comprehensive node properties including
#' position, dimensions, styling, and text attributes. Handles both rectangle
#' and oval shapes with proper coordinate transformation.
#'
#' @param node An XML node representing a graph node from SVG
#' @param svg The parent SVG XML document for dimension reference
#'
#' @return A data frame with node properties:
#'   - `id`: Character, node identifier
#'   - `node_name`: Character, node name from title element
#'   - `label`: Character, display label text
#'   - `shape`: Character, shape type ("rectangle" or "oval")
#'   - `x`, `y`: Numeric, center coordinates
#'   - `width`, `height`: Numeric, node dimensions
#'   - `stroke_color`: Character, border color
#'   - `fill_color`: Character, fill color (with fallback to white)
#'   - `alpha`: Numeric, transparency (0-1)
#'   - `text_x`, `text_y`: Numeric, text label position
#'   - `font_size`: Numeric, text font size
#'   - `text_color`: Character, text color
#'
#' @details
#' This function handles SVG coordinate system conversion (SVG uses top-left
#' origin while graph layouts typically use bottom-left origin) and extracts:
#' \itemize{
#'   \item Shape detection (rectangle vs oval)
#'   \item Dimension calculation from polygon points or ellipse radii
#'   \item Color and transparency extraction with hex alpha channel support
#'   \item Text properties and positioning
#'   \item Coordinate transformation for consistent layout
#' }
#'
#' @importFrom xml2 xml_find_first xml_attr xml_text
#' @importFrom stringr str_split
#' @keywords internal
#' @noRd
extract_node_properties <- function(node, svg) {
  dims <- get_svg_dimensions(svg)
  svg_height <- dims$height

  id <- xml_attr(node, "id")
  node_name <- xml_text(xml_find_first(node, "./title"))  # Gets "A", "B", etc.

  text_node <- xml_find_first(node, "./text")
  label <- xml_text(text_node)  # Gets "Start", "Data Collection", etc.

  shape <- ifelse(!is.na(xml_find_first(node, "./polygon")), "rectangle", "oval")

  if (shape == "rectangle") {
    points <- xml_attr(xml_find_first(node, "./polygon"), "points")
    coords <- str_split(points, " ")[[1]][1]  # First point (top-left corner)
    xy <- as.numeric(str_split(coords, ",")[[1]])
    all_coords <- str_split(str_split(points, " ")[[1]], ",")
    all_coords_matrix <- matrix(as.numeric(unlist(all_coords)), ncol = 2, byrow = TRUE)
    width <- abs(xy[1] - all_coords_matrix[2, 1])
    height <- abs(xy[2] - all_coords_matrix[4, 2])

  } else {
    rx <- as.numeric(xml_attr(xml_find_first(node, "./ellipse"), "rx"))
    ry <- as.numeric(xml_attr(xml_find_first(node, "./ellipse"), "ry"))
    width <- 2 * rx
    height <- 2 * ry
  }

  text_x <- as.numeric(xml_attr(text_node, "x"))
  text_y <- as.numeric(xml_attr(text_node, "y"))
  font_size <- as.numeric(xml_attr(text_node, "font-size"))
  text_color <- xml_attr(text_node, "fill")

  #center_y <- svg_height - center_y
  text_y <- svg_height - text_y

  stroke <- ifelse(shape == "rectangle",
                   xml_attr(xml_find_first(node, "./polygon"), "stroke"),
                   xml_attr(xml_find_first(node, "./ellipse"), "stroke"))

  fill <- ifelse(shape == "rectangle",
                 xml_attr(xml_find_first(node, "./polygon"), "fill"),
                 xml_attr(xml_find_first(node, "./ellipse"), "fill"))

  fill_opacity <- ifelse(shape == "rectangle",
                         xml_attr(xml_find_first(node, "./polygon"), "fill-opacity"),
                         xml_attr(xml_find_first(node, "./ellipse"), "fill-opacity"))

  if (is.na(fill_opacity)) {
    # If no explicit fill-opacity, check if fill color has alpha in hex format
    if (grepl("^#([A-Fa-f0-9]{8})$", fill)) {
      # Extract alpha from 8-digit hex (last 2 digits)
      alpha_hex <- substr(fill, 8, 9)
      fill_opacity <- as.numeric(strtoi(alpha_hex, 16)) / 255
    } else {
      fill_opacity <- 1.0 # Default to fully opaque
    }
  } else {
    fill_opacity <- as.numeric(fill_opacity)
  }

  data.frame(
    id = id,
    node_name = node_name,
    label = label,
    shape = shape,
    x = text_x,
    y = text_y,
    width = width,
    height = height,
    stroke_color = stroke,
    fill_color = ifelse(fill == 'none', '#FFFFFF' , fill),
    alpha = fill_opacity,
    text_x = text_x,
    text_y = text_y,
    font_size = font_size * 1.4,
    text_color = text_color,
    stringsAsFactors = FALSE
  )
}

#' Extract edge properties from SVG XML
#'
#' Parses SVG XML edges to extract comprehensive edge properties including
#' path coordinates, arrowheads, styling, and labels. Handles directed and
#' undirected edges with various arrow configurations.
#'
#' @param edge An XML node representing a graph edge from SVG
#' @param svg The parent SVG XML document for dimension reference
#'
#' @return A data frame with edge properties:
#'   - `source`, `target`: Character, source and target node names
#'   - `label`: Character, edge label text
#'   - `label_x`, `label_y`: Numeric, label position coordinates
#'   - `label_color`, `label_size`: Character/Numeric, label styling
#'   - `label_alpha`: Numeric, label transparency
#'   - `x_start`, `y_start`, `x_end`, `y_end`: Numeric, edge endpoints
#'   - `stroke_color`: Character, edge line color
#'   - `stroke_width`: Numeric, edge line width
#'   - `arrow_fill`: Character, arrowhead fill color
#'   - `arrow_config`: Character, arrow configuration ("none", "forward",
#'     "backward", "bidirectional", "multiple")
#'   - `alpha`: Numeric, arrowhead transparency
#'   - `arrow_stroke`: Character, arrowhead border color
#'   - `is_directed`: Logical, whether edge is directed
#'   - `is_curved`: Logical, whether edge is curved (currently not implemented)
#'   - `ctrl_x`, `ctrl_y`, `ctrl_x2`, `ctrl_y2`: Numeric, bezier control points
#'   - `curvature`: Numeric, curvature magnitude
#'
#' @details
#' This function analyzes SVG path elements and arrowhead polygons to determine:
#' \itemize{
#'   \item Edge directionality and arrowhead configuration
#'   \item Path coordinates with proper coordinate transformation
#'   \item Arrowhead positioning and styling
#'   \item Label extraction and positioning
#'   \item Color and transparency handling with multiple format support
#' }
#'
#' The function handles various arrow configurations including forward, backward,
#' bidirectional, and multiple arrowheads.
#'
#' @importFrom xml2 xml_find_first xml_attr xml_find_all xml_text
#' @importFrom stringr str_split str_match str_extract_all
#' @keywords internal
#' @noRd
extract_edge_properties <- function(edge, svg) {
  dims <- get_svg_dimensions(svg)
  svg_height <- dims$height

  id <- xml_attr(edge, "id")
  title <- xml_text(xml_find_first(edge, "./title"))

  edge_nodes <- str_match(title, "(\\w+)\\s*->\\s*(\\w+)")[1, 2:3]
  is_directed <- !any(is.na(edge_nodes))

  if (any(is.na(edge_nodes))) {
    edge_nodes <- str_match(title, "(\\w+)\\s*~~\\s*(\\w+)")[1, 2:3]
    is_directed <- FALSE
  }

  # Path properties
  path <- xml_find_first(edge, "./path")
  path_d <- xml_attr(path, "d")
  stroke <- xml_attr(path, "stroke")
  `%||%` <- function(a, b) ifelse(!is.na(a) & !is.null(a), a, b)
  stroke_width <- as.numeric(xml_attr(path, "stroke-width") %||% 1)

  is_curved <- FALSE
  ctrl_x <- NA
  ctrl_y <- NA
  ctrl_x2 <- NA
  ctrl_y2 <- NA
  curvature <- 0

  polygons <- xml_find_all(edge, "./polygon")
  arrowheads <- list()

  if (length(polygons) > 0) {
    # Extract arrowhead positions by analyzing their coordinates
    for (polygon in polygons) {
      points <- xml_attr(polygon, "points")
      if (!is.na(points)) {
        coords <- str_extract_all(points, "[0-9.-]+")[[1]] |> as.numeric()
        if (length(coords) >= 6) {  # Need at least 3 points (x,y pairs) for a triangle
          # Calculate centroid of the arrowhead
          centroid_x <- mean(coords[seq(1, length(coords), by = 2)])
          centroid_y <- mean(coords[seq(2, length(coords), by = 2)])

          arrowheads <- c(arrowheads, list(list(
            x = centroid_x,
            y = centroid_y,
            fill = xml_attr(polygon, "fill"),
            stroke = xml_attr(polygon, "stroke")
          )))
        }
      }
    }
  }

  arrowhead_count <- length(arrowheads)
  arrow_config <- "none"

  if (arrowhead_count > 0) {
    # Get path endpoints to determine which end is which
    path_coords <- if (!is.na(path_d)) str_extract_all(path_d, "[0-9.-]+")[[1]] |> as.numeric() else numeric()
    if (length(path_coords) >= 4) {
      start_x <- path_coords[1]
      start_y <- svg_height - path_coords[2]
      end_x <- path_coords[length(path_coords)-1]
      end_y <- svg_height - path_coords[length(path_coords)]

      # Calculate distances to determine which arrowhead is at which end
      if (arrowhead_count == 1) {
        # Single arrowhead - determine which end it's on
        arrow_x <- arrowheads[[1]]$x
        arrow_y <- arrowheads[[1]]$y

        dist_to_start <- sqrt((arrow_x - start_x)^2 + (arrow_y - start_y)^2)
        dist_to_end <- sqrt((arrow_x - end_x)^2 + (arrow_y - end_y)^2)

        arrow_config <- ifelse(dist_to_start < dist_to_end, "forward", "backward")

      } else if (arrowhead_count == 2) {
        # Two arrowheads - bidirectional edge
        arrow_config <- "bidirectional"
      } else if (arrowhead_count > 2) {
        # Multiple arrowheads (unusual, but handle it)
        arrow_config <- "multiple"
      }
    }
  }


  # Arrowhead properties
  polygon <- xml_find_first(edge, "./polygon")
  has_arrowhead <- !is.na(polygon)
  arrow_fill <- ifelse(!is.na(polygon), xml_attr(polygon, "fill"), NA)
  arrow_stroke <- ifelse(!is.na(polygon), xml_attr(polygon, "stroke"), NA)

  is_directed <- is_directed || has_arrowhead

  arrow_fill_opacity <- ifelse(!is.na(polygon), xml_attr(polygon, "fill-opacity"), NA)

  # Handle arrowhead opacity fallbacks
  if (!is.na(arrow_fill) && is.na(arrow_fill_opacity)) {
    if (grepl("^#([A-Fa-f0-9]{8})$", arrow_fill)) {
      alpha_hex <- substr(arrow_fill, 8, 9)
      arrow_fill_opacity <- as.numeric(strtoi(alpha_hex, 16)) / 255
    } else if (grepl("^rgba\\(", arrow_fill)) {
      rgba_match <- str_match(arrow_fill, "rgba\\((\\d+),\\s*(\\d+),\\s*(\\d+),\\s*([0-9.]+)\\)")
      if (!is.na(rgba_match[1, 1])) {
        arrow_fill_opacity <- as.numeric(rgba_match[1, 5])
      }
    } else {
      arrow_fill_opacity <- 1.0
    }
  }

  text_nodes <- xml_find_all(edge, ".//text")
  label_alpha <- 1
  if (length(text_nodes) > 0) {
    label <- xml_text(text_nodes[1])
    label_x <- as.numeric(xml_attr(text_nodes[1], "x") %||% NA)
    label_y <- as.numeric(xml_attr(text_nodes[1], "y") %||% NA)
    label_color <- xml_attr(text_nodes[1], "fill") %||% NA
    label_size <- as.numeric(xml_attr(text_nodes[1], "font-size") %||% NA)

    label_y <- ifelse(!is.na(label_y), svg_height - label_y, NA)


  } else {
    label <- label_x <- label_y <- label_color <- label_size <- NA
  }

  path_coords <- str_extract_all(path_d, "[0-9.-]+")[[1]] |> as.numeric()
  if (length(path_coords) >= 2) {
    start_x <- path_coords[1]
    start_y <- svg_height - path_coords[2]
    end_x <- path_coords[length(path_coords)-1]
    end_y <- svg_height - path_coords[length(path_coords)]
  } else {
    start_x <- start_y <- end_x <- end_y <- NA
  }


  if (is_curved) {
    curvature <- calculate_curvature_magnitude(
      start_x, start_y, end_x, end_y,
      ctrl_x, ctrl_y, ctrl_x2, ctrl_y2
    )
  }

  data.frame(
    source = edge_nodes[1],
    target = edge_nodes[2],
    label = label,
    label_x = label_x,
    label_y = label_y,
    label_color = label_color,
    label_alpha = label_alpha,
    label_size = label_size * 1.4,
    x_start = start_x,
    y_start = start_y,
    x_end = end_x,
    y_end = end_y,
    stroke_color = stroke,
    stroke_width = stroke_width,
    arrow_fill = arrow_fill,
    arrow_config = arrow_config,
    alpha = as.numeric(arrow_fill_opacity),
    arrow_stroke = arrow_stroke,
    is_directed = is_directed,
    is_curved = is_curved,
    ctrl_x = NA,
    ctrl_y = NA,
    ctrl_x2 = NA,
    ctrl_y2 = NA,
    curvature = NA,
    stringsAsFactors = FALSE
  )
}


#' Extract node definitions from DOT code
#'
#' Parses DOT language code to extract node definitions and their attributes.
#' Identifies node declarations and extracts the attributes contained within
#' square brackets.
#'
#' @param dot_code Character string containing DOT language code
#'
#' @return A data frame with node definitions:
#'   - `node_id`: Character, node identifier/name
#'   - `attributes`: Character, raw attribute string from brackets
#'
#' @details
#' This function uses regular expressions to parse DOT syntax node definitions
#' in the format: `node_id [attribute1=value1, attribute2=value2, ...]`
#'
#' The function handles both quoted and unquoted node identifiers and extracts
#' the complete attribute string for further processing.
#'
#' @importFrom stringr str_match_all
#' @keywords internal
#' @noRd
extract_node_matches <- function(dot_code) {
  pattern <- "([\"']?)([a-zA-Z0-9_]+)\\1\\s*\\[([^\\]]+)\\]"

  matches <- str_match_all(dot_code, pattern)[[1]]

  if (nrow(matches) == 0) {
    return(data.frame(
      node_id = character(),
      attributes = character(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    node_id = matches[, 3],  # The actual node name/number
    attributes = matches[, 4],  # The attributes inside []
    stringsAsFactors = FALSE
  )
}
