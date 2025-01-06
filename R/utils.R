#' Interpolate points along a straight line for gradient
#' Internal function
#' @param x_start X coordinate where line begins
#' @param y_start Y coordinate where line begins
#' @param x_end X coordinate where line ends
#' @param y_end Y coordinate where line ends
#' @param n Number of points to be used for interpolation
#' @return Data frame
#' @keywords internal
#' @noRd
interpolate_points <- function(x_start, y_start, x_end, y_end, n = 500) {
  # n = 100 in the shiny app to make it faster
  t <- seq(0, 1, length.out = n)
  x <- (1 - t) * x_start + t * x_end
  y <- (1 - t) * y_start + t * y_end
  data.frame(x = x, y = y)
}


#' Create curved lines (using Bezier approximation)
#' Internal function
#' @param x_start X coordinate where the curved line begins
#' @param y_start Y coordinate where the curved line begins
#' @param x_end X coordinate where the curved line ends
#' @param y_end Y coordinate where the curved line ends
#' @param ctrl_x X coordinate where curvature happens
#' @param ctrl_y Y coordinate where curvature happens
#' @param n_points Number of points in the curved line  (default 100)
#' @return Data frame
#' @keywords internal
#' @noRd
create_bezier_curve <- function(x_start, y_start, x_end, y_end, ctrl_x, ctrl_y, n_points = 100) {
  t <- seq(0, 1, length.out = n_points)

  bezier_x <- (1 - t)^2 * x_start + 2 * (1 - t) * t * ctrl_x + t^2 * x_end
  bezier_y <- (1 - t)^2 * y_start + 2 * (1 - t) * t * ctrl_y + t^2 * y_end

  data.frame(x = bezier_x, y = bezier_y)
}

#' Check if a hex code is valid
#'
#' @param x Hexcode of a color in string
#' @return A string output in hex code.
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

#' Rotate a point element's orientation (degrees)
#'
#' @param x x coordinate of the point to be rotated (numeric vector)
#' @param y y coordinate of the point to be rotated (numeric vector)
#' @param angle The angle (in degrees) by which to rotate the points. Positive angles correspond to counterclockwise rotation.
#' @param cx  The x coordinate of the center of rotation (default is the origin (0, 0)).
#' @param cy The y coordinate of the center of rotation (default is the origin (0, 0)).
#'
#' @return A list containing the rotated coordinates
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
#' @param x A string specifying the line style.
#' @return A valid line style (\code{"solid"}, \code{"dotted"}, or \code{"dashed"}).
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
#' @param x A string specifying the font face.
#' @return A valid font face (\code{"plain"}, \code{"bold"}, or \code{"italic"}).
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
#' @param x A string specifying the font family.
#' @return A valid font family (\code{"sans"}, \code{"mono"}, or \code{"serif"}).
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
#' @param x A string specifying the type.
#' @return A valid type (\code{"Straight Line"}, \code{"Straight Arrow"}, \code{"Curved Line"}, or \code{"Curved Arrow"}).
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
#' @param x A numeric value specifying the gradient position.
#' @return A valid gradient position (numeric, between 0 and 1). Defaults to \code{0.5}.
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

#' Validate alpha value
#'
#' @param x A numeric value specifying transparency.
#' @return A valid alpha value (numeric, between 0 and 1). Defaults to \code{1}.
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

#' Validate shape
#'
#' @param x A string specifying the shape.
#' @return A valid shape (\code{"circle"}, \code{"square"}, \code{"oval"}, \code{"triangle"}, \code{"rectangle"}, or \code{"diamond"}).
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
#' @param x A value indicating a logical input.
#' @return A logical value (\code{TRUE} or \code{FALSE}). Defaults to \code{FALSE}.
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
