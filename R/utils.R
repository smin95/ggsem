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
