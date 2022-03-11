#' @name new_area
#' @family new_polycurve
#' @title Add an area shape to a plot
#' @description An area shape is a polygon defined as the area between a curve
#'   (a set of x-y pairs) and a reference line. The default reference line is
#'   the x-axis.
#' @section Defining reference lines: Valid combinations of non-\code{NA}
#'   locating arguments are given below. \describe{
#'     \item{\code{ref}}{Pre-defined reference line}
#'     \item{\code{x1}}{Vertical line at \code{x1}.}
#'     \item{\code{y1}}{Horizontal line at \code{y1}.}
#'     \item{\code{x1 + y1 + x2 + y2}}{Point-point definition (line through
#'           \code{(x1, y1)} and \code{(x2, y2)}).}
#'     \item{\code{x1 + y1 + a}}{Point-angle definition (line through
#'           \code{(x1, y1)} at angle \code{a}).}
#'     \item{\code{x1 + y1 + i}}{Point-intercept definition (line through
#'           intercept and \code{(x1, y1)}).}
#'     \item{\code{x1 + y1 + s}}{Point-slope definition (line through
#'           \code{(x1, y1)} with slope \code{s}).}
#'     \item{\code{a + i}}{Angle-intercept definition (line through intercept
#'           at angle \code{a}).}
#'     \item{\code{s + i}}{Slope-intercept definition (line through intercept
#'           with slope \code{s}.}}
#' @inheritSection int_new_shapes Using \code{look} to make shapes appear
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @section Argument recycling: The argument set \code{\{x, y\}} is recycled.
#' @inheritParams int_new_shapes
#' @param x numeric vector defining the horizontal locations of points on a
#'   curve. The longer of \code{x} and \code{y} must have at least 2 elements.
#' @param y numeric vector defining the vertical locations of points on a curve.
#'   The longer of \code{x} and \code{y} must have at least 2 elements.
#' @param ref \code{NA} or a character scalar giving a pre-defined reference
#'   line. \code{'i'} indicates the identity line \code{'x'} indicates the
#'   x-axis, and \code{'y'} indicates the y-axis.
#' @param dir character scalar identifying the directional orientation of the
#'   reference line, where \code{'v'} indicates a vertically-oriented area plot
#'   (the vertical area between the x-y curve and the reference line) and
#'   \code{'h'} indicates a horizontally-oriented area plot.
#' @param x1 \code{NA} or a numeric scalar indicating either a vertical
#'   reference line at the value contained in \code{x1} (if used alone) or the
#'   horizontal location of a primary point used to define a reference line (if
#'   used with \code{y1}).
#' @param y1 \code{NA} or a numeric scalar indicating either a horizontal
#'   reference line at the value contained in\code{y1} (if used alone) or the
#'   vertical location of a primary point used to define a reference line (if
#'   used with \code{x1}).
#' @param a \code{NA} or a numeric scalar indicating the angle of the reference
#'   line.
#' @param i \code{NA} or a numeric scalar indicating the intercept of the
#'   reference line.
#' @param s \code{NA} or a numeric scalar indicating the slope of the reference
#'   line.
#' @param x2 \code{NA} or a numeric scalar indicating the horizontal location of
#'   the secondary points to define a reference line (in conjunction with
#'   \code{x1}, \code{y1}, and \code{y2}).
#' @param y2 \code{NA} or a numeric scalar indicating the vertical location of
#'   the secondary points to define a reference line (in conjunction with
#'   \code{x1}, \code{y1}, and \code{x2}).
#' @param au \code{NA} or a character vector indicating the angle units to be
#'   used where \code{'d'} indicates degrees, \code{'g'} indicates gradians,
#'   \code{'p'} indicates proportion of a full revolution, and \code{'r'}
#'   indicates radians.
#' @return \code{pj} with the addition of the specified area shape.
#' @export
new_area <- function(pj, x, y, ref = 'x', dir = 'v', x1 = NA, y1 = NA, x2 = NA, y2 = NA, s = NA, i = NA, a = NA, au = 'r', region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'area', region = region, name = name, look = look, mod = mod, x = x, y = y, ref = ref, dir = dir, x1 = x1, y1 = y1, x2 = x2, y2 = y2, s = s, i = i, a = a, au = au)
}

#' @name new_ribbon
#' @family new_polycurve
#' @title Add a ribbon shape to a plot
#' @description A ribbon shape is a polygon defined as the area between
#'   two curves (two sets of x-y pairs).
#' @inheritSection int_new_shapes Using \code{look} to make shapes appear
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @section Argument recycling: The argument set \code{\{x1, y1\}} is recycled.
#'   The argument set \code{\{x2, y2}\} is separately recycled.
#' @param dir character scalar identifying the directional orientation of the
#'   ribbon. \code{'v'} indicates a vertically-oriented ribbon, or the vertical
#'   area between the two curves. \code{'h'} indicates a horizontally-oriented
#'   ribbon.
#' @param order character scalar \code{'x'}, \code{'y'}, or \code{'i'}
#'   indicating ordering x-y pairs on the value of \code{x} and \code{x2},
#'   ordering x-y pairs on the value of \code{y} and \code{y2}, and identity, or
#'   leaving the order of x-y pairs unchanged.
#' @param x1 numeric vector giving the horizontal location of points on the
#'   first curve. The longer of \code{x} or \code{y} must have at least 2
#'   values.
#' @param y1 numeric vector giving the vertical location of points on the first
#'   curve. The longer of \code{x} or \code{y} must have at least 2 values.
#' @param x2 numeric vector giving the horizontal location of points on the
#'   second curve. The longer of \code{x2} or \code{y2} must have at least 3
#'   values.
#' @param y2 numeric vector giving the vertical location of points on the second
#'   curve. The longer of \code{x2} or \code{y2} must have at least 3 values.
#' @inheritParams int_new_shapes
#' @return \code{pj} with the addition of the defined ribbon.
#' @export
new_ribbon <- function(pj, order = 'x', dir = 'v', x1 = NA, y1 = NA, x2 = NA, y2 = NA, region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'ribbon', region = region, name = name, look = look, mod = mod, order = order, dir = dir, x1 = x1, y1 = y1, x2 = x2, y2 = y2)
}
