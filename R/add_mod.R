#' @name mod_dilate
#' @family add_mod
#' @title Add a dilation modification to a shape
#' @description A dilation modification squeezes and/or stretches a shape in the
#'   x and y directions. Dilations are not valid for refline shapes.
#' @section Argument recycling: The argument set \code{{x, y, mx, my, xu, yu}}
#'   is recycled with the number of subshapes defined for a given shape. For
#'   point, area, and ribbon shapes, all arguments in the set must be scalar.
#'   For segment shapes, arguments in the set must be recyclable with the number
#'   of segments defined by that shape. For shapes that define polygons,
#'   arguments in the set must be recyclable with the number of polygons defined
#'   by the shape.
#' @inheritParams int_add_mod
#' @param x integer vector identifying horizontal fixed points for anchoring
#'   shapes in dilation.
#' @param x integer vector identifying vertical fixed points for anchoring
#'   shapes in dilation.
#' @param mx positive numeric vector giving the value by which to multiply shape
#'   width (x direction).
#' @param my positive numeric vector giving the value by which to multiply shape
#'   height (y direction).
#' @return \code{pj} with the dilation modification applied to the specified
#'   shape.
#' @export
mod_dilate <- function(pj, x, y, mx, my, shape = '.', name = '.') {
  shape.type <- int_get_type(pj, shape)
  region <- int_get_parent(pj, shape)
  int_add_mod(pj = pj, region.id = region, shape.id = shape, shape = shape.type, type = 'ci', name = name, x = x, y = y, mx = mx, my = my)
}

#' @name mod_reflect
#' @family add_mod
#' @title Add a reflection modification to a shape
#' @description A reflection modification reflects a shape around a reference
#'   line. Reflections are not valid for refline shapes.
#' @section The ref argument:
#'   \itemize{
#'     \item \code{'i'} indicates reflection across the identity line,
#'     \item \code{'x'} indicates reflection across the x-axis,
#'     \item \code{'y'} indicates reflection across the y-axis,
#'     \item \code{'l'} indicates reflection across the vertical line through
#'                      the left edge of a shape's bounding rectangle,
#'     \item \code{'r'} indicates reflection across the vertical line through
#'                      the right edge of a shape's bounding rectangle,
#'     \item \code{'b'} indicates reflection across the horizontal line through
#'                      the bottom edge of a shape's bounding rectangle,
#'     \item \code{'t'} indicates reflection across the horizontal line through
#'                      the top edge of a shape's bounding rectangle,
#'     \item \code{'h'} indicates horizontal reflection across the vertical
#'                      midline of the shape's bounding rectangle.
#'     \item \code{'v'} indicates vertical reflection across the horizontal
#'                      midline of the shape's bounding rectangle.
#'     \item \code{'d'} indicates reflection across the line through th
#'                      downward diagonal between the top-left and bottom-right
#'                      corners of a shape's bounding rectangle, and
#'     \item \code{'u'} indicates reflection across the line through the upward
#'                      diagonal between to bottom-left and top-right corners of
#'                      a shape's bounding rectangle.
#'   }
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
#'     \item{\code{a + i}}{Angle-intercept definition (line through intercept at
#'           angle \code{a}).}
#'     \item{\code{s + i}}{Slope-intercept definition (line through intercept
#'           with slope \code{s}).}}
#' @section Argument recycling: The argument set \code{{x, y, ref, s, i, x2, y2,
#'   a, au, xu, yu}} is recycled with the number of subshapes defined for a
#'   given shape. For point, area, and ribbon shapes, all arguments in the set
#'   must be scalar. For segment shapes, arguments in the set must be recyclable
#'   with the number of segments defined by that shape. For shapes that define
#'   polygons, arguments in the set must be recyclable with the number of
#'   polygons defined by the shape.
#' @inheritParams int_add_mod
#' @param ref \code{NA} or character vector identifying pre-defined reference
#'   lines. See the \emph{the ref argument} section.
#' @param x1 \code{NA} or numeric vectors indicating either a vertical reference
#'   line at the value contained in \code{x1} (if used alone) or the horizontal
#'   location of a primary point used to define a reference line (if used with
#'   \code{y1}).
#' @param y1 \code{NA} or numeric vectors indicating either a horizontal
#'   reference line at the value contained in\code{y1} (if used alone) or the
#'   vertical location of a primary point used to define a reference line (if
#'   used with \code{x1}).
#' @param a \code{NA} or numeric vector indicating the angle of the reference
#'   line.
#' @param s \code{NA} or numeric vector indicating the slope of the reference
#'   line.
#' @param i \code{NA} or numeric vector indicating the x-intercept of the
#'   reference line.
#' @param x2 \code{NA} or numeric vector indicating the horizontal locations of
#'   the secondary points to define lines in conjunction with \code{x1},
#'   \code{y1}, and \code{y2}.
#' @param x2 \code{NA} or numeric vector indicating the vertical locations of
#'   the secondary points to define lines in conjunction with \code{x1},
#'   \code{y1}, and \code{y2}.
#' @param au \code{NA} or character vector indicating the angle units to be used
#'   where \code{'d'} indicates degrees, \code{'g'} indicates gradians,
#'   \code{'p'} indicates proportion of a full revolution, and \code{'r'}
#'   indicates radians.
#' @return \code{pj} with the reflection modification applied to the specified
#'   shape.
#' @export
mod_reflect <- function(pj, ref = 'h', x1 = NA, y1 = NA, x2 = NA, y2 = NA, s = NA, i = NA, a = NA, au = 'r', shape = '.', name = '.') {
  shape.type <- int_get_type(pj, shape)
  region <- int_get_parent(pj, shape)
  int_add_mod(pj = pj, region.id = region, shape.id = shape, shape = shape.type, type = 'ends', name = name, ref = ref, x1 = x1, y1 = y1, x2 = x2, y2 = y2, s = s, i = i, a = a, au = au)
}

#' @name mod_rotate
#' @family add_mod
#' @title Add a rotation modification to a shape
#' @description A rotation modification rotates a shape around a reference
#'   point. Rotations are not valid for refline shapes.
#' @section The ref argument:
#'   \itemize{
#'     \item \code{'c'} indicates center point of a shape's bounding rectangle.
#'     \item \code{'o'} indicates the origin (0, 0).
#'     \item \code{'bl'} indicates the bottom-left corner of a shape's bounding
#'           rectangle.
#'     \item \code{'br'} indicates the bottom-right corner of a shape's bounding
#'           rectangle.
#'     \item \code{'tl'} indicates the top-left corner of a shape's bounding
#'           rectangle.
#'     \item \code{'tr'} indicates the top-right corner of a shape's bounding
#'           rectangle.
#'     \item \code{'lm'} indicates the mid-point of the left edge of a shape's
#'           bounding rectangle.
#'     \item \code{'rm'} indicates the mid-point of the right edge of a shape's
#'           bounding rectangle.
#'     \item \code{'bm'} indicates the mid-point of the bottom edge of a shape's
#'           bounding rectangle.
#'     \item \code{'tm'} indicates the mid-point of the top edge of a shape's
#'           bounding rectangle.
#'   }
#' @section Argument recycling: The argument set \code{{ref, x, y, a, au, xu,
#'   yu}} is recycled with the number of subshapes defined for a given shape.
#'   For point, area, and ribbon shapes, all arguments in the set must be
#'   scalar. For segment shapes, arguments in the set must be recyclable with
#'   the number of segments defined by that shape. For shapes that define
#'   polygons, arguments in the set must be recyclable with the number of
#'   polygons defined by the shape.
#' @inheritParams int_add_mod
#' @param ref \code{NA} or a character vector identifying pre-defined
#'   reference points. See the \emph{the ref argument} section.
#' @param x \code{NA} or a numeric vector indicating the horizontal location of
#'   a point around which to rotate shapes.
#' @param y \code{NA} or a numeric vector indicating the vertical location of a
#'   point around which to rotate shapes.
#' @param a \code{NA} or a numeric vector indicating the angle of
#'   rotation.
#' @param au \code{NA} or a character vector indicating the angle
#'   units to be used where \code{'d'} indicates degrees, \code{'g'} indicates
#'   gradians, \code{'p'} indicates proportion of a full revolution, and
#'   \code{'r'} indicates radians.
#' @return \code{pj} with the rotation modification applied to the specified
#'   shape.
#' @export
mod_rotate <- function(pj, ref = 'c', x = NA, y = NA, a = NA, au = 'r', shape = '.', name = '.') {
  shape.type <- int_get_type(pj, shape)
  region <- int_get_parent(pj, shape)
  int_add_mod(pj = pj, region.id = region, shape.id = shape, shape = shape.type, type = 'fills', name = name, ref = ref, x = x, y = y, a = a, au = au)
}

#' @name mod_translate
#' @family add_mod
#' @title Add a translation modification to a shape
#' @description A translation modification moves a shape in the x and/or y
#'   directions.
#' @section Argument recycling: The argument set \code{{x, y, xu, yu}} is
#'   recycled with the number of subshapes defined for a given shape. For point,
#'   area, and ribbon shapes, all arguments in the set must be scalar. For
#'   segment shapes, arguments in the set must be recyclable with the number of
#'   segments defined by that shape. For shapes that define polygons, arguments
#'   in the set must be recyclable with the number of polygons defined by the
#'   shape.
#' @inheritParams int_add_mod
#' @param dx numeric vector indicating the distances for horizontal translation.
#' @param dy numeric vector indicating the distances for vertical translation.
#' @return \code{pj} with the translation modification applied to the specified
#'   shape.
#' @export
mod_translate <- function(pj, dx, dy, shape = '.', name = '.') {
  shape.type <- int_get_type(pj, shape)
  region <- int_get_parent(pj, shape)
  int_add_mod(pj = pj, region.id = region, shape.id = shape, shape = shape.type, type = 'glyphs', name = name, dx = dx, dy = dy)
}
