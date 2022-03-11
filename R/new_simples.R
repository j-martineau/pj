#' @name new_points
#' @family new_simples
#' @title Add a points shape to a plot
#' @description A points shape is one or more x-y pairs.
#' @section Using \code{look} to make shapes appear: By itself, this function
#'   simply defines the location of x-y pairs in the assigned region for the
#'   specified shape. Various looks can be added to the shape using the
#'   \code{look} parameter or via the \code{\link{add_ci}},
#'   \code{\link{add_glyphs}}, \code{\link{add_labels}}, and
#'   \code{\link{add_marks}} functions. Sub-lists of the look argument must be
#'   named \code{'ci'}, \code{'glyph'}, \code{'label'}, and/or \code{'mark'} to
#'   indicate that a confidence interval, glyphs (single characters), labels,
#'   and/or point marks be added to the shape.
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @section Argument recycling: The argument set \code{\{x, y, order\}} is
#'   recycled.
#' @inheritParams int_new_shapes
#' @param x numeric vector defining the horizontal location of each point.
#' @param y numeric vector defining the vertical location of each point.
#' @param order character scalar \code{'x'}, \code{'y'}, or \code{'i'}
#'   indicating, repectively, ordering x-y pairs on the value of \code{x},
#'   ordering x-y pairs on the value of \code{y}, and identity (leaving the
#'   order of x-y pairs unchanged).
#' @return \code{pj} with the addition of the specified point shape.
#' @export
new_points <- function(pj, x, y, order = 'x', region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'points', region = region, name = name, look = look, mod = mod, x = x, y = y, order = order)
}

#' @name new_segments
#' @family new_simples
#' @title Add a segments shape to a plot
#' @description A segments shape is a collection of lines between two points
#'   (each) that use the same subset of locating arguments (\code{x}, \code{y},
#'   \code{x2}, \code{y2}, \code{m}, \code{s}, \code{a}, and \code{au}).
#' @section Using \code{look} to make shapes appear: By itself, this function
#'   simply defines the location of x-y pairs in the assigned region for the
#'   specified shape. Various looks can be added to the shape using the
#'   \code{look} parameter or via the \code{\link{add_ends}},
#'   \code{\link{add_glyphs}}, \code{\link{add_labels}},
#'   \code{\link{add_lines}}, and \code{\link{add_marks}} functions. Sub-lists
#'   of the look argument must be named \code{'end'}, \code{'glyph'},
#'   \code{'label'}, \code{'line'}, and/or \code{'mark'} to indicate that line
#'   end marks, fill colors, glyphs (single characters), labels, lines, and/or
#'   point marks be added to the shape.
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @section Argument recycling: The argument set \code{\{x1, y1, x2, y2, m, s,
#'   a, au\}} is recycled.
#' @inheritParams int_new_shapes
#' @param x1 numeric vector giving the horizontal start point of each segment.
#' @param y1 numeric vector giving the vertical start point of each segment.
#' @param x2 \code{NA} or a numeric vector giving the horizontal end point of
#'   each segment.
#' @param y2 \code{NA} or a numeric vectors giving the vertical end point of
#'   each segment.
#' @param m \code{NA} or a numeric vector giving the magnitude (length) of each
#'   segment.
#' @param s \code{NA} or a numeric vector giving the slope of each segment.
#' @param a \code{NA} or a numeric vector giving the angle of each segment.
#' @param au \code{NA} or a character vector giving the angle units where
#'   \code{'d'} indicates degrees, \code{'g'} indicates gradians, \code{'p'}
#'   indicates proportion of a full rotation, and \code{'r'} indicates radians.
#' @return \code{pj} with the addition of the specified line segment shape(s).
#' @export
new_segments <- function(pj, x1, y1, x2 = NA, y2 = NA, d = NA, s = NA, a = NA, au = 'r', region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'segments', region = region, name = name, look = look, mod = mod, x1 = x1, y1 = y1, x2 = x2, y2 = y2, d = d, s = s, a = a, au = au)
}

#' @name new_reflines
#' @family new_simples
#' @title Add a reference lines shape to a plot
#' @description A reference lines shape is a collection of lines going from edge
#'   to edge of a plotting region (without regard to any pad reserved around the
#'   edges of the region) that use the same subset of locating arguments
#'   (\code{x1}, \code{y1}, \code{a}, \code{i}, \code{s}, \code{x2}, \code{y2},
#'   and \code{au}). Reference lines can be defined in a variety of ways as
#'   described in the \emph{defining reference lines} section.
#' @inheritSection new_area Defining reference lines
#' @section Using \code{look} to make shapes appear: By itself, this function
#'   simply defines the location of x-y pairs in the assigned region for the
#'   specified shape. Various looks can be added to the shape using the
#'   \code{look} parameter or via the \code{\link{add_lines}} function. The
#'   sub-list of the look argument must be named \code{'line'}.
#' @section Argument recycling: The argument set \code{\{ref, x1, y1, x2, y2, s,
#'   i, a, au\}} is recycled.
#' @param ref \code{NA} or a character vector giving pre-defined reference lines
#'   where \code{'i'} indicates the identity line \code{'x'} indicates the
#'   x-axis, and \code{'y'} indicates the y-axis.
#' @param x1 \code{NA} or a numeric vector indicating either a vertical
#'   reference line at the value contained in \code{x1} (if used alone) or the
#'   horizontal location of a primary point used to define a reference line (if
#'   used with \code{y1}).
#' @param y1 \code{NA} or a numeric vector indicating either a horizontal
#'   reference line at the value contained in\code{y1} (if used alone) or the
#'   vertical location of a primary point used to define a reference line (if
#'   used with \code{x1}).
#' @param x2 \code{NA} or a numeric vector indicating the horizontal locations
#'   of secondary points to define lines in conjunction with \code{x1},
#'   \code{y1}, and \code{y2}.
#' @param y2 \code{NA} or a numeric vector indicating the vertical locations of
#'   secondary points to define lines in conjunction with \code{x1}, \code{y1},
#'   and \code{x2}.
#' @param a \code{NA} or a numeric vector indicating the angle of each reference
#'   line.
#' @param i \code{NA} or a numeric vector indicating the x-intercept of each
#'   reference line.
#' @param s \code{NA} or a numeric vector indicating the slope of each reference
#'   line.
#' @param au \code{NA} or a character vector indicating the angle units to be
#'   used where \code{'d'} indicates degrees, \code{'g'} indicates gradians,
#'   \code{'p'} indicates proportion of a full revolution, and \code{'r'}
#'   indicates radians.
#' @return \code{pj} with the addition of the specified refline shape(s).
#' @export
new_reflines <- function(pj, x1, y1, ref = 'i', x2 = NA, y2 = NA, s = NA, i = NA, a = NA, au = 'r', region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'reflines', region = region, name = name, look = look, mod = mod, x1 = x1, y1 = y1, ref = ref, x2 = x2, y2 = y2, s = s, i = i, a = a, au = au)
}
