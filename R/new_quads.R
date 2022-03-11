#' @name new_rects
#' @family new_quads
#' @title Add a rectangles shape to a plot
#' @description A rectangles shape is a collection of rectangles that use the
#'   same subset of locating parameters from among \code{x}, \code{y}, \code{w},
#'   \code{y}, \code{re}, and \code{te}. Rectangles are located in plotting
#'   regions using bounding rectangles, a concept used in defining function
#'   parameters. See the \emph{defining bounding rectangles} section for
#'   details.
#' @section Defining bounding rectangles: Bounding rectangles can be defined in
#'   a number of ways. The following shows valid combination of non-\code{NA}
#'   locating arguments: \describe{
#'     \item{\code{(x + px) + w + (y + py) + h}}{(horizontal anchor) + width +
#'           (vertical anchor) + height}
#'     \item{\code{(x + px) + w + y + te}}{(horizontal anchor) + bottom edge +
#'           top edge}
#'     \item{\code{x + re + (y + py) + h}}{left edge + right edge + (vertical
#'           anchor) + height}
#'     \item{\code{x + re + y + te}}{left edge + right edge + bottom edge + top
#'           edge}}
#' @section Bounding rectangles, \code{px}, and \code{py}: When \code{px} is not
#'   \code{NA}, it represents where the value \code{x} is located as a
#'   proportion of the distance between the left and right edge of each bounding
#'   rectangle. Likewise, when \code{py} is not \code{NA}, it represents where
#'   the value \code{y} is located as a proportion of the distance between the
#'   bottom and top edge of each bounding rectangle. For example: \itemize{
#'     \item when \code{px = 0.5} and \code{py = 0.5}, the center of the
#'           bounding rectangle is located at \code{(x, y)}.
#'     \item when \code{px = 0} and \code{py = 0}, the bottom left corner of the
#'           bounding rectangle is located at \code{(x, y)}.
#'     \item when \code{px = 1} and \code{py = 1}, the top right corner of the
#'           bounding rectangle is located at \code{(x, y)}.
#'     \item when \code{px = 1/3} and \code{py = 2/3}, the point 1/3 of the way
#'          from left to right edge and 2/3 of the way from bottom to top edge
#'          of the bounding rectangle is located at \code{(x, y)}.}
#'   \code{px} and \code{py} can be less than 0 or greater than 1, resulting in
#'   the fixed point \code{(x, y)} being outside the bounding rectangle.
#' @inheritSection int_new_shapes Using \code{look} to make shapes appear
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @section Argument recycling: The argument set \code{\{x, y, w, h, re, te, px,
#'   py\}} is recycled.
#' @inheritParams int_new_shapes
#' @param x numeric vector giving horizontal location. When \code{x} is used in
#'   conjunction with \code{px} and \code{w} these vectors define a fixed
#'   horizontal point for each shape. When \code{x} is used in conjunction with
#'   \code{re}, \code{x} defines the left edge of the bounding rectangle of each
#'   shape.
#' @param y numeric vector giving vertical location. When \code{y} is used in
#'   conjunction with \code{py} and \code{h} these vectors define a fixed
#'   vertical point for each shape. When \code{y} is used in conjunction with
#'   \code{te},\code{y} defines the bottom edge of the bounding rectangle of
#'   each shape.
#' @param w \code{NA} or a numeric vector defining the width of each bounding
#'   rectangle.
#' @param h \code{NA} or a numeric vector defining the height of each bounding
#'   rectangle.
#' @param re \code{NA} or a numeric vector defining the right edge of each
#'   bounding rectangle.
#' @param te \code{NA} or a numeric vector defining the top edge of each
#'   bounding rectangle.
#' @param px \code{NA} or a proportion vector defining the location of \code{x}
#'   as the proportion of the distance from left to right edge of bounding
#'   rectangles. Values are generally between 0 and 1, inclusive, however, when
#'   they are outside those values, fixed points \code{x} will lie outside
#'   bounding rectangles.
#' @param py \code{NA} or a proportion vector defining the location of \code{y}
#'   as the proportion of the distance from bottom to top edge of bounding
#'   rectangles. Values are generally between 0 and 1, inclusive, however, when
#'   they are outside those values, fixed points \code{y} will lie outside
#'   bounding rectangles.
#' @return \code{pj} with the addition of the defined rectangles.
#' @export
new_rects <- function(pj, x, y, w = NA, h = NA, re = NA, te = NA, px = 0.5, py = 0.5, region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'rects', region = region, name = name, look = look, mod = mod, x = x, y = y, w = w, h = h, re = re, te = te, px = px, py = py)
}

#' @name new_quads
#' @family new_quads
#' @title Add a proportional-vertex quadrilaterals shape to a plot
#' @description A proportional-vertex quadrilaterals shape is a collection of
#'   quadrilaterals defined by the proportional locations of their vertices that
#'   use the same subset of locating parameters from among \code{x}, \code{y}, \code{w}, \code{y},
#'   \code{re}, and \code{te}. Proportional-vertex quadrilaterals are created by
#'   proportionally defining vertex locations within a bounding rectangle. These
#'   shapes are located in plotting regions using bounding rectangles; a concept
#'   used in defining function parameters. See the \emph{locating bounding
#'   rectangles} section for details.
#' @section Using \code{pvx} and \code{pvy} to locate vertices: \code{pvx}
#'   indicates, as a proportion, how far across the bounding rectangle each of
#'   the four vertices of all quads are located (from left to right). \code{pvy}
#'   indicates, as a proportion, how far up the bounding rectangle each of the
#'   four vertices of all quads are located. For example: \describe{
#'     \item{pvx = c(0, 1/2, 1, 1/2), pvy = c(1/2, 1, 1/2, 0)}{Produces a
#'           \link[=new_diamonds]{diamond/rhombus}, or a kite that is symmetric
#'           both horizontally and vertically.}
#'     \item{pvx = c(0, 0, 1, 1), pvy = c(0, 1, 1, 0)}{Produces a rectangle
#'           identical to the boundary rectangle.}
#'     \item{pvx = c(0, 0.2, 0.8, 1), pvy = c(0, 1, 1, 0)}{Produces a
#'           horizontally-oriented isosceles trapezoid whose top edge is 60% the
#'           width of its bottom edge.}
#'     \item{pvx = c(0, 0.2, 1, 0.8), pvy = c(0, 1, 1, 0)}{Produces a
#'           horizontally-oriented, right-leaning parallelogram whose top and
#'           bottom edges are 60% of the width of the bounding rectangle.}
#'     \item{pvx = c(0, 1, 0, 1), pvy = c(0, 1, 1, 0)}{Produces a
#'           vertically-oriented hourglass.}
#'     \item{pvx = c(0, 0, 1, 1), pvy = c(0, 1, 0, 1)}{Produces a
#'           horizontally-oriented hourglass.}
#'     \item{pvx = c(0, 0.8, 1, 0.2), pvy = c(0.2, 1, 0.8, 0)}{Produces a quad
#'           whose first through fourth vertices are, respectively, 20% up the
#'           left edge of the bounding rectangle, 80% of the way across the top
#'           edge, 80% of the way up the right edge, and 20% of the way across
#'           the bottom edge.}}
#' @inheritSection new_rects Defining bounding rectangles
#' @inheritSection new_rects Bounding rectangles, \code{px}, and \code{py}
#' @inheritSection int_new_shapes Using \code{look} to make shapes appear
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @inheritSection new_rects Argument recycling
#' @inheritParams new_rects
#' @param pvx proportion vector of length 4 indicating the proportional x
#'   locations of the four vertices of each quad to be plotted as a proportion
#'   of the distance from the left to right edges of their bounding rectangles.
#'   All values must be between 0 and 1, inclusive. See the \emph{using
#'   \code{pvx} and \code{pvy} to locate vertices} section.
#' @param pvy proportion vector of length 4 indicating the proportional y
#'   locations of the four vertices of each quad to be plotted as a proportion
#'   of the distance from the bottom to top edges of their bounding rectangles.
#'   All values must be between 0 and 1, inclusive. \emph{using \code{pvx} and
#'   \code{pvy} to locate vertices} section.
#' @return \code{pj} with the addition of the defined proportional-vertext
#'   quadrilaterals.
#' @export
new_quads <- function(pj, pvx, pvy, x, y, re = NA, te = NA, w = NA, h = NA, px = 0.5, py = 0.5, region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'quads', region = region, name = name, look = look, mod = mod, pvx = pvx, pvy = pvy, w = w, h = h, x = x, y = y, re = re, te = te, px = px, py = py)
}

#' @name new_diamonds
#' @family new_quads
#' @title Add a diamonds/rhombuses shape to a plot
#' @description A diamonds/rhombuses shape is a collection of diamonds/rhombuses
#'   that use the same subet of locating parameters from among \code{x},
#'   \code{y}, \code{w}, \code{y}, \code{re}, and \code{te}. Diamonds/rhombuses
#'   are located in plotting regions using bounding rectangles, a concept used
#'   in defining function parameters. See the \emph{defining bounding
#'   rectangles} section for details. Diamonds/rhombuses are
#'   \link[=new_kites]{kites} with left and right vertices located halfway up
#'   the bounding rectangle and with bottom and top vertices located halfway
#'   across the bounding rectangle.
#' @inheritSection new_rects Defining bounding rectangles
#' @inheritSection new_rects Bounding rectangles, \code{px}, and \code{py}
#' @inheritSection int_new_shapes Using \code{look} to make shapes appear
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @inheritSection new_rects Argument recycling
#' @inheritParams new_rects
#' @return \code{pj} with the addition of the diamond/rhombus shape(s).
#' @export
new_diamonds <- function(pj, x, y, w = NA, h = NA, re = NA, te = NA, px = 0.5, py = 0.5, region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'diamonds', region = region, name = name, look = look, mod = mod, x = x, y = y, w = w, h = h, pre = re, te = te, x = px, py = py)
}

#' @rdname new_diamonds
#' @export
new_rhombus <- new_diamonds

#' @name new_kites
#' @family new_quads
#' @title Add a kites shape to a plot
#' @description A kites shape is a collection of kites using the same subset of locating parameters from among \code{x}, \code{y}, \code{w}, \code{y},
#'   \code{re}, and \code{te}. Kites are located in plotting regions using bounding
#'   rectangles, a concept used in defining function parameters. See the
#'   \emph{defining bounding rectangles} section for details.
#' @section Using \code{pvx} and \code{pvy} to locate vertices: \code{pvx}
#'   indicates, as a proportion, how far across the bounding rectangle the
#'   bottom and top vertices are located (from left to right). \code{pvy}
#'   indicates, as a proportion, how far up the bounding rectangle the left and
#'   right vertices are located. For example:\describe{
#'     \item{pvx = 0.5, pvy = 0.5}{Produces a
#'           \link[=new_diamonds]{diamond/rhombus}, or a kite that is symmetric
#'           both horizontally and vertically.}
#'     \item{pvx = 0.5, pvy = 0.8}{Produces a kite that is symmetric
#'           horizontally, but whose left and right vertices are located 80% of
#'           the way up the bounding rectangle.}
#'     \item{pvx = 0.8, pvy = 0.5}{Produces a kite that is symmetric vertically,
#'           but whose bottom and top vertices are located 80% of the way across
#'           the bounding rectangle (from left to right).}
#'     \item{pvx = 0.8, pvy = 0.2}{Produces a kite that is asymmetric both
#'           vertically and horizontally, with left and right vertices located
#'           20% of the way up the bounding rectangle and bottom and top
#'           vertices located 80% of the way across the bounding rectangle (from
#'           left to right).}
#'     \item{pvx = 0.5, pvy = 0}{Produces an upward pointing isosceles
#'           triangle.}}
#' @inheritSection new_rects Defining bounding rectangles
#' @inheritSection new_rects Bounding rectangles, \code{px}, and \code{py}
#' @inheritSection int_new_shapes Using \code{look} to make shapes appear
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @inheritSection new_rects Argument recycling
#' @param pvx proportion scalar indicating how far across the bounding rectangle
#'   the bottom and top kite vertices are located. See the \emph{using
#'   \code{pvx} and \code{pvy} to locate vertices} section.. Value must be
#'   between 0 and 1 inclusive.
#' @param pvy proportion scalar indicating how far up the bounding rectangle the
#'   left and right kite vertices are located. See the \emph{using \code{pvx}
#'   and \code{pvy} to locate vertices} section. Value must be between 0 and 1,
#'   inclusive.
#' @inheritParams new_rects
#' @return \code{pj} with the addition of the defined kites.
#' @export
new_kites <- function(pj, pvx, pvy, x, y, w = NA, h = NA, re = NA, te = NA, px = 0.5, py = 0.5, region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'kites', region = region, name = name, look = look, mod = mod, pvx = pvx, pvy = pvy, x = x, y = y, w = w, h = h, re = re, te = te, px = px, py = py)
}
