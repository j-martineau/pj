#' @name new_circles
#' @family new_rounds
#' @title Add a circles shape to a plot
#' @description A circles shape is one or more circles that use the same subset
#'   of locating arguments from among \code{x}, \code{y}, \code{n}, \code{px},
#'   \code{py}, \code{r}, \code{re}, and \code{te}. Circles are located in
#'   plotting regions using bounding rectangles, a concept used in defining
#'   function parameters. See the \emph{defining bounding rectangles} section
#'   for details.
#' @section Defining bounding rectangles: Bounding rectangles can be located in
#'   a number of ways. The following shows valid combination of non-\code{NA}
#'   locating arguments: \describe{
#'     \item{\code{(x + px) + (y + py) + r}}{(horizontal anchor) + (vertical
#'           anchor) + radius}
#'     \item{\code{(x + px) + y + te}}{(horizontal anchor) + bottom edge + top
#'           edge}
#'     \item{\code{x + re + (y + py)}}{left edge + right edge + (vertical
#'           anchor)}}
#' @inheritSection new_rects Bounding rectangles, \code{px}, and \code{py}
#' @inheritSection int_new_shapes Using \code{look} to make shapes appear
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @section Argument recycling: The argument set \code{\{x, y, r, re, te, px,
#'   py, n\}} is recycled.
#' @inheritParams int_new_shapes
#' @param x numeric vector defining the horizontal anchors or left edges of each
#'   bounding rectangle.
#' @param y numeric vector defining the vertical anchors or bottom edges of each
#'   bounding rectangle.
#' @param r \code{NA} or a positive numeric vector giving the radius of each
#'   circle.
#' @param re \code{NA} or a numeric vector defining the right edge of each
#'   bounding rectangle, respectively.
#' @param te \code{NA} or a numeric vector defining the top edge of each
#'   bounding rectangle, respectively.
#' @param px \code{NA} or a numeric vector defining, proportionally, where from
#'   left to right edge of each bounding rectangle the corresponding \code{x} is
#'   located. Values are generally proportions (i.e., between 0 and 1,
#'   inclusive).
#' @param py \code{NA} or a numeric vector defining, proportionally, where from
#'   bottom to top edge of each bounding rectangle the corresponding \code{y} is
#'   located. Values are generally proportions (i.e., between 0 and 1,
#'   inclusive).
#' @param n positive whole-number vector giving the number of vertices to
#'   calculate for plotting each shape. All values must be between 3 and 1000,
#'   inclusive.
#' @return \code{pj} with the addition of the defined circles.
#' @export
new_circles <- function(pj, x, y, r = NA, re = NA, te = NA, px = 0.5, py = 0.5, n = 100, region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'circles', region = region, name = name, look = look, mod = mod, x = x, y = y, r = r, re = re, te = te, px = px, py = py, n = n)
}

#' @name new_ellipses
#' @family new_rounds
#' @title Add an ellipses shape to a plot
#' @description An ellipses shape is a collection of ellipses that uses the same
#'   subset of locating arguments from among \code{x}, \code{y}, \code{n},
#'   \code{px}, \code{py}, \code{rx}, \code{ry}, \code{re}, and \code{te}.
#'   Ellipses are located in plotting regions using bounding rectangles; a
#'   concept used in defining function parameters. See the \emph{defining
#'   bounding rectangles} section for details.
#' @section Defining bounding rectangles: Bounding rectangles can be located in
#'   a number of ways. The following shows valid combination of non-\code{NA}
#'   locating arguments: \describe{
#'     \item{\code{(x + px) rx + (y + py) + ry}}{(horizontal anchor) + x-radius
#'           + (vertical anchor) + y-radius}
#'     \item{\code{(x + px) + rx + y + te}}{(horizontal anchor) + x-radius +
#'           bottom edge + top edge}
#'     \item{\code{x + re + (y + py) + ry}}{left edge + right edge + (vertical
#'           anchor) + y-radius}
#'     \item{\code{x + re + y + te}}{left edge + right edge + bottom edge + top
#'           edge}}
#' @inheritSection new_rects Bounding rectangles, \code{px}, and \code{py}
#' @inheritSection int_new_shapes Using \code{look} to make shapes appear
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @section Argument recycling: The argument set \code{\{x, y, rx, ry, re, te,
#'   px, py, n\}} is recycled.
#' @param rx \code{NA} or a positive numeric vector giving the x-radius of each
#'   ellipse.
#' @param ry \code{NA} or a positive numeric vector giving the y-radius of each
#'   ellipse.
#' @param n positive whole-number vector giving the number of vertices to
#'   calculate for plotting each shape. All values must be between 3 and 1000,
#'   inclusive.
#' @inheritParams new_rects
#' @return \code{pj} with the addition of the defined ellipses.
#' @export
new_ellipses <- function(pj, x, y, rx = NA, ry = NA, re = NA, te = NA, px = 0.5, py = 0.5, n = 100, region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'ellipses', region = region, name = name, look = look, mod = mod, mod, x = x, y = y, rx = rx, ry = ry, re = re, te = te, px = px, py = py, n = n)
}

#' @name new_rounds
#' @family new_rounds
#' @title Add a proportional-vertex rounds shape to a plot
#' @description A proportional-vertex rounds shape is a collection of rounded
#'   shapes defined by proportional location of bottom, top, left, and right
#'   vertex locations that use the same subset of locating arguments from among
#'   \code{x}, \code{y}, \code{n}, \code{px}, \code{py}, \code{rx}, \code{ry},
#'   \code{re}, \code{te}, and \code{pv}. Pvrounds are located in plotting
#'   regions using bounding rectangles, a concept used in defining function
#'   parameters. See the \emph{defining bounding rectangles} section for
#'   details. They are defined by specifying, proportionally, using \code{pv},
#'   where from left to right edge the bottom-most and top-most vertices lie,
#'   and from bottom to top edge where left-most and right-most vertices lie.
#' @section Defining bounding rectangles: Bounding rectangles can be located in
#'   a number of ways. The following shows valid combination of non-\code{NA}
#'   locating arguments: \describe{
#'     \item{\code{(x + px) + w + (y + py) + h}}{(horizontal anchor) + width +
#'           (vertical anchor) + height}
#'     \item{\code{(x + px) + w + y + te}}{(horizontal anchor) + height + bottom
#'           edge + top edge}
#'     \item{\code{x + re + (y + py) + h}}{left edge + right edge + (vertical
#'           anchor) + height}
#'     \item{\code{x + re + y + te}}{left edge + right edge + bottom edge + top
#'           edge}}
#' @inheritSection new_rects Bounding rectangles, \code{px}, and \code{py}
#' @inheritSection int_new_shapes Using \code{look} to make shapes appear
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @section Argument recycling: The argument set \code{\{x, y, w, h, re, te, px,
#'   py, n\}} is recycled.
#' @param pv proportion vector of length 4 giving, in order, the vertical
#'   location of the left-edge vertex as a proportion of the distance from
#'   bottom edge to top edge, the bottom-edge vertex as a proportion of the
#'   distance from left edge to right edge, the right-edge vertex as a
#'   proportion of the distance from bottom to top edge, and the top-edge vertex
#'   as a proportion of the distance from left edge to right edge.
#' @param n positive whole-number vector giving the number of vertices to
#'   calculate for plotting each shape. All values must be between 3 and 1000,
#'   inclusive.
#' @inheritParams new_rects
#' @return \code{pj} with the addition of the defined proportional-vertex
#'   rounds.
#' @export
new_rounds <- function(pj, pv, x, y, w = NA, h = NA, re = NA, te = NA, px = 0.5, py = 0.5, n = 100, region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'rounds', region = region, name = name, look = look, mod = mod, mod, pv = pv, x = x, y = y, w = w, h = h, re = re, te = te, px = px, py = py, yn = n)
}
