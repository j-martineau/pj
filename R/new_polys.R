#' @name new_free
#' @family new_polygons
#' @title Add a free polygon shape to a plot
#' @description A free polygon shape is a polygon of arbitrary shape defined by
#'   any sequence of x-y pairs where the last x-y pair is connected to the first
#'   x-y pair to close the polygon.
#' @inheritSection int_new_shapes Using \code{look} to make shapes appear
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @section Argument recycling: The argument set \code{\{x, y\}} is recycled.
#' @inheritParams int_new_shapes
#' @param x numeric vector defining the horizontal location of each vertex. The
#'   longer of \code{x} and \code{y} must be of length 3 or greater.
#' @param y numeric vector defining the vertical location of each vertex. The
#'   longer of \code{x} and \code{y} must be of length 3 or greater.
#' @return \code{pj} with the addition of the specified free polygon shape.
#' @export
new_free <- function(pj, x, y, region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'free', region = region, name = name, look = look, mod = mod, x = x, y = y)
}

#' @name new_polys
#' @family new_polygons
#' @title Add one or more proportional-vertex based polygon shapes
#' @description A proportional-vertex based polygon shape is created by
#'   proportionally defining an arbitrary number of vertex locations within a
#'   bounding rectangle. The shapes  are located in plotting regions using
#'   bounding rectangles; a concept used in defining function parameters. See
#'   the \emph{defining bounding rectangles} section for details.
#' @section \code{pvx}, \code{pvy}, and vertex locations: \code{pvx} indicates,
#'   as a proportion, how far across the bounding rectangle each of the vertices
#'   of all polygons are located (from left to right). \code{pvy} indicates, as
#'   a proportion, how far up the bounding rectangle each of the vertices of all
#'   polygons are located. For example: \describe{
#'     \item{pvx = c(0, 1/2, 1), pvy = c(0, 1, 0)}{Produces an upward
#'           \link[=new_isosceles]{isosceles triangle}.}
#'     \item{pvx = c(0, 1/2, 1, 1/2), pvy = c(1/2, 1, 1/2, 0)}{Produces a
#'           \link[=new_diamonds]{diamond/rhombus}, or a kite that is symmetric
#'           both horizontally and vertically.}
#'     \item{pvx = c(0, 0, 1, 1), pvy = c(0, 1, 1, 0)}{Produces a rectangle
#'           identical to the boundary rectangle.}
#'     \item{pvx = c(0, 0, 1/2, 1, 1), pvy = c(0, 1, 1, 1/2, 0)}{Produces a
#'           five-sided polygon that is a modified rectangle where the top right
#'           corner is cut off by a line running from halfway across the top
#'           edge of the bounding rectangle to halfway up the right edge of the
#'           bounding rectangle.}}
#' @inheritSection new_rects Defining bounding rectangles
#' @inheritSection new_rects Bounding rectangles, \code{px}, and \code{py}
#' @inheritSection int_new_shapes Using \code{look} to make shapes appear
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @inheritSection new_rects Argument recycling
#' @inheritParams new_rects
#' @param pvx proportion vector of length 3 or greater indicating the
#'   proportional x locations of the vertices of all polygons to be plotted as a
#'   proportion of the distance from left to right edge of bounding rectangles.
#'   All polygons must have the same number of vertices, and these proportions
#'   will be the same for all polygons. All values must be between 0 and 1,
#'   inclusive. See the \emph{the pvx and pvy arguments} section. Length must be
#'   equal to the length of \code{pvx}.
#' @param pvy proportion vector of length 3 or greater indicating the
#'   proportional y locations of the vertices of all polygons to be plotted as a
#'   proportion of the distance from bottom to top edge of bounding rectangles.
#'   All polygons must have the same number of vertices, and these proportions
#'   will be the same for all polygons. All values must be between 0 and 1,
#'   inclusive. See the \emph{the pvx and pvy arguments} section. Length must be
#'   equal to the length of \code{pvy}.
#' @return \code{pj} with the addition of the defined proportional-vertex
#'   polygons
#' @export
new_polys <- function(pj, pvx, pvy, x, y, w = NA, h = NA, re = NA, te = NA, px = 0.5, py = 0.5, region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'polys', region = region, name = name, look = look, mod = mod, pvx = pvx, pvy = pvy, x = x, y = y, w = w, h = h, re = re, te = te, px = px, py = py)
}

#' @name new_regulars
#' @family new_polygons
#' @title Add a regular polygons shape to a plot
#' @description A regular polygons shape is a collection of regular polygons
#'   using the ame subset of locating arguments from among \code{x}, \code{y},
#'   \code{n}, \code{loc}, \code{px}, \code{py}, \code{s}, \code{x2}, and
#'   \code{y2}. Regular polygons are located in plotting regions using bounding
#'   rectangles; a concept used in defining function parameters. See the
#'   \emph{defining bounding rectangles} section for details.
#' @section Defining bounding rectangles: Bounding rectangles can be located in
#'   a number of ways. The following shows valid combination of non-\code{NA}
#'   locating arguments for regular polygons: \describe{
#'     \item{\code{(x + px) + (y + py) + e}}{(horizontal anchor) + (vertical
#'           anchor) + edge length}
#'     \item{\code{(x + px) + y + te}}{(horizontal anchor) + bottom edge + top
#'           edge}
#'     \item{\code{x + re + (y + px)}}{left edge + right edge + (vertical
#'           anchor)}}
#'   In addition, regular polygons can be squeezed or stretched horizontally and
#'   vertically by specifying the location of all four edges of the bounding
#'   rectangle as follows: \describe{
#'     \item{\code{x + re + y + te)}}{left edge + right edge +  bottom edge +
#'           top edge}}
#' @inheritSection new_rects Bounding rectangles, \code{px}, and \code{py}
#' @inheritSection int_new_shapes Using \code{look} to make shapes appear
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @section Argument recycling: The argument set \code{\{n, x, y, e, re, te, px,
#'   py, loc\}} is recycled.
#' @inheritParams new_rects
#' @param n integer vector with values between 3 and 1000, inclusive, indicating
#'   the numbers of sides/vertices of each regular polygon.
#' @param loc character vector indicating where the base of each regular polygon
#'   is located. \code{'l'}, \code{'r'}, \code{'b'}, and \code{'t'} indicate
#'   left, right, bottom, and top edges of the bounding rectangle, respectively.
#' @param e \code{NA} or a positive numeric vector indicating edge length of
#'   each polygon.
#' @return \code{pj} with the addition of the defined regular polygons.
#' @export
new_regulars <- function(pj, x, y, n, r = NA, e = NA, a = NA, px = 0.5, py = 0.5, loc = 'b', region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'regulars', region = region, name = name, look = look, mod = mod, x = x, y = y, n = n, r = r, e = e, a = a, px = px, py = py, loc = loc)
}

#' @name new_stars
#' @family new_polygons
#' @title Add a stars shape to a plot
#' @description A star shape is a collection of star polygons using the same
#'   subset of locating parameters from among \code{x}, \code{y}, \code{n},
#'   \code{loc}, \code{px}, \code{py}, \code{re}, and \code{te}. Star polygons
#'   are located in plotting regions using bounding rectangles; a concept used
#'   in defining function parameters. See the \emph{defining bounding
#'   rectangles} section for details.
#' @section Defining bounding rectangles: Bounding rectangles can be located in
#'   a number of ways. The following shows valid combination of non-\code{NA}
#'   locating arguments for star polygons: \describe{
#'     \item{\code{(x + px) + (y + py) + s}}{(horizontal anchor) + (vertical
#'           anchor) + side length}
#'     \item{\code{(x + px) + y + te}}{(horizontal anchor) + base length +
#'           bottom edge + top edge}
#'     \item{\code{x + re + (y + px)}}{left edge + right edge + (vertical
#'           anchor)}}
#'   In addition, star polygons can be squeezed or stretched horizontally and
#'   vertically by specifying the location of all four edges of the bounding
#'   rectangle as follows: \describe{
#'     \item{\code{x + re + y + te)}}{left edge + right edge +  bottom edge +
#'           top edge}}
#' @inheritSection new_rects Bounding rectangles, \code{px}, and \code{py}
#' @inheritSection int_new_shapes Using \code{look} to make shapes appear
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @section Argument recycling: The argument set \code{\{n, x, y, w, h, re, te,
#'   px, py, loc\}} is recycled.
#' @inheritParams new_rects
#' @param n integer vector with values between 3 and 1000, inclusive, indicating
#'   the numbers of sides/vertices of each star polygon.
#' @param loc character vector indicating where the base of each star polygon is
#'   located. \code{'l'}, \code{'r'}, \code{'b'}, and \code{'t'} indicate left,
#'   right, bottom, and top edges of the bounding rectangle, respectively.
#' @return \code{pj} with the addition of the defined star-polygons.
#' @export
new_stars <- function(pj, x, y, n, r = NA, e = NA, a = NA, px = 0.5, py = 0.5, loc = 'b', region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'stars', region = region, name = name, look = look, mod = mod, x = x, y = y, n = n, r = r, e = e, a = a, px = px, py = py, loc = loc)
}
