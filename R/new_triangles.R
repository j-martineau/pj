#' @name new_isosceles
#' @family new_triangles
#' @title Add an isosceles triangles shape to a plot
#' @description An isosceles triangles shapes is one or more isosceles triangles
#'   defined using the same subset of locating parameters (\code{x}, \code{y},
#'   \code{px}, \code{py}, \code{b}, \code{h}, \code{e}, \code{be}, \code{ee},
#'   \code{au}, \code{re}, and \code{te}). Isosceles triangles are located in
#'   plotting regions using bounding rectangles, a concept used in defining
#'   function arguments. See the \emph{defining bounding rectangles} section
#'   for details.
#' @section The \code{h} and \code{s} arguments: \code{h}) is calculated as the
#'   perpendicular distance from the vertex between non-base sides to the base
#'   side. \code{s} is defined as the length of the two equivalent-length
#'   non-base sides.
#' @section Defining bounding rectangles: Bounding rectangles can be located in
#'   a number of ways. The following shows valid combination of non-\code{NA}
#'   locating arguments: \describe{
#'     \item{\code{(x + px) + b + (y + py) + h}}{(x-anchor) + base + (y-anchor)
#'           + height}
#'     \item{\code{(x + px) + b + (y + px) + s}}{(x-anchor) + base + (y-anchor)
#'           + side length}
#'     \item{\code{(x + px) + b + (y + px) + sb}}{(x-anchor) + base + (y-anchor)
#'           + base/side angle}
#'     \item{\code{(x + px) + b + (y + px) + ss}}{(x-anchor) + base + (y-anchor)
#'           + side-side angle}
#'     \item{\code{(x + px) + b + y + te}}{(x-anchor) + base + bottom + top}
#'     \item{\code{x + re + (y + py) + h }}{left + right + (y-anchor) + height}
#'     \item{\code{x + re + y + te}}{left + right + bottom + top}}
#' @inheritSection new_rects Bounding rectangles, \code{px}, and \code{py}
#' @inheritSection int_new_shapes Using \code{look} to make shapes appear
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @section Argument recycling: The argument set \code{\{x, y, b, h, e, re, te,
#'   be, ee, au, px, py, loc\}} is recycled.
#' @inheritParams new_rects
#' @param loc character vector indicating where the base of the isosceles
#'   triangles are located. \code{'l'}, \code{'r'}, \code{'b'}, and \code{'t'}
#'   indicate left, right, bottom, and top edges of the bounding rectangle,
#'   respectively.
#' @param b \code{NA} or a positive numeric vector indicating base edge lengths.
#' @param e \code{NA} or a positive numeric vector indicating non-base edge
#'   lengths.
#' @param be \code{NA} or a positive numeric vector indicating the angles
#'   between (non-base) edges and base edge.
#' @param ee \code{NA} or a positive numeric vector indicating the angles
#'   between the two same-length non-base edges.
#' @param au \code{NA} or a character vector indicating angle units where
#'   \code{'d'}, \code{'g'}, \code{'r'}, and \code{'p'} indicate degrees,
#'   gradians, radians, and proportions of a revolution.
#' @return \code{pj} with the addition of the defined isosceles-triangles.
#' @export
new_isosceles <- function(pj, x, y, b = NA, h = NA, e = NA, re = NA, te = NA, eb = NA, ee = NA, loc = 'b', px = 0.5, py = 0.5, au = 'r', region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'isosceles', region = region, name = name, look = look, mod = mod, x = x, y = y, b = b, h = h, e = e, re = re, te = te, eb = eb, ee = ee, loc = loc, px = px, py = py, au = au)
}

#' @name new_triangles
#' @family new_triangles
#' @title Add a proportional-vertex triangles shape to a plot
#' @description A proportional-vertex triangles shape is a collection of
#'   triangles defined by the location of their vertices that uses the same
#'   subset of locating arguments (\code{x}, \code{y}, \code{px}, \code{py},
#'   \code{w}, \code{h}, \code{re}, \code{te}, \code{pvx}, and \code{pvy}).
#'   These shapes are located in a plottngi region by proportionally defining
#'   vertex locations within a bounding rectangle, a concept used in defining
#'   function parameters. See the \emph{locating bounding rectangles} section
#'   for details.
#' @section \code{pvx}, \code{pvy}, and vertex locations: \code{pvx} indicates,
#'   as a proportion, how far across the bounding rectangle each of the three
#'   vertices of all triangles are located (from left to right). \code{pvy}
#'   indicates, as a proportion, how far up the bounding rectangle each of the
#'   three vertices of all triangles are located. For example: \describe{
#'     \item{pvx = c(0, 1/2, 1), pvy = c(0, 1, 0)}{Produces an upward
#'           \link[=new_isosceles]{isosceles triangle}.}
#'     \item{pvx = c(0, 0, 1), pvy = c(0, 1, 0)}{Produces a
#'           \link[=new_rights]{right triangle} with the 90-degree angle located
#'           at the bottom left corner of the bounding rectangle.}
#'     \item{pvx = c(0, 0, 1), pvy = c(0, 1, 1/2)}{Produces a rightward
#'           \link[=new_isosceles]{isosceles triangle}.}}
#' @inheritSection new_rects Defining bounding rectangles
#' @inheritSection new_rects Bounding rectangles, \code{px}, and \code{py}
#' @inheritSection int_new_shapes Using \code{look} to make shapes appear
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @inheritSection new_rects Argument recycling
#' @inheritParams new_rects
#' @param pvx proportion vector of length 3 indicating the proportional x
#'   locations of the three vertices of all triangles to be plotted (i.e.,
#'   proportion of the way from left edge to right edge). All values must be
#'   between 0 and 1, inclusive. See the \emph{the pvx and pvy arguments}
#'   section.
#' @param pvy proportion vector of length 3 indicating the proportional y
#'   locations of the three vertices of all triangles to be plotted (i.e.,
#'   proportion of the way from bottom edge to top edge). All values must be
#'   between 0 and 1, inclusive. See the \emph{the pvx and pvy arguments}
#'   section.
#' @return \code{pj} with the addition of the defined proportional-vertex
#'   triangles.
#' @export
new_triangles <- function(pj, pvx, pvy, x, y, w = NA, h = NA, re = NA, te = NA, px = 0.5, py = 0.5, region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'triangles', region = region, name = name, look = look, mod = mod, pvx = pvx, pvy = pvy, x = x, y = y, w = w, h = h, re = re, te = te, px = px, py = py)
}

#' @name new_rights
#' @family new_triangles
#' @title Add a right triangles shape to a plot
#' @description Right triangle shapes defined as a collection of right triangles
#'   sharing the same subset of locating arguments (\code{x}, \code{y},
#'   \code{loc}, \code{px}, \code{py}, \code{b}, \code{h}, \code{e}, \code{re},
#'   and \code{te}, \code{hb}, \code{hh}, \code{au}). Right triangles are
#'   located in plotting regions using bounding rectangles, a concept used in
#'   defining function parameters. See the \emph{defining bounding rectangles}
#'   section for details.
#' @section Defining bounding rectangles: Bounding rectangles can be located in
#'   a number of ways. The following shows valid combination of non-\code{NA}
#'   locating arguments:
#'   \describe{
#'     \item{\code{(x + px) + (y + py) + b + h}}{(x anchor) + (y anchor) + base
#'           + height}
#'     \item{\code{(x + px) + (y + py) + b + hyp}}{(x anchor) + (y anchor) +
#'           base + hypotenuse}
#'     \item{\code{(x + px) + (y + py) + h + hyp}}{(x anchor) + (y anchor) +
#'           height + hypotenuse}
#'     \item{\code{(x + px) + (y + py) + b + hb}}{(x anchor) + (y anchor) +
#'           base + base/hypotenuse angle}
#'     \item{\code{(x + px) + (y + py) + b + hh}}{(x anchor) + (y anchor) +
#'           base + height/hypotenuse angle}
#'     \item{\code{(x + px) + (y + py) + h + hb}}{(x anchor) + (y anchor) +
#'           height + base/hypotenuse angle}
#'     \item{\code{(x + px) + (y + py) + h + hh}}{(x anchor) + (y anchor) +
#'           height + height/hypotenuse angle}
#'     \item{\code{(x + px) + (y + py) + hyp + hb}}{(x anchor) + (y anchor) +
#'           hypotenuse + base/hypotenuse angle}
#'     \item{\code{(x + px) + (y + py) + hyp + hh}}{(x anchor) + (y anchor) +
#'           hypotenuse + height/hypotenuse angle}
#'     \item{\code{(x + px) + b + y + te}}{(x-anchor) + base + bottom edge + top
#'           edge}
#'     \item{\code{x + re + (y + py) + h}}{left edge + right edge + (y-anchor) +
#'           height}
#'     \item{\code{x + re + y + te}}{left edge + right edge + bottom edge + top
#'           edge}}
#' @inheritSection new_rects Bounding rectangles, \code{px}, and \code{py}
#' @inheritSection int_new_shapes Using \code{look} to make shapes appear
#' @inheritSection int_new_shapes Using \code{mod} to transform shapes
#' @section Argument recycling: The argument set \code{\{x, y, b, h, e, re, te,
#'   be, ee, au, px, py, loc\}} is recycled.
#' @inheritParams new_rects
#' @param loc character vector indicating where the 90-degree angle of the right
#'   triangles are located. \code{'bl'}, \code{'br'}, \code{'tl'}, and
#'   \code{'tr'} indicate bottom-left, bottom-right, top-left, and top-right
#'   corners of the bounding rectangle, respectively.
#' @param b \code{NA} or a positive numeric vector indicating base length.
#' @param hyp \code{NA} or a positive numeric vector indicating hypotenuse
#'   length.
#' @param hb \code{NA} or a positive numeric vectors indicating, the angle
#'   between the hypotenuse and base sides.
#' @param hh \code{NA} or a positive numeric vectors indicating, respectively,
#'   the angle between the hypotenuse and height sides.
#' @param au \code{NA} or a character vector indicating angle units where
#'   \code{'d'}, \code{'g'}, \code{'r'}, and \code{'p'} indicate degrees,
#'   gradians, radians, and proportions of a revolution.
#' @return \code{pj} with the addition of the defined right triangles.
#' @export
new_rights <- function(pj, x, y, b = NA, h = NA, hyp = NA, re = NA, te = NA, hb = NA, hh = NA, loc = 'bl', px = 0.5, py = 0.5, au = 'r', region = '.', look = NULL, mod = NULL, name = '.') {
  int_new_shapes(pj = pj, type = 'rights', region = region, name = name, look = look, mod = mod, x = x, y = y, b = b, h = h, hyp = hyp, re = re, te = te, hb = hb, hh = hh, loc = loc, px = px, py = py, au = au)
}
