#' @name new_isosceles
#' @family new_shape
#' @family new_triangle
#' @title Add one or more isosceles triangle shapes
#' @description isosceles triangle shapes are located in plotting regions
#'   using bounding rectangles; a concept used in defining function parameters.
#'   See the \emph{locating bounding rectangles} section for details.
#' @inheritSection new_rect Making the new shape show up on a plot
#' @inheritSection new_rect The px and py arguments
#' @section The h and s arguments: \code{h}) is calculated as the perpendicular
#'   distance from the vertex between non-base sides to the base side. \code{s}
#'   is defined as the length of the two equivalent-length non-base sides.
#' @section Locating bounding rectangles: Bounding rectangles can be located in
#'   a number of ways. The following shows valid combination of non-\code{NA}
#'   locating arguments: \describe{
#'     \item{\code{(x + px) + b + (y + py) + h}}{(x-anchor) + base length +
#'           (y-anchor) + height}
#'     \item{\code{(x + px) + b + (y + px) + s}}{(x-anchor) + base length +
#'           (y-anchor) + side length}
#'     \item{\code{(x + px) + b + (y + px) + sb}}{(x-anchor) + base length +
#'           (y-anchor) + base/side angle}
#'     \item{\code{(x + px) + b + (y + px) + ss}}{(x-anchor) + base length +
#'           (y-anchor) + side-side angle}
#'     \item{\code{(x + px) + b + y + y2}}{(x-anchor) + base length + bottom +
#'           top}
#'     \item{\code{x + x2 + (y + py) + h }}{left + right + (y-anchor) + height}
#'     \item{\code{x + x2 + y + y2}}{left + right + bottom + top}}
#' @section Argument recycling: Arguments \code{x}, \code{y}, \code{loc},
#'   \code{px}, \code{py}, \code{b}, \code{h}, \code{s}, \code{ab}, \code{as},
#'   \code{au}, \code{x2}, and \code{y2} are recycled.
#' @inheritParams new_rect
#' @param loc (required) character vector indicating where the base of the
#'   isosceles triangles are located. \code{'l'}, \code{'r'}, \code{'b'}, and
#'   \code{'t'} indicate left, right, bottom, and top edges of the bounding
#'   rectangle, respectively.
#' @param b (optional) positive numeric vector indicating base side lengths.
#' @param s (optional) positive numeric vector indicating non-base side lengths.
#' @param sb,ss (optional) positive numeric vector indicating, respectively, the
#'   angles between (non-base side) and base side vs. angles between the two
#'   equal-length non-base sides.
#' @param au (optional) character vector indicating angle units where
#'   \code{'d'}, \code{'g'}, \code{'r'}, and \code{'p'} indicate degrees,
#'   gradians, radians, and proportions of a revolution.
#' @return \code{jp} with the addition of the specified isosceles-triangle
#'   shape(s).
#' @export
new_isosceles <- function(jp, x, y, loc = "b", b = NA, h = NA, s = NA, sb = NA, ss = NA, px = 0.5, py = 0.5, x2 = NA, y2 = NA, au = NA, look = NULL, mod = NULL, region = ".", name = ".") {
  int_new_shape(jp, "isosceles", region, name, look, mod, x = x, y = y, loc = loc, b = b, h = h, s = s, sb = sb, ss = ss, px = px, py = py, x2 = x2, y2 = y2)
}

#' @name new_pvtri
#' @family new_shape
#' @family new_triangle
#' @title Add one or more proportional-vertex based triangle shapes
#' @description Proportional-vertex based triangle shapes are created by
#'   proportionally defining vertex locations within a bounding rectangle. These
#'   shapes are located in plotting regions using bounding rectangles; a concept
#'   used in defining function parameters. See the \emph{locating bounding
#'   rectangles} section for details.
#' @inheritSection new_rect Making the new shape show up on a plot
#' @inheritSection new_rect The px and py arguments
#' @inheritSection new_rect Locating bounding rectangles
#' @section The pvx and pvy arguments: \code{pvx} indicates, as a proportion,
#'   how far across the bounding rectangle each of the three vertices of all
#'   triangles are located (from left to right). \code{pvy} indicates, as a
#'   proportion, how far up the bounding rectangle each of the three vertices of
#'   all triangles are located. For example: \describe{
#'     \item{pvx = c(0, 1/2, 1), pvy = c(0, 1, 0)}{Produces an upward
#'           \link[=new_isosceles]{isosceles triangle}.}
#'     \item{pvx = c(0, 0, 1), pvy = c(0, 1, 0)}{Produces a
#'           \link[=new_right]{right triangle} with the 90-degree angle located
#'           at the bottom left corner of the bounding rectangle.}
#'     \item{pvx = c(0, 0, 1), pvy = c(0, 1, 1/2)}{Produces a rightward
#'           \link[=new_isosceles]{isosceles triangle}.}}
#' @section Argument recycling: Arguments \code{x}, \code{y}, \code{px},
#'   \code{py}, \code{w}, \code{h} \code{x2}, and \code{y2} are recycled.
#' @inheritParams new_rect
#' @param pvx,pvy (required) proportion vectors of length 3 indicating the
#'   proportional x and y locations of the three vertices of all triangles to be
#'   plotted. All values must be between 0 and 1, inclusive. See the \emph{the
#'   pvx and pvy arguments} section.
#' @return \code{jp} with the addition of the specified proportional-based
#'   triangle shape(s).
#' @export
new_pvtri <- function(jp, x, y, pvx, pvy, w = NA, h = NA, px = 0.5, py = 0.5, x2 = NA, y2 = NA, look = NULL, mod = NULL, region = ".", name = ".") {
  int_new_shape(jp, "pvtri", region, name, look, mod, x = x, y = y, pvx = pvx, pvy = pvy, w = w, h = h, ox = px, py = py, x2 = x2, y2 = y2)
}

#' @name new_right
#' @family new_shape
#' @family new_triangle
#' @title Add one or more right triangle shapes
#' @description right triangle shapes are located in plotting regions using
#'   bounding rectangles; a concept used in defining function parameters. See
#'   the \emph{locating bounding rectangles} section for details.
#' @inheritSection new_rect Making the new shape show up on a plot
#' @inheritSection new_rect The px and py arguments
#' @section Locating bounding rectangles: Bounding rectangles can be located in
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
#'     \item{\code{(x + px) + b + y + y2}}{(x-anchor) + base + bottom edge + top
#'           edge}
#'     \item{\code{x + x2 + (y + py) + h}}{left edge + right edge + (y-anchor) +
#'           height}
#'     \item{\code{x + x2 + y + y2}}{left edge + right edge + bottom edge + top
#'           edge}}
#' @section Argument recycling: Arguments \code{x}, \code{y}, \code{loc},
#'   \code{px}, \code{py}, \code{b}, \code{h}, \code{s}, \code{ab}, \code{as},
#'   \code{au}, \code{x2}, and \code{y2} are recycled.
#' @inheritParams new_rect
#' @param loc (required) character vector indicating where the 90-degree angle
#'   of the right triangles are located. \code{'bl'}, \code{'br'}, \code{'tl'},
#'   and \code{'tr'} indicate bottom-left, bottom-right, top-left, and top-right
#'   corners of the bounding rectangle, respectively.
#' @param b (optional) positive numeric vector indicating base length.
#' @param hyp (optional) positive numeric vector indicating hypotenuse length.
#' @param hb,hh (optional) positive numeric vectors indicating, respectively,
#'   the angle between (\code{hb}) the hypotenuse and base and (\code{hs}) the
#'   angle between the hypotenuse and the non-base side.
#' @param au (optional) character vector indicating angle units where
#'   \code{'d'}, \code{'g'}, \code{'r'}, and \code{'p'} indicate degrees,
#'   gradians, radians, and proportions of a revolution.
#' @return \code{jp} with the addition of the specified right triangle shape(s).
#' @export
new_right <- function(jp, x, y, loc = "bl", px = 0.5, py = 0.5, b = NA, h = NA, hyp = NA, hb = NA, hh = NA, x2 = NA, y2 = NA, au = NA, look = NULL, mod = NULL, region = ".", name = ".") {
  int_new_shape(jp, "right", region, name, look, mod, x = x, y = y, loc = loc, b = b, h = h, hyp = hyp, hb = hb, hh = hh, x2 = x2, y2 = y2, au = au)
}

