#' @name int_calc_dilate_xy
#' @description Dilate x and y values about a fixed point.
#' @param X numeric vector of input horizontal point locations.
#' @param Y numeric vector of input vertical point locations.
#' @param x numeric scalar fixed horizontal point for dilation (the horizontal
#'   point on the shape that stays in the same horizontal location).
#' @param y numeric scalar fixed vertical point for dilation (the vertical point
#'   on the shape that stays in the same vertical location).
#' @param mx positive numeric scalar giving the horizontal dilation
#'   multiplication factor.
#' @param my positive numeric scalar giving the vertical dilation multiplication
#'   factor.
#' @return \code{X} and \code{Y} dilated as specified by remaining arguments.
int_calc_dilate_xy <- function(X, Y, x, y, mx, my) {
  X <- X - x          # horizontal deviation from fixed x point
  Y <- Y - y          # vertical deviation from fixed y point
  X <- mx * X         # dilated horizontal deviation
  Y <- my * Y         # dilated vertical deviation
  X <- x + X          # horizontal deviation fixed to fixed x point
  Y <- y + Y          # vertical deviation fixed to fixed y point
  list(x = X, y = Y)
}

#' @name int_calc_reflect_xy
#' @description Reflect x and y values across a reference line.
#' @param X numeric vector of input horizontal point locations.
#' @param Y numeric vector of input vertical point locations.
#' @param ref \code{NA} or character scalar giving predefined reference lines.
#' @param x \code{NA} or numeric scalar fixed (primary) horizontal location of
#'   reference line
#' @param y \code{NA} or numeric scalar fixed (primary) vertical location of
#'   reference line.
#' @param x2 \code{NA} or numeric scalar fixed horizontal location of second
#'   point defining the reference line.
#' @param y2 \code{NA} or numeric scalar fixed vertical location of second point
#'   defining the reference line.
#' @param a \code{NA} or numeric scalar angle of reference line.
#' @param s \code{NA} or numeric scalar slope of reference line.
#' @param i \code{NA} or numeric scalar intercept of reference line.
#' @param au \code{NA} or character scalar angle units.
#' @return \code{X} and \code{Y} reflected as specified by remaining arguments.
int_calc_reflect_xy <- function(X, Y, ref, x, y, x2, y2, a, s, i, au) {
  PP <- int_calc_refline_pp(ref, x, y, x2, y2, a, s, i, au, X = X, Y = Y)        # convert refline to point-point representation
  x1 <- PP[1]; y1 <- PP[2]; x2 <- PP[3]; y2 <- PP[4]                             # get x1, y1, x2, and y2
  s  <- (y2 - y1) / (x2 - x1)                                                    # slope
  i  <- ((x2 * y1) - (x1 * y2)) / (x2 - x1)                                      # intercept
  d  <- (X + (Y - i) * s) / (1 + s^2)                                            # see https://stackoverflow.com/questions/3306838/algorithm-for-reflecting-a-point-across-a-line
  X  <- 2 * d - X
  Y  <- 2 * d * s - Y + 2 * i
  list(x = X, y = Y)
}

#' @name int_calc_rotate_xy
#' @description Reflect x and y values around a reference point.
#' @param X numeric vector of input x values to be rotated.
#' @param Y numeric vector of input y values to be rotated.
#' @param ref \code{NA} or character scalar indicating a predefined reference
#'   point.
#' @param x \code{NA} or numeric scalar horizontal location of reference point
#'   to rotate around.
#' @param y \code{NA} or numeric scalar vertical location of reference point to
#'   rotate around.
#' @param a \code{NA} or numeric scalar angle of rotation.
#' @param d \code{NA} or character scalar angle units.
#' @return \code{X} and \code{Y} rotated as specified by remaining arguments.
int_calc_rotate_xy <- function(X, Y, ref, x, y, a, au) {
  a <- int_calc_a2rad(a, au)                                                     # convert angles to radians
  if (!uj::isNa(ref)) {                                                          # if a predefined reference line is specified
    LX <- min(X); RX <- max(X); MX <- (LX + RX) / 2                              # > left, right, and horizontal middle
    BY <- min(Y); TY <- max(Y); MY <- (BY + TY) / 2                              # > bottom, top, and vertical middle
    if      (ref == "o" ) {x <- 0 ; y <- 0 }                                     # > orgin
    else if (ref == "c" ) {x <- MX; y <- MY}                                     # > center of bounding rectangle
    else if (ref == "bl") {x <- LX; y <- BY}                                     # > bottom left of bounding rectangle
    else if (ref == "br") {x <- RX; y <- BY}                                     # > bottom right of boundihng rectangle
    else if (ref == "tl") {x <- LX; y <- TY}                                     # > top left of bounding rectangle
    else if (ref == "tr") {x <- RX; y <- TY}                                     # > top right of bounding rectangle
    else if (ref == "lm") {x <- LX; y <- MY}                                     # > middle of left edge of bounding rectangle
    else if (ref == "rm") {x <- RX; y <- MY}                                     # > middle of right edge of bounding rectangle
    else if (ref == "bm") {x <- MX; y <- BY}                                     # > middle of bottom edge of bounding rectangle
    else if (ref == "tm") {x <- MY; y <- TY}                                     # > middle of top edge of bounding rectangle
  }
  rotatedX <- cos(a) * (X - x) - sin(a) * (Y - y) + x                            #' see https://gamefromscratch.com/gamedev-math-recipes-rotating-one-point-around-another-point
  rotatedY <- sin(a) * (X - x) + cos(a) * (Y - y) + y
  list(x = rotatedX, y = rotatedY)
}

#' @name int_calc_translate_xy
#' @description translate x and y values by specified values.
#' @param X numeric vector of input x values to be rotated.
#' @param Y numeric vector of input y values to be rotated.
#' @param dx numeric scalar horizontal translation value.
#' @param dy numeric scalar vertical translation value.
#' @param xu character scalar units \code{dx} is expressed in.
#' @param yu character scalar units \code{dy} is expressed in.
#' @return \code{X} and \code{Y} translated as specified by remaining arguments.
int_calc_translate_xy <- function(X, Y, dx, dy, xu, yu) {
  if (xu == "p") {dx <- dx * (max(X) - min(X))}                                  # convert proportions to raw x, if needed
  if (yu == "p") {dy <- dy * (max(Y) - min(Y))}                                  # convert proportions to raw y, if needed
  X <- X + dx; Y <- Y + dy                                                       # apply the translation values
  list(x = X, y = Y)
}
