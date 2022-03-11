#' @name int_calc_reflines
#' @description Convert a line representation to point-point form, or calculate
#'   a refline slope or intercept.
#' @param ref \code{NA} or character scalar indicating a predefined reference
#'   line. \code{'i'} indicates the identity line, \code{'x'} indicates the
#'   x-axis, \code{'y'} indicates the y-axis. \code{NA} indicates an alternate
#'   line definition usinig other arguments.
#' @param x1 \code{NA} or numeric scalar. When non-\code{NA} and \code{y1 = NA},
#'   indicates a vertical line at its value. When both \code{x1} and \code{y1}
#'   are non-\code{NA}, anchors a line to a point.
#' @param y1 \code{NA} or numeric scalar. When non-\code{NA} and \code{x1 = NA},
#'   indicates a horizontal line at its value. When both \code{x1} and \code{y1}
#'   are non-\code{NA}, anchors a line to a point.
#' @param x2 \code{NA} or numeric scalar. When non-\code{NA} (and \code{x1},
#'   \code{y1}, and \code{y2} are also non-\code{NA}), anchors a line to a
#'   second point.
#' @param y2 \code{NA} or numeric scalar. When non-\code{NA} (and \code{x1},
#'   \code{y1}, and \code{x2} are also non-\code{NA}), anchors a line to a
#'   second point.
#' @param a \code{NA} or numeric scalar giving the angle of the reference line.
#' @param s \code{NA} or numeric scalar giving the slope of the reference line.
#' @param i \code{NA} or numeric scalar giving the intercept of the reference
#'   line.
#' @param au \code{NA}, \code{'d'} for degrees, \code{'g'} for gradians,
#'   \code{'p'} for proportion of a revolution, or \code{'r'} for radians.
#' @param x numeric scalar giving the x-value for which a y-value is to be
#'   calculated.
#' @param y numeric scalar giving the y-value for which an x-value is to be
#'   calculated.
#' @param pp numeric vector containing the values \code{c(x1, y1, x2, y2)} to
#'   represent a point-point formulation of a reference line.
#' @param ppA numeric vector containing the values \code{c(x1, y1, x2, y2)} to
#'   represent a point-point formulation of a first line to be intersected with
#'   the line \code{ppB}.
#' @param ppB numeric vector containing the values \code{c(x1, y1, x2, y2)} to
#'   represent a point-point formulation of a second line to be intersected with
#'   the line \code{ppB}.
#' @param X optional numeric vector of input horizontal point locations.
#' @param Y optional numeric vector of input vertical point locations.
#' @return \code{int_refline_pp} returns a numeric vector of length 4 containing
#'   the coordinates \code{c(x1, y1, x2, y2)} extending to the edges of the
#'   plotting region (not accounting for a pad around the region).
#'   \cr\cr
#'   \code{int_intersect_pp} returns a numeric vector of length 2 containing the
#'   coordinates \code{c(x, y)} where two lines intersect.
#'   \cr\cr
#'   All others return a numeric scalar.
int_calc_refline_pp <- function(ref, x1, y1, x2, y2, a, s, i, au, X = NA, Y = NA) {
  if (!uj::isNa(a)) {s <- int_calc_a2s(a, au)}                                   # convert angle to slope, if angle was provided
  PP <- !uj::isNa(x2); X1 <- !uj::isNa(X1); Y1 <- !uj::isNa(y1)
  P  <- X1 & Y1      ; I  <- !uj::isNa(i) ; S  <- !uj::isNa(s)
  RI <- ref == "i"   ; RX <- ref == "x"   ; RY <- ref == "y"; RL <- ref == "l"
  RR <- ref == "r"   ; RB <- ref == "b"   ; RT <- ref == "t"; RH <- ref == "h"
  RV <- ref == "v"   ; RD <- ref == "d"   ; RU <- ref == "u"
  if (!uj::isNa(X)) {LX <- min(X); RX <- max(X); MX <- (LX + RX) / 2}
  if (!uj::isNa(Y)) {BY <- min(Y); TY <- max(Y); MY <- (BY + TY) / 2}
  if      (PP   ) {matrix(c(x1, y1, x2    , y2    ), nrow = 1)}                  # point-point form
  else if (I & S) {matrix(c( 0,  i,  1    , i  + s), nrow = 1)}                  # intercept-slope form
  else if (I & P) {matrix(c( 0,  i, x1    , y2    ), nrow = 1)}                  # intercept-point form
  else if (S & P) {matrix(c(x1, y1, x1 + 1, y1 + s), nrow = 1)}                  # slope-point form
  else if (S    ) {matrix(c( 0,  0,  1    , s     ), nrow = 1)}                  # origin-slope form
  else if (X1   ) {matrix(c(x1,  0, x1    , 1     ), nrow = 1)}                  # vertical line at x1
  else if (Y1   ) {matrix(c( 0, y1,  0    , y1    ), nrow = 1)}                  # horizontal line at y1
  else if (RI   ) {matrix(c( 0,  0,  1    , 1     ), nrow = 1)}                  # identity line
  else if (RX   ) {matrix(c( 0,  0,  1    , 0     ), nrow = 1)}                  # x-axis line
  else if (RY   ) {matrix(c( 0,  0,  0    , 1     ), nrow = 1)}                  # y-axis line
  else if (RL   ) {matrix(c(LX,  0, LX    , 1     ), nrow = 1)}                  # left object edge
  else if (RR   ) {matrix(c(RX,  0, RX    , 1     ), nrow = 1)}
  else if (RB   ) {matrix(c( 0, BY,  1    , BY    ), nrow = 1)}
  else if (RT   ) {matrix(c( 0, TY,  1    , TY    ), nrow = 1)}
  else if (RH   ) {matrix(c(MX,  0, MX    , 1     ), nrow = 1)}
  else if (RV   ) {matrix(c( 0, MY,  1    , MY    ), nrow = 1)}
  else if (RD   ) {matrix(c(LX, TY, RX    , BY    ), nrow = 1)}
  else if (RU   ) {matrix(c(LX, BY, RX    , TY    ), nrow = 1)}
  else {stop("Unknown refline definition: ref = '", ref, "', x1 = ", x1,
             ", y1 = ", y1, ", x2 = ", x2, ", y2 = ", y2, ", a = ", a,
             ", s = ", s, ", i = ", i, ", au = '", au, "'.")}
}

#' @rdname int_calc_reflines
int_calc_y_from_x_pp <- function(x, pp) {
  # calculate value of y from value of x and a point-point line representation
  if (pp[4] == pp[2]) {return(pp[4])}
  if (pp[3] == pp[1]) {return(NA   )}
  m <- (pp[4] - pp[2]) / (pp[3] - pp[1])
  b <- pp[2] - m * pp[1]
  m * x + b
}

#' @rdname int_calc_reflines
int_calc_x_from_y_pp <- function(y, pp) {
  # calculate value of x from value of y and a point-point line representation
  if (pp[3] == pp[1]) {return(pp[3])}
  if (pp[4] == pp[2]) {return(NA   )}
  m <- (pp[4] - pp[2]) / (pp[3] - pp[1])
  b <- pp[2] - m * pp[1]
  (y - b) / m
}

#' @rdname int_calc_reflines
int_calc_intersect_pp <- function(ppA, ppB) {
  # calculate point of intersection between two lines in point-point representation
  x1 <- ppA[1]; y1 <- ppA[2]; x2 <- ppA[3]; y2 <- ppA[4]
  x3 <- ppB[1]; y3 <- ppB[2]; x4 <- ppB[3]; y4 <- ppB[4]
  c(((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) / ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)),
    ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) / ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)))
}

#' @rdname int_calc_reflines
int_calc_refline_s <- function(ref, x1, y1, x2, y2, a, s, i, au) {
  # calculate reference line slope from any combination of specifications
  if      (!uj::isNa(s )) {s}
  else if (!uj::isNa(a )) {int_calc_a2s(a, au)}
  else if (!uj::isNa(x2)) {(y2 - y1) / (x2 - x1)}
  else if (!uj::isNa(i )) {(y1 - i ) / (x1 -  0)}
  else if (ref == "i"   ) {1}
  else if (ref == "y"   ) {0}
  else                    {Inf}
}

#' @rdname int_calc_reflines
int_calc_refline_i <- function(ref, x1, y1, x2, y2, a, s, i, au) {
  # calculate reference line intercept from any combination of specifications
  if      (!uj::isNa( i)) {i}
  else if (!uj::isNa( s)) {y1 - x1 * s}
  else if (!uj::isNa( a)) {y1 - x1 * int_calc_a2s(a, au)}
  else if (!uj::isNa(x2)) {y1 - x1 * (y2 - y1) / (x2 - x1)}
  else if (ref == "i"   ) {0}
  else if (ref == "x"   ) {0}
  else                    {NaN}
}
