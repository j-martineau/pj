#' @name int_calc_ci_xy
#' @description calculate x and y values for confidence intervals.
#' @param X numeric vector horizontal location of points around which to draw CIs.
#' @param Y numeric vector vertical location of points around which to draw CIs.
#' @param minx numeric scalar minimum plottable x value in region.
#' @param miny numeric scalar minimum plottable y value in region.
#' @param maxx numeric scalar maximum plottable x value in region.
#' @param maxy numeric scalar maximum plottable y value in region.
#' @param type character scalar CI type.
#' @param dir character scalar direction of CI.
#' @param m numeric scalar multiple of error distances.
#' @param d numeric vector of symmetric error distances.
#' @param pos numeric vector of positive error distances.
#' @param neg numeric vector of negative error distances.
#' @return additional X-Y pairs for drawing confidence intervals.
int_calc_ci_xy <- function(X, Y, minx, miny, maxx, maxy, type, dir, m, d, pos, neg) {
  V <- dir  == "v"  ; H <- dir  == "H"     ; L <- type == "line"
  C <- type == "cap"; R <- type == "ribbon"; A <- type == "taper"
  if (!uj::isNa(d)) {pos <- d; neg <- d}
  P <- m * pos
  N <- m * neg
  if      (R & V) {x <- c(X, rev(X)); y <- c(Y - N, rev(Y + P))}
  else if (R & H) {y <- c(Y, rev(Y)); x <- c(X - N, rev(Y + P))}
  else if (L & V) {x <- c(X, X); y = c(Y - N, Y + P)}
  else if (L & H) {y <- c(Y, Y); x = c(X - N, X + P)}
  else {
    N  <- length(X)
    if (V) {D <- X[order(X)]; W <- maxx - minx}
    else   {D <- Y[order[Y]]; W <- maxy - miny}
    D <- D[2:N] - D[1:(N - 1)]
    D <- mean(D)
    D <- max(D / 5, (maxx - minx) / 50)
    if (V & C) {
      x <- c(X - D, X + D, X, X, X - D, X + D)
      y <- c(Y - N, Y - N, Y - N, Y + P, Y + P, Y + P)
    }
    else if (H & C) {
      x <- c(X - N, X - N, X - N, X + P, X + P, X + P)
      y <- c(Y - D, Y + D, Y, Y, Y - D, Y + D)
    }
    else if (V & A) {x <- c(X, X - D, X, X + D); y <- c(Y - N, Y, Y + P, Y)}
    else if (H & A) {x <- c(X - N, X, X + P, X); y <- c(Y, Y - D, Y, Y + D)}
  }
  list(x = x, y = y)
}

#' @name int_calc_ci_xy
#' @description calculate x and y values for label background rectangles.
#' @param X numeric vector horizontal location of labels.
#' @param Y numeric vector vertical location of laberls
#' @param dx numeric vector of deviations from X
#' @param dy numeric vector of deviations from Y
#' @param text character vector of labels.
#' @param font character vector of font names.
#' @param size positive numeric vector of point sizes.
#' @param style character vector of text styles
#' @param hj proportion vector of horizontal justifications.
#' @param vj proportion vector of vertical justifications.
#' @param a numeric vector of angles of rotation.
#' @param parse logical vector whether to parse text.
#' @param fc character vector fill color.
#' @param lc character vector line color.
#' @param au character vector angle units.
#' @return additional X-Y pairs for drawing confidence intervals.
int_calc_label_box_xy <- function(X, Y, dx, dy, text, font, size, style, hj, vj, a, parse, fc, lc, au) {
  if (uj::isNa(fc) & uj::isNa(lc)) {return(NULL)}
  a <- int_calc_a2rad(a, au)
  X <- X + dx
  Y <- Y + dy
  if      (style == "p") {style <- 1}
  else if (style == "b") {style <- 2}
  else if (style == "i") {style <- 3}
  else                   {style <- 4}
  if (any(parse)) {text <- parse(text = text)}
  W <- strwidth( text, ps = size, font = style, family = font)
  H <- strheight(text, ps = size, font = style, family = font)
  Pad <- H / 4
  LE <- X - W * (1 - hj); RE <- LE + W
  BE <- Y - H * (1 - vj); TE <- BE + H
  X  <- c(LE - Pad, RE + Pad, RE + Pad, RE - Pad); MX <- (LE + RE) / 2
  Y  <- c(BE - Pad, BE - Pad, TE + Pad, TE + Pad); MY <- (BE + TE) / 2
  rotatedX <- cos(a) * (X - MX) - sin(a) * (Y - MY) + MX                         #' see https://gamefromscratch.com/gamedev-math-recipes-rotating-one-point-around-another-point
  rotatedY <- sin(a) * (X - MX) + cos(a) * (Y - MY) + MY
  list(x = rotatedX, y = rotatedY)
}
