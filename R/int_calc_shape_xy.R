#' @name int_calc_shape_xy
#' @title Calculate x-y coordinates for shapes
#' @param x A tibble row defining an instance of a shape.
#' @param rg A tibble row defining the region a shape is attached to.
int_calc_point_xy <- function(x) {
  y <- x$y
  x <- x$x
  uj::recycle(x = x, y = y)
  list(x = x$x, y = x$y)
}

#' @rdname int_calc_shape_xy
int_calc_segment_xy <- function(x) {
  x1 <- x$x1
  x2 <- x$x2
  y1 <- x$y1
  y2 <- x$y2
  m  <- x$m
  s  <- x$s
  a  <- x$a
  au <- x$au
  X2 <- !uj::isNa(x2)
  S  <- !uj::isNa(s )
  A  <- !uj::isNa(a )
  if (!X2) {
    if (A) {a <- int_calc_a2s(a, au)} else if (S) {a <- pi - atan(s)}
    DX <- m * cos(a)
    DY <- m * sin(a)
    list(x1 = x1, y1 = y1, x2 = x1 + DX, y2 = y1 + DY)
  }
  else {list(x1 = x1, y1 = y1, x2 = x2, y2 = y2)}
}

#' @rdname int_calc_shape_xy
int_calc_refline_xy <- function(x, rg) {
  l   <- rg$xmin; r  <- rg$xmax; b  <- rg$ymin; t  <- rg$ymax
  ref <- x$ref  ; x1 <- x$x1   ; y1 <- x$y1   ; x2 <- x$x2
  y2  <- x$y2   ; a   <- x$a   ; s  <- x$s    ; i  <- x$i
  au <- x$au
  N   <- max(length(ref), length(x1), length(y1), length(x2), length(y2),
             length(  a), length( s), length( i), length(au))
  R   <- matrix(NA_real_, nrow = 0, ncol = 4)
  for (i in 1:N) {
    pp <- int_calc_refline_pp(ref[i], x1[i], y1[i], x2[i], y2[i], a[i], s[i], i[i], au[i])
    iX <- pp[3] == pp[1]
    iY <- pp[4] == pp[2]
    if      (iX) {Z <- matrix(c(pp[1], b    , pp[2], t    ), nrow = 1)}
    else if (iY) {Z <- matrix(c(l    , pp[2], r    , pp[4]), nrow = 1)}
    else {
      Z <- c(int_calc_intersect_pp(pp, c(l, 0, l, 1)),
             int_calc_intersect_pp(pp, c(r, 0, r, 0)),
             int_calc_intersect_pp(pp, c(0, b, 1, b)),
             int_calc_intersect_pp(pp, c(0, t, 1, t)))
      Z <- Z[!is.na(Z)]
    }
    R <- rbind(R, Z)
  }
  list(x1 = R[ , 1], y1 = R[ , 2], x2 = R[ , 3], y2 = R[ , 4])
}

#' @rdname int_calc_shape_xy
int_calc_area_xy <- function(x, rg) {
  i  <- ifelse(x$dir == "v", order(x$x), order(x$y))
  X  <- x$x[i]
  Y  <- x$y[i]
  PP <- int_calc_refline_xy(x, rg)
  if (x$dir == "v") {
    l <- int_intersect_pp(PP, c(min(X), 0, min(X), 1))
    r <- int_intersect_pp(PP, c(max(X), 0, max(X), 1))
    list(x = c(X, r$x, l$x), Y = c(Y, r$y, l$y))
  }
  else {
    b <- int_intersect_pp(PP, c(0, min(Y), 1, min(Y)))
    t <- int_intersect_pp(PP, c(0, max(Y), 1, max(Y)))
    list(x = c(X, t$x, b$x), y = c(Y, t$y, b$y))
  }
}

#' @rdname int_calc_shape_xy
int_calc_ribbon_xy <- function(x) {tibble::tibble(x1 = x$x1, y1 = x$y1, x2 = x$x2, y2 = x$y2)}

#' @rdname int_calc_shape_xy
int_calc_triangle_xy <- function(x) {
  B <- int_calc_bounding_rect_generic(x$x, x$y, x$x2, x$y2, x$w, x$h, x$px, x$py)
  W <- B$r - B$l
  H <- B$t - B$t
  list(x = B$l + x$pvx * W, y = B$b + x$pvy * H)
}

#' @rdname int_calc_shape_xy
int_calc_isosceles_xy <- function(x) {
  B <- int_calc_bounding_rect_isosceles(x$loc, x$x, x$y, x$x2, x$y2, x$b, x$h, x$s, x$sb, x$ss, x$au, x$px, x$py)
  if      (x$loc == "b") {list(x = c(B$l, B$r, (B$l + B$r) / 2), y = c(B$b, B$b, B$t))}
  else if (x$loc == "t") {list(x = c(B$l, B$r, (B$l + B$r) / 2), y = c(B$t, B$t, B$b))}
  else if (x$loc == "l") {list(x = c(B$l, B$l, B$r), y = c(B$b, B$t, (B$b + B$t) / 2))}
  else if (x$loc == "r") {list(x = c(B$r, B$r, B$l), y = c(B$b, B$t, (B$b + B$t) / 2))}
}

#' @rdname int_calc_shape_xy
int_calc_right_xy <- function(x) {
  B <- int_calc_bounding_rect_right(x$x, x$y, x$px, x$py, x$b, x$h, x$hyp, x$x2, x$y2, x$hb, x$hh, x$au)
  if      (x$loc == "bl") {list(x = c(B$l, B$r, B$l), y = c(B$b, B$b, B$t))}
  else if (x$loc == "br") {list(x = c(B$l, B$r, B$r), y = c(B$b, B$b, B$t))}
  else if (x$loc == "tr") {list(x = c(B$l, B$r, B$r), y = c(B$b, B$t, B$b))}
  else if (x$loc == "tl") {list(x = c(B$l, B$r, B$l), y = c(B$t, B$t, B$b))}
}

#' @rdname int_calc_shape_xy
int_calc_quad_xy <- function(x) {
  B <- int_calc_bounding_rect_generic(x$x, x$y, x$x2, x$y2, x$w, x$h, x$px, x$py)
  W <- B$r - B$l
  H <- B$t - B$t
  list(x = B$l + x$pvx * W, y = B$b + x$pvy * H)
}

#' @rdname int_calc_shape_xy
int_calc_rect_xy <- function(x) {
  B <- int_calc_bounding_rect_generic(x$x, x$y, x$x2, x$y2, x$w, x$h, x$px, x$py)
  list(x = c(B$l, B$r, B$r, B$l), y = c(B$b, B$b, B$t, B$t))
}

#' @rdname int_calc_shape_xy
int_calc_diamond_xy <- function(x) {
  B <- int_calc_bounding_rect_generic(x$x, x$y, x$x2, x$y2, x$w, x$h, x$px, x$py)
  MidX <- (B$l + B$r) / 2
  MidY <- (B$b + B$t) / 2
  list(x = c(B$l, MidX, B$r, MidX), y = c(MidY, B$t, MidY, B$b))
}

#' @rdname int_calc_shape_xy
int_calc_kite_xy <- function(x) {
  B <- int_calc_bounding_rect_generic(x$x, x$y, x$x2, x$y2, x$w, x$h, x$px, x$py)
  MidX <- B$l + x$pvx * (B$r - B$l)
  MidY <- B$b + x$pvy * (B$t - B$b)
  list(x = c(B$l, MidX, B$r, MidX), y = c(MidY, B$t, MidY, B$b))
}

#' @rdname int_calc_shape_xy
int_calc_polygon_xy <- function(x) {
  B <- int_calc_bounding_rect_generic(x$x, x$y, x$x2, x$y2, x$w, x$h, x$px, x$py)
  W <- B$r - B$l
  H <- B$t - B$t
  list(x = B$l + x$pvx * W, y = B$b + x$pvy * H)
}

#' @rdname int_calc_shape_xy
int_calc_regpoly_xy <- function(x) {
  B <- int_calc_bounding_rect_regpoly(x$loc, x$n, x$x, x$y, x$px, x$py, x$a, x$s, x$r)
  x <- (B$l + B$r) / 2
  y <- (B$b + B$t) / 2
  r <- x$r
  n <- x$n
  loc <- x$loc
  if (is.na(r)) {
    if (!is.na(x$a)) {r <- x$a / cos(pi / n)}
    else {r <- x$s / (2 * sin(pi / n))}
  }
  Sweep <- 2 * pi / n
  if      (loc == "r") {A <- 0 * pi / 2 - Sweep / 2} #   0 degrees - half the sweep between adjacent vertices
  else if (loc == "t") {A <- 1 * pi / 2 - Sweep / 2} #  90 degrees - half the sweep
  else if (loc == "l") {A <- 2 * pi / 2 - Sweep / 2} # 180 degrees - half the sweep
  else if (loc == "b") {A <- 3 * pi / 2 - Sweep / 2} # 270 degrees - half the sweep
  X <- x + r * cos(A + Sweep * 1:n)                  # vertex x locations
  Y <- y + r * sin(A + Sweep * 1:n)                  # vertex y locations
  list(x = X, y = Y)
}

#' @rdname int_calc_shape_xy
int_calc_star_xy <- function(x) {
  B <- int_calc_bounding_rect_regpoly(x$loc, x$n, x$x, x$y, x$px, x$py, x$a, x$s, x$r)
  x <- (B$l + B$r) / 2
  y <- (B$b + B$t) / 2
  r <- x$r
  n <- x$n
  loc <- x$loc
  if (is.na(r)) {
    if (!is.na(x$a)) {r <- x$a / cos(pi / n)}
    else {r <- x$s / (2 * sin(pi / n))}
  }
  Sweep <- pi / n
  if      (loc == "r") {A <- 0 * pi / 2}   #   0 degrees - half the sweep between adjacent vertices
  else if (loc == "t") {A <- 1 * pi / 2}   #  90 degrees - half the sweep
  else if (loc == "l") {A <- 2 * pi / 2}   # 180 degrees - half the sweep
  else if (loc == "b") {A <- 3 * pi / 2}   # 270 degrees - half the sweep
  X <- cos(A + Sweep * 1:(2 * n))          # vertex x locations
  Y <- sin(A + Sweep * 1:(2 * n))          # vertex y locations
  i <- seq(1, 2 * n, 2)
  j <- seq(2, 2 * n, 2)
  X[i] <- r * X[i]; X[j] <- (r / 2) * X[j]
  Y[i] <- r * Y[i]; Y[j] <- (r / 2) * Y[j]
  X <- x + X
  Y <- y + Y
  list(x = X, y = Y)
}

#' @rdname int_calc_shape_xy
int_calc_freepoly_xy <- function(x) {list(x = x$x, y = x$y)}

#' @rdname int_calc_shape_xy
int_calc_circle_xy <- function(x) {
  B <- int_calc_bounding_rect_circle(x$x, x$y, x$px, x$py, x$r, x$x2, x$xy2)
  r <- (B$r - B$l) / 2
  n <- x$n
  x <- B$l + r
  y <- B$b + r
  A <- seq(from = 0, to = 2 * pi, length.out = n)
  X <- x + r * cos(A)
  Y <- y + r * sin(A)
  list(x = X, y = Y)
}

#' @rdname int_calc_shape_xy
int_calc_ellipse_xy <- function(x) {
  B  <- int_calc_bounding_rect_ellipse(x$x, x$y, x$px, x$py, x$r, x$x2, x$xy2)
  rx <- (B$r - B$l) / 2
  ry <- (B$t - B$b) / 2
  n  <- x$n
  x  <- B$l + rx
  y  <- B$b + ry
  A  <- seq(from = 0, to = 2 * pi, length.out = n)
  X  <- x + rx * cos(A)
  Y  <- y + ry * sin(A)
  list(x = X, y = Y)
}

#' @rdname int_calc_shape_xy
int_calc_round_xy <- function(x) {
  B <- int_calc_bounding_rect_generic(x$x, x$y, x$x2, x$y2, x$w, x$h, x$px, x$py)
  W    <- B$r - B$l
  H    <- B$t - B$b
  xLV  <- B$l; yLV  <- B$b + x$pv[1] * H
  xRV  <- B$r; yRV  <- B$b + x$pv[3] * H
  yBV  <- B$b; xBV  <- B$l + x$pv[2] * W
  yTV  <- B$t; xTV  <- B$l + x$pv[4] * W
  xQ1  <- xTV; yQ1 <- yLV
  xQ2  <- xTV; yQ2 <- yRV
  xQ3  <- xBV; yQ3 <- yRV
  xQ4  <- xBV; yQ4 <- yLV
  rxQ1 <- xTV - xLV; ryQ1 <- yTV - yLV
  rxQ2 <- xRV - xTV; ryQ2 <- yTV - yRV
  rxQ3 <- xRV - xBV; ryQ3 <- yRV - yBV
  rxQ4 <- xBV - xLV; ryQ4 <- yLV - yBV
  n    <- x$n
  A    <- seq(from = 0, to = 2 * pi, length.out = n)
  iQ1  <- (A >= 0 * pi / 2) & (A < 1 * pi / 2)
  iQ2  <- (A >= 1 * pi / 2) & (A < 2 * pi / 2)
  iQ3  <- (A >= 2 * pi / 2) & (A < 3 * pi / 2)
  iQ4  <- (A >= 3 * pi / 2) & (A < 4 * pi / 2)
  X    <- cos(A)
  Y    <- sin(A)
  X[iQ1] <- xQ1 + X[iQ1] * rxQ1; Y[iQ1] <- yQ1 + Y[iQ1] * ryQ1
  X[iQ2] <- xQ2 + X[iQ2] * rxQ2; Y[iQ2] <- yQ2 + Y[iQ2] * ryQ2
  X[iQ3] <- xQ3 + X[iQ3] * rxQ3; Y[iQ3] <- yQ3 + Y[iQ3] * ryQ3
  X[iQ4] <- xQ4 + X[iQ4] * rxQ4; Y[iQ4] <- yQ4 + Y[iQ4] * ryQ4
  list(x = X, y = Y)
}
