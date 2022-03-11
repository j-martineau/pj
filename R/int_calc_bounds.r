#' @name int_calc_bounds
#' @description Calculating parameterized polygon bounding rectangles
#' @param loc character scalar indicating (a) for isosceles triangles and
#'   regular polygons, whether the base is at the bottom, top, left, or right,
#'   and (b) for stars, whether a point must be located at the bottom (mid), top
#'   (mid), left (mid), or right (mid) of the bounding rectangle.
#' @param x numeric scalar giving the x anchor point for a shape.
#' @param y numeric scalar giving the y anchor point for a shape.
#' @param px \code{NA} or a proportion scalar giving the proportion of the
#'   distance from left to right edge where the anchor point \code{x} is
#'   located.
#' @param py \code{NA} or a proportion scalar giving the proportion of the
#'   distance from bottom to top edge where the anchor point \code{y} is
#'   located.
#' @param a \code{NA} or positive numeric scalar giving the apothem of a regular
#'   polygon (the distance from center to midpoint of one of the sides)
#' @param w \code{NA} or positive numeric scalar giving the width of a polygon.
#' @param h \code{NA} or positive numeric scalar giving the height of a polygon.
#' @param b \code{NA} or positive numeric scalar giving the length of the base
#'   of an isosceles or right triangle.
#' @param s \code{NA} or positive numeric scalar giving the length of the side
#'   of a regular polygon or length of the non-base side of an equilateral
#'   triangle
#' @param r \code{NA} or positive numeric scalar giving the radius of a circle
#'   or circumradius of a regular polygon (distance from center to vertex).
#' @param hyp \code{NA} or positive numeric scalar giving the length of the
#'   hypotenuse of a right triangle.
#' @param x2 \code{NA} or positive numeric scalar giving the right edge of a
#'   polygon (implies that \code{x} is the left edge and \code{px} = 0).
#' @param y2 \code{NA} or positive numeric scalar giving the top edge of a
#'   polygon (implies that \code{y} is the bottom edge and \code{py} = 0).
#' @param sb \code{NA} or positive numeric numeric scalar giving the angle
#'   between the base and non-base side of an equilateral triangle.
#' @param ss \code{NA} or positive numeric numeric scalar giving the angle
#'   between the two non-base sides of an equilateral triangle.
#' @param hb \code{NA} or positive numeric numeric scalar giving the angle
#'   between the hypotenuse and base of a right triangle.
#' @param hh \code{NA} or positive numeric numeric scalar givnig the angle
#'   between the hypotenuse and height of a right triangle.
#' @param au \code{NA} or character scalar indicating angle units in which
#'   \code{sb}, \code{ds}, \code{hb}, and \code{hh} are expressed.
#' @return A list of four numeric scalars named \code{'l'}, \code{'r'},
#'   \code{'b'}, and \code{'t'} for left, right, bottom, and top edge location
#'   of the bounding rectangle.
int_calc_bounding_rect_generic <- function(x, y, x2, y2, w, h, px, py) {
  if (!uj::isNa(x2)) {w <- x2 - x}                                               # calculate width if not provided
  if (!uj::isNa(y2)) {h <- y2 - y}                                               # calculate height if not provided
  if (!uj::isNa(px)) {x <- x - px * w}                                           # relocate x at left edge if px is provided
  if (!uj::isNa(py)) {y <- y - py * h}                                           # relocate y at bottom edge if py is provided
  x2 <- x + w                                                                    # right edge
  y2 <- y + h                                                                    # top edge
  return(list(l = min(x, x2), b = min(y, y2), r = max(x, x2), t = max(y, y2)))
}

#' @rdname int_calc_bounds
int_calc_bounding_rect_isosceles <- function(loc, x, y, x2, y2, b, h, s, sb, ss, au, px, py) {
  # ISOSCELES TRIANGLE WITH SIDE s, BASE b, and HEIGHT h IS TWO RIGHT
  # TRIANGLES OF BASE b/2, HEIGHT h, and HYPOTENUSE s, SO BASE FORMULAE ARE
  #   ◆ hyp^2           = base^2 + height^2 >> s^2    = (b/2)^2 + h^2
  #   ◆ sin(hyp-base  ) = opp/hyp           >> sin(sb  ) = h/s
  #   ◆ cos(hyp-base  ) = adj/hyp           >> cos(sb  ) = b/2s
  #   ◆ sin(hyp-height) = opp/hyp           >> sin(ss/2) = b/2s
  #   ◆ cos(hyp-height) = adj/hyp           >> cos(ss/2) = h/s
  #   ◆ sin(sb) = h / s
  #       substitute s = b/2cos(sb)         >> sin(sb) = h / (b / (2 * cos(sb)))
  #                                         >> sin(sb) = 2h * cos(sb) / b
  #       solve for h                       >> 2h = b * sin(sb) / cos(sb)
  #                                         >> h = b * sin(sb) / 2cos(sb)
  #       solve for b                       >> b = 2h * cos(sb) / sin(sb)
  #   ◆ cos(ss/2) = h/s
  #       substitute s = b / sin(ss / 2)    >> cos(ss/2) = h / (b / sin(ss/2))
  #                                         >> cos(ss/2) = h * sin(ss/2) / b
  #       solve for h                       >> h = b * cos(ss/2) / sin(ss/2)
  #       solve for b                       >> b = h * sin(ss/2) / cos(ss/2)
  # CALCULATIONS OF h AND b FROM ANY TWO VALUES
  #   h from s and b : s^2 = (b/2)^2 + h^2  >> b = 2 * sqrt(s^2 - h^2)
  #   h from s and sb: sin(sb) = h/s        >> h = s * sin(sb)
  #   h from s and ss: cos(ss/2) = h/s      >> h = s * cos(ss / 2)
  #   b from s and h : s^2 = (b/2)^2 + h^2  >> h = sqrt(s^2 - (b / 2)^2)
  #   b from s and sb: cos(sb) = b/2s       >> b = 2 * s * cos(sb)
  #   b from s and ss: sin(ss/2) = b/2s     >> b = 2 * s * sin(ss / 2)
  #   h from b and sb:                      >> h = b * sin(sb) / (2 * cos(sb))
  #   h from b and ss:                      >> h = b * cos(ss / 2) / sin(ss / 2)
  #   b from h and sb:                      >> b = 2 * h * cos(sb) / sin(sb)
  #   b from h and ss:                      >> b = h * sin(ss / 2) / cos(ss / 2)
  sb <- int_calc_a2rad(sb, au)                                                   # convert angles to radians
  ss <- int_calc_a2rad(ss, au)
  BT <- loc == "b" | loc == "t"                                                  # base is on bottom or top
  if (BT) {if (!uj::isNa(x2)) {w <- x2 - x}; if (!uj::isNa(y2)) {h <- y2 - y}}   # if so    , calculate base and height from x2 and y2 if present
  else    {if (!uj::isNa(x2)) {h <- x2 - x}; if (!uj::isNa(y2)) {w <- y2 - y}}   # otherwise, calculate base and height from y2 and x2 if present
  B  <- !uj::isNa(b ); H  <- !uj::isNa(h ); S  <- !uj::isNa(s)                   # indicators of whether b, h, s, sb, and ss were provided
  SB <- !uj::isNa(sb); SS <- !uj::isNa(ss)
  if      (S & H ) {b <- 2 * sqrt(s^2 - h^2)}
  else if (S & B ) {h <- sqrt(s^2 - (b / 2)^2)}
  else if (H & SB) {b <- 2 * h * cos(sb) / sin(sb)}
  else if (B & SB) {h <- b * sin(sb) / (2 * cos(sb))}
  else if (H & SS) {b <- h * sin(ss / 2) / cos(ss / 2)}
  else if (B & SS) {h <- b * cos(ss / 2) / sin(ss / 2)}
  else if (S & SB) {h <- s * sin(sb); b <- 2 * s * cos(sb)}
  else if (S & SS) {h <- s * cos(ss / 2); b <- 2 * s * sin(ss / 2)}
  if (!is.na(px)) {x <- x - px * b}                                              # rescale x to left edge if not already at left edge
  if (!is.na(py)) {y <- y - py * h}                                              # rescale y to bottom edge if not already at bottom edge
  if (BT) {x2 <- x + b; y2 <- y + h}                                             # calculate right and top edges, depending on location of
  else    {x2 <- x + h; y2 <- y + b}                                             #   base edge
  return(list(l = min(x, x2), b = min(y, y2), r = max(x, x2), t = max(y, y2)))
}

#' @rdname int_calc_bounds
int_calc_bounding_rect_right <- function(x, y, x2, y2, b, h, hyp, hb, hh, au, px, py) {
  # BASE FORMULAE
  #   ◆ b^2 + h^2 = hyp^2 >> b = sqrt(hyp^2 - b^2); h = sqrt(hyp^2 - h^2)
  #   ◆ sin(hb) = h / hyp >> h = hyp * sin(hb)
  #   ◆ cos(hb) = b / hyp >> b = hyp * cos(hb)
  #   ◆ sin(hh) = b / hyp >> b = hyp * sin(hh)
  #   ◆ cos(hh) = h / hyp >> h = hyp * cos(hh)
  # CALCULATIONS OF h AND b FROM ANY TWO VALUES
  #   h from b & hyp:  b^2 + h^2 = hyp^2      >> h = sqrt(hyp^2 - b^2)
  #   h from b & hh:   h = sqrt(hyp^2 - b^2)
  #      substitute    hyp = b / sin(hh)      >> h = sqrt((b / sin(hh))^2 - b^2)
  #   h from b & hb:   h = sqrt(hyp^2 - b^2)
  #      substitute    hyp = b / cos(hb)      >> h = sqrt((b / cos(hb))^2 - h^2)
  #   h from hh & hyp: cos(hh) = h / hyp      >> h = hyp * cos(hh)
  #   h from hb & hyp: sin(hb) = h / hyp      >> h = hyp * sin(hb)
  #   b from h & hyp:  b^2 + h^2 = hyp^2      >> b = sqrt(hyp^2 - h^2)
  #   b from h & hh:   b = sqrt(hyp^2 - h^2)
  #      substitute    hyp = h / cos(hh)      >> b = sqrt((h / cos(hh))^2 - h^2)
  #   b from h & hb:   b = sqrt(hyp^2 - h^2)
  #      substitute    hyp = h / sin(hb)      >> b = sqrt((h / sin(hb))^2 - h^2)
  #   b from hh & hyp: sin(hh) = b / hyp      >> b = hyp * sin(hh)
  #   b from hb & hyp: cos(hb) = b / hyp      >> b = hyp * cos(hb)
  hb <- int_calc_a2rad(hb, au)
  hh <- int_calc_a2rad(hh, au)
  if (!uj::isNa(x2)) {b <- x2 - x}                                               # get base from x2 if present
  if (!uj::isNa(y2)) {h <- y2 - y}                                               # get height from y2 if present
  B   <- !uj::isNa(b  )                                                          # whether various values were submitted
  H   <- !uj::isNa(h  )
  HYP <- !uj::isNa(hyp)
  HB  <- !uj::isNa(hb )
  HH  <- !uj::isNa(hh )
  if      (B  & HYP) {h <- sqrt(hyp^2 - b^2)}
  else if (H  & HYP) {b <- sqrt(hyp^2 - h^2)}
  else if (B  & HH ) {h <- sqrt((b / sin(hh))^2 - b^2)}
  else if (B  & HB ) {h <- sqrt((b / cos(hb))^2 - h^2)}
  else if (H  & HH ) {b <- sqrt((h / cos(hh))^2 - h^2)}
  else if (H  & HB ) {b <- sqrt((h / sin(hb))^2 - h^2)}
  else if (HH & HYP) {h <- hyp * cos(hh); b <- hyp * sin(hh)}
  else if (HB & HYP) {h <- hyp * sin(hb); b <- hyp * cos(hb)}
  if (!is.na(px)) {x <- x - px * b}                                              # rescale x to left edge if not already there
  if (!is.na(py)) {y <- y - py * h}                                              # rescale y to bottom edge if not already there
  x2 <- x + b                                                                    # left edge + base gives right edge
  y2 <- y + h                                                                    # bottom edge + height gives top edge
  return(list(l = min(x, x2), b = min(y, y2), r = max(x, x2), t = max(y, y2)))
}

#' @rdname int_calc_bounds
int_calc_bounding_rect_regpoly <- function(loc, n, x, y, a, s, r, px, py) {
  # VALUES
  #   s = side = side length
  #   a = apothem = distance from center to midpoint of an edge/side
  #   r = circumradius = distance from center to vertex
  # BASE FORMULAE
  #   a = s / (2 * tan(pi / n))
  #   a = r * cos(pi / n)
  #   r = s / (2 * sin(pi / n))
  #   r = a / cos(pi / n)
  #   s = a * 2 * tan(pi / n)
  #   s = r * 2 * sin(pi / n)
  #   Interior ∠ = 180 - 360 / n = pi - 2 * pi / n
  #   Sweep ∠    = 180 - Interior ∠ = pi - (pi - 2 * pi / n) = 2 * pi / n
  if (uj::isNa(r)) {                                                             # if radius was not provided
    if (!uj::isNa(a)) {r <- a / cos(pi / n)}                                     # > if apothem was provided, calculate from a and n
    else              {r <- s / (2 * sin(pi / n))}                               # > otherwise, calculate from side and n
  }
  Sweep <- 2 * pi / n
  if      (loc == "r") {A <- 0 * pi / 2 - Sweep / 2}                             #   0 degrees - half the sweep between adjacent vertices
  else if (loc == "t") {A <- 1 * pi / 2 - Sweep / 2}                             #  90 degrees - half the sweep
  else if (loc == "l") {A <- 2 * pi / 2 - Sweep / 2}                             # 180 degrees - half the sweep
  else if (loc == "b") {A <- 3 * pi / 2 - Sweep / 2}                             # 270 degrees - half the sweep
  X <- cos(A + Sweep * 1:n)                                                      # vertex x locations
  Y <- sin(A + Sweep * 1:n)                                                      # vertex y locations
  W <- max(X) - min(X)                                                           # shape width and height
  H <- max(Y) - min(Y)
  if (!uj::isNa(px)) {x <- x - px * W}                                           # rescale x and y to left, bottom edges, if needed
  if (!uj::isNa(py)) {y <- y - py * H}
  x2 <- x + W                                                                    # right and bottom edges
  y2 <- y + H                                                                    # top edge
  return(list(l = min(x, x2), b = min(y, y2), r = max(x, x2), t = max(y, y2)))
}

#' @rdname int_calc_bounds
int_calc_bounding_rect_star <- int_calc_bounding_rect_regpoly

#' @rdname int_calc_bounds
int_calc_bounding_rect_circle <- function(x, y, x2, y2, r, px, py) {
  if      (!uj::isNa(x2)) {r <- (x2 - x) / 2}                                    # if radius not provided, calculate
  else if (!uj::isNa(y2)) {r <- (y2 - y) / 2}
  if (!uj::isNa(px)) {x <- x - px * 2 * r}                                       # rescale x and y to left and bottom edges, if needed
  if (!uj::isNa(py)) {y <- y - py * 2 * r}
  x2 <- x + 2 * r                                                                # right and bottom edges
  y2 <- y + 2 * r
  return(list(l = min(x, x2), b = min(y, y2), r = max(x, x2), t = max(y, y2)))
}

#' @rdname int_calc_bounds
int_calc_bounding_rect_ellipse <- function(x, y, x2, y2, rx, ry, px, py) {
  if      (!uj::isNa(x2)) {rx <- (x2 - x) / 2}                                   # calculate rx and ry if not provided
  else if (!uj::isNa(y2)) {ry <- (y2 - y) / 2}
  if (!uj::isNa(px)) {x <- x - px * 2 * rx}                                      # rescale x and y to left and bottom edges, if needed
  if (!uj::isNa(py)) {y <- y - py * 2 * ry}
  x2 <- x + 2 * rx                                                               # right and bottom edges
  y2 <- y + 2 * ry
  return(list(l = min(x, x2), b = min(y, y2), r = max(x, x2), t = max(y, y2)))
}
