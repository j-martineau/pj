#' @name int_calc_angle
#' @description convert angles in other units to radians, angles to slopes,
#' and slopes to angles.
#' @param a numeric vector
#' @param au character vector indicating degrees ('d'), gradians ('g'), radians
#'   ('r'), or proportions of a revolution ('p').
#' @return numeric vector
int_calc_a2rad <- function(a, au) {
  if (uj::isNa(a)) {return(NA)}
  if (length(au) == 1) {au <- rep.int(au, length(a))}                            # recycle {au} if needed
  a[au == "d"] <- 2 * pi * a[au == "d"] / 360                                    # degrees >> radians
  a[au == "g"] <- 2 * pi * a[au == "g"] / 400                                    # gradians >> radians
  a[au == "p"] <- 2 * pi * a[au == "p"]                                          # proportion of revolution >> radians
  a
}

#' @rdname int_calc_angle
int_calc_a2s <- function(a, au) {
  if (uj::isNa(a)) {return(NA)}
  a <- int_calc_a2rad(a, au)                                                     # convert to radians
  sin(a) / cos(a)                                                                # slope is rise over run
}

#' @rdname int_calc_angle
int_calc_s2a <- function(s) {
  if (uj::isNa(s)) {return(NA)}
  atan(s)
}
