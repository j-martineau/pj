#' @rdname int_calc_pad
#' @param p Proportion vector of length 1 (all pads), 2 (horizontal and
#'   vertical), or 4 (left, right, bottom, top).
#' @param w,h Region width and height.
int_calc_lpad <- function(p, w, h) {if (length(p) == 1) {p * min(w, h)} else {p[1] * w}}

#' @rdname int_calcpad
int_calc_rpad <- function(p, w, h) {if (length(p) == 1) {p * min(w, h)} else if (length(p) == 2) {p[1] * w} else {p[2] * w}}

#' @rdname int_calcpad
int_calc_bpad <- function(p, w, h) {if (length(p) == 1) {p * min(w, h)} else {p[3] * h}}

#' @rdname int_calcpad
int_calc_tpad <- function(p, w, h) {if (length(p) == 1) {p * min(w, h)} else if (length(p) == 2) {p[2] * w} else {p[4] * w}}
