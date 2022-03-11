#' @name new_pj
#' @title Creates a new pj object
#' @description Set the minimum and maximum x and y values of a pj object
#'   background its fill color, line color, line width, and edge padding (where
#'   plotting does not occur).
#' @section Edge padding: Edge padding can be achieved in three ways using the
#'   \code{pad} argument as shown in the following table:
#' \tabular{llll}{
#'    \emph{edge}  \tab\emph{\code{pad = p}}\tab\emph{\code{pad = c(px, py)}}
#'                 \tab\emph{\code{pad = c(pl, pr, pb, pt)}}
#' \cr\emph{left}  \tab\code{p \* min(w, h)}\tab\code{px \* w}\tab\code{pl \* w}
#' \cr\emph{right} \tab\code{p \* min(w, h)}\tab\code{px \* w}\tab\code{pr \* w}
#' \cr\emph{bottom}\tab\code{p \* min(w, h)}\tab\code{py \* h}\tab\code{pb \* h}
#' \cr\emph{top}   \tab\code{p \* min(w, h)}\tab\code{py \* h}\tab\code{pt \* h}
#' }
#' @param xmin numeric scalar giving the minimum x-value for plotting objects in
#'   the background region.
#' @param ymin numeric scalar giving the minimum y-value for plotting objects in
#'   the background region.
#' @param xmax numeric scalar giving the maximum x-value for plotting objects in
#'   the background region.
#' @param ymax numeric scalar giving the maximum y-value for plotting objects in
#'   the background region.
#' @param w positive numeric scalar giving the width of the background in
#'   inches, centimeters, or millimeters.
#' @param h positive numeric scalar giving the height of the background in
#'   inches, centimeters, or millimeters.
#' @param pu character scalar defining units in which \code{w} and \code{h} are
#'   expressed. Valid values are \code{'in'}, \code{'cm'}, and \code{'mm'} for
#'   inches, centimeters, and millimeters.
#' @param pad proportion vector of length 1, 2, or 4 (see details section) with
#'   value no larger than 0.25.
#' @param fc character scalar fill color value.
#' @param lc character scalar line color value.
#' @param lw positive numeric scalar line width (default = 1).
#' @return A pj object (a list of specific tibbles).
#' @export
new_pj <- function(xmin, xmax, ymin, ymax, w = 6.5, h = 4.5, pad = 0.01, pu = "in", fc = "grey95", lc = "black", lw = 1) {
  int_validate_args(type = "region", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, le = 0, re = 0, be = NA, te = NA, w = w, h = h, pad = pad, pu = pu, fc = fc, fa = 1, lc = lc, la = 1, lw = lw, lt = "-")
  uj::err_check()                                                                # process any validation errors
  pj <- pj.$blank                                                                # get a blank pj object
  pj$region <- list(region.0 = list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, le = 0, re = 0, be = NA, te = NA, w = w, h = h, pad = pad, pu = pu, fc = fc, fa = 1, lc = lc, la = 1, lw = lw, lt = "-"))
  class(pj) <- "pj"                                                              # set its class to 'pj'
  pj
}
