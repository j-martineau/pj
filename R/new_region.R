#' @name new_region
#' @title Create a new plotting region contained in the background region
#' @description Locate a new plotting region within the background region, set
#'   its units relative to the background region, set its min and max x and y
#'   values, put a pad between the edges of the region and available area for
#'   plotting, and apply fill and line looks.
#' @inheritSection new_pj Edge padding
#' @section The lt argument: The \code{lt} argument can take on the following
#'   values to indicate the following line types:\tabular{ll}{
#'     VALUE      \tab LINE TYPE          \cr
#'     \code{'-'} \tab solid              \cr
#'     \code{':'} \tab dotted             \cr
#'     \code{'='} \tab dashed             \cr
#'     \code{':='}\tab dot-dashed         \cr
#'     \code{'L='}\tab long-dashed        \cr
#'     \code{'2='}\tab double-width dashed}
#' @param pj an object of class 'pj'.
#' @param xmin a numeric scalar giving the minimum x-value for plotting objects
#'   in the region.
#' @param ymin a numeric scalar giving the minimum y-value for plotting objects
#'   in the region.
#' @param xmax a numeric scalar giving the maximum x-value for plotting objects
#'   in the region.
#' @param ymax a numeric scalar giving the maximum y-value for plotting objects
#'   in the region.
#' @param w \code{NA} or a positive numeric scalars giving the width of the
#'   region either in x-units of the background region or as a proportion of the
#'   width of the background region that is available for plotting.
#' @param h \code{NA} or a positive numeric scalars giving the height of the
#'   region either in x-units of the background region or as a proportion of the
#'   height of the background region that is available for plotting.
#' @param le \code{NA} or a numeric scalar locating the left edge of the region
#'   either in x units of the background region or as a proportion of the
#'   distance from the left to right edge of the area of the background region
#'   available for plotting.
#' @param re \code{NA} or a numeric scalar locating the right edge of the region
#'   either in x units of the background region or as a proportion of the
#'   distance from the left to right edge of the area of the background region
#'   available for plotting.
#' @param be \code{NA} or a numeric scalar locating the bottom edge of the
#'   region either in y units of the background region or as a proportion of the
#'   distance from the bottom to top edge of the area of the background region
#'   available for plotting.
#' @param te \code{NA} or a numeric scalar locating the top edge of the region
#'   either in y units of the background region or as a proportion of the
#'   distance from the bottom to top edge of the area of the background region
#'   available for plotting.
#' @param pad proportion vector of length 1, 2, or 4 (see the \emph{edge
#'   padding} section) with value no larger than 0.25.
#' @param fc \code{NA} or a character scalar region fill color value.
#' @param lc \code{NA} or a character scalar region line color value.
#' @param fa \code{NA} or a proportion scalars (i.e., between 0 and 1,
#'   inclusive) indicating the alpha opacity level of the region fill color.
#' @param la \code{NA} or a proportion scalars (i.e., between 0 and 1,
#'   inclusive) indicating the alpha opacity level of the region line color.
#' @param lt \code{NA} or a character scalar giving line type. Valid values are
#'   \code{c("-", ":", "=", ":=", "L=", "2=")}. See \code{the lt argument}
#'   section.
#' @param lw \code{NA} or a positive numeric scalar line width indicating a
#'   multiple of the default line width (i.e., default = 1).
#' @param name character scalar name for the new region. The special value
#'   \code{'.'} indicates that the pj package should create a name for the new
#'   region.
#' @return \code{pj} with the addition of the specified region.
#' @export
new_region <- function(pj, xmax, xmin, ymax, ymin, le = NA, be = NA, re = NA, te = NA, w = NA, h = NA, fc = 'white', fa = 0.5, lc = 'black', la = 0.5, lt = '-', lw = 1, name = '.') {
  int_validate_args(type = "region", xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, le = le, be = be, re = re, te = te, w = w, h = h, fc = fc, fa = fa, lc = lc, la = la, lt = lt, lw = lw)
  uj::err_check()                                                                # process any errors
  ID      <- int_next_id(pj, "region")                                           # get the next-up region ID
  Name    <- int_new_region_name(pj, ID, name)                                   # generate the name of the new region
  Element <- tibble::tibble_row(class = "region", type = "layer", id = ID, name = Name, parent.id = "region.0")
  pj$element <- rbind(pj$element, Element)
  Command <- paste0("list(", ID, " = list(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, le = le, be = be, re = re, te = te, w = w, h = h, fc = fc, fa = fa, lc = lc, la = la, lt = lt, lw = lw))")
  cat("\nRUN COMMAND:", Command)
  Region  <- uj::run(Command)
  pj$region <- c(pj$region, Region)                                              # append it to the regions list of {pj}
  int_update_ids(pj, "region", ID, new = T)                                      # update the most recent modified region ID
}

