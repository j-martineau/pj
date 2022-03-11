#' @name int_new_shapes
#' @description Add a shape to a pj object.
#' @section Using \code{look} to make shapes appear: By itself, this function
#'   simply defines the location of x-y pairs in the assigned region for the
#'   specified shape. Various looks can be added to the shape using the
#'   \code{look} parameter or via the \code{\link{add_ends}},
#'   \code{\link{add_fills}}, \code{\link{add_glyphs}},
#'   \code{\link{add_labels}}, \code{\link{add_lines}}, and
#'   \code{\link{add_marks}} functions. Sub-lists of the look argument must be
#'   named \code{'end'}, \code{'fill'}, \code{'glyph'}, \code{'label'},
#'   \code{'line'}, and/or \code{'mark'} to indicate that line end marks, fill
#'   colors, glyphs (single characters), labels, lines, and/or point marks be
#'   added to the shape.
#' @section Using \code{mod} to transform shapes: Shapes can be transformed in
#'   four ways: dilation, reflection, rotation, and translation/. using the
#'   \code{mod} argument or the \code{\link{mod_dilate}},
#'   \code{\link{mod_reflect}}, \code{\link{mod_rotate}}, and
#'   \code{\link{mod_translate}}. Sub-lists of the \code{mod} argument must be
#'   named \code{'dilate'}, \code{'reflect'}, \code{'rotate'}, and/or
#'   \code{'translate'} to indicate that a dilation, reflection, rotation, and
#'   or translation be applied to the shape.
#' @param pj an object of class \code{'pj'}.
#' @param type character scalar giving the type of shape to be added.
#' @param region either character scalar \code{"."} to indicate the most
#'   recently used region, a positive integer to indicate the ID number of a
#'   region (\code{0} indicates the background region), or a character scalar to
#'   indicate the name of a region (\code{'bg'} indicates the background
#'   region).
#' @param name character scalar indicating a name for the shape. The special
#'   value \code{"."} indicates that the pj package create a default name for
#'   the shape.
#' @param look an optional list containing up to four lists specifying looks to
#'   apply to the shape. See the \emph{using \code{look} to make shapes appear}
#'   section.
#' @param mod an optional list containing up to four lists specifying
#'   modifications to apply to the shape. See the \emph{using \code{mod} to
#'   transform shapes} section.
int_new_shapes <- function(pj, type, region, name, look, mod, ...) {
  int_validate_args(type = type, ..., gens. = 1)
  uj::err_check(gens. = 1)
  Names   <- ...names()
  N       <- ...length()
  ID      <- int_next_id(pj, "shape")
  region  <- int_get_id(pj, "region", name)
  Name    <- int_new_shape_name(pj, region, ID, type, name)
  Element <- tibble::tibble_row(class = "shape", type = type, id = ID, name = Name, parent.id = region)
  pj$element <- rbind(pj$element, Element)
  Args    <- paste0(Names, " = ...elt(", 1:N, ")")
  Args    <- paste0(Args, collapse = ", ")
  Command <- paste0("list(", ID, " = list(", Args, "))")
  cat("\nRUN COMMAND:", Command)
  Shape   <- uj::run(Command)
  pj[[type]] <- c(pj[[type]], Shape)
  pj <- int_add_looks_with_shapes(pj, ID, look)
  pj <- int_add_mods_with_shapes( pj, ID, mod )
  int_update_ids(pj, "shape", ID, new = T)
}
