#' @name int_add_mod
#' @param pj an object of class \code{'pj'}.
#' @param region.id character scalar giving the unique ID of the region
#'   containing the shape to which a mod is to be applied.
#' @param shape.id character scalar giving the unique ID of the shape to which
#'   a mod should be applied. Use \code{"."} to indicate the most
#'   recently modified shape.
#' @param shape character scalar indicating the type of shape to which the
#'   mod will be applied.
#' @param type character scalar indicating the type of mod to be applied.
#' @param name character scalar name to give to the specified mod. The special
#'   value \code{"."} indicates that the pj package should create a default name
#'   for the mod.
#' @param ... optional mod arguments.
#' @param gens. number of generations back in the call stack to go for error
#'   checking.
#' @return \code{pj} with the specified mod added to the specified shape.
int_add_mod <- function(pj, region.id, shape.id, shape, type, name = ".", ..., gens. = 1) {
  int_validate_args(type = type, ..., gens. = gens. + 1)                         # validate arguments
  Names   <- ...names()                                                          # ... argument names
  N       <- ...length()                                                         # number of ... arguments
  ID      <- int_next_id(pj, "mod")                                              # new mod ID
  shape   <- int_get_id(pj, "mod", shape)                                        # get ID of shape too add mod to
  Name    <- int_new_mod_name(type, region.id, shape.id, ID, shape, type, name)  # new mod name
  Element <- tibble::tibble_row(class = "mod", type = type, id = ID, name = Name, parent = shape.id)
  if (is.null(pj$element)) {pj$element <- Element}
  else {pj$element <- rbind(pj$element, Element)}
  Args    <- paste0(Names, " = ...elt(", 1:N, ")")
  Args    <- paste0(Args, collapse = ", ")
  Command <- paste0("list(", ID, " = list(", Args, "))")
  cat("\nRUN COMMAND:", Command)
  Mod     <- uj::run(Command)
  if (is.null(pj[[type]])) {pj[[type]] <- Mod}
  else {pj[[type]] <- c(pj[[type]], Mod)}
  int_update_ids(pj, "mod", ID, new = T)
}
