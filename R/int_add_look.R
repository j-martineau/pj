#' @name int_add_look
#' @param pj an object of class \code{'pj'}.
#' @param region.id character scalar giving the unique ID of the region
#'   containing the shape to which a look is to be applied.
#' @param shape.id character scalar giving the unique ID of the shape to which
#'   a look should be applied. Use \code{"."} to indicate the most
#'   recently modified shape.
#' @param shape character scalar indicating the type of shape to which the
#'   look will be applied.
#' @param type character scalar indicating the type of look to be applied.
#' @param  character scalar name to give to the specified look. The special
#'   value \code{"."} indicates that the pj package should create a default name
#'   for the look.
#' @param ... optional look arguments.
#' @param gens. number of generations back in the call stack to go for error
#'   checking.
#' @return \code{pj} with the specified look added to the specified shape.
int_add_look <- function(pj, region.id, shape.id, shape, type, name = ".", ..., gens. = 1) {
  int_validate_args(type = type, ..., gens. = gens. + 1)                         # validate arguments
  Names    <- ...names()                                                         # ... argument names
  N        <- ...length()                                                        # number of ... arguments
  ID       <- int_next_id(pj, "look")                                            # new look ID
  shape.id <- int_get_id(pj, "shape", shape.id)                                  # get ID of shape too add look to
  Name     <- int_new_look_name(type, region.id, shape.id, ID, shape, type, name) # new look name
  Element  <- tibble::tibble_row(class = "look", type = type, id = ID, name = Name, parent.id = shape.id)
  if (is.null(pj$element)) {pj$element <- Element}
  Args     <- paste0(Names, " = ...elt(", 1:N, ")")
  Args     <- paste0(Args, collapse = ", ")
  Command  <- paste0("list(", ID, " = list(", Args, "))")
  cat("\nRUN COMMAND:", Command)
  Look     <- uj::run(Command)
  if (is.null(pj[[type]])) {pj[[type]] <- Look}
  else                     {pj[[type]] <- c(pj[[type]], Look)}
  int_update_ids(pj, "look", ID, new = T)
}
