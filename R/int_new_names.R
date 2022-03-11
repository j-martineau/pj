#' @name int_new_names
#' @description Create new region, shape, look, or mod names
#' @param type character scalar. The type of shape, look, or mod.
#' @param name character scalar. The special value \code{'.'} indicates that the
#'   pj package should create a default name.
#' @param names character vector of names already in use for the same type of
#'   plot element.
#' @param id positive whole number scalar givinig the ID number of the element
#'   for which a name is to be created.
#' @param shape positive whole number scalar giving the ID number of the shape
#'   to which a look or mod is to be applied.
#' @return Character scalar.
int_new_region_name <- function(pj, region.id, name) {
  if (name == ".") {name <- region.id}                                           # create default name if specified >> [region.id]
  if (name %in% pj$element$name) {                                               # if the name already exists
    uj::bank_err("Duplicate element name \"", name, "\".", gens. = 1)            # > bank and process
    uj::err_check(gens. = 1)                                                     # > ...an error
  }
  name
}

#' @rdname int_new_names
int_new_shape_name <- function(pj, region, id, type, name) {
  if (name == ".") {name <- paste0(region, "(", id , ".", type, ")")}            # if requested, bui;d default shape name  >> [region.id][shape.id.type]
  if (name %in% pj$element$name) {                                               # if the name already exists
    uj::bank_err("Duplicate element name \"", name, "\".", gens. = 1)            # > bank and process
    uj::err_check(gens. = 1)                                                     # > ...an error
  }
  name
}

#' @rdname int_new_names
int_new_look_name <- function(pj, region, shape, id, shape.type, type, name) {
  if (name == ".") {                                                             # if requested, bui;d default shape name
    name <- paste0(region, "(", shape, ".", shape.type, "(", id , ".", type , "))" )} #    >> [region.id][shape.id.type][look.id.type]
  if (name %in% pj$element$name) {                                               # if the name already exists
    uj::bank_err("Duplicate element name \"", name, "\".", gens. = 1)            # > bank and process
    uj::err_check(gens. = 1)                                                     # > ...an error
  }
  name
}

#' @rdname int_new_names
int_new_mod_name <- function(pj, region, shape, id, shape.type, type, name) {
  if (name == ".") {                                                             # if requested, bui;d default shape name
    name <- paste0(region, "(", shape, ".", shape.type, "(", id , ".", type , "))" )} #    >> [region.id][shape.id.type][look.id.type]
  if (name %in% pj$element$name) {                                               # if the name already exists
    uj::bank_err("Duplicate element name \"", name, "\".", gens. = 1)            # > bank and process
    uj::err_check(gens. = 1)                                                     # > ...an error
  }
  name
}
