#' @name int_ids
#' @description Manage plot element ID numbers.
#' @param pj an object of class \code{'pj'}.
#' @param class character scalar from \code{c('region', 'shape', 'look', 'mod')}.
#' @param ids non-negative whole-number vector of object ID numbers to search.
#' @param names character vector of object names to search.
#' @param search non-negative whole number scalar or character scalar to search
#'   for. The special value \code{'.'} indicates the most recently created or
#'   modified object of class \code{class}.
#' @param new logical scalar indicating whether the function was called from
#'   a function that creates a new region, shape, look, or mod.
#' @return \code{int_get_id} returns a non-negative whole number scalar object
#'   ID number.
#'   \cr\cr
#'   \code{int_curr_id} returns the ID number of the most recently modified
#'   element of class \code{class}.
#'   \cr\cr
#'   \code{int_next_id} returns the next-up ID number for a new element of class
#'   \code{class} to be added to the object \code{pj}.
#'   \cr\cr
#'   \code{int_update_current_id} updates \code{pj} to store the ID number of
#'   the plot element of class \code{class} that was most recently
#'   created/modified.
#'   \cr\cr
#'   \code{int_update_next_id} udpates \code{pj} to store the next-up ID number
#'   to be assigned to a new plot element of class \code{class}.
int_get_id <- function(pj, class, search) {
  if (search == "background") {return(search)}
  if (search == ".") {return(pj$curr.ids[[class]])}                              # if the most recent is specified, get that ID
  i <- pj$element$class == class
  Names <- pj$element$name[i]
  IDs   <- pj$element$id[  i]
  if (search %in% IDs  ) {return(search)}
  if (search %in% Names) {return(IDs[Names == search])}
  uj::bank_err("Invalid ", class, "identifier {'", search, "'}.", gens. = 2)
  uj::err_check(gens. = 2)
}

#' @rdname int_ids
int_next_id <- function(pj, class) {paste0(class, ".", pj$next.ids[[class]])}

#' @rdname int_ids
int_update_ids <- function(pj, class, id, new = F) {
  pj$curr.ids[[class]] <- id
  if (new) {pj$next.ids[[class]] <- pj$next.ids[[class]] + 1}
  pj
}

#' @rdname int_ids
int_get_type <- function(pj, id) {pj$element$type[pj$element$id == id]}

#' @rdname int_ids
int_get_parent <- function(pj, id) {pj$element$parent[pj$element$id == id]}
