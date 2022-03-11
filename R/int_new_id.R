#' @name int_new_id
#' @title Create new region, shape, look, or mod ID numbers
#' @param jp A jplot object.
#' @param type Character scalar from \code{c('region', 'shape', 'look', 'mod')}.
#' @return Non-negative whole number scalar.
int_new_id <- function(jp, type) {jp$next.ids[[type]]}
