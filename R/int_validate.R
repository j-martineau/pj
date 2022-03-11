#' @name int_validate
#' @description Evaluates whether an object is a valid pj object or whether pj
#'   function arguments are valid for the type of plot element specified
#' @param x an object.
#' @param type character scalar indicating the type of plot element (type of
#'   region, type of shape, type of look, type of mod) the calling function
#'   deals with.
#' @param ... named arguments to the calling function.
#' @param gens. positive whole number scalar indicating how many generations
#'   back in the call stack the originating calling function is.
#' @return \code{int_validate_plot} returns a logical scalar.
#'   \code{int_validate_args} returns \code{NULL} as it is called for the side
#'   effect of validating arguments.
int_validate_plot <- function(x) {
  B  <- pj.$blank                                                                # blank pj object
  N  <- length(B)                                                                # number of tibbles in the blank pj object
  NX <- names(x); NB <- names(B)                                                 # element names for comparison
  if (!("pj" %in% class(x))) {return(F)}                                         # if not of class 'pj', not valid
  if (!uj::is_vlist(x)     ) {return(F)}                                         # if not a vlist, not valid
  if (uj::notID(NX, NB)    ) {return(F)}                                         # if any element names don't match, not valid
  if (uj::notID(colnames(x$element), colnames(B$element))) {return(F)}           # if the {element} tibbles don't match, not valid
  for (i in 2:N) {if (!uj::is_vlist(x[[i]])) {return(F)}}                        # for each remaining element > if not a vlist, not valid
  T                                                                              # passes all tests, so valid
}

#' @rdname int_validate
int_validate_args <- function(type, ..., gens. = 0) {
  N <- ...names()                                                                # names of argumemnts to be validated
  for (I in 1:...length()) {uj::run(paste0(N[I], " <- ..", I))}                  # for each {...} argument, save it by name
  P <- pj.$patterns[[type]]                                                      # valid patterns of argument characteristics for the element type
  P <- as.matrix(P)                                                              # as a matrix
  P <- P[ , colnames(P) %in% N, drop = F]                                        # only the relevant columns (matching arg names)
  R <- matrix(NA, nrow = nrow(P), ncol = ncol(P))                                # initialize result
  V <- uj::uv(P)                                                                 # unique, non-NA values from pattern matrix
  for (VV in V) {
    cat("\nRUN COMMAND:", VV)
    if (!VV == "")
    R[P == VV] <- uj::run(VV)
  }                                      # run each command from pattern matrix, store result in relevant cells
  OK <- apply(R, 1, all)                                                         # whether all values of each row of the result matrix are TRUE
  if (any(OK)) {return(NULL)}                                                    # if any row is all TRUE, a pattern has been met
  Call <- uj::callers(gens. + 1)                                                 # otherwise, get the relevant calling function
  N    <- paste0(N, collapse = ", ")                                             # comma list of names of args in {...}
  uj::bank_err("Characteristics of arguments {", N, "} are not among the valid patterns listed in the help documentation for the function {", Call, "}.", gens. = gens. + 1)
  uj::err_check(gens. = gens.)
  NULL
}
