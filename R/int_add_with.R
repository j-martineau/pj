#' @name int_add_with
#' @description Add a modification or look to a shape using the \code{mod}
#'   argument for a new shape
#' @param pj an object of class \code{'pj'}.
#' @param shape.id character scalar giving the unique ID of the shape to which a
#'   look or mod is to be applied.
#' @param shape.type character scalar giving the type of shape to which a look
#'   or mod is to be applied.
#' @param look an optional list containing up to seven sub-lists specifying
#'   looks to apply to the shape. Sub-lists must be named \code{'ci'},
#'   \code{'end'}, \code{'fill'}, \code{'glyph'}, \code{'label'}, \code{'line'},
#'   and/or \code{'mark'} to indicate that a confidence interval, line end
#'   marks, fill color, glyph (single letter), label, line color and styling, or
#'   a mark (symbol used for plotting points) be added to the shape.
#' @param mod an optional list containing up to four lists specifying mods to
#'   apply to the shape. Sub-lists must be named \code{'dilate'},
#'   \code{'reflect'}, \code{'rotate'}, and/or \code{'translate'} to indicate
#'   that a dilation, reflection, rotation, and or translation be added to the
#'   shape.
#' @return \code{pj} with the specified looks or mods applied to the specified
#'   shape.
int_add_mods_with_shapes <- function(pj, shape.id, mod = NULL) {
  if (is.null(mod)) {return(pj)}
  D <- mod$dilate; E <- mod$reflect; O <- mod$rotate; R <- mod$translate
  shape <- int_get_type(pj, shape.id)
  region.id  <- int_get_parent(pj, shape.id)
  if (shape != "refline") {
    if (!is.null(E)) {pj <- int_add_mod(pj = pj, region.id = region.id, shape.id = shape.id, shape = shape, type = "reflect"  , name = ".", ref = E$ref, x = E$x, y = E$y, a = E$a, i = E$i, s = E$s, y1 = E$y1, y2 = E$y2, au = E$au, gens. = 2)}
    if (!is.null(D)) {pj <- int_add_mod(pj = pj, region.id = region.id, shape.id = shape.id, shape = shape, type = "dilate"   , name = ".", x = D$x, y = D$y, mx = D$mx, my = D$my, xu = D$xu, yu = D$yu, gens. = 2)}
    if (!is.null(O)) {pj <- int_add_mod(pj = pj, region.id = region.id, shape.id = shape.id, shape = shape, type = "rotate"   , name = ".", ref = O$ref, x = O$x, y = O$y, a = O$a, au = O$au, gens. = 2)}
    if (!is.null(R)) {pj <- int_add_mod(pj = pj, region.id = region.id, shape.id = shape.id, shape = shape, type = "translate", name = ".", dx = R$dx, dy = R$dy, xu = R$xu, yu = R$yu, gens. = 2)}
  }
  else if (!is.null(R)) {uj::bank_err("Translation specifications are not valid mods for shapes of type 'refline'.", gens. = 2)}
  else if (!is.null(E)) {uj::bank_err("Reflection specifications are not valid mods for shapes of type 'refline'.", gens. = 2)}
  else if (!is.null(D)) {uj::bank_err("Dilation specifications are not valid mods for shapes of type 'refline'.", gens. = 2)}
  else if (!is.null(O)) {uj::bank_err("Rotation specifications are not valid mods for shapes of type 'refline'.", gens. = 2)}
  pj
}

#' @rdname int_add_with
int_add_looks_with_shapes <- function(pj, shape.id, look = NULL) {
  if (is.null(look)) {return(pj)}
  shape <- int_get_type(  pj, shape.id)
  region <- int_get_parent(pj, shape.id)
  PT   <- shape == "points"
  RL   <- shape == "reflines"
  RS   <- shape %in% c("reflines", "segments")
   C   <- look$ci    ;  E <- look$end   ;  I <- look$fill  ;  G <- look$glyph ;  L <- look$line  ;  A <- look$label ;  M <- look$mark
  iC   <- !is.null(C); iE <- !is.null(E); iI <- !is.null(I); iG <- !is.null(G); iL <- !is.null(L); iA <- !is.null(A); iM <- !is.null(M)
  eC   <- iC & !PT   ; eE <- iE &  RL   ; eI <- iI &  RS   ; eG <- iG &  RL   ;                    eA <- iA &  RL   ; eM <- iM &  RL
  vC   <- iC &  PT   ; vE <- iE & !RL   ; vI <- iI & !RS   ; vG <- iG & !RL   ; vL <- iL         ; vA <- iA & !RL   ; vM <- iM & !RL
  if (eC) {uj::bank_err("Confidence intervals are not valid looks for shapes of type '", shape, "'.", gens. = 2)}
  if (eI) {uj::bank_err("Fill colors are not valid looks for shapes of type 'refline' or 'segment.", gens. = 2)}
  if (eE) {uj::bank_err("End-marks are not valid looks for shapes of type 'refline'.", gens. = 2)}
  if (eG) {uj::bank_err("Glyphs are not valid looks for shapes of type 'refline'.", gens. = 2)}
  if (eA) {uj::bank_err("Labels are not valid looks for shapes of type 'refline'.", gens. = 2)}
  if (eM) {uj::bank_err("Marks are not valid looks for shapes of type 'refline'.", gens. = 2)}
  if (vI) {pj <- int_add_look(pj = pj, region.id = region.id, shape.id = shape.id, shape = shape, look = "fills" , name = ".", fc = I$fc, fa = I$fa, gens. = 2)}
  if (vL) {pj <- int_add_look(pj = pj, region.id = region.id, shape.id = shape.id, shape = shape, look = "lines" , name = ".", lc = L$lc, la = L$la, lt = L$lt, lw = L$lw, gens. = 2)}
  if (vC) {pj <- int_add_look(pj = pj, region.id = region.id, shape.id = shape.id, shape = shape, look = "ci"   , name = ".", m = C$m, dir = C$dir, type = C$type, d = C$d, neg = C$neg, pos = C$pos, gens. = 2)}
  if (vE) {pj <- int_add_look(pj = pj, region.id = region.id, shape.id = shape.id, shape = shape, look = "ends"  , name = ".", loc = E$loc, which = E$which, mark = E$mark, size = E$size, lc = E$lc, la = E$la, lw = E$lw, fc = E$fc, fa = E$fa, gens. = 2)}
  if (vM) {pj <- int_add_look(pj = pj, region.id = region.id, shape.id = shape.id, shape = shape, look = "marks" , name = ".", mark = M$mark, size = M$size, lc = M$lc, la = M$la, lw = M$lw, fc = M$fc, fa = M$fa, et = M$et, ec = M$ec, ea = M$ea, gens. = 2)}
  if (vG) {pj <- int_add_look(pj = pj, region.id = region.id, shape.id = shape.id, shape = shape, look = "glyphs", name = ".", glyph = G$glyph, size = G$size, lc = G$lc, la = G$la, lw = G$lw, et = G$et, ec = G$ec, ea = G$ea, a = G$a, au = G$au, dx = G$dx, dy = G$dy, xu = G$xu, yu = G$yu, gens. = 2)}
  if (vA) {pj <- int_add_look(pj = pj, region.id = region.id, shape.id = shape.id, shape = shape, look = "labels", name = ".", text = A$text, tc = A$tc, ta = A$ta, font = A$font, size = A$size, style = A$style, hj = A$hj, vj = A$vj, a = A$a, parse = A$parse, fc = A$fc, fa = A$fa, lc = A$lc, la = A$la, lt = A$lt, lw = A$lw, au = A$au, dx = A$dx, dy = A$dy, xu = A$xu, yu = A$yu, gens. = 2)}
  pj
}
