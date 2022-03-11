#' @name add_ci
#' @family add_look
#' @title Add a confidence interval look to a shape.
#' @description Confidence interval looks can only be added to point shapes.
#' @section Types of confidence intervals:
#'   \cr\emph{line}
#'   \cr A line confidence interval is simply a line between the minimum point
#'   of a confidence interval and its maximum point.
#'   \cr\cr
#'   \emph{cap}
#'   \cr A capped-line confidence interval is a line interval with a cap at the
#'   ends of the line.
#'   \cr\cr
#'   \emph{taper}
#'   \cr A tapered confidence interval is a thin polygon thickest at the middle
#'   of the confidence interval and narrowing to zero width at the lower and
#'   upper bounds of the interval.
#'   \cr\cr
#'   \emph{ribbon}
#'   \cr A ribbon confidence interval is a polygon defined by the area between
#'   minimum and maximum values of confidence intervals for a sorted series of
#'   points.
#' @section The d, pos, and neg arguments: Either \code{d} must be supplied or
#'   both \code{pos} and \code{neg} must be supplied.
#' @section Valid combinations of argument properties:
#'   Intervals can be validly described using a number of patterns of argument
#'   properties as follows: \describe{
#'     \item{\code{x + y + m + dir + type + d}}{symmetric confidence interval of
#'           width \code{2dm}}.
#'     \item{\code{x + y + m + dir + type + neg + pos}}{non-symmetric confidence
#'           interval of width \code{m(pos + neg)}}.
#'   }
#' @section Argument recycling: The argument \code{d} or the arguments
#'   \code{pos} and \code{neg} are recycled with the values of \code{x} and
#'   \code{y} associated with the points shape the confidence interval is to be
#'   applied to.
#' @param m positive numeric vector of length recyclable with \code{x} and
#'   \code{y} defining the multiplier to apply to confidence interval distances
#'   in \code{d} or \code{pos} and \code{neg}.
#' @param dir \code{'h'} for horizontal intervals or \code{'v'} for vertical
#'   intervals.
#' @param type \code{'line'}, \code{'cap'}, \code{'taper'}, or \code{'ribbon'}.
#'   See the \emph{types of confidence intervals} section.
#' @param d \code{NA} or a positive numeric vector of length recyclable with
#'   \code{x} and \code{y} defining the symmetric positive/negative error
#'   magnitude at each midpoint. See \code{the d, pos, and neg arguments}
#'   section.
#' @param pos \code{NA} or positive numeric vector of length recyclable with
#'   \code{x} and \code{y} defining positive error magnitude at each midpoint.
#'   See \code{the d, pos, and neg arguments} section.
#' @param neg \code{NA} or positive numeric vectors of length recyclable with
#'   \code{x} and \code{y} defining negative error magnitude at each midpoint.
#'   See \code{the d, pos, and neg arguments} section.
#' @inheritParams int_add_look
#' @return \code{pj} with the addition of the confidence interval look applied
#'   to the specified points shape.
#' @export
add_ci <- function(pj, m, type = 'line', dir = 'v', d = NA, pos = NA, neg = NA, shape = '.', name = '.') {
  shape.type <- int_get_type(pj, shape)
  region <- int_get_parent(pj, shape)
  int_add_look(pj, region.id = region, shape.id = shape, shape = shape.type, name = name, type = 'ci', m = m, dir = dir, d = d, pos = pos, neg = neg)
}

#' @name add_ends
#' @family add_look
#' @title Add an ends look to a shape.
#' @description An ends look is an end mark at the start, midpoint, and/or end
#'   of a line segment. End marks can be line-based arrows, triangle-based
#'   arrows, circles, diamonds, and squares.
#' @section Valid combinations of argument properties: Intervals can be validly
#'   described using a number of patterns of argument properties as follows,
#'   where \code{base} indicates that the argument set \code{(loc, which, size)}
#'   have all been supplied, \code{line} indicates that the argument set
#'   \code{(lc, la, lw)} have all been supplied, and \code{fill} indicates that
#'   the argument set \code{(fc, fa)} have both been supplied. \describe{
#'     \item{\code{base + mark = ">" + line}}{line-based arrowheads as end
#'           mark.}
#'     \item{\code{base + mark = "t" + line}}{empty triangle-based arrowheads as
#'           end mark.}
#'     \item{\code{base + mark = "t" + fill}}{solid triangle-based arrowheads as
#'           end mark.}
#'     \item{\code{base + mark = "t" + line + fill}}{filled and outlined
#'           triangle-based arrowheads as end mark.}
#'     \item{\code{base + mark = "c" + line}}{empty circle as end mark.}
#'     \item{\code{base + mark = "c" + fill}}{solid circle as end mark.}
#'     \item{\code{base + mark = "c" + line + fill}}{filled and outlined circle
#'           as end mark.}
#'     \item{\code{base + mark = "d" + line}}{empty diamond as end mark.}
#'     \item{\code{base + mark = "d" + fill}}{solid diamond as end mark.}
#'     \item{\code{base + mark = "d" + line + fill}}{filled and outlined diamond
#'           as end mark.}
#'     \item{\code{base + mark = "s" + line}}{empty square as end mark.}
#'     \item{\code{base + mark = "s" + fill}}{solid square as end mark.}
#'     \item{\code{base + mark = "s" + line + fill}}{filled and outlined square
#'           as end mark.}
#'   }
#' @section Argument recycling: The arguments \code{mark}, \code{size},
#'   \code{lc}, \code{la}, \code{lw}, \code{fc}, and/or \code{fa} are recycled
#'   with the number of segments between x-y pairs of the shape to which the
#'   end-mark look is applied.
#' @param loc character scalar giving the location of end marks on line
#'   segments. \code{'s'} indicates an end mark at the starts of segments,
#'   \code{'e'} indicates an end mark at the ends of segments, \code{'b'}
#'   indicates end marks at both ends of segments, and \code{'m'} indicates end
#'   marks at the midpoints of segments.
#' @param which character scalar indicating which segments in a series of x-y
#'   pairs should be given end marks. \code{'f'} indicates the first segment
#'   only, \code{'l'} indicates the last segment only, \code{'b'} indicates
#'   first and last segments only and \code{'a'} indicates all segments.
#' @param mark character scalar giving the type of end mark to plot. \code{'>'}
#'   indicates a line-only arrowhead end mark. \code{'t'} indicates an
#'   outlineable and fillable (triangular) arrrowhead end mark.\code{'c'}
#'   indicates an outlineable and fillable circular end mark, \code{'d'}
#'   indicates an outlineable and fillable diamond end mark. Finally \code{'s'}
#'   indicates an outlineable and fillable square end mark, and
#' @param size positive numeric vector indicating symbol size relative to the
#'   default (thus 1 indicates default sized, 1/2 indicates half sized, and 2
#'   indicates double sized). For characters as marks, 1 indicates point size
#'   10.
#' @param lc \code{NA} or a character vector containing valid color
#'   representations giving the color of lines used to draw end marks.
#' @param la \code{NA} or a proportion vector (i.e., between 0 and 1 inclusive)
#'   indicating the alpha opacity of the lines used to draw arrow marks.
#' @param lw \code{NA} or a positive numeric vector indicating proportional line
#'   width relative to the default giving the width of lines used to draw arrow
#'   marks.
#' @param fc \code{NA} or a character vector containing valid color
#'   representations giving the color used to fill end marks.
#' @param fa \code{NA} or a numeric proportion vector (i.e., between 0 and 1
#'   inclusive) indicating the alpha opacity of the arrow-mark fill color (0 =
#'   fully transparent, 0.5 = translucent, 1 = fully opaque).
#' @inheritParams int_add_look
#' @return \code{pj} with the addition of the end-mark look applied to the
#'   specified shape.
#' @export
add_ends <- function(pj, segs = 'a', ends = 'e', mark = '>', size = 1, fc = 'white', fa = 0.5, lc = 'black', la = 0.5, lw = 1, shape = '.', name = '.') {
  shape.type <- int_get_type(pj, shape)
  region <- int_get_parent(pj, shape)
  int_add_look(pj, region.id = region, shape.id = shape, shape = shape.type, type = 'ends', name = name, segs = segs, ends = ends, mark = mark, size = size, fc = fc, fa = fa, lc = lc, la = la, lw = lw)
}

#' @name add_fills
#' @family add_look
#' @title Add fill looks to a shape.
#' @description Not applicable for shapes of type \code{'refline'} or
#'   \code{'segment'}. Adding a fill look to a point shape will effectively turn
#'   the point shape into a free polygon.
#' @section Argument recycling: The arguments \code{fc} and \code{fa} are
#'   recycled with the number of polygons contained by the shape to which the
#'   fill look is applied.
#' @param fc character vector containing valid color representations. Length
#'   must be consistent with the shape a fill look is being added to.
#' @param fa proportion vector (i.e., between 0 and 1 inclusive) indicating the
#'   alpha opacity of the fill color (0 = fully transparent, 0.5 = translucent,
#'   1 = fully opaque).
#' @inheritParams int_add_look
#' @return \code{pj} with the addition of the fill look applied to the specified
#'   shape.
#' @export
add_fills <- function(pj, fc = 'white', fa = 0.5, shape = '.', name = '.') {
  shape.type <- int_get_type(pj, shape)
  region <- int_get_parent(pj, shape)
  int_add_look(pj, region.id = region, shape.id = shape, shape = shape.type, type = 'fills', name = name, fc = fc, fa = fa)
}

#' @name add_glyphs
#' @family add_look
#' @title Add a glyphs look to a shape.
#' @description A glyph is a single character. A glyphs look is a glyph marker
#'   for each specific x-y location of a shape. Not applicable for shapes of
#'   type \code{'refline'}.
#' @section Valid combinations of argument properties: Glyphs can be validly
#'   described using two patterns of argument properties as follows,
#'   where \code{base} indicates that the argument set \code{{mark, size, lc,
#'   la}} have all been supplied and \code{enclosure} indicates that the
#'   argument set \code{{et, ec, ea}} have all been supplied. \describe{
#'     \item{\code{base}}{The glyph in \code{mark} is printed without any
#'           enclosure}.
#'     \item{\code{base + enclosure}}{The glyph in \code{mark} is printed within
#'           an enclosure}.}
#' @section Argument recycling: The arguments \code{mark}, \code{size},
#'   \code{lc}, \code{la}, \code{lw}, \code{et}, \code{ec}, and/or \code{ea} are
#'   recycled with the number of x-y pairs used to draw the shape to which the
#'   glyph look is applied
#' @param glyph character vector of glyphs to mark x-y coordinates. Each glyph
#'   must be a single character (which is interpreted literally). The only
#'   exception is when \code{mark = "."}, in which case, a small dot is plotted.
#' @param size positive numeric vector giving glyph text size in points.
#' @param gc \code{NA} or a character vector indicating the glyph text color.
#' @param ga \code{NA} or a proportion vector (i.e., between 0 and 1 inclusive)
#'   indicating the alpha opacity of glyph text colors. 0 = fully transparent,
#'   0.5 = translucent, and 1 = fully opaque.
#' @param et character vector indicating the type of enclosure with which to
#'   surround symbols in \code{mark}. Values of \code{NA} indicates no
#'   enclosure. Values of \code{'c'}, \code{'d'} or \code{'s'} indicate an empty
#'   circle, diamond, or square, respectively. \code{'C'}, \code{'D'} or
#'   \code{'S'} indicate a solid circle, diamond, or square, respectively.
#' @param ec \code{NA} or a character vector indicating the colors of (optional)
#'   mark enclosures.
#' @param ea \code{NA} or a  proportion vector (i.e., between 0 and 1 inclusive)
#'   indicating the alpha opacity of (optional) mark enclosure colors. 0 = fully
#'   transparent, 0.5 = translucent, and 1 = fully opaque.
#' @inheritParams int_add_look
#' @return \code{pj} with the addition of the glyph look applied to the
#'   specified shape.
#' @export
add_glyphs <- function(pj, glyph = '.', size = 10, gc = 'black', ga = 0.5, et = NA, ec = NA, ea = NA, shape = '.', name = '.') {
  shape.type <- int_get_type(pj, shape)
  region <- int_get_parent(pj, shape)
  int_add_look(pj, region.id = region, shape.id = shape, shape = shape.type, type = 'glyphs', name = name, glyph = glyph, size = size, gc = gc, ga = ga, et = et, ec = ec, ea = ea)
}

#' @name add_labels
#' @family add_look
#' @title Add a labels look to a shape.
#' @description Not applicable for shapes of type \code{'refline'}.
#' @section Valid combinations of argument properties: Labels can be validly
#'   described using patterns of argument properties as follows, where
#'   \code{base} indicates that the argument set \code{(text, tc, ta, font,
#'   size, style, hj, vj, a, parse, au, dx, dy)} have all been supplied,
#'   \code{line} indicates that the argument set \code{(lc, la, lt)} have all
#'   been supplied. and \code{fill} indicates that the argument set \code{(fc,
#'   fa)} have both been supplied. \describe{
#'     \item{\code{base}}{The label \code{text} is printed without any
#'           enclosure}.
#'     \item{\code{base + line}}{The label in \code{text} is printed in an
#'           outlined enclosure}.
#'     \item{\code{base + fill}}{The label in \code{text} is printed in a filled
#'           enclosure}.
#'     \item{\code{base + line + fill}}{The label in \code{text} is printed in
#'           an outlined and filled enclosure}.}
#' @section Argument recycling: The arguments \code{text}, \code{tc}, \code{ta},
#'   \code{font}, \code{size}, \code{style}, \code{hj}, \code{vj}, \code{a},
#'   \code{parse}, \code{fc}, \code{fa}, \code{lc}, \code{la}, \code{lt},
#'   \code{lw}, \code{au}, \code{dx}, and/or \code{dy} are recycled with the
#'   number of \code{x-y} pairs used to draw the shape to which the label look
#'   is applied
#' @param text character vector containing labels to add to x-y locations of
#'   shapes. Special values are \code{'@@xX'} for printing the x values with
#'   \code{X} decimal point; \code{'@@yY'} for printing the y values with
#'   \code{Y} decimal points, and \code{'@@xXyY'} to print the x and y values
#'   with \code{X} and \code{Y} decimal points respectively. \cr\cr \code{X} and
#'   \code{Y} can be any digit from 0 to 9. For example, if the x and y values
#'   of a point were \code{pi} and \code{e}, the following labels wquld be
#'   printed at the coordinate \code{(pi, e)}: \code{'@@x0'} would print
#'   \code{'3'}, \code{'@@y8'} would print \code{'2.71828183'}, and
#'   \code{'@@x2y3'} would print \code{'(3.14, 2.718)'}
#' @param font character vector giving label fonts. Valid values are
#'   \code{'mono'}, \code{'sans'}, \code{'serif'}, \code{'American Typewriter'},
#'   \code{'Arial'}, \code{'Bradley Hand ITC'}, \code{'Calibri'}, \code{'Comic
#'   Sans MS'}, \code{'Courier'}, \code{'Courier New'}, \code{'Garamond'},
#'   \code{'Geneva'}, \code{'Helvetica'}, \code{'Impact'}, \code{'Monaco'},
#'   \code{'Palatino'}, \code{'Symbol'}, \code{'Tahoma'}, \code{'Times'},
#'   \code{'Times New Roman'}, \code{'Trebuchet MS'}, and \code{'Verdana'}.
#'   \cr\cr \code{'mono'} uses the default monospaced font for the device/R
#'   configuration. \code{'sans'} uses the default sans serif font for the
#'   device/R configuration. \code{'serif'} uses the default serif font for the
#'   device/R configuration.
#' @param size positive numeric vector giving text size in points.
#' @param style character vector giving text style. \code{'p'} indicates plain,
#'   \code{'b'} indicates bold, \code{'i'} indicates italic, and \code{'bi'}
#'   indicates bold italic.
#' @param tc character vector giving valid text color specifications for labels.
#' @param ta proportion vector (i.e., between 0 and 1 inclusive) indicating the
#'   alpha opacity of the text color (0 = fully transparent, 0.5 = translucent,
#'   1 = fully opaque).
#' @param hj,vj numeric proportion vectors (i.e., all values are between 0 and 1
#'   inclusive) giving the horizontal justification and vertical justification,
#'   respectively. \code{0} indicates left or bottom justification, \code{1}
#'   indicates right or top justification, and \code{0.5} indicates middle or
#'   center justification.
#' @param a numeric vector giving angle of text rotation.
#' @param parse logical vector indicating whether to parse the labels (e.g., to
#'   print labels with Greek letters or mathematical expressions).
#' @param fc \code{NA} or a character vector of fill colors for (optional) label
#'   background rectangles.
#' @param fa \code{NA} or a proportion vector (i.e., all values are between 0
#'   and 1, inclusive) indicating fill alpha opacity values for (optional) label
#'   background rectangles (0 is transparent, 1 is opaque, 0.5 is translucent).
#' @param lc \code{NA} or a character vector of line colors for (optional) label
#'   background rectangles.
#' @param la \code{NA} or a proportion vector (i.e., all values are between 0
#'   and 1, inclusive) indicating line alpha opacity values for (optional) label
#'   background rectangles (0 is transparent, 1 is opaque, 0.5 is translucent).
#' @param lt \code{NA} or a character vector indicating line type for (optional)
#'   label background rectangles, where \code{'-'} indicates a solid line,
#'   \code{':'} indicates a dotted line, \code{'='} indicates a dashed line,
#'   \code{':='} indicates a dot-dash line, \code{'LD'} indicates a long-width
#'   dashed line, and \code{'2D'} indicates a double-width dashed line.
#' @param lw \code{NA} or a positive numeric vector indicating proportional line
#'   width for (optional) background rectangles relative to the default (i.e.,
#'   \code{1} gives the default line width, 0\code{1/2} gives half the default
#'   line width, and \code{2} gives double the default line width.)
#' @param au \code{NA} or a character vector giving units of angle of rotation.
#'   \code{'d'} indicates degrees, \code{'g'} indicates gradians, \code{'r'}
#'   indicates radians, and \code{'p'} indicates proportions of a full
#'   revolution.
#' @param dx numeric vector giving horizontal offset from \code{x} for label
#'   positioning.
#' @param dy numeric vector giving vertical offset from \code{x} for label
#'   positioning.
#' @inheritParams int_add_look
#' @return \code{pj} with the addition of the label look applied to the
#'   specified shape.
#' @export
add_labels <- function(pj, text, size = 10, font = 'sans', style = 'plain', tc = 'black', ta = 0.5, hj = 0.5, vj = 0.5, parse = FALSE, a = 0, fc = 'white', fa = 0.5, lc = 'black', la = 0.5, lt = '-', lw = 1, au = 'r', dx = 0, dy = 0, shape = '.', name = '.') {
  shape.type <- int_get_type(pj, shape)
  region <- int_get_parent(pj, shape)
  int_add_look(pj, region.id = region, shape.id = shape, shape = shape.type, type = 'labels', name = name, text = text, size = size, font = font, style = style, tc = tc, ta = ta, hj = hj, vj = vj, parse = parse, a = a, fc = fc, fa = fa, lc = lc, la = la, lt = lt, lw = lw, au = au, dx = dx, dy = dy)
}

#' @name add_lines
#' @family add_look
#' @title Add a lines look to a shape.
#' @section Argument recycling: For shapes that are polygons, the arguments
#'   \code{lc}, \code{la}, \code{lt}, and \code{lw} are recycled with the number
#'   of distinct polygons described by the shape to which the look is to be
#'   applied. For point shapes, these arguments are recycled with the number of
#'   defined points. For refline and segment shapes, the arguments must be
#'   recyclable with the number of lines/segments defined by the shape.
#' @param lc character vector containing valid color representations. Length
#'   must be consistent with the shape a line look is being added to.
#' @param la proportion vector (i.e., between 0 and 1 inclusive) indicating the
#'   alpha opacity of the line color (0 = fully transparent, 0.5 = translucent,
#'   1 = fully opaque).
#' @param lt character vector indicating line type, where \code{'-'} indicates a
#'   solid line, \code{':'} indicates a dotted line, \code{'='} indicates a
#'   dashed line, \code{':='} indicates a dot-dash line, \code{'LD'} indicates a
#'   long-width dashed line, and \code{'2D'} indicates a double-width dashed
#'   line.
#' @param lw positive numeric vector indicating proportional line width relative
#'   to the default (i.e., \code{1} gives the default line width, \code{1/2}
#'   gives half the default line width, and \code{2} gives double the default
#'   line width.)
#' @inheritParams int_add_look
#' @return \code{pj} with the addition of the line look applied to the
#'   specified shape.
#' @export
add_lines <- function(pj, lc = 'black', la = 0.5, lt = '-', lw = 1, shape = '.', name = '.') {
  shape.type <- int_get_type(pj, shape)
  region <- int_get_parent(pj, shape)
  int_add_look(pj = pj, region.id = region, shape.id = shape, shape = shape.type, type = 'lines', name = name, lc = lc, la = la, lt = lt, lw = lw)
}

#' @name add_marks
#' @family add_look
#' @title Add a marks look to a shape.
#' @description A mark is a polygon-based marker for a given x-y location. Not
#'   applicable for shapes of type \code{'refline'}.
#' @section Valid combinations of argument properties: Marks can be validly
#'   described using the patterns of argument properties as follows, where
#'   \code{base} indicates that the argument set \code{{mark, size}} has been
#'   supplied, \code{fill} indicates that the argument set \code{{fc, fa}} has
#'   been supplied, \code{line} inidicates that the argument set \code{{lc, la,
#'   lt, lw}} have been supplied and \code{enclosure} indicates that the
#'   argument set \code{{et, ec, ea}} has been supplied. \describe{
#'     \item{\code{base + line}}{Outlined mark.}
#'     \item{\code{base + fill}}{Solid mark.}
#'     \item{\code{base + line + fill}}{Outlined and filled mark}.
#'     \item{\code{base + line + enclosure}}{Outlined mark in an enclosure}.
#'     \item{\code{base + fill + enclosure}}{Solid mark in an enclosure}.
#'     \item{\code{base + line + fill + enclosure}}{Outlined and filled mark in
#'           an enclosure}.
#'   }
#' @section Argument recycling: The arguments \code{mark}, \code{size},
#'   \code{lc}, \code{la}, \code{lw}, \code{fc}, \code{fa}, \code{et},
#'   \code{ec}, and/or \code{ea} are recycled with the number of x-y pairs used
#'   to draw the shape to which the mark look is applied.
#' @param mark character vector of symbols to mark x-y coordinates. Each value
#'   must be a single character, where \code{'c'} indicates a circle, \code{'d'}
#'   indicates a diamond, \code{'s'} indicates a square, \code{'3'} through
#'   \code{'9'} indicate 3 through 9 pointed stars, \code{'<'} indicates a
#'   left-pointing triangle,  \code{'>'} indicates a right-pointing triangle,
#'   \code{'v'} indicates a down-pointing triangle, and \code{'^'} indicates an
#'   up-pointing triangle.
#' @param size positive numeric vector indicating symbol size relative to the
#'   default (thus 1 indicates default sized, 1/2 indicates half sized, and 2
#'   indicates double sized).
#' @inheritParams add_glyphs
#' @return \code{pj} with the addition of the mark look applied to the
#'   specified shape.
#' @export
add_marks <- function(pj, mark = 'd', size = 1, fc = 'white', fa = 0.5, lc = 'black', la = 0.5, lw = 1, et = NA, ec = NA, ea = NA, shape = '.', name = '.') {
  shape.type <- int_get_type(pj, shape)
  region <- int_get_parent(pj, shape)
  int_add_look(pj = pj, region.id = region, shape.id = shape, shape = shape.type, type = 'marks', name = name, mark = mark, size = size, fc = fc, fa = fa, lc = lc, la = la, lw = lw, et = et, ec = ec, ea = ea)
}
