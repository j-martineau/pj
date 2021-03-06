% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_triangle.R
\name{new_pvtri}
\alias{new_pvtri}
\title{Add one or more proportional-vertex based triangle shapes}
\usage{
new_pvtri(
  jp,
  x,
  y,
  pvx,
  pvy,
  w = NA,
  h = NA,
  px = 0.5,
  py = 0.5,
  x2 = NA,
  y2 = NA,
  look = NULL,
  mod = NULL,
  region = ".",
  name = "."
)
}
\arguments{
\item{pvx, pvy}{(required) proportion vectors of length 3 indicating the
proportional x and y locations of the three vertices of all triangles to be
plotted. All values must be between 0 and 1, inclusive. See the \emph{the
pvx and pvy arguments} section.}
}
\value{
\code{jp} with the addition of the specified proportional-based
triangle shape(s).
}
\description{
Proportional-vertex based triangle shapes are created by
proportionally defining vertex locations within a bounding rectangle. These
shapes are located in plotting regions using bounding rectangles; a concept
used in defining function parameters. See the \emph{locating bounding
rectangles} section for details.
}
\section{The pvx and pvy arguments}{
 \code{pvx} indicates, as a proportion,
how far across the bounding rectangle each of the three vertices of all
triangles are located (from left to right). \code{pvy} indicates, as a
proportion, how far up the bounding rectangle each of the three vertices of
all triangles are located. For example: \describe{
\item{pvx = c(0, 1/2, 1), pvy = c(0, 1, 0)}{Produces an upward
\link[=new_isosceles]{isosceles triangle}.}
\item{pvx = c(0, 0, 1), pvy = c(0, 1, 0)}{Produces a
\link[=new_right]{right triangle} with the 90-degree angle located
at the bottom left corner of the bounding rectangle.}
\item{pvx = c(0, 0, 1), pvy = c(0, 1, 1/2)}{Produces a rightward
\link[=new_isosceles]{isosceles triangle}.}}
}

\section{Argument recycling}{
 Arguments \code{x}, \code{y}, \code{px},
\code{py}, \code{w}, \code{h} \code{x2}, and \code{y2} are recycled.
}

\seealso{
Other new_shape: 
\code{\link{new_isosceles}()},
\code{\link{new_right}()}

Other new_triangle: 
\code{\link{new_isosceles}()},
\code{\link{new_right}()}
}
\concept{new_shape}
\concept{new_triangle}
