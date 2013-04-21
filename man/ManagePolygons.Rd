\name{ManagePolygons}

\alias{ManagePolygons}

\title{Manage Polygons}

\description{
A \acronym{GUI} for managing and manipulating polygons that is based on the
\pkg{rgeos} package.
}

\usage{
ManagePolygons(polys = NULL, encoding = getOption("encoding"), parent = NULL)
}

\arguments{
\item{polys}{list; its components are objects of \code{\link{gpc.poly-class}}.}
\item{encoding}{character; encoding to be assumed for input strings. If the
  value is \code{"latin1"} or \code{"UTF-8"} it is used to mark character
  strings as known to be in Latin-1 or UTF-8: it is not used to re-encode
  the input.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\details{
The text file representation of a polygon is of the following format:\cr\cr
<number of contours>\cr
<number of points in first contour>\cr
<hole flag>\cr
x1 y1\cr
x2 y2\cr
...\cr
<number of points in second contour>\cr
<hole flag>\cr
x1 y1\cr
x2 y2\cr
...\cr\cr
The hole flag is either 1 to indicate a hole, or 0 for a regular contour.
See \code{\link{read.polyfile}} within the \pkg{rgeos} package for details.
}

\value{
Queries and sets the \code{polys} component of \code{\link{Data}};
includes attributes of \code{polys} (\code{data.poly} and \code{crop.poly}),
see \code{\link{SetPolygonLimits}}.
}

\author{J.C. Fisher}

\seealso{
\code{\link{polyfile}}, \code{\link{union}}, \code{\link{setdiff}},
\code{\link{intersect}}
}

\examples{
\donttest{ManagePolygons()}
}

\keyword{misc}
