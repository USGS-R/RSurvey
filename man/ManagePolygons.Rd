\name{ManagePolygons}

\alias{ManagePolygons}

\title{Manage Polygons}

\description{
A \acronym{GUI} for managing and manipulating polygons that is based on the
\pkg{gpclib} package.
}

\usage{
ManagePolygons(ply = NULL, encoding = getOption("encoding"),
               parent = NULL)
}

\arguments{
\item{ply}{list; its components are objects of class \code{gpc.poly-class}.}
\item{encoding}{character; encoding to be assumed for input strings. If the
  value is \code{"latin1"} or \code{"UTF-8"} it is used to mark character
  strings as known to be in Latin-1 or UTF-8: it is not used to re-encode
  the input.}
\item{parent}{tkwin; the parent window (optional).}
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
See \code{read.polyfile} within the \pkg{gpclib} package for details.
}

\value{
Queries and sets the \code{ply} (see \sQuote{Arguments}),
\code{poly.data}, and \code{poly.crop} (see \code{\link{SetPolygonLimits}})
components of \code{\link{Data}}.
}

\author{Fisher, J.C.}

\seealso{
\code{polyfile}, \code{union}, \code{setdiff}, \code{intersect}
}

\examples{
\donttest{ManagePolygons()}
}

\keyword{misc}
