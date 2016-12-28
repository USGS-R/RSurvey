\name{ManagePolygons}

\alias{ManagePolygons}

\title{Manage Polygons}

\description{
A \acronym{GUI} for managing and manipulating polygons that is based on the
\pkg{rgeos} package.
}

\usage{
ManagePolygons(polys = NULL, poly.data = NULL, poly.crop = NULL,
               crs = CRS(as.character(NA)), parent = NULL)
}

\arguments{
\item{polys}{list; its components are objects of \code{\link{gpc.poly-class}}.}
\item{poly.data}{character; the name of the polygon that defines the
  data limits boundary.}
\item{poly.crop}{character; the name of the polygon that defines the crop
  region for interpolated data.}
\item{crs}{CRS; default coordinate reference system.}
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
See \code{\link[rgeos]{read.polyfile}} within the \pkg{rgeos} package for
details.
}

\value{
A list with components \code{polys}, \code{poly.data}, \code{poly.crop}, and \code{crs}
(see \sQuote{Arguments}).
}

\author{J.C. Fisher}

\seealso{
\code{\link{polyfile}}, \code{\link{union}}, \code{\link{setdiff}},
\code{\link{intersect}}
}

\examples{\dontrun{ManagePolygons()}}

\keyword{misc}
