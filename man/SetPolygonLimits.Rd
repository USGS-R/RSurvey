\name{SetPolygonLimits}

\alias{SetPolygonLimits}

\title{Set Polygon Limits}

\description{
A \acronym{GUI} for specifying polygon limits.
}

\usage{
SetPolygonLimits(poly.names = NULL, poly.data = NULL,
                 poly.crop = NULL, parent = NULL)
}

\arguments{
\item{poly.names}{character; the vector of names corresponding to polygons
  contained within \code{ply} (see \code{\link{ManagePolygons}}).}
\item{poly.data}{character; the name of the polygon that defines the
  data limits boundary (optional).}
\item{poly.crop}{character; the name of the polygon that defines the crop
  region for interpolated data (optional).}
\item{parent}{tkwin; the parent window (optional).}
}

\value{
Returns a list with components \code{poly.data} and \code{poly.crop}
  (see \sQuote{Arguments}).
}

\seealso{
\code{\link{AutocropPolygon}}, \code{tri.mesh}
}

\author{Fisher, J.C.}

\examples{
\donttest{SetPolygonLimits(c("Polygon1", "Polygon2", "Polygon3"))}
}

\keyword{misc}
