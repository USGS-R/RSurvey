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
  contained within \code{ply}, a list of polygon components,
 see \code{\link{ManagePolygons}}.}
\item{poly.data}{character; the name of the polygon that defines the
  data limits boundary.}
\item{poly.crop}{character; the name of the polygon that defines the crop
  region for interpolated data.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Returns a list with components \code{poly.data} and \code{poly.crop}.
}

\author{J.C. Fisher}

\seealso{
\code{\link{AutocropPolygon}}, \code{tri.mesh}
}

\examples{
\donttest{SetPolygonLimits(c("Polygon1", "Polygon2", "Polygon3"))}
}

\keyword{misc}
