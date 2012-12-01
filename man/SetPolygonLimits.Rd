\name{SetPolygonLimits}

\alias{SetPolygonLimits}

\title{Set Polygon Limits}

\description{
A \acronym{GUI} for specifying polygon limits.
}

\usage{
SetPolygonLimits(poly.names = NULL, data.poly = NULL, crop.poly = NULL, 
                 parent = NULL)
}

\arguments{
\item{poly.names}{character; a vector of polygon names.}
\item{data.poly}{character; the name of the polygon that defines the
  data limits boundary.}
\item{crop.poly}{character; the name of the polygon that defines the crop
  region for interpolated data.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Returns a list with components \code{data.poly} and \code{crop.poly}.
}

\author{J.C. Fisher}

\examples{
\donttest{SetPolygonLimits(c("Polygon1", "Polygon2", "Polygon3"))}
}

\keyword{misc}
