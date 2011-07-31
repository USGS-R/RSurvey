\name{AutocropPolygon}

\alias{AutocropPolygon}

\title{Set Autocrop Input Parameters}

\description{
A \acronym{GUI} for specifying input parameters of the \code{\link{Autocrop}}
function.
}

\usage{
AutocropPolygon(d, parent = NULL, ...)
}

\arguments{
\item{d}{data.frame; a data table with required coordinate components
  \code{d$x} and \code{d$y}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
\item{...}{other graphical parameters, see \code{\link{Plot2d}}.}
}

\details{
A Delaunay triangulation is created from the set of
arbitrarily distributed points and the area defining these
points is approximated using the \code{\link{Autocrop}} function.
The default maximum arc length is the maximum outer arc length
for the mesh. Entering arc lengths less than the
default value will result in a reduced area for the polygon.
A point plot is drawn showing the resulting polygon based on user defined
input parameters.
}

\value{
Returns a polygon object of \code{\link{gpc.poly-class}}.
}

\author{Fisher, J.C.}

\seealso{
\code{tri.mesh}
}

\examples{
\donttest{
data(tritest)
AutocropPolygon(as.data.frame(tritest), asp = 1)
}
}

\keyword{misc}
