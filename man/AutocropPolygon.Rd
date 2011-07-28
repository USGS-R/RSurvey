\name{AutocropPolygon}

\alias{AutocropPolygon}

\title{Set Autocrop Input Parameters}

\description{
A \acronym{GUI} for specifying input parameters for the \code{\link{Autocrop}}
function.
}

\usage{
AutocropPolygon(parent = NULL)
}

\arguments{
\item{parent}{tkwin; the parent window (optional).}
}

\details{
This subroutine calls on the \code{x} and \code{y} components of
\code{data.pts}, a data table stored in \code{\link{Data}}
(see \code{\link{ReadData}}).
A Delaunay triangulation is created from the set of
arbitrarily distributed points and the area defining these
points is approximated using the \code{\link{Autocrop}} function.
The default maximum arc length is the maximum outer arc length
for the mesh. Entering arc lengths less than the
default value will result in a reduced area for the polygon.
A point plot is drawn showing the resulting polygon based on the user defined
input parameters. Plotting parameters are queried from \code{\link{Data}}.
}

\value{
Returns a polygon object of \code{\link{gpc.poly-class}}.
}

\author{Fisher, J.C.}

\seealso{
\code{\link{Autocrop}}, \code{\link{Plot2d}}, \code{tri.mesh}
}

\examples{
\donttest{
data(tritest)
Data("data.pts", as.data.frame(tritest))
Data("vars", list(x = 1, y = 2))
AutocropPolygon()
}
}

\keyword{misc}
