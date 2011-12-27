\name{Autocrop}

\alias{Autocrop}

\title{Autocrop Spatial Domain}

\description{
Approximate the shape of an area defined by a set of points in a plane.
}

\usage{
Autocrop(mesh, max.len, max.itr = 10000)
}

\arguments{
\item{mesh}{tri; a Delaunay triangulation.}
\item{max.len}{numeric; maximum arc length for an outer triangle.}
\item{max.itr}{integer; maximum number of iterations.}
}

\details{
This subroutine uses a Delaunay triangulation to approximate the shape of an
area defined by a set of arbitrarily distributed points in a plane.
All triangles with arc lengths greater than an established maximum length are
removed; a polygon is created from the union of the remaining triangles.
}

\value{
Returns a polygon object of \code{\link{gpc.poly-class}}.
}

\seealso{
\code{\link{AutocropPolygon}}, \code{tri.mesh}
}

\author{J.C. Fisher}

\examples{
data(tritest)
mesh <- tri.mesh(tritest$x, tritest$y)
plot(mesh)
ply <- Autocrop(mesh, max.len = 0.5, max.itr = 100)
plot(ply, add = TRUE, poly.args = list(col = 2))
}

\keyword{symbolmath}
