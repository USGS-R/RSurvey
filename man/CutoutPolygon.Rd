\name{CutoutPolygon}

\alias{CutoutPolygon}

\title{Determine Grid Points within Polygon}

\description{
This function excludes gridded data located outside a given polygon.
}

\usage{
CutoutPolygon(dat, ply = NULL)
}

\arguments{
\item{dat}{list; with components \code{x}, \code{y}, and \code{z},
  see \sQuote{Value}.}
\item{ply}{gpc.poly; the polygon defining the crop region for the gridded
  data.}
}

\details{
Values of \code{z} corresponding to coordinates (\code{x}, \code{y}) located
outside the polygon will be set to \code{NA}.
}

\value{
Returns a list containing the following components:
  \item{x, y}{numeric; a vector of \code{x} and \code{y} coordinates.}
  \item{z}{matrix; the state variable corresponding to coordinates in the grid.}
}

\author{Fisher, J.C.}

\seealso{
\code{\link{point.in.polygon}}
}

\examples{
x11()

ply <- as(cbind(c(2, 8, 9, 6, 3), c(3, 1, 4, 8, 6)), "gpc.poly")
x <- seq(0, 10, 0.1)
y <- seq(0, 10, 0.1)
z <- matrix(runif(length(x) * length(y)), nrow = length(y),
            ncol = length(x))

d <- list(x = x, y = y, z = z)
filled.contour(d, plot.axes = {axis(1); axis(2); plot(ply, add = TRUE)})

d <- CutoutPolygon(d, ply)
filled.contour(d, color.palette = terrain.colors)
}

\keyword{manip}
