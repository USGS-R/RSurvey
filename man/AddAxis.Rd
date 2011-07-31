\name{AddAxis}

\alias{AddAxis}

\title{Add an Axis to a Plot}

\description{
Adds an axis to the current plot.
}

\usage{
AddAxis(side, lim, ticks.inside = FALSE, minor.ticks = FALSE, ...)
}

\arguments{
\item{side}{integer; a vector of values specifying the plot sides for the axis
  to be drawn.}
\item{lim}{numeric or POSIXt; the axis limits (\code{x1}, \code{x2}) of
  the plot.}
\item{ticks.inside}{logical; if \code{TRUE} tickmarks are placed inside the
  plot region; its default is \code{FALSE}.}
\item{minor.ticks}{logical; if \code{TRUE} minor tickmarks are added to the
  plot; its default is \code{FALSE}.}
\item{...}{other graphical parameters may also be passed as arguments to this
  function, see \code{\link{axis}}.}
}

\details{
The plot sides are designated as: 1 = below, 2 = left, 3 = above, and 4 = right.
}

\author{Fisher, J.C.}

\seealso{
\code{\link{axis}}, \code{\link{axis.POSIXct}},
\code{\link{seq}}, \code{\link{pretty}}
}

\examples{
x <- as.POSIXlt("2001/1/1") + 700 * sort(runif(10))
y <- rnorm(10)
xlim <- extendrange(x, f = 0.02)
ylim <- extendrange(y, f = 0.02)
plot(x, y, axes = FALSE)
box()
AddAxis(side = 1, lim = xlim)
AddAxis(side = 2, lim = ylim, ticks.inside = TRUE)
AddAxis(side = 3, lim = xlim, minor.ticks = TRUE)
AddAxis(side = 4, lim = ylim, ticks.inside = TRUE, minor.ticks = TRUE)
}

\keyword{aplot}
