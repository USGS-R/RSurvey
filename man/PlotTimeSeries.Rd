\name{PlotTimeSeries}

\alias{PlotTimeSeries}

\title{Plot Temporal Data}

\description{
Draws a time-series plot with points and connecting lines.
}

\usage{
PlotTimeSeries(x, y = NULL, xlim = NULL, ylim = NULL, ylab = NULL,
               tgap = NULL, width = 7, cex.pts = 1,
               pointsize = 12, fmt = NULL, axis.side = 1:2,
               minor.ticks = FALSE, ticks.inside = FALSE)
}

\arguments{
\item{x}{POSIXct; a vector specifying \code{x} values.}
\item{y}{numeric; a vector specifying \code{y} values.}
\item{xlim}{POSIXct; the \code{x} limits \code{(x1,x2)} of the plot.}
\item{ylim}{numeric; the \code{y} limits \code{(y1,y2)} of the plot.}
\item{ylab}{character; the label for the \code{y} axis.}
\item{tgap}{numeric; time gap exceedance level in seconds.}
\item{width}{numeric; the width of the plotting window canvas in inches.}
\item{cex.pts}{numeric; the amount by which point symbols should be magnified
  relative to the default.}
\item{pointsize}{integer; the point size of plotted text.}
\item{fmt}{character; date-time format for \code{x} axis tic mark labels, see
  \code{\link{strptime}}.}
\item{axis.side}{integer; the side of the plot the axis is to be drawn on.
  The axis is placed as follows: 1 = below, 2 = left, 3 = above and 4 = right.}
\item{minor.ticks}{logical; if \code{TRUE} minor tickmarks are added to the
  plot; its default is \code{FALSE}.}
\item{ticks.inside}{logical; if \code{TRUE} tickmarks are placed inside the
  plot region; its default is \code{FALSE}.}
}

\details{
Line segments will not be drawn where time differences between consecutive
points are greater than \code{tgap}.
}

\author{Fisher, J.C.}

\seealso{
\code{\link{plot}}
}

\examples{
data(project)
d <- project$data.pts
PlotTimeSeries(x = d$t, y = d$z)
PlotTimeSeries(x = d$t, y = d$z, tgap = 3000)
}

\keyword{hplot}
