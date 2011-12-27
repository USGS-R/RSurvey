\name{Plot2d}

\alias{Plot2d}

\title{Plot Points or Interpolated Surface}

\description{
Draws a scatter plot or contour plot with arrows. A key showing how
the colors map to state variable values is shown to the right of the plot.
}

\usage{
Plot2d(x = NULL, y = NULL, z = NULL, vx = NULL, vy = NULL,
       type = "p", xlim = NULL, ylim = NULL, zlim = NULL,
       xlab = NULL, ylab = NULL, zlab = NULL, asp = NA,
       csi = NA, width = 7, pointsize = 12, cex.pts = 1,
       nlevels = 20, rkey = FALSE,
       color.palette = terrain.colors,
       vuni = FALSE, vmax = NULL, vxby = NULL, vyby = NULL,
       axis.side = 1:2, minor.ticks = FALSE,
       ticks.inside = FALSE, add.contour.lines = FALSE,
       rm.pnt.line = FALSE)
}

\arguments{
\item{x}{numeric; a vector of \code{x} coordinates for the plot. If \code{x} is
  a data frame, its components \code{x$x}, \code{x$y}, \code{x$z}, \code{x$vx},
  and \code{x$vy} are used for \code{x}, \code{y}, \code{z}, \code{vx}, and
  \code{vy}, respectively.}
\item{y}{numeric; a vector of \code{y} coordinates for the plot.}
\item{z}{numeric or matrix; the state variable values to be plotted,
  NAs allowed. A matrix is required for contour plots.}
\item{vx, vy}{numeric; a vector of arrow component lengths in the
  \code{x} and \code{y} directions.}
\item{type}{character; a 1-character string giving the type of plot desired.
  The following values are possible: "p" for points, "l" for level contour,
  "g" for grid contour.}
\item{xlim}{numeric; a vector of \code{x} limits \code{(x1,x2)} for the plot.}
\item{ylim}{numeric; a vector of \code{y} limits \code{(y1,y2)} for the plot.}
\item{zlim}{numeric; a vector of \code{z} limits \code{(z1,z2)} for the plot.}
\item{xlab, ylab}{character; the label for the \code{x} and \code{y} axis.}
\item{zlab}{character; the label for the \code{z} legend.}
\item{asp}{numeric; the \code{y/x} aspect ratio.}
\item{csi}{numeric; height of text characters in inches.}
\item{width}{numeric; the width of the plotting window canvas in inches.}
\item{pointsize}{integer; the point size of plotted text.}
\item{cex.pts}{numeric; the amount by which point symbols should be magnified
  relative to the default.}
\item{nlevels}{integer; number of contour levels desired.}
\item{rkey}{logical; if \code{TRUE} the legend key is reversed with \code{z}
  values descending from top to bottom; its default is \code{FALSE}.}
\item{color.palette}{function; a color \link{palette} to be used to assign
  colors in the plot.}
\item{vuni}{logical; if \code{TRUE} all arrow lengths are set equal; its
  default is \code{FALSE}.}
\item{vmax}{numeric; the maximum length of arrows in inches.}
\item{vxby, vyby}{integer; increment for the sequence of arrows in the \code{x}
  and \code{y} direction.}
\item{axis.side}{integer; the side of the plot the axis is to be drawn on.
  The axis is placed as follows: 1 = below, 2 = left, 3 = above and 4 = right.}
\item{minor.ticks}{logical; if \code{TRUE} minor tickmarks are added to the
  plot; its default is \code{FALSE}.}
\item{ticks.inside}{logical; if \code{TRUE} tickmarks are placed inside the
  plot region; its default is \code{FALSE}.}
\item{add.contour.lines}{logical; if \code{TRUE} and \code{type} is either "l"
  or "g" than contour lines are drawn; its default is \code{FALSE}.}
\item{rm.pnt.line}{logical; if \code{TRUE} the line boundary on point symbols is
  not drawn; its default is \code{FALSE}.}
}

\details{
The length of \code{x} and \code{y} should be equal to the \code{nrow(z)}
and \code{ncol(z)}, respectively.
}

\author{J.C. Fisher}

\seealso{
\code{\link{filled.contour}}, \code{\link{image}}, \code{\link{arrows}},
\code{\link{AddAxis}}
}

\examples{
data(project)

d <- project$data.pts
Plot2d(d, type = "p")

d <- project$data.grd
Plot2d(d, type = "l")
Plot2d(d, type = "g")
}

\keyword{hplot}
