\name{SetConfiguration}

\alias{SetConfiguration}

\title{Set Window and Plotting Parameters}

\description{
A \acronym{GUI} for specifying window geometry and universal plotting
parameters.
}

\usage{SetConfiguration(parent = NULL)}

\arguments{\item{parent}{tkwin; the \acronym{GUI} parent window.}}

\value{
Queries and sets the following components of \code{\link{Data}}:
\item{nlevels}{integer; approximate number of contour levels desired; its
  default is 20.}
\item{width}{numeric; the width of the plotting window canvas in inches; its
  default is 7.}
\item{cex.pts}{numeric; the amount by which point symbols should be magnified
  relative to the default value, \code{1.0}. For example, \code{cex.pts = 0.5}
  reduces the point symbol to half of its default size.}
\item{asp.yx, asp.zx}{numeric; the \code{y/x} and \code{z/x} aspect ratios,
  respectively (optional).}
\item{rkey}{logical; if \code{TRUE} the legend key is reversed with \code{z}
  values descending from top to bottom; its default is \code{FALSE}.}
\item{img.contour}{logical; if \code{TRUE} the \code{\link{image}} function is
  used to plot interpolated surfaces; if \code{FALSE}, the default, the
  \code{\link{filled.contour}} function is used.}
\item{show.lines}{logical; if \code{TRUE} the line contours will be plotted on
  the two-dimensional interpolated surface; its default is \code{FALSE}.}
\item{show.points}{logical; if \code{TRUE} the point values associated with
  \code{(x,y)} will be plotted on the interpolated surface; its default is
  \code{FALSE}.}
\item{show.poly}{logical; if \code{TRUE} polygons describing the spatial
  domain are added to the scatter plot and two-dimensional surface plot; its
  default is \code{FALSE}.}
\item{show.2.axes}{logical; if \code{TRUE} axes tickmarks will be drawn
  on all sides; its default is \code{FALSE}.}
\item{minor.ticks}{logical; if \code{TRUE} minor tickmarks are added to the
  plot; its default is \code{FALSE}.}
\item{ticks.inside}{logical; if \code{TRUE} tickmarks are placed inside the
  plot region; its default is \code{FALSE}.}
\item{rm.pnt.line}{logical; if \code{TRUE} the line boundary on point symbols is
  not drawn; its default is \code{FALSE}.}
}

\note{Re-importing data does not affect values specified in this \acronym{GUI}.}

\author{J.C. Fisher}

\examples{\dontrun{SetConfiguration()}}

\keyword{misc}
