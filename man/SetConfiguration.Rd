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
\item{nlevels}{integer; approximate number of contour levels desired; its default is 20.}
\item{width}{numeric; the width of the plotting window canvas in inches; its default is 7.}
\item{cex.pts}{numeric; the amount by which point symbols should be magnified relative to the default value, \code{1.0}.
  For example, \code{cex.pts = 0.5} reduces the point symbol to half of its default size.}
\item{asp.yx, asp.zx}{numeric; the \code{y/x} and \code{z/x} aspect ratios, respectively (optional).}
\item{contour.lines}{logical; if \code{TRUE} contour lines will be plotted on the 2D interpolated surface;
  its default is \code{FALSE}.}
\item{useRaster}{logical; something...}
}

\note{Re-importing data does not affect values specified in this \acronym{GUI}.}

\author{J.C. Fisher}

\examples{\dontrun{SetConfiguration()}}

\keyword{misc}
