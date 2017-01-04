\name{DefineInterpGrid}

\alias{DefineInterpGrid}

\title{Define Interpolation Grid}

\description{
A \acronym{GUI} for defining the interpolation grid.
}

\usage{DefineInterpGrid(grid.res = NULL, grid.geo = NULL, parent = NULL)}

\arguments{
\item{grid.res}{list; see \sQuote{Value}.}
\item{grid.geo}{list; see \sQuote{Value}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Returns a list with the following components:
\item{grid.res}{list; numeric components \code{x} and \code{y}
  giving the grid spacing along the x- and y-axis of the
  interpolated surface, respectively.}
\item{grid.mba}{list; integer components \code{m}, \code{n}, and \code{h}
  giving the initial size of the spline space in the hierarchical
  construction along the \code{x}- and \code{y}-axis, and the
  number of levels in the hierarchical construction;
  its default is 11.}
}

\note{
If data is re-imported, parameters in this \acronym{GUI} are set to default values.
}

\author{J.C. Fisher}

\examples{\dontrun{DefineInterpGrid()}}

\keyword{misc}
