\name{DefineInterpGrid}

\alias{DefineInterpGrid}

\title{Define Interpolation Grid}

\description{
A \acronym{GUI} for defining the interpolation grid.
}

\usage{DefineInterpGrid(grid = NULL, parent = NULL)}

\arguments{
\item{grid}{list; see \sQuote{Value}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Returns a list with the following components:
\item{opt}{integer; something...}
\item{res}{numeric; numeric components \code{x} and \code{y}
  giving the grid spacing along the x- and y-axis of the
  interpolated surface, respectively.}
\item{geo}{RasterLayer; something...}
}

\author{J.C. Fisher}

\examples{\dontrun{DefineInterpGrid()}}

\keyword{misc}
