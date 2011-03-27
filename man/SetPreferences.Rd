\name{SetPreferences}

\alias{SetPreferences}

\title{Set Data Preferences}

\description{
A \acronym{GUI} for specifying the interpolation algorithms input parameters.
}

\usage{SetPreferences(parent = NULL)}

\arguments{
\item{parent}{tkwin; the parent window (optional).}
}

\value{
Queries and sets the following components in \code{\link{Data}}:
\item{grid.dx, grid.dy}{numeric; the grid spacing along the \code{x}- and
  \code{y}-axis for the interpolated surface, respectively (optional).}
\item{mba.n, mba.m}{integer; initial size of the spline space in the
  hierarchical construction along the \code{x}- and \code{y}-axis, respectively
  (optional).}
\item{mba.h}{integer; number of levels in the hierarchical construction, its
  default is 11.}
}

\note{
If data is re-imported, parameters in this \acronym{GUI} are set to default
values.
}

\author{Fisher, J.C.}

\seealso{
\code{\link{mba.points}}
}

\examples{
\donttest{SetPreferences()}
}

\keyword{misc}
