\name{SetPreferences}

\alias{SetPreferences}

\title{Set Data Preferences}

\description{
A \acronym{GUI} for specifying the interpolation algorithms input parameters.
}

\usage{SetPreferences(parent = NULL)}

\arguments{
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Queries and sets the following components in \code{\link{Data}}:
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
If data is re-imported, parameters in this \acronym{GUI} are set to default
values.
}

\author{J.C. Fisher}

\seealso{
\code{\link{mba.points}}
}

\examples{
\donttest{SetPreferences()}
}

\keyword{misc}
