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
\item{grid.res}{list; numeric components giving the grid spacing along
  the x- (\code{$x}) and y-axis (\code{$y}) of the
  interpolated surface, respectively (optional).}
\item{grid.mba}{list; integer components giving the initial size of the
  spline space in the hierarchical construction along the
  \code{x}- (\code{$n}) and \code{y}-axis (\code{$m}), and
  number of levels in the hierarchical construction (\code{$h}),
  its default is 11.
}
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
