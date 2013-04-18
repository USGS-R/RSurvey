\name{OpenRSurvey}

\alias{OpenRSurvey}

\title{RSurvey}

\description{
Opens the main graphical user interface (\acronym{GUI}) for \pkg{RSurvey}.
}

\usage{
OpenRSurvey()
}

\details{
All functions within \pkg{RSurvey} are accessible through this \acronym{GUI}. 
Files can be one of four types as indicated by their extension:
tables (\file{.txt}, \file{.csv}, \file{.dat}, or \file{.shp}),
grids (\file{.grd}), polygons (\file{.ply}), or binary project images
(\file{.rda}).Tables (\file{.txt}, \file{.csv}, \file{.dat})
can be compressed by \href{http://www.gzip.org/}{gzip}
with additional extension \file{.gz}. Shapefiles (\file{.shp}) and interpolated
grid files (\file{.grd}) are limited to data export.
Measurement unit consistency is tasked to the user. Time zones, 
spatial datum's and projections are not supported.
}

\value{
Quaries and sets the \code{vars} component of \code{\link{Data}}. The
\code{vars} object is a list with components:
\item{x, y, z, vx, vy}{integer; the index number of the corresponding
  state variable in \code{cols}, see \code{\link{ManageVariables}}.}
}

\author{J.C. Fisher}

\examples{
\donttest{OpenRSurvey()}
}

\keyword{misc}
