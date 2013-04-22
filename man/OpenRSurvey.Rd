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
