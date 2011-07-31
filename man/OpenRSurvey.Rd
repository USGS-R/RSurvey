\name{OpenRSurvey}

\alias{OpenRSurvey}

\title{Open Main Graphical User Interface}

\description{
This function activates the main \acronym{GUI} for \pkg{RSurvey}.
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
\item{x, y, z, t, vx, vy}{integer; the index number of the corresponding
  state variable in \code{cols}, see \code{\link{ManageData}}.}
}

\author{Fisher, J.C.}

\examples{
\donttest{OpenRSurvey()}
}

\keyword{misc}
