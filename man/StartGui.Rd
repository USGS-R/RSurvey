\name{StartGui}

\alias{StartGui}

\title{Open Graphical User Interface}

\description{
Launches a \pkg{RSurvey} graphical user interface (\acronym{GUI}) for \pkg{RSurvey}.
}

\usage{StartGui()}

\details{
All functions within \pkg{RSurvey} are accessible through this \acronym{GUI}.
}

\value{
Quaries and sets the \code{vars} component of \code{\link{Data}}. The
\code{vars} object is a list with components:
\item{x, y, z}{integer; the index number of the corresponding
  coordinate-dimension variable in \code{cols}, see \code{\link{ManageVariables}}.}
}

\author{J.C. Fisher}

\examples{\dontrun{StartGui()}}

\keyword{misc}
