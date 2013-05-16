\name{LoadPackages}

\alias{LoadPackages}

\title{Load Required and Suggested Packages for RSurvey}

\description{
This function installs \R packages required by \pkg{RSurvey}.
If a required package is unavailable on the local computer an attempt
is made to acquire the package from
\href{http://cran.r-project.org/}{CRAN}
using an existing network connection.
}

\usage{
LoadPackages(repo = "http://cran.r-project.org")
}

\arguments{
\item{repo}{character; the base \acronym{URL} of the repositories to use for
  package installation.}
}

\author{J.C. Fisher}

\seealso{
\code{\link{install.packages}}, \code{\link{require}}
}

\examples{\donttest{LoadPackages()}}

\keyword{misc}
