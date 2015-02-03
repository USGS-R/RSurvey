\name{LoadPackages}

\alias{LoadPackages}

\title{Load Suggested Packages}

\description{
This function installs \R packages suggested by \pkg{RSurvey}. If a suggested
package is unavailable on the local computer an attempt is made to acquire the
package from \href{http://cran.r-project.org/}{CRAN} using an existing network
connection.
}

\usage{LoadPackages()}

\author{J.C. Fisher}

\seealso{\code{\link{install.packages}}, \code{\link{require}}}

\examples{\dontrun{LoadPackages()}}

\keyword{misc}
