\name{ManagePackages}

\alias{ManagePackages}

\title{Manage Packages}

\description{
This function installs \R packages suggested by \pkg{RSurvey}. If a suggested
package is unavailable on the local computer an attempt is made to acquire the
package from \href{https://cran.r-project.org/}{CRAN} using an existing network
connection.
}

\usage{ManagePackages()}

\author{J.C. Fisher}

\seealso{\code{\link{install.packages}}, \code{\link{require}}}

\examples{\dontrun{ManagePackages()}}

\keyword{misc}
