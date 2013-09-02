\name{ImportPackageData}

\alias{ImportPackageData}

\title{Import Data from Package}

\description{
A \acronym{GUI} for loading selected data sets from R packages.
}

\usage{
ImportPackageData(classes = NULL, parent = NULL)
}

\arguments{
\item{classes}{character; the object classes of data sets that can be
  loaded. Set to \code{NULL} to enable loading for all object classes.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Returns a list with the following components:
\item{d}{data.frame or matrix; data set.}
\item{src}{character; a vector of length 2 that includes the dataset and
  package names.}
}

\author{J.C. Fisher}

\seealso{
\code{\link{data}}
}

\examples{
\donttest{ans <- ImportPackageData(c("data.frame", "matrix"))}
}

\keyword{IO}
