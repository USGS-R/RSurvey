\name{ImportPackage}

\alias{ImportPackage}

\title{Import Data from R Package}

\description{A \acronym{GUI} for loading selected data sets from R packages.}

\usage{ImportPackage(classes = NULL, parent = NULL)}

\arguments{
\item{classes}{character; the object classes of data sets that can be
  loaded. Set to \code{NULL} to enable loading for all object classes.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Returns a list with the following components:
\item{d}{data.frame or matrix; data set.}
\item{src}{character; a vector of length 3 that includes the dataset name,
  package name, and access date.}
}

\author{J.C. Fisher}

\seealso{\code{\link{data}}}

\examples{\donttest{ans <- ImportPackage(c("data.frame", "matrix"))}}

\keyword{IO}
