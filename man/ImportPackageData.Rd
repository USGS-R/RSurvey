\name{ImportPackageData}

\alias{ImportPackageData}

\title{Import Data from Package}

\description{
A \acronym{GUI} for loading selected data sets from R packages.
}

\usage{
ImportPackageData(classes = c("data.frame", "matrix"), parent = NULL)
}

\arguments{
\item{classes}{character; the object classes of data sets that can be 
  loaded. Set to \code{NULL} to enable loading for all object classes.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
The data set object.
}

\author{J.C. Fisher}

\seealso{
\code{\link{data}}
}

\examples{
\donttest{ds <- ImportPackageData()}
}

\keyword{IO}
