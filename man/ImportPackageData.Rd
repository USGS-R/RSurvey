\name{ImportPackageData}

\alias{ImportPackageData}

\title{Import Data from Package}

\description{
A \acronym{GUI} for loading selected data sets from R packages.
}

\usage{
ImportPackageData(parent = NULL)
}

\arguments{
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
