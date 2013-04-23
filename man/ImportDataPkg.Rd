\name{ImportDataPkg}

\alias{ImportDataPkg}

\title{Import Data from Package}

\description{
A \acronym{GUI} for loading selected data sets from R packages.
}

\usage{
ImportDataPkg(parent = NULL)
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
\donttest{ds <- ImportDataPkg()}
}

\keyword{IO}
