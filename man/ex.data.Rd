\name{ex.data}

\alias{ex.data}

\docType{data}

\title{Example Data Set}

\description{
This is an example data set that is typically used to test \pkg{RSurvey}.
}

\usage{
ex.data
}

\format{
A data frame with 50 rows and the following columns:
\describe{
  \item{T.POSIXct}{POSIXct; vector of date-time values.}
  \item{X.numeric..m}{numeric; vector.}
  \item{Y.numeric..m}{numeric; vector.}
  \item{Z.numeric..ft}{numeric; vector.}
  \item{Z.numeric..ft..1.}{numeric; vector.}
  \item{Z.numeric..m}{numeric; vector.}
  \item{Unknown}{logical; vector.}
  \item{Logical}{logical; vector.}
  \item{Integer..psia}{integer; vector.}
  \item{Unknown..1.}{logical; vector.}
  \item{Logical..1.}{logical; vector.}
  \item{Date.character}{factor; vector.}
  \item{Time.character}{factor; vector.}
  \item{Character}{factor; vector.}
}
}

\examples{
\donttest{data(ex.data)
summary(ex.data)
}
}

\keyword{datasets}
