\name{ex.data}

\alias{ex.data}

\docType{data}

\title{Example Data Set}

\description{
This is an example data set that is typically used to test \pkg{RSurvey}.
}

\usage{ex.data}

\format{
A data frame with 50 rows and the following columns:
\describe{
 \item{T.date.time}{POSIXct; vector of date-time values.}
  \item{X.numeric..m}{numeric; vector.}
  \item{Y.numeric..m}{numeric; vector.}
  \item{Z.numeric..ft}{numeric; vector.}
  \item{Z.numeric..ft..1.}{numeric; vector.}
  \item{Z.numeric..m}{numeric; vector.}
  \item{Unknown}{numeric; vector.}
  \item{Logical}{integer; vector.}
  \item{Integer..psia}{integer; vector.}
  \item{Date}{Date; vector.}
  \item{Time}{POSIXct; vector.}
  \item{Character}{factor; vector.}
}}

\examples{\donttest{
data(ex.data)
summary(ex.data)
}}

\keyword{datasets}
