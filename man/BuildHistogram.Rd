\name{BuildHistogram}

\alias{BuildHistogram}

\title{Set Histogram Input Parameters}

\description{
A \acronym{GUI} for specifying input parameters of the \code{\link{hist}}
function.
}

\usage{
BuildHistogram(d, var.names = NULL, var.default = 1L, parent = NULL)
}

\arguments{
\item{d}{list, data.frame, matrix, or numeric; vector(s) of values for which the
  histogram is desired.}
\item{var.names}{character; names corresponding to each vector (column) in
  \code{d}.}
\item{var.default}{character or integer; vector name or index in \code{d},
  defaults to \code{1L}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\details{
Plot histogram and view results.
}

\value{
Does not return any value.
}

\author{J.C. Fisher}

\seealso{
\code{\link{hist}}, \code{\link{plot.histogram}}, \code{\link{rug}},
\code{\link{density}}
}

\examples{
\donttest{data(ex.project)
BuildHistogram(ex.project$data.pts)
}}

\keyword{misc}
