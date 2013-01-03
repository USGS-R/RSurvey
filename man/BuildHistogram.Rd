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
\item{d}{data.frame, matrix, or numeric; vector(s) of values for which the 
  histogram is desired.}
\item{var.names}{character; names corresponding to each vector (column) in \code{d}.}
\item{var.default}{character or integer; vector name or index in \code{d}, 
  defaults to \code{1L}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\details{
Histogram is interactively plotted or viewed in table format.
}

\value{
Does not return any value.
}

\author{J.C. Fisher}

\seealso{
\code{\link{hist}}, \code{\link{plot.histogram}}, \code{\link{ViewData}}
}

\examples{
\donttest{tclRequire("Tktable", warn = TRUE)
data(project)
d <- project$data.pts
BuildHistogram(d)
}
}

\keyword{misc}
