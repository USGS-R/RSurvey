\name{SummarizeData}

\alias{SummarizeData}

\title{Summarize Object}

\description{
A summary of the descriptive statistics of an array object.
}

\usage{
SummarizeData(obj, fmt = NULL)
}

\arguments{
\item{obj}{an array object for which the summary is desired.}
\item{fmt}{character; the conversion specification format,
  see \code{\link{sprintf}}.}
}

\value{
Results are dependent on the class of \code{obj}. Returns a list with the
following components:
  \item{Count}{integer; array length.}
  \item{NAs}{integer; number of \code{\link{NA}} values.}
  \item{Class}{character; the objects \code{class} attribute.}
  \item{Min., Max.}{numeric; extreme values with \code{NA} values ignored.}
  \item{1st Qu., Median, 3rd Qu.}{numeric; estimates of the underlying
    distribution quantiles with \code{NA} values ignored.}
  \item{Mean}{numeric; arithmetic mean with \code{NA} values ignored.}
  \item{St.Dev.}{numeric; standard deviation with \code{NA} values ignored.}
  \item{Sum}{numeric; sum with \code{NA} values ignored.}
  \item{Hist}{histogram; an object of class histogram, see \code{hist}
    documentation for details.}
  \item{Unique}{integer; number of unique factors.}
  \item{TRUE, FALSE}{integer; number of \code{TRUE} and \code{FALSE} values,
    respectively.}
  \item{String}{character; a formatted text summary of the descriptive
    statistics.}
  \item{Time Per.}{character; a formatted time duration.}
}

\seealso{
\code{\link{quantile}}, \code{\link{hist}}
}

\author{Fisher, J.C.}

\examples{
summary(attenu$dist, digits = 4)
SummarizeData(attenu$dist, fmt = "\%.2f")
SummarizeData(as.POSIXct(Sys.time() + 1:10), fmt = "\%a \%H:\%M:\%S")
}

\keyword{misc}
