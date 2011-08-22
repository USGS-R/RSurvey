\name{FormatDateTime}

\alias{FormatDateTime}

\title{A \acronym{GUI} for constructing date and time formats.}

\description{
A \acronym{GUI} for converting between character representations and objects of
class \dQuote{POSIXt}, calendar dates and times.
}

\usage{
FormatDateTime(spec = "", parent = NULL)
}

\arguments{
\item{spec}{character; the conversion specification for date-time values.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Returns a character string representing the formatted time.
}

\seealso{
\code{\link{strptime}}, \code{\link{format}}
}

\author{Fisher, J.C.}

\examples{
\donttest{FormatDateTime(spec = "\%d/\%m/\%Y")}
}

\keyword{misc}
