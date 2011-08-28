\name{FormatDateTime}

\alias{FormatDateTime}

\title{A \acronym{GUI} for constructing date and time formats.}

\description{
A \acronym{GUI} for converting between character representations and objects of
class \dQuote{POSIXt}, calendar dates and times.
}

\usage{
FormatDateTime(sample = as.POSIXct("1991-08-25 20:57:08"),
               fmt = NULL, parent = NULL)
}

\arguments{
\item{sample}{POSIXct or POSIXlt; a calendar date and time object.}
\item{fmt}{character; the conversion specification format for date-time values.}
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
\donttest{FormatDateTime(fmt = "\%d/\%m/\%Y")}
}

\keyword{misc}
