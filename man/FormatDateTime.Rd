\name{FormatDateTime}

\alias{FormatDateTime}

\title{Build Date-Time String Formats}

\description{
A \acronym{GUI} for converting between character representations and objects of
class \dQuote{POSIXt} or \dQuote{Date}.
}

\usage{
FormatDateTime(sample = as.POSIXct("1991-08-25 20:57:08", tz = "GMT"), fmt = "",
               parent = NULL)
}

\arguments{
\item{sample}{POSIXt or Date; a time object.}
\item{fmt}{character; the format conversion specification string for time
  values.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Returns a character string representing the formatted date-time value.
}

\seealso{
\code{\link{strptime}}, \code{\link{format}}
}

\author{J.C. Fisher}

\examples{
\donttest{new.fmt <- FormatDateTime(fmt = "\%A \%B \%d \%I:\%M \%p")
FormatDateTime(Sys.Date())
}}

\keyword{misc}
