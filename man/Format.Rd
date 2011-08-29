\name{Format}

\alias{Format}

\title{Build C-style string formats.}

\description{
A \acronym{GUI} for the system \code{sprintf} C-library function.
}

\usage{
Format(sample = pi, fmt = NULL, parent = NULL)
}

\arguments{
\item{sample}{a sample value. Only logical, integer, numeric, character, and
  factor classes are supported.}
\item{fmt}{character; the conversion specification format, see \code{\link{sprintf}}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Returns a character string.
}

\seealso{
\code{\link{sprintf}}, \code{\link{format}}
}

\author{Fisher, J.C.}

\examples{
\donttest{Format(sample = pi, fmt = "\%3.8f")
Format(sample = 3L)
Format(sample = TRUE)
Format(sample = "string")}
}

\keyword{misc}
