\name{Format}

\alias{Format}

\title{Build C-Style String Formats}

\description{A \acronym{GUI} for the system \code{sprintf} C-library function.}

\usage{Format(sample = pi, fmt = "", parent = NULL)}

\arguments{
\item{sample}{logical, integer, numeric, character, or factor;
  a sample value.}
\item{fmt}{character; the conversion specification format,
  see \code{\link{sprintf}}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{Returns a character string.}

\seealso{\code{\link{sprintf}}, \code{\link{format}}}

\author{J.C. Fisher}

\examples{\donttest{
Format(sample = pi, fmt = "\%3.8f")
Format(sample = 3L)
Format(sample = TRUE)
Format(sample = "string")
}}

\keyword{misc}
