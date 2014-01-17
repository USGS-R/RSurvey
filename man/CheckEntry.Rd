\name{CheckEntry}

\alias{CheckEntry}

\title{Control Content in Entry Widget}

\description{Content control for character strings based object class.}

\usage{CheckEntry(obj.class, ent.str = "")}

\arguments{
\item{obj.class}{character; object class.}
\item{ent.str}{character; entry value.}
}

\details{
The allowed object classes include: \var{real}, \var{integer}, and
  \var{logical}.
}

\value{
A character string with strict adherence to the format for the object class.
}

\author{J.C. Fisher}

\examples{
CheckEntry("numeric", "3.14ab")
## [1] "3.14"

CheckEntry("integer", "3.")
## [1] "3"
}

\keyword{manip}
