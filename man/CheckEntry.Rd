\name{CheckEntry}

\alias{CheckEntry}

\title{Content Control within Entry Widget}

\description{
Content control for character strings based on an expected data type.
}

\usage{
CheckEntry(ent.typ, ent.str = "")
}

\arguments{
\item{ent.typ}{character; the entry type.}
\item{ent.str}{character; the entry value.}
}

\details{
The entry types include: \var{real}, \var{integer},
\var{hour}, \var{minute}, \var{second}, and \var{date}.
}

\value{
A character string with strict adherence to the specified data format.
}

\author{Fisher, J.C.}

\seealso{
\code{\link{tkentry}}
}

\examples{
CheckEntry("numeric", "3.14ab")
## [1] "3.14"

CheckEntry("integer", "3.")
## [1] "3"

CheckEntry("hour", "13")
## [1] "13"

CheckEntry("hour", "25")
## [1] "23"
}

\keyword{manip}
