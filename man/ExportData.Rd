\name{ExportData}

\alias{ExportData}

\title{Set Parameters for Data Export}

\description{
A \acronym{GUI} for exporting data to text files or shapefiles.
}

\usage{
ExportData(file.type = "text", parent = NULL)
}

\arguments{
\item{file.type}{character; the output file type: either
  \dQuote{text} for \emph{Text Files} or
  \dQuote{shape} for \emph{ESRI Shapefiles}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\seealso{
\code{\link{write.table}}, \code{writeOGR}
}

\author{J.C. Fisher}

\examples{
\donttest{ExportData(file.type = "text")}
}

\keyword{IO}
