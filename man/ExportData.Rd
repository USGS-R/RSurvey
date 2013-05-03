\name{ExportData}

\alias{ExportData}

\title{Set Parameters for Data Export}

\description{
A \acronym{GUI} for exporting data to text files or shapefiles.
}

\usage{
ExportData(file.type = "txt", parent = NULL)
}

\arguments{
\item{file.type}{character; the output file type: either
  \dQuote{txt} for \emph{Text Files},
  \dQuote{rda} for \emph{R Data File}, or
  \dQuote{shp} for \emph{ESRI Shapefiles}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\seealso{
\code{\link{write.table}}, \code{\link{save}}, \code{writeOGR}
}

\author{J.C. Fisher}

\examples{
\donttest{ExportData(file.type = "txt")}
}

\keyword{IO}
