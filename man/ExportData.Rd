\name{ExportData}

\alias{ExportData}

\title{Export Data}

\description{
A \acronym{GUI} for exporting data to text files, shapefiles, or R data files.
}

\usage{
ExportData(file.type = "txt", parent = NULL)
}

\arguments{
\item{file.type}{character; the output file type: either
  \dQuote{txt} for \emph{Text Files},
  \dQuote{rda} for \emph{R Data Files}, or
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
