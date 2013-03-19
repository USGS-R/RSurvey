\name{WriteFile}

\alias{WriteFile}

\title{Write Data File}

\description{
Writes data to a file.
}

\usage{
WriteFile(file.type = "text", file.name = NULL, col.ids = NULL,
          is.processed = TRUE, headers = c(FALSE, FALSE), 
          sep = "\t", is.compressed = FALSE, encoding = getOption("encoding"))
}

\arguments{
\item{file.type}{character; the output file type:
  either \dQuote{text} for \emph{Text Files},
  \dQuote{shape} for \emph{ESRI Shapefiles}, or
  \dQuote{grid} for \emph{Interpolated Grid Text Files}.}
\item{file.name}{character; the name of the file which the data are to
  be written to.}
\item{col.ids}{character; a vector of strings identifying the variables to
  include in the exported data table.}
\item{is.processed}{logical; if \code{TRUE}, only include record numbers in
  the processed data table, see \code{\link{ProcessData}};
  its default is \code{TRUE}.}
\item{headers}{logical; a vector of length 2 that indicates whether the
  data table contains header lines.}
\item{sep}{character; the field separator string.}
\item{is.compressed}{logical; if \code{TRUE}, the file will be compressed by
  \href{http://www.gzip.org/}{gzip}; its default is \code{FALSE}.}
\item{encoding}{character; encoding to be assumed for input strings. If the
  value is \code{"latin1"} or \code{"UTF-8"} it is used to mark character
  strings as known to be in Latin-1 or UTF-8: it is not used to re-encode the
  input.}
}

\author{J.C. Fisher}

\seealso{
\code{\link{write.table}}, \code{writeOGR}
}

\examples{
\donttest{
data(project)
Data(replace.all = project)
WriteFile(file.type = "grid")
}
}

\keyword{file}
