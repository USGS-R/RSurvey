\name{ExportData}

\alias{ExportData}

\title{Set Parameters for Data Export}

\description{
A \acronym{GUI} for exporting data to text files or shapefiles.
}

\usage{
ExportData(col.ids, file.type = "text", parent = NULL)
}

\arguments{
\item{col.ids}{character; a vector of strings identifying the variables to
  include in the exported data table.}
\item{file.type}{character; the output file type: either
  \dQuote{text} for \emph{Text Files} or
  \dQuote{shape} for \emph{ESRI Shapefiles}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\seealso{
\code{\link{WriteFile}}
}

\author{Fisher, J.C.}

\examples{
\donttest{ExportData(col.ids = c("V1", "V2", "V3"), file.type = "text")}
}

\keyword{misc}
