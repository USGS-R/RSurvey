\name{ExportData}

\alias{ExportData}

\title{Export Data to File}

\description{
A \acronym{GUI} for exporting data to text files or shapefiles.
}

\usage{
ExportData(col.ids, file.type = "text", parent = NULL)
}

\arguments{
\item{col.ids}{character; something.}
\item{file.type}{character; something.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Something
}

\seealso{
\code{\link{WriteFile}}
}

\author{Fisher, J.C.}

\examples{
\donttest{ExportData(col.ids = c("V1", "V2", "V3"), file.type = "txt")}
}

\keyword{misc}
