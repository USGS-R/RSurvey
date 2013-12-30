\name{ImportSpreadsheetData}

\alias{ImportSpreadsheetData}

\title{Import Data from XML Spreadsheet}

\description{
A \acronym{GUI} for loading selected data sets from a Open XML Spreadsheet file.
}

\usage{
ImportSpreadsheetData(parent = NULL)
}

\arguments{
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Returns a list with the following components:
\item{d}{data.frame; data set.}
\item{src}{character; something.}
}

\author{J.C. Fisher}

\seealso{
\code{\link{data}}
}

\examples{
\donttest{ans <- ImportSpreadsheetData()}
}

\keyword{IO}
