\name{ImportSpreadsheetData}

\alias{ImportSpreadsheetData}

\title{Import Data from XML Spreadsheet}

\description{
A \acronym{GUI} for loading selected data sets from an Open XML Spreadsheet
file.
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
\item{src}{character; a vector of length 2 that includes the pathname of the
  spreadsheet file and access date.}
}

\author{J.C. Fisher}

\references{
This code was derived (with permission) from Schaun Wheeler's
\href{https://gist.github.com/schaunwheeler/5825002}{xlsxToR} function,
accessed on 2014-01-01.
}

\seealso{
\code{\link{data}}
}

\examples{
\donttest{ans <- ImportSpreadsheetData()}
}

\keyword{IO}
