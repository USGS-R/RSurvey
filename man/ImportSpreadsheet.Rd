\name{ImportSpreadsheet}

\alias{ImportSpreadsheet}

\title{Import Data from XML Spreadsheet File}

\description{
A \acronym{GUI} for loading selected data sets from an Open XML Spreadsheet
file (*.xlsx).
}

\usage{ImportSpreadsheet(parent = NULL)}

\arguments{\item{parent}{tkwin; the \acronym{GUI} parent window.}}

\value{
Returns a list with the following components:
\item{d}{data.frame; data set.}
\item{src}{character; a vector of length 2 that includes the pathname of the
  spreadsheet file and access date.}
}

\author{J.C. Fisher}

\references{
This code was derived with permission from Schaun Wheeler's
\href{https://gist.github.com/schaunwheeler/5825002}{xlsxToR} function.
}

\examples{\dontrun{ans <- ImportSpreadsheet()}}

\keyword{IO}
