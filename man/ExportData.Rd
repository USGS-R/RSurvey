\name{ExportData}

\alias{ExportData}

\title{Export Data}

\description{
A \acronym{GUI} for exporting data to text files, shapefiles, or R data files.
}

\usage{ExportData(file.type = "txt", parent = NULL)}

\arguments{
\item{file.type}{character; the output file type: either
  \dQuote{txt} for \emph{Text Files},
  \dQuote{rda} for \emph{R Data Files}, or
  \dQuote{shp} for \emph{ESRI Shapefiles}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Saves the \acronym{GUI} options in the \code{export} component of
\code{\link{Data}}. List components of \code{export} include:
\item{processed}{logical; are exported records limited to processed
  data?}
\item{fmts}{logical; indicates whether a header line of conversion
  specification format strings is written (text only).}
\item{cols}{logical; indicates whether a header line of column names is
  written (text only).}
\item{rows}{logical; indicates whether the row names are written
  (text only).}
\item{comment}{logical; indicates whether to write comments using the
  comment character, \code{com} (text only).}
\item{sep}{character; the field separator character (text only).}
\item{dec}{character; string used for decimal points (text only).}
\item{nas}{character; string interpreted as \code{\link{NA}} value
  (text only).}
\item{com}{character; comment character (text only).}
\item{qmethod}{character; a string specifying how to deal with embedded
  double quote characters when quoting strings (text only).}
\item{quote}{logical; if \code{TRUE}, any character or factor columns
  will be surrounded by double quotes (text only).}
\item{encoding}{character; declares the encoding to be used on the file
  (text only).}
\item{eol}{character; the character to print at the end of each line
  (text only).}
\item{compressed}{logical; indicate whether the file should be
  compressed using \href{http://www.bzip.org/}{bzip2} (text only).}
\item{changelog}{logical; indicate if a separate text file should be
  written with the change log (text only).}
\item{ascii}{logical; if \code{TRUE}, an \acronym{ASCII} representation of the
  data is written (R data only).}
}

\seealso{\code{\link{write.table}}, \code{\link{save}}, \code{writeOGR}}

\author{J.C. Fisher}

\examples{\donttest{
data(ex.project)
Data(replace.all = ex.project)
ExportData(file.type = "txt")
}}

\keyword{IO}
