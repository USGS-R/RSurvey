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

\value{
Sets the following components in \code{\link{Data}}:
\item{export$processed}{logical; are exported records limited to processed
  data?}
\item{export$fmts}{logical;  indicates whether a header line of conversion
  specification format strings is written (text only).}
\item{export$cols}{logical;  indicates whether a header line of column names is
  written (text only).}
\item{export$rows}{logical; indicates whether the row names are written
  (text only).}
\item{export$comment}{logical; indicates whether to write comments using the
  comment character, \code{export$com} (text only).}
\item{export$sep}{character; the field separator character (text only).}
\item{export$dec}{character; string used for decimal points (text only).}
\item{export$nas}{character; string interpreted as \code{\link{NA}} value
  (text only).}
\item{export$com}{character; comment character (text only).}
\item{export$qmethod}{character; a string specifying how to deal with embedded
  double quote characters when quoting strings (text only).}
\item{export$quote}{logical; if \code{TRUE}, any character or factor columns
  will be surrounded by double quotes (text only).}
\item{export$encoding}{character; declares the encoding to be used on the file
  (text only).}
\item{export$eol}{character; the character to print at the end of each line
  (text only).}
\item{export$compressed}{logical; indicate whether the file should be
  compressed using \href{http://www.gzip.org/}{gzip} (text only).}
\item{export$changelog}{logical; indicate if a separate text file should be
  written with the change log (text only).}
\item{export$ascii}{logical; if \code{TRUE}, an ASCII representation of the data
  is written (R data only).}
}

\seealso{
\code{\link{write.table}}, \code{\link{save}}, \code{writeOGR}
}

\author{J.C. Fisher}

\examples{
\donttest{ExportData(file.type = "txt")}
}

\keyword{IO}
