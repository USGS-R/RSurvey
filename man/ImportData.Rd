\name{ImportData}

\alias{ImportData}

\title{Import Data}

\description{
A \acronym{GUI} for reading table formatted data.
}

\usage{
ImportData(parent = NULL)
}

\arguments{
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\details{
This \acronym{GUI} lets you specify the format and connection type for table
data. Data connections are defined as the path to the file to be opened, a
complete \acronym{URL} (e.g. http://, ftp:// or file://), or windows clipboard.
Files are limited to text format (\file{.txt} \file{.csv}, or
\file{.dat}); however, they can be compressed by \code{gzip} with additional
extension \file{.gz}.
}

\value{
Queries and sets \code{\link{Data}} components:
\code{headers}, \code{skip}, \code{sep}, \code{nrows}, \code{na.strings},
\code{quote}, \code{comment.char}, see \code{\link{ReadData}}; and
\code{data.source}, where
\item{data.source}{character; a description of the connection (i.e.
  a file pathname).}
}

\note{
Requires the Tcl package
\href{http://tktable.sourceforge.net/}{Tktable}.
If Tktable is not available the \code{\link{ReadData}} function is called
directly using default argument values.
}

\seealso{
\code{\link{ReadData}}, \code{\link{read.table}}, \code{\link{connections}}
}

\author{J.C. Fisher}

\examples{
\donttest{tclRequire("Tktable", warn = TRUE)
ImportData()
}
}

\keyword{misc}
