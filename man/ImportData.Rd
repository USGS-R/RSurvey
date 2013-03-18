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
\code{table.headers}, \code{table.skip}, \code{table.sep}, 
\code{table.na.strings}, \code{table.quote}, and \code{table.comment.char}, 
see \code{\link{ReadData}}.
}

\note{
Requires the Tcl package
\href{http://tktable.sourceforge.net/}{Tktable}.
If Tktable is unavailable, an open-file dialog box is activated,  
followed by a call to the \code{\link{ReadData}} function using default 
argument values.
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
