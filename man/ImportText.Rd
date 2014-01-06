\name{ImportText}

\alias{ImportText}

\title{Import Data from Text File}

\description{
A \acronym{GUI} for reading table formatted data from a text file.
}

\usage{
ImportText(parent = NULL)
}

\arguments{
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\details{
This \acronym{GUI} is a wrapper for the \code{\link{read.table}} function.
Data connections are defined as the path to the file to be opened, a
complete \acronym{URL} (e.g. http://, ftp:// or file://), or windows clipboard.
Files are limited to text format (e.g., \file{.tsv} \file{.csv}, or
\file{.txt}); however, they can be compressed by
\emph{\href{http://www.bzip.org/}{bzip2}} with additional extension \file{.bz2}.

Conversion specification formats are the character representation of object
types used to: identify column classes prior to reading in data,
and format values for printing.
Conversion specifications are based on C-style string formatting
commands for \code{numeric}, \code{integer}, and \code{character}
object classes, see \code{\link{sprintf}}; for example,
a format string of "\%.5f" applied to the mathematical constant \emph{pi}
results in "3.14159". Calendar date and time objects of class \code{POSIXct} are
defined by the ISO C99 / POSIX standard, see \code{\link{strftime}};
for example, "02/26/2010 02:05:39 PM" is represented using
"\%d/\%m/\%Y \%I:\%M:\%S \%p". The default time zone is Coordinated Universal
Time (UTC).

Comments located above data records and header lines are preserved; all other
comments are ignored. Requires the specification of a comment character.

Performance issues associated with reading in large files can be alleviated
by specifying formats in a header line, and giving the maximum number of
rows to read in.
}

\value{
Sets the following components in \code{\link{Data}}:
\item{data.raw}{data.frame; the imported (or raw) data table.}
\item{cols}{list; length equal to the current number of data variables.
  Each component in \code{cols} is linked to a specific variable,
  see \code{\link{ManageVariables}}.}
\item{comment}{character; a vector of comment strings.}
\item{import}{list; saved \acronym{GUI} options.}
Components of \code{import} include:
\item{source}{character; a vector of length 2 that includes the pathname of the
  text file and access date.}
\item{fmts}{logical; indicates whether the file contains the conversion
  specification format strings of the variables.}
\item{cols}{logical; indicates whether the file contains the names of the
  variables.}
\item{skip}{integer; the number of lines skiped before data is read.}
\item{sep}{character; the field separator string.}
\item{dec}{character; used in the file for decimal points.}
\item{na}{character; string interpreted as \code{\link{NA}} values.}
\item{quote}{character; the set of quoting characters.}
\item{comment}{character; comment character.}
\item{encoding}{character; encoding that was assumed for input strings, see
  \code{\link{Encoding}}.}
\item{str.as.fact}{logical; if \code{TRUE}, character variables are converted to
  factors.}
}

\note{
Requires the Tcl package \href{http://tktable.sourceforge.net/}{Tktable}.
}

\seealso{
\code{\link{read.table}}
}

\author{J.C. Fisher}

\examples{
\donttest{ImportText()}
}

\keyword{IO}
