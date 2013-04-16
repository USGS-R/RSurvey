\name{ImportData}

\alias{ImportData}

\title{Import Data}

\description{
A \acronym{GUI} for reading table formatted data from a text file.
}

\usage{
ImportData(parent = NULL)
}

\arguments{
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\details{
This \acronym{GUI} is a wrapper for the \code{\link{read.table}} function. 
Data connections are defined as the path to the file to be opened, a
complete \acronym{URL} (e.g. http://, ftp:// or file://), or windows clipboard.
Files are limited to text format (\file{.txt} \file{.csv}, or
\file{.dat}); however, they can be compressed by 
\emph{\href{http://www.gzip.org/}{gzip}} with additional extension \file{.gz}.

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
"\%d/\%m/\%Y \%I:\%M:\%S \%p".

Performance issues associated with reading in large files can be alleviated
by specifying formats in a header line, and giving the maximum number of
rows to read in.
}

\value{
Sets the following components in \code{\link{Data}}:
\item{import.file}{character; the name of the file which the data is read 
  from.}
\item{import.fmts}{logical; indicates whether the file contains the conversion 
  specification formats of the variables.}
\item{import.cols}{logical; indicates whether the file contains the names of 
  the variables.}
\item{import.skip}{integer; the number of lines skiped before data is read.}
\item{import.sep}{character; the field separator string.}
\item{import.dec}{character; used in the file for decimal points.}
\item{import.na}{character; string interpreted as \code{\link{NA}} values.}
\item{import.quote}{character; the set of quoting characters.}
\item{import.comment}{character; comment character.}
\item{import.encoding}{character; encoding that was assumed for input strings, 
  see \code{\link{Encoding}}.}
\item{data.raw}{data.frame; the imported (or raw) data table.}
\item{cols}{list; length equal to the current number of data variables.
  Each component in \code{cols} is linked to a specific variable,
  see \code{\link{ManageData}}.}
\item{vars}{list; an initial guess of the state variables. Integer components
  \code{x}, \code{y}, and \code{z} specify the index number in
  \code{cols} that correspond to the respective state variable.}
}

\note{
Requires the Tcl package
\href{http://tktable.sourceforge.net/}{Tktable}.
If Tktable is unavailable, an open-file dialog box is activated,  
followed by a call to the \code{\link{read.table}} function using default 
argument values.
}

\seealso{
\code{\link{read.table}}
}

\author{J.C. Fisher}

\examples{
\donttest{tclRequire("Tktable", warn = TRUE)
ImportData()
}
}

\keyword{misc}
