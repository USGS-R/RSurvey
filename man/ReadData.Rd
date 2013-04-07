\name{ReadData}

\alias{ReadData}

\title{Read Data}

\description{
Reads table formatted data from a connection and creates a data frame from it.
}

\usage{
ReadData(con, headers = c(FALSE, FALSE), sep = "\t", dec = ".",
         quote = "\"'", nrows = -1, na.strings = c("", "NA"),
         skip = 0, comment.char = "#", encoding = getOption("encoding"))
}

\arguments{
\item{con}{connection; a \code{\link{connection}} object.}
\item{headers}{logical; a vector of length 2 that indicates whether the
  data table contains header lines: see \sQuote{Details}.}
\item{sep}{character; the field separator string. Values on each line of the
  file are separated by this string.}
\item{dec}{character; used in the file for decimal points.}
\item{quote}{character; the set of quoting characters.}
\item{nrows}{integer; the maximum number of rows to read in. Negative and other
  invalid values are ignored (optional).}
\item{na.strings}{character; a vector of strings which are interpreted as
  \code{\link{NA}} values. Blank fields are also considered to be missing
  values.}
\item{skip}{integer; the number of lines to skip before beginning to read data.}
\item{comment.char}{character; a vector of length one containing a single
  character or an empty string. Use "" to turn off the interpretation of
  comments altogether.}
\item{encoding}{character; encoding to be assumed for input strings. If the
  value is \code{"latin1"} or \code{"UTF-8"} it is used to mark character
  strings as known to be in Latin-1 or UTF-8: it is not used to re-encode the
  input.}
}

\format{
The imported data table requires at least two numeric variables.
}

\details{
This function is the primary method for importing table formatted data from
a text file. The \code{headers} argument, a logical vector of length 2,
indicates whether the file contains the  conversion specification formats and 
names of the variables as its initial lines. For example,
\code{headers = c(FALSE, TRUE)} indicates that the first 
line contains the names of variables, and no formats are included. 
If \code{headers = c(FALSE, FALSE)}, the default, no header information is 
contained within the file.

Formats are the character representation of object types
used to: identify column classes prior to reading in data,
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
\item{data.raw}{data.frame; a data table with headers and comments removed.}
\item{cols}{list; length equal to the current number of data variables.
  Each component in \code{cols} is linked to a specific variable,
  see \code{\link{ManageData}}.}
\item{vars}{list; an initial guess of the state variables. Integer components
  \code{x}, \code{y}, and \code{z} specify the index number in
  \code{cols} that correspond to the respective state variable.}
}

\author{J.C. Fisher}

\seealso{
\code{\link{read.table}}
}

\examples{
f <- system.file("extdata/DataExample.txt", package = "RSurvey")
con <- file(f, open = "r", encoding = "latin1")
ans <- ReadData(con, headers = c(TRUE, TRUE))
close(con)
}

\keyword{file}
