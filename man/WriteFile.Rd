\name{WriteFile}

\alias{WriteFile}

\title{Write Data File}

\description{
Writes data to a file.
}

\usage{
WriteFile(file.type = "text", file.name = NULL, col.ids = NULL,
          headers = c(FALSE, FALSE, FALSE), sep = "\t",
          is.processed = TRUE, is.compress = FALSE,
          encoding = getOption("encoding"))
}

\arguments{
\item{file.type}{character; somthing.}
\item{file.name}{character; the name of the file which the data are to
  be written to.}
\item{col.ids}{character; a vector identifying the variables to include
  in the exported data table.}
\item{headers}{logical; a vector of length 3 that indicates whether the
  data table contains header lines.}
\item{sep}{character; somthing.}
\item{is.processed}{logical; somthing.}
\item{is.compress}{logical; somthing.}
\item{encoding}{character; encoding to be assumed for input strings. If the
  value is \code{"latin1"} or \code{"UTF-8"} it is used to mark character
  strings as known to be in Latin-1 or UTF-8: it is not used to re-encode the
  input.}
}

\value{
The format and type of data written is based on the file type chosen in
the file management pop-up dialog box. A selection of \emph{Text Files}
(\file{*.txt}, \file{*.csv}, \file{*.dat}) writes the contents of
\code{data.pts} to a text file;
selecting \emph{Interpolated Grid Test Files} (\file{*.grd}) writes the
contents of \code{data.grd} to a text file; and a selection of
\emph{ESRI Shapefiles} (\file{*.shp}) writes the contents of
\code{data.pts} to a binary file.
}

\author{Fisher, J.C.}

\seealso{
\code{\link{GetFile}}, \code{\link{write.table}}, \code{writeOGR}
}

\examples{
\donttest{
data(project)
Data(replace.all = project)


WriteFile(file.type = "grid")
}
}

\keyword{file}
