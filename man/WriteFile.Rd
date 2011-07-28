\name{WriteFile}

\alias{WriteFile}

\title{Write Data File}

\description{
Exports post-processed data to a file.
}

\usage{
WriteFile(ext = "txt")
}

\arguments{
\item{ext}{character; the default \code{file} extension.}
}

\value{
The format and type of data written is based on the file type chosen within
the file management pop-up dialog box. A selection of \emph{Text Files}
(\file{*.txt}, \file{*.csv}, \file{*.dat}) writes the contents of
\code{data.pts} to a text file;
selecting \emph{Interpolated Grid Test Files} (\file{*.grd}) writes the contents of
\code{data.grd} to a text file; and a selection of
\emph{ESRI Shapefiles} (\file{*.shp}) writes the contents of \code{data.pts} to
a binary file.
}

\author{Fisher, J.C.}

\seealso{
\code{\link{GetFile}}, \code{\link{write.table}}, \code{writeOGR}
}

\examples{
\donttest{
data(project)
Data(replace.all = project)
WriteFile("txt")
}
}

\keyword{file}
