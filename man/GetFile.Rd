\name{GetFile}

\alias{GetFile}

\title{Select a File to Open or Save As}

\description{
A \acronym{GUI} for selecting files to open or save.
}

\usage{
GetFile(cmd = "Open", file = NULL, exts = NULL, initialdir = NULL,
        initialfile = NULL, defaultextension = NULL,
        win.title = cmd, multi = FALSE, parent = NULL)
}

\arguments{
\item{cmd}{character; specifies if an \code{"Open"} or \code{"Save As"} file
  management pop up dialog box is implemented.}
\item{file}{character; the file name which the data are to be read from.
  Alternatively, \code{file} can be a readable text-mode
  \code{\link{connection}}.}
\item{exts}{character; a vector of default file extensions.}
\item{initialdir}{character; specifies that the files in this directory should
  be displayed when the dialog pops up.}
\item{initialfile}{character; the file name to be displayed in the dialog when
  it pops up.}
\item{defaultextension}{character; the string that will be appended to the
  file name if the user enters a file name without an extension.}
\item{win.title}{character; a string to display as the title of the dialog box.}
\item{multi}{logical; if \code{TRUE} multiple files may be selected;
  its default is \code{FALSE}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
If \code{multi} is \code{FALSE} returns the file path as a charcter object with
the following attributes:
  \item{directory}{character; the directory that contains the file}
  \item{name}{character; the file name}
  \item{extension}{character; the file extension}
  \item{type}{character; the file type}
Otherwise, a list is returned containing a character object for each file.
}

\author{J.C. Fisher}

\examples{
\donttest{GetFile()}
}

\keyword{file}
