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
\item{multi}{logical; if \code{TRUE} multiple files may be selected,
  its default is \code{FALSE}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
If \code{multi} is \code{FALSE} returns a list containing the following
components:
  \item{path}{character; the \code{file} path}
  \item{dir}{character; the directory that contains the \code{file}}
  \item{name}{character; the \code{file} name}
  \item{ext}{character; the \code{file} extension}
  \item{type}{character; the \code{file} type}
Otherwise, a list is returned containing list components for each file.
}

\author{Fisher, J.C.}

\examples{
\donttest{GetFile()}
}

\keyword{file}
