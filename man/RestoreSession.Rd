\name{RestoreSession}

\alias{RestoreSession}

\title{Restore R Session from Source Files}

\description{
This function restores local objects within the current \R session.
}

\usage{RestoreSession(path, save.objs, fun.call)}

\arguments{
\item{path}{character; directory pathname, if missing a folder browser pop up
  dialog box is implemented.}
\item{save.objs}{character; a vector of local object names.  These objects will
  be preserved during the recompiling process.}
\item{fun.call}{character, the name of the function to call after restoring the
  session (optional.)}
}

\value{
An updated \R session compiled from \file{.R} source code files located in
\code{path} and its recursive directories.
}

\note{
Restoring \code{\link{Data}} clears its components.
}

\author{Fisher, J.C.}

\seealso{
\code{\link{list.files}}, \code{\link{source}}
}

\examples{
\dontrun{RestoreSession()}
}

\keyword{internal}
