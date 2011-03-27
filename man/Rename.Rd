\name{Rename}

\alias{Rename}

\title{Rename Values in Character Vector}

\description{
A \acronym{GUI} for renaming values in a vector of character strings.
}

\usage{
Rename(names = NULL, cur.name = NULL, win.title = NULL,
       parent = NULL)
}

\arguments{
\item{names}{character; a vector of character strings.}
\item{cur.name}{character; sets the combobox value, name must be included in
  \code{names} (optional).}
\item{win.title}{character; the title of the main window (optional).}
\item{parent}{tkwin; the parent window (optional).}
}

\value{
Returns a character vector with updated values of \code{names}.
}

\author{Fisher, J.C.}

\examples{
\donttest{Rename(names = c("Name1", "Name2", "Name3"), cur.name = "Name2")}
}

\keyword{misc}
