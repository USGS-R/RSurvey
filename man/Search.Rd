\name{Search}

\alias{Search}

\title{Search}

\description{
A \acronym{GUI} for establishing find and replace arguments in a data table.
}

\usage{
Search(is.replace = FALSE, defaults = NULL, parent = NULL)
}

\arguments{
\item{is.replace}{logical; if \code{TRUE}, the replace component is included.}
\item{defaults}{list; see \sQuote{Value}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\details{
Something
}

\value{
A \code{list} with the following components:
  \item{find.what}{character; the string to search for.}
  \item{replace.with}{character; the replace string.}
  \item{is.match.word}{logical; should matches be restricted to whole words
    only?}
  \item{is.match.case}{logical; is the search case sensitive?}
  \item{is.reg.exps}{logical; should the search be made using
    \link{regular expression} (that is, a pattern that describes a set of
    strings)?}
  \item{is.search.col}{logical; is the search limited to a single column?}
  \item{is.perl}{logical; should Perl style regular expressions be used?}
  \item{is.replace.first}{logical; replace only the first instance?}
  \item{is.search.sel}{logical; is the search limited to selected cells?}
}

\author{J.C. Fisher}

\examples{\donttest{
ans <- Search()
}}

\keyword{misc}
