\name{Search}

\alias{Search}

\title{Search}

\description{
A \acronym{GUI} for establishing find and replace arguments.
}

\usage{
Search(is.replace = FALSE, defaults = NULL, col.names = NULL, parent = NULL)
}

\arguments{
\item{is.replace}{logical; if \code{TRUE}, the replace component is included.}
\item{defaults}{list; see \sQuote{Value}.}
\item{col.names}{character; a vector of variable (column) names.}
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
  \item{is.search.col}{logical; is the search limited to a single variable?}
  \item{is.perl}{logical; should Perl style regular expressions be used?.}
  \item{col.name}{character; variable (column) name which the search will be
    restricted to.}
}

\author{J.C. Fisher}

\examples{\donttest{
ans <- Search()
}}

\keyword{misc}
