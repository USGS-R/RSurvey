\name{EditFunction}

\alias{EditFunction}

\title{Function editor for table data}

\description{
A \acronym{GUI} for defining functions in the \R language.
}

\usage{
EditFunction(cols, index = NULL, parent = NULL)
}

\arguments{
\item{cols}{list; see \code{\link{ManageData}}.}
\item{index}{integer; an element index number in \code{cols}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\details{
This \acronym{GUI} is appropriate for defining new variables in a
pre-existing data frame.
}

\value{
Results in a character string of the user defined function; when evaluated,
this string must be parseable and result in a vector length equal to the
number of rows in the \code{data.raw} data frame (see \code{\link{ReadData}}).
}

\author{Fisher, J.C.}

\seealso{
\code{\link{EvalFunction}}, \code{\link{parse}}
}

\examples{
\donttest{data(tritest)
Data("data.raw", as.data.frame(tritest))
cols <- list()
cols[[1]] <- list(id = "X", index = 1, fun = "DATA[[\"X\"]]")
cols[[2]] <- list(id = "Y", index = 2, fun = "DATA[[\"Y\"]]")
cols[[3]] <- list(id = "New Variable",
                  fun = "DATA[[\"X\"]] + DATA[[\"Y\"]]")
EditFunction(cols, index = 3)
}
}

\keyword{misc}
