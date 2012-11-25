\name{EditFunction}

\alias{EditFunction}

\title{Function Editor for Table Data}

\description{
A \acronym{GUI} for defining functions in the \R language.
}

\usage{
EditFunction(cols, index = NULL, fun = NULL, value.length = NULL, 
             value.class = NULL, win.title = "Edit Function", parent = NULL)
}

\arguments{
\item{cols}{list; see \code{\link{ManageData}}.}
\item{index}{integer; an element index number in \code{cols}.}
\item{fun}{character; existing function, only used if \code{index=NULL}.}
\item{value.length}{integer; the required \code{\link{length}} for the 
  evaluated function.}
\item{value.class}{character; the required \code{\link{class}} for the 
  evaluated function.}
\item{win.title}{character; a string to display as the title of the dialog box.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\details{
This \acronym{GUI} is appropriate for defining new variables in a
pre-existing data frame or query building.
}

\value{
Results in a character string of the user defined function; when evaluated,
this string must be parseable.
}

\author{J.C. Fisher}

\seealso{
\code{\link{EvalFunction}}, \code{\link{parse}}
}

\examples{
\donttest{data(tritest)
Data("data.raw", as.data.frame(tritest))
cols <- list()
cols[[1]] <- list(id = "X", index = 1, fun = "\"X\"")
cols[[2]] <- list(id = "Y", index = 2, fun = "\"Y\"")
cols[[3]] <- list(id = "New Variable",
                  fun = "\"X\" + \"Y\"")
EditFunction(cols, index = 3)
}
}

\keyword{misc}
