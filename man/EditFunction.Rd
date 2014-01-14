\name{EditFunction}

\alias{EditFunction}

\title{Edit Function}

\description{
A \acronym{GUI} for defining functions in the \R language.
}

\usage{
EditFunction(cols, index = NULL, fun = NULL, value.length = NULL,
             value.class = NULL, win.title = "Edit Function", parent = NULL)
}

\arguments{
\item{cols}{list; see \code{\link{ManageVariables}}.}
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
This \acronym{GUI} is appropriate for deriving new variables in a
pre-existing data frame or query building.
}

\value{
Returns a list with two four components:
\code{fun}, the user defined function (when evaluated, this string must be parseable);
\code{class}, the object class of the evaluated function;
\code{summary}, a numeric summary; and
\code{sample} of the object.
}

\author{J.C. Fisher}

\seealso{
\code{\link{EvalFunction}}
}

\examples{\donttest{
d <- list(x = 1:10, y = 10:1)
Data("data.raw", d)
cols <- list()
cols[[1]] <- list(id = "X", index = 1, fun = "\"X\"")
cols[[2]] <- list(id = "Y", index = 2, fun = "\"Y\"")
cols[[3]] <- list(id = "New Variable", fun = "\"X\" + \"Y\"")
EditFunction(cols, index = 3)
}}

\keyword{misc}
