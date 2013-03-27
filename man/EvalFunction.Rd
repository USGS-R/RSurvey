\name{EvalFunction}

\alias{EvalFunction}

\title{Evaluates an \R Statement}

\description{
Evaluates a character string representation of an \R statement.
}

\usage{
EvalFunction(txt, cols)
}

\arguments{
\item{txt}{character; a string representation of an \R function;
  see \sQuote{Details}.}
\item{cols}{list; see \code{\link{ManageData}}.}
}

\value{
The result of evaluating the \code{txt} object after the appropriate
substitutions have been made.
\code{Inf}, \code{-Inf}, and \code{NaN} values are converted to \code{NA} in
numeric vectors.
}

\seealso{
\code{\link{parse}}, \code{\link{eval}}
}

\author{J.C. Fisher}

\examples{
d <- data.frame(cbind(x = 1, y = 1:10))
Data("data.raw", d)
cols <- list()
cols[[1]] <- list(id = "X", index = 1, fun = "\"X\"")
cols[[2]] <- list(id = "Y", index = 2, fun = "\"Y\"")
EvalFunction("\"Y\"", cols)
EvalFunction("\"X\" + \"Y\"", cols)
EvalFunction("rnorm(12)", cols)
}

\keyword{utilities}
