\name{EvalFunction}

\alias{EvalFunction}

\title{Parse and Evaluate an RSurvey Expression}

\description{
Parses and evaluates a character string representation of an RSurvey expression.
}

\usage{
EvalFunction(txt, cols)
}

\arguments{
\item{txt}{character; a string representation of an \R function;
  see \sQuote{Details}.}
\item{cols}{list; see \code{\link{ManageVariables}}.}
}

\value{
The result of evaluating the text epression.
}

\seealso{
\code{\link{parse}}, \code{\link{eval}}
}

\author{J.C. Fisher}

\examples{
d <- list(x = 1:10, y = 10:1)
Data("data.raw", d)
cols <- list()
cols[[1]] <- list(id = "X", index = 1, fun = "\"X\"")
cols[[2]] <- list(id = "Y", index = 2, fun = "\"Y\"")
EvalFunction("\"Y\"", cols)
EvalFunction("\"X\" + \"Y\"", cols)
EvalFunction("rnorm(12)", cols)
}

\keyword{utilities}
