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

\details{
The \dQuote{DATA} identifier is a reserved word within the \code{txt} argument.
\dQuote{DATA} is used to reference the \code{data.raw} data
frame, a component of \code{\link{Data}} with variable names keyed to
column index numbers in \code{data.raw} using the \code{vars} argument.
}

\value{
The result of evaluating the \code{txt} object after the appropriate
substitutions for \dQuote{DATA} has been made.
\code{Inf}, \code{-Inf}, and \code{NaN} values are converted to \code{NA} in
numeric vectors.
}

\seealso{
\code{\link{parse}}, \code{\link{eval}}, \code{\link{round}},
\code{\link{is.infinite}}, \code{\link{is.nan}}
}

\author{Fisher, J.C.}

\examples{data(tritest)
Data("data.raw", as.data.frame(tritest))
cols <- list()
cols[[1]] <- list(id = "X", index = 1, fun = "DATA[[\"X\"]]")
cols[[2]] <- list(id = "Y", index = 2, fun = "DATA[[\"Y\"]]")
EvalFunction("DATA[[\"X\"]]", cols)
cols[[1]]$digits <- 0
EvalFunction("DATA[[\"X\"]]", cols)
EvalFunction("DATA[[\"X\"]] + DATA[[\"Y\"]]", cols)
EvalFunction("rnorm(12)", cols)
}

\keyword{utilities}
