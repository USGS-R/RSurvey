\name{ChoosePch}

\alias{ChoosePch}

\title{Set Graphic Symbol}

\description{
A \acronym{GUI} for selecting a graphic symbol (pch).
}

\usage{
ChoosePch(pch = NA, parent = NULL)
}

\arguments{
\item{pch}{numeric or character; the initial selection of pch.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Returns a numeric or integer value based on user selection.
}

\author{Fisher, J.C.}

\examples{
\donttest{ChoosePch(pch = "+")}
}

\keyword{misc}
