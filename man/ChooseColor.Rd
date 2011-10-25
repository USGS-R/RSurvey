\name{ChooseColor}

\alias{ChooseColor}

\title{Set Graphic Color}

\description{
A \acronym{GUI} for electing a graphic color.
}

\usage{
ChooseColor(col = NA, parent = NULL)
}

\arguments{
\item{col}{character; the initial color in hexadecimal form.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Returns the selected color as a character string in hexadecimal format.
}

\author{Fisher, J.C.}

\examples{
\donttest{ChooseColor(col = "#669933")}
}

\keyword{misc}
