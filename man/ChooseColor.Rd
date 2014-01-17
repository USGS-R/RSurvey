\name{ChooseColor}

\alias{ChooseColor}

\title{Set Graphic Color}

\description{A \acronym{GUI} for selecting a graphic color.}

\usage{ChooseColor(col, parent = NULL)}

\arguments{
\item{col}{character; the initial color, see \sQuote{Value}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Returns a selected color in terms of its \code{RGB} components with
a string of the form \code{"#RRGGBB"} where each of the pairs
\code{RR}, \code{GG}, \code{BB} consist of two hexadecimal digits
giving a value in the range \code{00} to \code{FF}.
}

\author{J.C. Fisher}

\seealso{\code{\link{col2rgb}}}

\examples{\donttest{ChooseColor(col = "#669933")}}

\keyword{misc}
