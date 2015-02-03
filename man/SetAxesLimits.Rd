\name{SetAxesLimits}

\alias{SetAxesLimits}

\title{Set Axes Limits}

\description{A \acronym{GUI} for specifying axes limits.}

\usage{SetAxesLimits(lim = NULL, parent = NULL)}

\arguments{
\item{lim}{list; contains the current plotting limits, see \sQuote{Value}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Returns a list containing the following components:
  \item{x1, x2}{numeric; the minimum and maximum \code{x} value.}
  \item{y1, y2}{numeric; the minimum and maximum \code{y} value.}
  \item{z1, z2}{numeric; the minimum and maximum \code{z} value.}
  \item{x1.chk, x2.chk}{logical; if \code{TRUE} a default
    value is used for the minimum and maximum \code{x} value.}
  \item{y1.chk, y2.chk}{logical; if \code{TRUE} a default
    value is used for the minimum and maximum \code{y} value.}
  \item{z1.chk, z2.chk}{logical; if \code{TRUE} a default
    value is used for the minimum and maximum \code{z} value.}
  \item{x}{numeric; a vector of \code{x} limits \code{(x1,x2)},
    default is \code{(NA,NA)}.}
  \item{y}{numeric; a vector of \code{y} limits \code{(y1,y2)},
    default is \code{(NA,NA)}.}
  \item{z}{numeric; a vector of \code{z} limits \code{(z1,z2)},
    default is \code{(NA,NA)}.}
}

\author{J.C. Fisher}

\examples{\dontrun{SetAxesLimits()}}

\keyword{misc}
