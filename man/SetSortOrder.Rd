\name{SetSortOrder}

\alias{SetSortOrder}

\title{Set Sort Order}

\description{
A \acronym{GUI} for specifying the variable used to sort the data set.
}

\usage{
SetSortOrder(col.ids, sort.on = NULL, parent = NULL)
}

\arguments{
\item{col.ids}{character; a vector of variable names.}
\item{sort.on}{integer; the index of the variable used to sort the data set.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Returns an updated \code{sort.on} object.
}

\author{J.C. Fisher}

\examples{\donttest{
col.ids <- c("Variable1", "Variable2", "Variable3")
sort.on <- 2
attr(sort.on, "decreasing") <- TRUE
attr(sort.on, "na.last") <- FALSE
SetSortOrder(col.ids, sort.on)
}}

\keyword{misc}
