\name{SetSortOrder}

\alias{SetSortOrder}

\title{Set Sort Order}

\description{
A \acronym{GUI} for specifying the variable used to sort the data set.
}

\usage{SetSortOrder(col.ids, sort.on = NULL, parent = NULL)}

\arguments{
\item{col.ids}{character; a vector of variable names.}
\item{sort.on}{integer; the index of the variable used to sort the data set.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{
Returns an integer object that specifies the index of the variable used
to sort the data set. Attributes for this object include:
\code{decreasing}, a logical value indicating if the sort order is increasing
or decreasing; and \code{na.last}, a logical value for controlling the
treatment of \code{NA}s during sorting. If \code{TRUE}, missing values in the
data are put last; if \code{FALSE}, they are put first; if \code{NA},
they are removed.
}

\author{J.C. Fisher}

\seealso{\code{\link{order}}}

\examples{\dontrun{
col.ids <- c("Variable1", "Variable2", "Variable3")
sort.on <- 2
attr(sort.on, "decreasing") <- TRUE
attr(sort.on, "na.last") <- FALSE
SetSortOrder(col.ids, sort.on)
}}

\keyword{misc}
