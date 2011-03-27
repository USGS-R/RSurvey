\name{ViewData}

\alias{ViewData}

\title{View Data}

\description{
A \acronym{GUI} for viewing table formatted data.
}

\usage{
ViewData(d, col.names = NULL, col.units = NULL, col.digs = NULL,
         parent = NULL)
}

\arguments{
\item{d}{data.frame; data used to populate the table.}
\item{col.names}{character; a vector giving the column names for the
  data table (optional).}
\item{col.units}{character; a vector giving the measurement units for
  each column of the data table (optional).}
\item{col.digs}{integer; a vector giving the decimal places of numeric
  columns in the data table (optional).}
\item{parent}{tkwin; the parent window (optional).}
}

\details{
Column titles are a concatenation of variables \code{col.names}
and \code{col.units}. Row titles are taken from the row names attribute
of the data frame.
}

\note{
Requires the Tcl package Tktable, \url{http://tktable.sourceforge.net/}.
}

\author{Fisher, J.C.}

\seealso{
\code{\link{tclArray}}, \code{\link{row.names}}
}

\examples{
\donttest{tclRequire("Tktable", warn = TRUE)

n <- 1000
V1 <- sample(c(1:9, NA), n, replace = TRUE)
V2 <- sample(LETTERS, n, replace = TRUE)
V3 <- as.POSIXct(rnorm(n, mean = 0, sd = 1e6), origin = "2010-01-01")
V4 <- sample(V1 * pi, n)
d <- data.frame(V1, V2, V3, V4)
col.names <- c("Integers", "Letters", "POSIXt", "Numeric")
col.units <- c("units", NA, "\%m/\%d/\%Y \%H:\%M", NA)
col.digs <- c(0, NA, NA, 3)
ViewData(d, col.names, col.units, col.digs)

row.names(d) <- 1:n + n
ViewData(d, col.names, col.units = NULL, digs)
}
}

\keyword{misc}
