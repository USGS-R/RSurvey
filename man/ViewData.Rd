\name{ViewData}

\alias{ViewData}

\title{View Data}

\description{
A \acronym{GUI} for viewing table formatted data.
}

\usage{
ViewData(d, column.names = NULL, column.units = NULL,
         column.formats = NULL, parent = NULL)
}

\arguments{
\item{d}{data.frame; the data used to populate the table.}
\item{column.names}{character; a vector giving the column names for the
  data table (optional).}
\item{column.units}{character; a vector giving the measurement units for
  each column of the data table (optional).}
\item{column.formats}{character; the conversion specification
  format (optional).}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\details{
Column titles are a concatenation of variables \code{column.names}
and \code{column.units}. Row titles are taken from the row names attribute
of \code{d}. Pattern searches are performed using \code{\link{grep}}
with \acronym{GUI} options available for setting its
\code{fixed} and \code{perl} arguments.
}

\note{
Requires the Tcl package
\href{http://tktable.sourceforge.net/}{Tktable}.
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
column.names <- c("Integers", "Letters", "DateTime", "Numeric")
column.units <- c("units", NA, NA, NA)
column.formats <- c("\%d", "\%s", "\%m/\%d/\%Y \%H:\%M", NA)
ViewData(d, column.names, column.units, column.formats)

row.names(d) <- 1:n + n
ViewData(d, column.names, column.formats = column.formats)
}
}

\keyword{misc}
