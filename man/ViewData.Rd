\name{ViewData}

\alias{ViewData}

\title{View Data Table}

\description{
A \acronym{GUI} for viewing table formatted data.
}

\usage{
ViewData(d, col.names = NULL, col.formats = NULL, read.only = FALSE,
         win.title = "View Data", parent = NULL)
}

\arguments{
\item{d}{data.frame; the data used to populate the table.}
\item{col.names}{character; a vector giving the column names for the
  data table (optional).}
\item{col.formats}{character; the conversion specification
  format (optional).}
\item{read.only}{logical or integer; specifies whether the table is read only.
  Alternatively, read only can be specified for individual columns using an
  integer array of column indexes.}
\item{win.title}{character; a string to display as the title of the dialog box.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\details{
Row titles are taken from the row names attribute
of \code{d}. Pattern searches are performed using \code{\link{grep}}.
}

\note{
Requires the Tcl package
\href{http://tktable.sourceforge.net/}{Tktable}.
}

\author{J.C. Fisher}

\examples{\donttest{
tclRequire("Tktable", warn = TRUE)

n <- 1000
V1 <- sample(c(1:9, NA), n, replace = TRUE)
V2 <- sample(LETTERS, n, replace = TRUE)
V3 <- as.POSIXct(rnorm(n, mean = 0, sd = 1e6), origin = "2010-01-01")
V4 <- sample(V1 * pi, n)
d <- data.frame(V1, V2, V3, V4)
col.names <- c("Integers", "Letters", "DateTime", "Numeric")
col.formats <- c("\%d", "\%s", "\%m/\%d/\%Y \%H:\%M", "")
ViewData(d, col.names, col.formats, read.only = 1L)

rownames(d) <- paste0(sample(LETTERS, n, replace = TRUE), 1:n)
ViewData(d)

colnames(d) <- NULL
rownames(d) <- NULL
ViewData(d)
}}

\keyword{misc}
