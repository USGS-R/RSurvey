\name{ViewData}

\alias{ViewData}

\title{View Data}

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
\item{read.only}{logical or list; specifies whether the table is read only.
  Alternatively, read only can be specified for individual rows and columns
  using a \code{list} object with components \code{rows} and \code{cols}. Each
  component is a vector of integer values giving the index number(s) of the
  rows and columns that will be disabled.}
\item{win.title}{character; a string to display as the title of the dialog box.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\details{
Row titles are taken from the row names attribute
of \code{d}. Pattern searches are performed using \code{\link{grep}}
with \acronym{GUI} options available for setting its
\code{fixed} and \code{perl} arguments.
}

\note{
Requires the Tcl package
\href{http://tktable.sourceforge.net/}{Tktable}.
}

\author{J.C. Fisher}

\seealso{
\code{\link{tclArray}}, \code{\link{grep}}
}

\examples{\donttest{
tclRequire("Tktable", warn = TRUE)

n <- 1000
V1 <- sample(c(1:9, NA), n, replace = TRUE)
V2 <- sample(LETTERS, n, replace = TRUE)
V3 <- as.POSIXct(rnorm(n, mean = 0, sd = 1e6), origin = "2010-01-01")
V4 <- sample(V1 * pi, n)
d <- data.frame(V1, V2, V3, V4)
col.names <- c("Integers", "Letters", "DateTime", "Numeric")
col.formats <- c("\%d", "\%s", "\%m/\%d/\%Y \%H:\%M", NA)
ViewData(d, col.names, col.formats)

rownames(d) <- paste(sample(LETTERS, n, replace = TRUE), 1:n, sep="")
ViewData(d)

colnames(d) <- NULL
rownames(d) <- NULL
ViewData(d)
}}

\keyword{misc}
