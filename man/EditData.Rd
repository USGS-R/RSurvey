\name{EditData}

\alias{EditData}

\title{Edit Data}

\description{
A \acronym{GUI} for viewing and editing table formatted data.
}

\usage{
EditData(d, col.names = NULL, col.formats = NULL, read.only = FALSE,
         changelog = NULL, win.title = "Edit Data", parent = NULL)
}

\arguments{
\item{d}{data.frame; the data used to populate the table.}
\item{col.names}{character; a vector giving the column names for the
  data table (optional).}
\item{col.formats}{character; the conversion specification
  format (optional).}
\item{read.only}{logical; specifies whether the table is read only.}
\item{changelog}{data.frame; history of all data table edits.}
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

n <- 1000L
V1 <- sample(c(1:9, NA), n, replace = TRUE)
V2 <- sample(LETTERS, n, replace = TRUE)
V3 <- as.POSIXct(rnorm(n, mean = 0, sd = 1e6), origin = "2010-01-01")
V4 <- sample(V1 * pi, n)
d <- data.frame(V1, V2, V3, V4)
col.names <- c("Integers", "Letters", "DateTime", "Numeric")
col.formats <- c("\%d", "\%s", "\%m/\%d/\%Y \%H:\%M", "")
lst <- EditData(d, col.names, col.formats, read.only = TRUE)
str(lst)

rownames(d) <- paste0(sample(LETTERS, n, replace = TRUE), 1:n)
EditData(d)

colnames(d) <- NULL
rownames(d) <- NULL
EditData(d)
}}

\keyword{misc}
