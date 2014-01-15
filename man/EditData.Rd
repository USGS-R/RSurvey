\name{EditData}

\alias{EditData}

\title{Edit Data}

\description{
A \acronym{GUI} for viewing and editing table formatted data.
}

\usage{
EditData(d, col.names = names(d), row.names = NULL, col.formats = NULL,
         read.only = FALSE, changelog = NULL, win.title = "Data",
         parent = NULL)
}

\arguments{
\item{d}{list, matrix, or data.frame; the data used to populate the table.}
\item{col.names}{character; a vector of column names.}
\item{row.names}{character; a vector of row names.}
\item{col.formats}{character; a vector of format conversion specification
  strings (optional), see \code{\link{sprintf}} and \code{\link{strftime}}.}
\item{read.only}{logical; specifies whether the table is in read only mode.}
\item{changelog}{data.frame; history of all data table edits (see
  \sQuote{Value}).}
\item{win.title}{character; a string to display as the title of the dialog box.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\details{
Row titles are taken from the row names attribute
of \code{d}. Pattern searches are performed using \code{\link{grep}}.
Edits are reflected in the \code{changelog}.
}

\value{
\code{NULL} is returned if no edits were made; otherwise, new values of
\code{d} and \code{changelog} are returned as components in a \code{list}. The
\code{changelog} data frame contains the following variables:
  \item{timestamp}{POSIXct; identifies when the edit event occurred.}
  \item{record}{character; row name.}
  \item{variable}{character; column name.}
  \item{old}{character; value before editing.}
  \item{new}{character; value after editing.}
}

\note{
Requires the Tcl package
\href{http://tktable.sourceforge.net/}{Tktable}.
}

\author{J.C. Fisher}

\examples{
\donttest{tcltk::tclRequire("Tktable", warn = TRUE)

n <- 1000L
V1 <- sample(c(1:9, NA), n, replace = TRUE)
V2 <- sample(LETTERS, n, replace = TRUE)
V3 <- as.POSIXct(rnorm(n, mean = 0, sd = 1e6), origin = "2010-01-01")
V4 <- sample(V1 * pi, n)
d <- data.frame(V1, V2, V3, V4)
col.names <- c("Integers", "Letters", "DateTime", "Numeric")
col.formats <- c("\%d", "\%s", "\%m/\%d/\%Y \%H:\%M", "")
ans <- EditData(d, col.names, col.formats)
str(ans)

rownames(d) <- paste0(sample(LETTERS, n, replace = TRUE), seq_len(n))
EditData(d, read.only = TRUE)

colnames(d) <- NULL
rownames(d) <- NULL
EditData(d, read.only = TRUE)
}}

\keyword{misc}
