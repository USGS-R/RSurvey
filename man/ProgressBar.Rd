\name{ProgressBar}

\alias{ProgressBar}
\alias{SetProgressBar}

\title{Progress Bar}

\description{
A progress bar that shows the status of long-running operations.
}

\usage{
ProgressBar(win.title = "Progress Bar", label = "", maximum = 100, parent = NULL)

SetProgressBar(pb, value, label = NULL)
}

\arguments{
\item{win.title}{character; a string to display as the title of the dialog box.}
\item{label}{character; a string to display in the dialog box.}
\item{maximum}{numeric; the maximum value for the progress bar. The minimum
  value is set to \code{0}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
\item{pb}{ProgressBar; an object returned from \code{ProgressBar},
  see \sQuote{Value}.}
\item{value}{tkwin; value for the progress bar, between \code{0} and
  \code{maximum}.}
}

\value{
For \code{ProgressBar} an object of class \code{"ProgressBar"} is returned.
Components of the list object include:
\item{GetValue}{function; returns the value of the progress bar.}
\item{MoveProgressBar}{function; moves progress bar, passes a numeric argument.}
\item{SetLabel}{function; sets label in the dialog box, passes a character
  argument.}
\item{DestroyWindow}{function; closes the dialog box.}
\item{GetWindowState}{function; returns \code{FALSE} if the dialog box has been
  closed, otherwise \code{TRUE}.}
\item{label}{character; returns the label shown in the dialog box.}

Nothing is returned for \code{SetProgressBar}.
}

\author{J.C. Fisher}


\references{
This code was derived from the \code{\link[tcltk]{tkProgressBar}} function.
}

\examples{\donttest{
maximum <- 50
txt <- "Estimated time to completion is"
pb <- ProgressBar(label = paste(txt, "being calculated\u2026"), maximum = maximum)

values <- 1:maximum
old.time <- Sys.time()
for (i in values) {
  if (!pb$GetWindowState())
    break
  Sys.sleep(0.5)
  label <- ifelse(i == 1L, pb$label, paste(txt, format(round(time.diff))))
  SetProgressBar(pb, values[i], label)
  new.time <- Sys.time()
  time.diff <- (new.time - old.time) * (maximum - i)
  old.time <- new.time
}
}}

\keyword{misc}
