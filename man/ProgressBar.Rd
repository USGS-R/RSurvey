\name{ProgressBar}

\alias{ProgressBar}
\alias{SetProgressBar}

\title{Progress Bar}

\description{
A progress bar that shows the status of long-running operations.
}

\usage{
ProgressBar(win.title = "Progress Bar", label = "", maximum = 100,
            nsteps = NULL, min.nsteps = 10L, parent = NULL)

SetProgressBar(pb, value, label = NULL, step = NULL)
}

\arguments{
\item{win.title}{character; a string to display as the title of the dialog box.}
\item{label}{character; a string to display in the dialog box.}
\item{maximum}{numeric; maximum value for the progress bar. The minimum
  value is \code{0}.}
\item{nsteps}{numeric; total number of increments the progress bar will
  make.}
\item{min.nsteps}{numeric; minimum number of increments. If greater than
  \code{nsteps}, the dialog box is not opened.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
\item{pb}{ProgressBar; an object returned from \code{ProgressBar}
  (see \sQuote{Value}).}
\item{value}{numeric; value for the progress bar, between \code{0} and
  \code{maximum}.}
\item{step}{numeric; number of progress bar increments. If equal to
  \code{nsteps}, the dialog box will close.}
}

\value{
For \code{ProgressBar} an object of class \code{"ProgressBar"} and mode
\code{list} is returned. Components of the list object include:
\item{GetValue}{function; returns the value of the progress bar.}
\item{MoveProgressBar}{function; moves progress bar, passes a numeric argument.}
\item{SetLabel}{function; sets label in the dialog box, passes a character
  argument.}
\item{DestroyWindow}{function; closes the dialog box.}
\item{GetWindowState}{function; returns \code{FALSE} if the dialog box has been
  closed, otherwise \code{TRUE}.}
\item{nsteps}{numeric; see \sQuote{Arguments}.}

For \code{SetProgressBar}, the previous value of the progress bar. An error is
returned if the progress has terminated prematurely.
}

\author{J.C. Fisher}


\references{
This code was derived from the \code{\link[tcltk]{tkProgressBar}} function.
}

\examples{\donttest{
maximum <- 10
label <- "Estimated time to completion is being calculated\u2026"
pb <- ProgressBar(label = label, maximum = maximum, nsteps = maximum)

for (i in 1:maximum) {
  est.time <- system.time(Sys.sleep(1))["elapsed"] * (maximum - i)
  label <- paste("Estimated time to completion is", round(est.time), "secs")
  ans <- try(SetProgressBar(pb, value = i, label = label, step = i))
  if (inherits(ans, "try-error"))
    break
}
}}

\keyword{misc}
