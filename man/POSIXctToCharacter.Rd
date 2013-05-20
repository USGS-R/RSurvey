\name{POSIXctToCharacter}

\alias{POSIXctToCharacter}

\title{Convert POSIXct to Character}

\description{
Convert objects from \code{\link{POSIXct}} to \code{\link{character}} class.
}

\usage{
POSIXctToCharacter(obj, fmt = "\%Y-\%m-\%d \%H:\%M:\%OS3")
}

\arguments{
\item{obj}{POSIXct; vector of calendar dates and times.}
\item{fmt}{character; the conversion specification format.}
}

\value{
Returns a character vector representing time.
}

\note{
R incorrectly formats objects of calss \code{\link{POSIXct}} with fractional
seconds. For example, a POSIXct time with fractional part \code{.3} seconds
(stored as \code{0.29999}) is printed as \code{.2} when represented with one
decimal digit. The fractional part on outputs is not rounded.
}

\author{J.C. Fisher}

\references{
A detailed explanation of the problem is provided
\href{http://stackoverflow.com/questions/7726034/how-r-formats-posixct-with-fractional-seconds}{here};
solution provided
\href{http://stackoverflow.com/questions/15383057/accurately-converting-from-character-posixct-character-with-sub-millisecond-da}{here}.
}

\seealso{
\code{\link{strptime}}
}

\examples{
val <- c("11/10/2011 07:49:36.3",
         "04/01/2013 17:22:08.139",
         "01/06/2013 01:02:16.333",
         "12/14/2038 15:42:04.577895")
date.time <- as.POSIXct(val, format="\%m/\%d/\%Y \%H:\%M:\%OS")

options("digits.secs" = 3)
format(date.time, fmt = "\%d/\%m/\%Y \%H:\%M:\%OS")
format(date.time, fmt = "\%d/\%m/\%Y \%H:\%M:\%OS3")

POSIXctToCharacter(date.time, fmt = "\%d/\%m/\%Y \%H:\%M:\%OS3")
POSIXctToCharacter(date.time, fmt = "\%d/\%m/\%Y \%H:\%M:\%OS4")
POSIXctToCharacter(date.time, fmt = "\%d/\%m/\%Y \%H:\%M:\%OS2")

POSIXctToCharacter(date.time, fmt = "\%H:\%M:\%OS3 \%Y-\%m-\%d")
}

\keyword{manip}
