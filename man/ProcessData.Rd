\name{ProcessData}

\alias{ProcessData}

\title{Process Data}

\description{
This function performs data processing on the state variables.
}

\usage{
ProcessData(d, type = "p", coerce.rows = NULL, ply = NULL,
            grid.res = list(x = NA, y = NA),
            grid.mba = list(n = NA, m = NA, h = 11))
}

\arguments{
\item{d}{data.frame; the data to be processed, a table
  with variables \code{x}, \code{y}, \code{z}, \code{t},
  \code{vx}, \code{vy}, and \code{vz}.}
\item{type}{character; a 1-character string giving the resulting
  output type. The following values are possible: "p" for points
  and "g" for interpolated grid.}
\item{coerce.rows}{logical; a vector indicating rows in \code{d} to include
  in the processed data set. \code{\link{NA}}} values are considered
  \code{FALSE}.
\item{ply}{gpc.poly; if \code{type = "p"} a polygon that defines either the
  data limits, else if \code{type = "g"} a crop region for gridded data.}
\item{grid.res}{list; numeric components giving the grid spacing along
  the x- and y-axis for interpolated values, see \code{\link{SetInterpolation}}.}
\item{grid.mba}{list; integer components giving the input parameters for the
  multilevel B-splines algorithm, see \code{\link{SetInterpolation}}.}
}

\details{
Any row in the \code{d} data table with \code{\link{NA}} values for
either \code{x} or \code{y} is removed. Rows are also removed
where \code{coerce.rows} is \code{FALSE}.
The spatial domain may be limited using a polygon defined in the xy-plane.
Interpolated grid values corresponding to grid nodes located outside
the polygon boundary are set to \code{\link{NA}}.
}

\value{
If \code{type = "p"} returns a data frame with variables:
\item{x, y}{numeric; a vector of \code{x} and \code{y} coordinates.}
\item{z}{numeric; a vector of state variable values (optional).}
\item{vx, vy, vz}{numeric; a vector of velocity components in the \code{x},
  \code{y} and \code{z} directions, respectively (optional).}
If \code{type = "g"} returns a list with components:
\item{x, y}{numeric; a vector of grid line locations at which the values in
  \code{z} are measured.}
\item{z}{matrix; interpolated surface of state variable with rows and columns
  corresponding to grid lines in the \code{x} and \code{y} directions,
  respectively.}
\item{vx, vy, vz}{matrix; interpolated surface of velocity components with rows
  and columns corresponding to grid lines in the \code{x} and \code{y}
  directions (optional).}
\item{vf}{numeric; volumetric flux (optional).}
}

\author{J.C. Fisher}

\seealso{
\code{\link{point.in.polygon}}, \code{\link{CutoutPolygon}},
\code{\link{mba.points}}
}

\examples{
x <- c(7.8, 5, 2.2, 3.7, NA, -1.6, -7.5)
y <- c(-2.3, -4.7, -2.2, -2.3, -3.4, -1.6, -7.5)
z <- c(-0.9, -1.2, -2.4, -2.4, -0.4, 0.1, 2)
t <- c("4/23/2009 10:43", "4/24/2009 11:20", "4/24/2009 12:08", "4/24/2009 12:50",
       "4/24/2009 13:51", "4/24/2009 14:24", "4/24/2009 14:44")
t <- as.POSIXct(t, format = "\%m/\%d/\%Y \%H:\%M")

data.raw <- as.data.frame(list(x = x, y = y, z = z, t = t))
data.pts <- ProcessData(data.raw, type = "p")
data.grd <- ProcessData(data.pts, type = "g")

ply <- as(cbind(c(-4, 2, -6), c(-7, -3, -3)), "gpc.poly")
grid.res <- list(x = 0.2, y = 0.5)
data.grd <- ProcessData(data.pts, type = "g", ply = ply, grid.res = grid.res)
}

\keyword{manip}
