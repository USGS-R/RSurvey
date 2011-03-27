\name{ProcessData}

\alias{ProcessData}

\title{Process Data}

\description{
This function performs data processing on the state variables.
}

\usage{
ProcessData()
}

\details{
The raw data being processed is queried from the \code{data.raw}
component of \code{\link{Data}}. Processing control parameters are also
queried from \code{Data} and include: \code{cols}, \code{vars},
\code{lim.data}, \code{poly}, \code{poly.data}, \code{poly.crop},
\code{grid.dx}, \code{grid.dy}, \code{mba.m}, \code{mba.n}, and \code{mba.h}.

A data frame based on the user-defined state variables is first created.
Records outside the user-defined spatial and temporal domains are then removed
using: (1) data limits, where the \code{x}, \code{y}, \code{z}, and \code{t}
limits are specified in the \code{xlim}, \code{ylim}, \code{zlim}, and
\code{tlim} components of the \code{lim.data} list, respectively; and (2) a
two-dimensional polygon defining the spatial domain within the \code{xy}-plane.

An interpolated grid of \code{z} values in constructed using a
Multilevel B-spline approximation. The spatial extent of the interpolated
surface is constrained using \code{polyLimit}, a polygon that sets spatial
limits for grided data; where \code{z} values corresponding to grid nodes located
outside this polygon are set to \code{NA}.
}

\value{
Sets the \code{data.pts} and \code{data.grd} components of \code{\link{Data}}.
The \code{data.pts} component is a data frame with variables:
\item{x, y}{numeric; a vector of \code{x} and \code{y} coordinates.}
\item{z}{numeric; a vector of state variable values (optional).}
\item{t}{POSIXct; a vector of time stamps (optional).}
\item{vx, vy}{numeric; a vector of velocity components in the \code{x} and
  \code{y} directions, respectively (optional).}
The \code{data.grd} component is a list with components:
\item{x, y}{numeric; a vector of grid line locations at which the values in
  \code{z} are measured.}
\item{z}{matrix; interpolated surface of state variable with rows and columns
  corresponding to grid lines in the \code{x} and \code{y} directions,
  respectively.}
\item{vx, vy}{matrix; interpolated surface of velocity components with rows
  and columns corresponding to grid lines in the \code{x} and \code{y}
  directions, respectively (optional).}
Row names in \code{data.pts} coincide with row indexes in \code{data.raw}.
}

\author{Fisher, J.C.}

\seealso{
\code{mba.points}
}

\examples{
data(project)
ProcessData()
}

\keyword{manip}
