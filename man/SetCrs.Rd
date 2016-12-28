\name{SetCrs}

\alias{SetCrs}

\title{Set Coordinate Reference System}

\description{
A \acronym{GUI} for specifying PROJ.4 arguments associated with a coordinate reference system (CRS).
The arguments must be entered exactly as in the PROJ.4 documentation,
in particular there cannot be any white space in +<arg>=<value> strings,
and successive such strings can only be separated by blanks.
}

\usage{SetCrs(crs = sp::CRS(as.character(NA)), parent = NULL)}

\arguments{
\item{crs}{CRS; coordinate reference system described using PROJ.4 arguments.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\value{Returns an updated value of \code{projargs}.}

\author{J.C. Fisher}

\examples{\dontrun{
SetCrs("+init=epsg:4326")
}}

\keyword{misc}
