\name{RSurvey-package}

\alias{RSurvey-package}

\docType{package}

\title{
Analysis of Spatially Distributed Data
}

\description{
This package is a processing program for spatially distributed data.
It features graphing tools, query building, and polygon clipping.
A graphical user interface (\acronym{GUI}) is provided.
}

\note{
The \pkg{RSurvey} \acronym{GUI} requires \R operate as an \acronym{SDI}
application, using multiple top-level windows for the console, graphics, and
pager. Files can be one of four types as indicated by their extension:
tables (\file{.txt}, \file{.csv}, \file{.dat}, or \file{.shp}),
grids (\file{.grd}), polygons (\file{.ply}), or binary project images
(\file{.rda}).Tables (\file{.txt}, \file{.csv}, \file{.dat})
can be compressed by \href{http://www.gzip.org/}{gzip}
with additional extension \file{.gz}. Shapefiles (\file{.shp}) and interpolated
grid files (\file{.grd}) are limited to data export. Measurement unit consistency 
is tasked to the user. Time zones, spatial datum's and projections are not supported.

The set of standards used for coding \pkg{RSurvey} is documented in
\href{http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html}{Google's R Style Guide}.
}

\examples{
\donttest{library(RSurvey)
OpenRSurvey()}
}

\keyword{package}

