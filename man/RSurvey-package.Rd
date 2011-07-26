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

\details{
  \tabular{ll}{
    Package: \tab RSurvey\cr
    Type: \tab Package\cr
    Version: \tab 0.6-3\cr
    Date: \tab 2011-07-26\cr
    License: \tab GPL (>= 2)
  }
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
grid files (\file{.grd}) are limited to data export. Support for programmatic
manipulation of measurement units is only provided for date and time values;
therefore, the bulk of unit consistency is tasked to the user.
Time zones, spatial datum's and projections are not supported.

The set of standards used for coding \pkg{RSurvey} is documented in
\href{http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html}{Google's R Style Guide}.
}

\author{
Jason C. Fisher

Maintainer: <\email{jfisher@usgs.gov}>
}

\examples{
\donttest{library(RSurvey)}
}

\keyword{package}

