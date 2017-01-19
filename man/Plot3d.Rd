\name{Plot3d}

\alias{Plot3d}

\title{Plot Points and Surface in 3D}

\description{Draws plot of points and surface in three-dimensional (\acronym{3D}) space.}

\usage{
Plot3d(p = NULL, r = NULL, xlim = NULL, ylim = NULL, zlim = NULL,
       vasp = NULL, hasp = NULL, width = 7, ppi = 96, cex.pts = 1, n = NULL,
       color.palette = terrain.colors, maxpixels = 500000)
}

\arguments{
\item{p}{SpatialPointsDataFrame; points}
\item{r}{RasterLayer; surface}
\item{xlim}{numeric; a vector of \code{x} limits \code{(x1,x2)} for the plot.}
\item{ylim}{numeric; a vector of \code{y} limits \code{(y1,y2)} for the plot.}
\item{zlim}{numeric; a vector of \code{z} limits \code{(z1,z2)} for the plot.}
\item{vasp}{numeric; the \code{z/x} aspect ratio.}
\item{hasp}{numeric; the \code{y/x} aspect ratio.}
\item{width}{numeric; the width of the plotting window canvas in inches.}
\item{ppi}{integer; screen resolution in points per inch.}
\item{cex.pts}{numeric; the amount by which point symbols should be magnified
  relative to the default.}
\item{n}{integer; number of contour levels desired.}
\item{color.palette}{function; a color \link{palette} to be used to assign
  colors in the plot.}
\item{maxpixels}{integer; maximum number of cells to use for the plot.}
}

\details{
The interpolated surface is rendered using \pkg{rgl}, a \acronym{3D}
visualization device system for \R based on
\href{https://www.opengl.org/}{OpenGL}.
The mouse is used for interactive viewpoint navigation where
the left, right, and center mouse buttons rotate the scene, rotate the scene
around the x-axis, and zooms the display, respectively.
}

\author{J.C. Fisher}

\examples{\dontrun{
Plot3d()
rgl::rgl.quit()
}}

\keyword{hplot}
