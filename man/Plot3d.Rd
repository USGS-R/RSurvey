\name{Plot3d}

\alias{Plot3d}

\title{Plot Points and Surface in 3D}

\description{Draws plot of points and surface in three-dimensional (\acronym{3D}) space.}

\usage{
Plot3d(x = NULL, y = NULL, z = NULL, px = NULL, py = NULL, pz = NULL,
       xlim = NULL, ylim = NULL, zlim = NULL, vasp = NA, hasp = NA,
       width = 7, ppi = 96, cex.pts = 1, nlevels = 20,
       color.palette = terrain.colors, maxpixels = 500000)
}

\arguments{
\item{x}{numeric, list, or RasterLayer; if \code{x} is numeric,
  values corresponding to rows of \code{y}.
  These must be in ascending order. If \code{x} is a list, its numeric
  components \code{x$x} and \code{x$y} are used for \code{x} and \code{y},
  respectively. If the list has component \code{x$z} this is used for \code{z}.
  If \code{x} is a RasterLayer, \code{y} and \code{z} are ignored.}
\item{y}{numeric; values corresponding to columns of \code{x}.}
\item{z}{matrix; the values to be plotted. The number of rows and columns
  should be equal to the \code{length(x)} and \code{length(y)}, respectively.}
\item{px}{numeric, list, or SpatialPointsDataFrame; if \code{px} numeric,
  a vector of \code{x} values for points in the plot.
  If \code{px} is a list, its components \code{px$px}, \code{px$py} and
  \code{px$pz} are used for \code{px}, \code{py} and \code{pz}, respectively.
  If \code{px} is a SpatialPointsDataFrame, \code{py} and \code{pz} are ignored.}
\item{py}{numeric; a vector of \code{y} values for points in the plot.}
\item{pz}{numeric; a vector of \code{z} values for points in the plot.}
\item{xlim}{numeric; a vector of \code{x} limits \code{(x1,x2)} for the plot.}
\item{ylim}{numeric; a vector of \code{y} limits \code{(y1,y2)} for the plot.}
\item{zlim}{numeric; a vector of \code{z} limits \code{(z1,z2)} for the plot.}
\item{vasp}{numeric; the \code{z/x} aspect ratio.}
\item{hasp}{numeric; the \code{y/x} aspect ratio.}
\item{width}{numeric; the width of the plotting window canvas in inches.}
\item{ppi}{integer; screen resolution in points per inch.}
\item{cex.pts}{numeric; the amount by which point symbols should be magnified
  relative to the default.}
\item{nlevels}{integer; number of contour levels desired.}
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
data(ex.project)
Plot3d(ex.project$data.grd)
rgl::rgl.quit()
}}

\keyword{hplot}
