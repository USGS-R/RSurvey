\name{Plot3d}

\alias{Plot3d}

\title{Plot Surface using OpenGL}

\description{
Draws a three-dimensional (\acronym{3D}) surface plot.
}

\usage{
Plot3d(x = NULL, y = NULL, z = NULL,
       px = NULL, py = NULL, pz = NULL,
       xlim = NULL, ylim = NULL, zlim = NULL,
       vasp = NA, hasp = NA, width = 7, ppi = 96,
       cex.pts = 1, nlevels = 20,
       color.palette = terrain.colors,
       mouse.mode = c("trackball", "zAxis", "zoom"),
       bg = "white")
}

\arguments{
\item{x, y}{numeric; locations of grid lines at which the values in z are
  measured. These must be in ascending order. If \code{x} is a list, its
  components \code{x$x} and \code{x$y} are used for \code{x} and \code{y},
  respectively. If the list has component \code{x$z} this is used for \code{z}.}
\item{z}{matrix; the values to be plotted.}
\item{px}{numeric; a vector of \code{x} coordinates for points in the plot.
  If \code{px} is a list, its components \code{px$px}, \code{px$py} and
  \code{px$pz} are used for \code{px}, \code{py} and \code{pz}, respectively.}
\item{py}{numeric; a vector of \code{y} coordinates for points in the plot.}
\item{pz}{numeric; a vector of \code{z} coordinates for points in the plot.}
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
\item{mouse.mode}{character; a vector of 3 strings describing what the 3 mouse
  buttons do, see \code{par3d}.}
\item{bg}{character; the primary color for the background.}
}

\details{
The interpolated surface data is rendered using \pkg{rgl}, a \acronym{3D}
visualization device system for \R based on
\href{http://www.opengl.org/}{OpenGL}.
The mouse is used for interactive viewpoint navigation where
the left, right, and center mouse buttons rotate the scene, rotate the scene
around the x-axis, and zooms the display, respectively.
}

\author{Fisher, J.C.}

\seealso{
\code{surface3d}, \code{points3d}
}

\examples{
\donttest{
data(project)
d <- Data("data.grd")
Plot3d(d)
rgl.quit()
}
}

\keyword{hplot}
