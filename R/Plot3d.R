#' Plot Points and Surface in 3D
#'
#' This function renders raster and point data in three-dimensional (\acronym{3D}) space.
#'
#' @param r RasterLayer.
#'   Gridded surface data
#' @param p SpatialPointsDataFrame.
#'   Spatial point data
#' @param xlim numeric.
#'   Vector of length 2 giving the minimum and maximum values for the \emph{x}-axis.
#' @param ylim numeric.
#'   Vector of length 2 giving the minimum and maximum values for the \emph{y}-axis.
#' @param zlim numeric.
#'   Vector of length 2 giving the minimum and maximum values for the \emph{z}-axis.
#' @param vasp numeric.
#'   The \emph{z/x} aspect ratio for spatial axes.
#' @param hasp numeric.
#'   The \emph{y/x} aspect ratio for spatial axes.
#'   Defaults to 1 (one unit on the \emph{x}-axis equals one unit on the \emph{y}-axis) when \code{r} is projected,
#' @param cex.pts numeric.
#'   Amount by which point symbols should be magnified relative to the default.
#' @param n integer.
#'   Number of contour levels desired.
#' @param color.palette function.
#'   Color \link{palette} to be used to assign colors in the plot.
#' @param maxpixels integer.
#'   Maximum number of cells to use for the plot.
#'
#' @details The interpolated surface is rendered using \pkg{rgl},
#'   a \acronym{3D} visualization device system for \R based on \href{https://www.opengl.org/}{OpenGL}.
#'   The mouse is used for interactive viewpoint navigation where the left, right, and center mouse buttons rotate the scene,
#'   rotate the scene around the x-axis, and zooms the display, respectively.
#'
#' @return Used for the side-effect of a new plot generated.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{matplot}}, \code{\link{boxplot}}
#'
#' @keywords hplot
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   Plot3d()
#'   rgl::rgl.quit()
#' }
#'

Plot3d <- function(r=NULL, p=NULL, xlim=NULL, ylim=NULL, zlim=NULL,
                   vasp=NULL, hasp=NULL, cex.pts=1, n=NULL,
                   color.palette=grDevices::terrain.colors, maxpixels=500000) {

  if (!requireNamespace("rgl", quietly=TRUE)) stop()
  is.r <- inherits(r, "RasterLayer")
  is.p <- inherits(p, "SpatialPointsDataFrame")

  if (is.r) {
    try(p <- sp::spTransform(p, r@crs), silent=TRUE)
    if (is.null(hasp) && !is.na(rgdal::CRSargs(raster::crs(r)))) hasp <- 1
  } else if (is.p) {
    if (is.null(hasp) && !is.na(rgdal::CRSargs(raster::crs(p)))) hasp <- 1
  }

  # defaults
  if (!is.numeric(xlim)) xlim <- c(NA, NA)
  if (!is.numeric(ylim)) ylim <- c(NA, NA)
  if (!is.numeric(zlim)) zlim <- c(NA, NA)
  if (is.null(cex.pts)) cex.pts <- 1

  # raster resolution
  if (is.r) r <- raster::sampleRegular(r, size=maxpixels, asRaster=TRUE)

  # limits
  e <- cbind(if (is.p) as.vector(raster::extent(p)) else NULL,
             if (is.r) as.vector(raster::extent(r)) else NULL)
  e <- c(min(e[1, ]), max(e[2, ]), min(e[3, ]), max(e[4, ]))
  if (!is.na(xlim[1])) e[1] <- xlim[1]
  if (!is.na(xlim[2])) e[2] <- xlim[2]
  if (!is.na(ylim[1])) e[3] <- ylim[1]
  if (!is.na(ylim[2])) e[4] <- ylim[2]
  if (is.p) p <- raster::crop(p, raster::extent(e), snap="near")
  if (is.r) r <- raster::crop(r, raster::extent(e), snap="near")
  zran <- range(c(if (is.p) p@data[, 1] else NULL, if (is.r) r[] else NULL), finite=TRUE)
  if (is.na(zlim[1])) zlim[1] <- zran[1]
  if (is.na(zlim[2])) zlim[2] <- zran[2]
  if (is.p) {
    is.ge <- p@data[, 1] >= zlim[1] & !is.na(p@data[, 1])
    is.le <- p@data[, 1] <= zlim[2] & !is.na(p@data[, 1])
    p <- p[is.ge & is.le, ]
  }
  if (is.r) {
    r[r[] < zlim[1] | r[] > zlim[2]] <- NA
    r <- raster::trim(r)
  }
  xran <- range(c(if (is.p) sp::bbox(p)[1, ] else NULL,
                  if (is.r) sp::bbox(r)[1, ] else NULL))
  yran <- range(c(if (is.p) sp::bbox(p)[2, ] else NULL,
                  if (is.r) sp::bbox(r)[2, ] else NULL))
  if (is.na(xlim[1])) xlim[1] <- xran[1]
  if (is.na(xlim[2])) xlim[2] <- xran[2]
  if (is.na(ylim[1])) ylim[1] <- yran[1]
  if (is.na(ylim[2])) ylim[2] <- yran[2]

  # scale
  yscale <- if (is.null(hasp)) diff(xlim) / diff(ylim) else hasp
  zscale <- if (is.null(vasp)) (diff(xlim) * 0.25) / diff(zlim) else vasp

  # device
  if (rgl::rgl.cur() == 0)
    rgl::open3d()
  else
    rgl::clear3d()

  # grid
  if (is.r) {
    x <- raster::xFromCol(r, 1:ncol(r))
    y <- raster::yFromRow(r, nrow(r):1) * yscale
    z <- t((raster::getValues(r, format="matrix"))[nrow(r):1, ]) * zscale
    zran <- range(r[], finite=TRUE)
    if (is.null(n) || n > 200L)
      breaks <- seq(zlim[1], zlim[2], length.out=200L)
    else
      breaks <- pretty(zlim, n=n)
    n <- length(breaks) - 1L
    interval <- findInterval(z, breaks * zscale, all.inside=TRUE)
    cols <- color.palette(n)[interval]
    rgl::persp3d(x, y, z, xlab="", ylab="", zlab="", xlim=xlim, ylim=ylim * yscale,
                 zlim=zlim * zscale, aspect=FALSE, color=cols, axes=FALSE)
  }

  # points
  if (is.p) {
    x <- sp::coordinates(p)[, 1]
    y <- sp::coordinates(p)[, 2] * yscale
    z <- p@data[, 1] * zscale
    size <- 3 * cex.pts
    rgl::plot3d(x=x, y=y, z=z, xlab="", ylab="", zlab="", col="black", size=size,
                add=is.r, aspect=FALSE, xlim=xlim, ylim=ylim * yscale,
                zlim=zlim * zscale, axes=FALSE)
  }

  rgl::box3d()
  rgl::title3d(xlab="x", ylab="y", zlab="z", line=0.1)
}
