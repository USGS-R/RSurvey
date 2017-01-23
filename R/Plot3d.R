Plot3d <- function(r=NULL, p=NULL, xlim=NULL, ylim=NULL, zlim=NULL,
                   vasp=NULL, hasp=NULL, cex.pts=1, n=NULL,
                   color.palette=terrain.colors, maxpixels=500000) {

  if (!requireNamespace("rgl", quietly=TRUE)) stop()
  is.r <- inherits(r, "RasterLayer")
  is.p <- inherits(p, "SpatialPointsDataFrame")

  if (is.r) {
    try(p <- spTransform(p, r@crs), silent=TRUE)
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
  xran <- range(c(if (is.p) bbox(p)[1, ] else NULL,
                  if (is.r) bbox(r)[1, ] else NULL))
  yran <- range(c(if (is.p) bbox(p)[2, ] else NULL,
                  if (is.r) bbox(r)[2, ] else NULL))
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
    rgl::surface3d(x, y, z, color=cols)
  }

  # points
  if (is.p) {
    x <- sp::coordinates(p)[, 1]
    y <- sp::coordinates(p)[, 2] * yscale
    z <- p@data[, 1] * zscale
    rgl::points3d(x=x, y=y, z=z, size=cex.pts * 3, point_antialias=TRUE)
  }

  rgl::box3d()
  rgl::title3d(xlab="x", ylab="y", zlab="z", line=0.1)
}
