Plot3d <- function(x=NULL, y=NULL, z=NULL, px=NULL, py=NULL, pz=NULL,
                   xlim=NULL, ylim=NULL, zlim=NULL,
                   vasp=NA, hasp=NA, width=7, ppi=96, cex.pts=1, nlevels=20,
                   color.palette=terrain.colors, maxpixels=500000) {

  if (!requireNamespace("rgl", quietly=TRUE)) stop()

  # surface attributes
  show.surface <- !is.null(x) | !is.null(z)
  if (show.surface) {
    if (inherits(x, "list")) {
      z <- x$z
      y <- x$y
      x <- x$x
    } else if (inherits(x, "RasterLayer")) {
      r <- raster::sampleRegular(x, size=maxpixels, asRaster=TRUE)
      x <- raster::xFromCol(r, 1:ncol(r))
      y <- raster::yFromRow(r, nrow(r):1)
      z <- t((raster::getValues(r, format="matrix"))[nrow(r):1, ])
      if (!is.numeric(hasp)) hasp <- 1
    }

    if (is.null(x)) x <- seq(0, 1, length.out=nrow(z))
    if (is.null(y)) y <- seq(0, 1, length.out=ncol(z))
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
      stop("increasing 'x' and 'y' values expected")
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1)
      stop("invalid 'z' matrix specified")
  }


  # point attributes
  show.points <- !is.null(px)
  if (show.points) {
    if (inherits(px, "list")) {
      pz <- px$z
      py <- px$y
      px <- px$x
    } else if (inherits(px, "SpatialPointsDataFrame")) {
      pz <- px@data$z
      py <- sp::coordinates(px)[, 2]
      px <- sp::coordinates(px)[, 1]
    }
  }

  # options
  if (is.null(width)) width <- 7
  if (is.null(cex.pts)) cex.pts <- 1

  # axis limits
  if (!is.null(xlim)) {
    if (!is.na(xlim[1])) {
      if (show.surface) {
        is <- x >= xlim[1]
        x <- x[is]
        z <- z[is, ]
      }
      if (show.points) {
        is <- px >= xlim[1]
        px <- px[is]
        py <- py[is]
        pz <- pz[is]
      }
    }
    if (!is.na(xlim[2])) {
      if (show.surface) {
        is <- x <= xlim[2]
        x <- x[is]
        z <- z[is, ]
      }
      if (show.points) {
        is <- px <= xlim[2]
        px <- px[is]
        py <- py[is]
        pz <- pz[is]
      }
    }
  }

  if (!is.null(ylim)) {
    if (!is.na(ylim[1])) {
      if (show.surface) {
        is <- y >= ylim[1]
        y <- y[is]
        z <- z[, is]
      }
      if (show.points) {
        is <- py >= ylim[1]
        px <- px[is]
        py <- py[is]
        pz <- pz[is]
      }
    }
    if (!is.na(ylim[2])) {
      if (show.surface) {
        is <- y <= ylim[2]
        y <- y[is]
        z <- z[, is]
      }
      if (show.points) {
        is <- py <= ylim[2]
        px <- px[is]
        py <- py[is]
        pz <- pz[is]
      }
    }
  }

  if (!is.null(zlim)) {
    if (!is.na(zlim[1])) {
      if (show.surface) z[z < zlim[1]] <- NA
      if (show.points) {
        is <- pz >= zlim[1]
        px <- px[is]
        py <- py[is]
        pz <- pz[is]
      }
    }
    if (!is.na(zlim[2])) {
      if (show.surface) z[z > zlim[2]] <- NA
      if (show.points) {
        is <- pz <= zlim[2]
        px <- px[is]
        py <- py[is]
        pz <- pz[is]
      }
    }
  } else {
    zlim <- range(z, na.rm=TRUE)
  }
  if (is.na(zlim[1])) zlim[1] <- min(c(z, pz), na.rm=TRUE)
  if (is.na(zlim[2])) zlim[2] <- max(c(z, pz), na.rm=TRUE)

  # scale
  if (inherits(hasp, "numeric")) {
    yscale <- hasp
  } else {
    yscale <- (diff(range(c(x, px), na.rm=TRUE)) / diff(range(c(y, py), na.rm=TRUE)))
  }

  if (inherits(vasp, "numeric")) {
    zscale <- vasp
  } else {
    maxdiff <- max(diff(range(c(x, px), na.rm=TRUE)),
                   diff(range(c(y, py), na.rm=TRUE))) * 0.15
    zscale <- if (all(is.na(c(z, pz)))) 1 else (maxdiff / diff(range(c(z, pz), na.rm=TRUE)))
  }

  if (show.surface) {
    y <- y * yscale
    z <- z * zscale
    n <- length(pretty(zlim, nlevels)) - 1
    zran <- range(z, na.rm=TRUE)
    cols <- color.palette(n)[((z - zran[1]) / (zran[2] - zran[1])) * (n - 1) + 1]
  }

  if (show.points) {
    py <- py * yscale
    pz <- pz * zscale
  }

  # open device
  if (rgl::rgl.cur() == 0) {
    rgl::open3d()
    win.dim <- ppi + width * ppi
    win.rect <- c(ppi, ppi, win.dim, win.dim * 0.75)
    rgl::par3d(windowRect=win.rect)
  } else {
    rgl::clear3d()
  }

  # draw graphics
  if (show.surface) rgl::surface3d(x, y, z, color=cols)
  if (show.points)  rgl::points3d(x=px, y=py, z=pz, size=cex.pts * 3, point_antialias=TRUE)

  rgl::box3d()
  rgl::title3d(xlab="x", ylab="y", zlab="z", line=0.1)
}
