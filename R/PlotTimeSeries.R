PlotTimeSeries <- function(x, y=NULL, xlim=NULL, ylim=NULL, ylab=NULL,
                           tgap=NULL, width=7, cex.pts=1, pointsize=12,
                           fmt=NULL, axis.side=1:2, minor.ticks=FALSE,
                           ticks.inside=FALSE) {
  # A time series plot is drawn with points and connecting lines.

  # Additional functions (subroutines)

  # Plot points and line segments seperated by time gap

  PlotData <- function(x, y, gap, col="black") {
    if (!is.null(tgap)) {
      idxs <- (1:(length(x) - 1))[diff(as.numeric(x)) > tgap]
      for (i in idxs) {
        x <- append(x, (x[i] + (x[i + 1] - x[i]) / 2))
        y <- append(y, NA)
      }
      idxs <- order(x)
      x <- x[idxs]
      y <- y[idxs]
    }
    lwd <- 0.8 * (96 / (6 * 12))
    lines(x, y, col=col, lwd=lwd)

    lwd <- 0.5 * (96 / (6 * 12))
    points(x, y, pch=21, cex=cex.pts, col=col, bg="gray", lwd=lwd)
  }


  # Main program

  # Account for missing arguments

  if (is.list(x)) {
    y <- x$y
    x <- x$x
  }

  # Window setup

  x11(width=width, height=width / 2, pointsize=pointsize)

  op <- par(mfrow=c(1, 1), bg="white", mar=c(2.5, 3.75, 2, 2))
  on.exit(par(op))

  # Remove NA values

  include <- !is.na(x)
  x <- x[include]
  y <- y[include]

  # Sort on datetime

  idxs <- order(x)
  x <- x[idxs]
  y <- y[idxs]

  # Require datetime

  origin <- as.POSIXct("1970-01-01 00:00:00.0")
  if (!inherits(x, "POSIXct"))
    x <- as.POSIXct(x, origin=origin)

  # Initialize axes limits

  if (is.null(xlim))
    xlim <- as.POSIXct(c(NA, NA))
  if (is.null(ylim))
    ylim <- c(NA, NA)

  # Determine points in plot

  include <- rep(TRUE, length(x))
  if (!is.na(xlim[1]))
    include <- include & x >= xlim[1]
  if (!is.na(xlim[2]))
    include <- include & x <= xlim[2]
  if (!is.na(ylim[1]))
    include <- include & y >= ylim[1]
  if (!is.na(ylim[2]))
    include <- include & y <= ylim[2]

  # Determine axes limits

  if (is.na(xlim[1]) || is.na(xlim[2])) {
    xran <- extendrange(x[include], f=0.02)
    if (is.na(xlim[1]))
      xlim[1] <- xran[1]
    if (is.na(xlim[2]))
      xlim[2] <- xran[2]
  }
  if (is.na(ylim[1]) || is.na(ylim[2])) {
    yran <- extendrange(y[include], f=0.02 * (par("pin")[1] / par("pin")[2]))
    if (is.na(ylim[1]))
      ylim[1] <- yran[1]
    if (is.na(ylim[2]))
      ylim[2] <- yran[2]
  }

  # Set line width

  lwd <- 0.5 * (96 / (6 * 12))

  # Plot

  plot.new()
  plot.window(xlim=xlim, ylim=ylim, xaxs="i", yaxs="i")

  PlotData(x, y, tgap)

  # Add axes

  for (side in axis.side) {
    if (side %in% c(1, 3)) {
      if (is.null(fmt)) {
        AddAxis(side=side, lim=xlim, ticks.inside=ticks.inside,
                minor.ticks=minor.ticks, padj=-1)
      } else {
        AddAxis(side=side, lim=xlim, ticks.inside=ticks.inside,
                minor.ticks=minor.ticks, padj=-1, format=fmt)
      }
    } else {
      AddAxis(side=side, lim=ylim, ticks.inside=ticks.inside,
              minor.ticks=minor.ticks, padj=0.7)
    }
  }

  # Label y-axis and box

  if (!is.null(ylab))
    title(ylab=ylab, cex.main=0.9, cex.lab=0.9, line=2.0)

  box(lwd=lwd)
}
