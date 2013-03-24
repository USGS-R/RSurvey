OpenRSurvey <- function() {
  # Activates the main GUI for RSurvey.

  # Additional functions (subroutines)

  # Close GUI

  CloseGUI <- function() {
    tclServiceMode(FALSE)
    if (as.integer(tclvalue(tt.done.var)) != 0)
      return()
    CloseDevices()

    geo <- unlist(strsplit(as.character(tkwm.geometry(tt)), "\\+"))
    Data("win.loc", paste("+", as.integer(geo[2]),
                          "+", as.integer(geo[3]), sep=""))

    tclvalue(tt.done.var) <- 1
    tkdestroy(tt)
    tclServiceMode(TRUE)
  }

  # Open binary project file

  OpenProj <- function() {
    f <- GetFile(cmd="Open", exts="rda", win.title="Open Project File",
                 parent=tt)
    if (is.null(f))
      return()
    if (ClearObjs() == "cancel")
      return()

    project <- NULL
    load(file=f)
    Data(replace.all=project)
    Data("proj.file", f)

    SetCsi()
    SetVars()
  }

  # Save binary project file

  SaveProj <- function() {
    if (!is.null(Data("proj.file"))) {
      if (file.access(Data("proj.file"), mode = 0) != 0)
          Data("proj.file", NULL)
    }
    if (is.null(Data("proj.file"))) {
      f <- GetFile(cmd="Save As", exts="rda", win.title="Save Project As",
                   defaultextension="rda", parent=tt)
      if (!is.null(f)) {
        Data("proj.file", f)
        Data("default.dir", attr(f, "directory"))
      }
    }
    if (!is.null(Data("proj.file"))) {
      csi <- Data("csi")
      Data("csi", NULL)

      project <- Data()
      save(project, file=Data("proj.file"), compress=TRUE)

      Data("csi", csi)
    }
  }

  # Save a new binary project file

  SaveProjAs <- function() {
    Data("proj.file", NULL)
    SaveProj()
    tkfocus(tt)
  }

  # Clear objects

  ClearObjs <- function() {
    msg <- "Save the existing project?"
    if (is.null(Data("proj.file")))
      ans <- "no"
    else
      ans <- as.character(tkmessageBox(icon="question", message=msg,
                                       title="Warning", type="yesnocancel",
                                       parent=tt))
    if (ans == "cancel") {
      return(ans)
    } else if (ans == "yes") {
      SaveProj()
    }
    Data(clear.proj=TRUE)
    SetVars()
    ans
  }

  # Import survey data

  CallImportData <- function() {
    ImportData(tt)
    SetVars()
  }

  # Set button state

  ButtonState <- function(vars) {
    s <- "normal"
    if (is.null(vars$x) | is.null(vars$y))
      s <- "disabled"
    tkconfigure(frame2.but.1.1, state=s)

    s <- "normal"
    if (is.null(vars$x) | is.null(vars$y) | is.null(vars$z))
      s <- "disabled"
    tkconfigure(frame2.but.1.2, state=s)
    tkconfigure(frame2.but.1.3, state=s)
  }

  # Set variables

  SetVars <- function() {
    tkset(frame1.box.1.2, "")
    tkset(frame1.box.2.2, "")
    tkset(frame1.box.3.2, "")
    tkset(frame1.box.4.2, "")
    tkset(frame1.box.5.2, "")

    cols <- Data("cols")
    vars <- Data("vars")

    if (is.null(cols) | is.null(vars)) {
      tkconfigure(frame1.box.1.2, value="")
      tkconfigure(frame1.box.2.2, value="")
      tkconfigure(frame1.box.3.2, value="")
      tkconfigure(frame1.box.4.2, value="")
      tkconfigure(frame1.box.5.2, value="")
      ButtonState(vars)
      if (is.null(cols))
        return()
    }

    ids <- sapply(cols, function(i) i$id)
    classes <- sapply(cols, function(i) i$class)

    idxs.n <- which(classes %in% c("numeric", "integer"))

    vals.n <- c("", ids[idxs.n])

    tkconfigure(frame1.box.1.2, value=vals.n)
    tkconfigure(frame1.box.2.2, value=vals.n)
    tkconfigure(frame1.box.3.2, value=vals.n)
    tkconfigure(frame1.box.4.2, value=vals.n)
    tkconfigure(frame1.box.5.2, value=vals.n)

    if (!is.null(vars$x))
      tcl(frame1.box.1.2, "current", which(vars$x  == idxs.n))
    if (!is.null(vars$y))
      tcl(frame1.box.2.2, "current", which(vars$y  == idxs.n))
    if (!is.null(vars$z))
      tcl(frame1.box.3.2, "current", which(vars$z  == idxs.n))
    if (!is.null(vars$vx))
      tcl(frame1.box.4.2, "current", which(vars$vx == idxs.n))
    if (!is.null(vars$vy))
      tcl(frame1.box.5.2, "current", which(vars$vy == idxs.n))

    ButtonState(vars)
  }

  # Refresh variables

  RefreshVars <- function(item) {
    cols <- Data("cols")

    col.classes <- sapply(cols, function(i) i$class)

    idxs.n <- which(col.classes %in% c("numeric", "integer"))

    idx.x  <- as.integer(tcl(frame1.box.1.2, "current"))
    idx.y  <- as.integer(tcl(frame1.box.2.2, "current"))
    idx.z  <- as.integer(tcl(frame1.box.3.2, "current"))
    idx.vx <- as.integer(tcl(frame1.box.4.2, "current"))
    idx.vy <- as.integer(tcl(frame1.box.5.2, "current"))

    vars <- list()

    if (idx.x > 0)
      vars$x[1] <- idxs.n[idx.x]
    if (idx.y > 0)
      vars$y[1] <- idxs.n[idx.y]
    if (idx.z > 0)
      vars$z[1] <- idxs.n[idx.z]
    if (idx.vx > 0)
      vars$vx[1] <- idxs.n[idx.vx]
    if (idx.vy > 0)
      vars$vy[1] <- idxs.n[idx.vy]

    if (!identical(vars, Data("vars"))) {
      Data("vars", vars)
      Data("data.pts", NULL)
      Data("data.grd", NULL)
    }
    ButtonState(vars)
  }

  # Manage data

  CallManageData <- function() {
    if (is.null(Data("data.raw")))
      return()
    
    ans <- ManageData(Data("cols"), Data("vars"), tt)
    if (!is.null(ans)) {
      Data("cols", ans$cols)
      Data("vars", ans$vars)
      SetVars()
    }
    tkfocus(tt)
  }

  # Export data

  CallExportData <- function(file.type) {
    if (is.null(Data("data.raw")))
      return()

    is.coordinate <- !is.null(Data("vars")$x) & !is.null(Data("vars")$y)
    if (!is.coordinate & file.type %in% c("shape", "grid"))
      stop("Spatial coordinates missing")

    if (file.type == "grid") {
      CallProcessData(interpolate=TRUE)
      WriteFile(file.type="grid")
    } else {
      CallProcessData()
      col.ids <- sapply(Data("cols"), function(i) i$id)
      ExportData(col.ids, file.type=file.type, parent=tt)
    }

    tkfocus(tt)
  }

  # Close graphic devices

  CloseDevices <- function() {
    graphics.off()
    while (rgl.cur() != 0)
      rgl.close()
  }

  # Save R graphic devices

  SaveRDevice <- function() {
    if (is.null(dev.list()))
      return()
    exts <- c("eps", "png", "jpg", "jpeg", "pdf", "bmp", "tif", "tiff")
    f <- GetFile(cmd="Save As", exts=exts, win.title="Save R Graphic As",
                 defaultextension="eps", parent=tt)
    if (is.null(f))
      return()
    savePlot(filename=f, type=attr(f, "extension"))
  }

  # Save RGL graphic devices

  SaveRGLDevice <- function() {
    if (rgl.cur() == 0)
      return()
    f <- GetFile(cmd="Save As", exts=c("png", "eps", "pdf"),
                 win.title="Save RGL Graphic As", defaultextension="png",
                 parent=tt)
    if (is.null(f))
      return()

    if (attr(f, "extension") == "png")
      rgl.snapshot(filename=f, fmt=attr(f, "extension"))
    else
      rgl.postscript(filename=f, fmt=attr(f, "extension"))
  }

  # About package

  AboutPackage <- function() {
    if ("package:RSurvey" %in% search())
      path <- system.file("DESCRIPTION", package="RSurvey")
    else
      path <- file.path(getwd(), "DESCRIPTION")
    msg <- paste(readLines(path, n=-1L), collapse="\n")
    tkmessageBox(icon="info", message=msg, title="About", parent=tt)
  }

  # Manage polygons

  CallManagePolygons <- function() {
    old.polygons <- Data("polys")
    old.data.poly <- attr(old.polygons, "data.poly")
    old.crop.poly <- attr(old.polygons, "crop.poly")
    if (is.null(old.data.poly))
      old.data.poly <- NA
    if (is.null(old.crop.poly))
      old.crop.poly <- NA
    new.polygons <- ManagePolygons(Data("polys"), parent=tt)
    if (is.null(new.polygons) | identical(new.polygons, old.polygons))
      return()
    new.data.poly <- attr(new.polygons, "data.poly")
    new.crop.poly <- attr(new.polygons, "crop.poly")
    if (is.null(new.data.poly))
      new.data.poly <- NA
    if (is.null(new.crop.poly))
      new.crop.poly <- NA
    if (!identical(new.polygons[[new.data.poly]], 
                   old.polygons[[old.data.poly]])) {
      attr(new.polygons, "data.poly") <- NULL
      Data("data.pts",  NULL)
      Data("data.grd",  NULL)
    }
    if (!identical(new.polygons[[new.crop.poly]], 
                   old.polygons[[old.crop.poly]])) {
      attr(new.polygons, "crop.poly") <- NULL
      Data("data.grd",  NULL)
    }
    Data("polys", new.polygons)
  }

  # Set polygon range and limit

  CallSetPolygonLimits <- function() {
    old.data.poly <- attr(Data("polys"), "data.poly")
    old.crop.poly <- attr(Data("polys"), "crop.poly")
    ans <- SetPolygonLimits(names(Data("polys")), old.data.poly, old.crop.poly, 
                            tt)
    if (is.null(ans))
      return()
    if (!identical(ans$data.poly, old.data.poly)) {
      Data("polys", ans$data.poly, which.attr="data.poly")
      Data("data.pts", NULL)
      Data("data.grd", NULL)
    }
    if (!identical(ans$crop.poly, old.crop.poly)) {
      Data("polys", ans$crop.poly, which.attr="crop.poly")
      Data("data.grd", NULL)
    }
  }

  # Construct polygon

  ConstructPolygon <- function(type) {
    if (is.null(Data("data.raw")))
      return()
    msg <- paste("After the graph has been created, use the mouse to identify",
                 "the vertices of the polygon. The identification process can",
                 "be terminated by clicking the second button and selecting",
                 "[Stop] from the menu, or from the [Stop] menu on the",
                 "graphics window.", sep="\n")
    if (shown.construct.polygon.msgbox)
      tkmessageBox(icon="info", message=msg, title="Build Polygon", type="ok",
                   parent=tt)
    shown.construct.polygon.msgbox <<- FALSE
    CallPlot2d(type=type, build.poly=TRUE)
    tkfocus(tt)
  }

  # Autocrop polygon

  CallAutocropRegion <- function() {
    if (is.null(Data("data.raw")))
      return()
    CallProcessData()

    d <- Data("data.pts")

    xlab <- Data("cols")[[Data("vars")$x]]$id
    ylab <- Data("cols")[[Data("vars")$y]]$id
    zlab <- Data("cols")[[Data("vars")$z]]$id

    asp     <- Data("asp.yx")
    csi     <- Data("csi")
    width   <- Data("width")
    nlevels <- Data("nlevels")
    cex.pts <- Data("cex.pts")
    rkey    <- Data("rkey")

    ply.new <- AutocropRegion(d, tt, xlab=xlab, ylab=ylab, zlab=zlab,
                              asp=asp, csi=csi, width=width, nlevels=nlevels,
                              cex.pts=cex.pts, rkey=rkey)

    if (inherits(ply.new, "gpc.poly")) {
      ply <- list()
      if (!is.null(Data("polys")))
        ply <- Data("polys")
      ply.name <- NamePolygon(old=names(ply))
      ply[[ply.name]] <- ply.new
      Data("polys", ply)
      Data("polys", ply.name, which.attr="crop.poly")
      Data("data.grd", NULL)
    }
    tkfocus(tt)
  }

  # Name polygon

  NamePolygon <- function(old=NULL, nam=NA){
    if (is.na(nam))
      nam <- "New Polygon"
    idx <- 1
    chk <- nam
    while (chk %in% old) {
      chk <- paste(nam, " (", idx, ")", sep="")
      idx <- idx + 1
    }
    chk
  }

  # Plot point or 2d surface data

  CallPlot2d <- function(type, build.poly=FALSE) {
    if (type == "p")
      CallProcessData()
    else
      CallProcessData(interpolate=TRUE)

    if (is.null(Data("data.grd")) && type %in% c("g", "l")) {
      return()
    } else if (is.null(Data("data.pts"))) {
      return()
    }
    
    if (type == "p") 
      ply <- Data("polys", which.attr="data.poly")
    else
      ply <- Data("polys", which.attr="crop.poly")
    
    if (!is.null(ply) && !is.na(ply))
      ply <- Data("polys")[[ply]]

    show.poly   <- Data("show.poly") && inherits(ply, "gpc.poly")
    show.lines  <- type %in% c("l", "g") && Data("show.lines")
    show.points <- type %in% c("l", "g") && Data("show.points")

    axis.side <- 1:2
    if (Data("show.2.axes"))
      axis.side <- 1:4

    nlevels <- Data("nlevels")
    cols    <- Data("cols")
    vars    <- Data("vars")

    xlab <- cols[[vars$x]]$id
    ylab <- cols[[vars$y]]$id
    zlab <- if (is.null(vars$z)) NULL else cols[[vars$z]]$id

    if (type == "p") {
      dat <- Data("data.pts")
    } else if (type %in% c("l", "g")) {
      dat <- Data("data.grd")
    }

    if (type == "g") {
      x.midpoint <- dat$x[1:(length(dat$x) - 1)] + diff(dat$x) / 2
      y.midpoint <- dat$y[1:(length(dat$y) - 1)] + diff(dat$y) / 2
      xran <- range(x.midpoint, finite=TRUE)
      yran <- range(y.midpoint, finite=TRUE)
    } else {
      xran <- range(dat$x, finite=TRUE)
      yran <- range(dat$y, finite=TRUE)
    }

    # Adjust axes limits for polygon

    lim <- Data("lim.axes")

    xlim <- lim$x
    if (is.null(xlim))
      xlim <- c(NA, NA)
    ylim <- lim$y
    if (is.null(ylim))
      ylim <- c(NA, NA)

    if (show.poly) {
      bbx <- bby <- NULL
      bb <- get.bbox(ply)

      if (!is.na(xlim[1]))
        bb$x[1] <- xlim[1]
      if (!is.na(xlim[2]))
        bb$x[2] <- xlim[2]
      if (!is.na(ylim[1]))
        bb$y[1] <- ylim[1]
      if (!is.na(ylim[2]))
        bb$y[2] <- ylim[2]

      xy <- cbind(x=c(bb$x, rev(bb$x)), y=c(bb$y[c(1,1)], bb$y[c(2,2)]))
      bb <- get.bbox(intersect(ply, as(xy, "gpc.poly")))
      bbx <- range(bb$x)
      bby <- range(bb$y)
      bbx <- extendrange(bbx, f=0.02)
      bby <- extendrange(bby, f=0.02)
      if (is.na(xlim[1]) && bbx[1] < xran[1])
        lim$x[1] <- bbx[1]
      if (is.na(xlim[2]) && bbx[2] > xran[2])
        lim$x[2] <- bbx[2]
      if (is.na(ylim[1]) && bby[1] < yran[1])
        lim$y[1] <- bby[1]
      if (is.na(ylim[2]) && bby[2] > yran[2])
        lim$y[2] <- bby[2]
    }

    tkconfigure(tt, cursor="watch")
    Plot2d(dat, type=type, xlim=lim$x, ylim=lim$y, zlim=lim$z,
           xlab=xlab, ylab=ylab, zlab=zlab, asp=Data("asp.yx"),
           csi=Data("csi"), width=Data("width"), nlevels=nlevels,
           cex.pts=Data("cex.pts"), rkey=Data("rkey"),
           color.palette=Data("color.palette"),
           vuni=Data("vuni"), vmax=Data("vmax"),
           vxby=Data("vxby"), vyby=Data("vyby"),
           axis.side=axis.side, minor.ticks=Data("minor.ticks"),
           ticks.inside=Data("ticks.inside"), rm.pnt.line=Data("rm.pnt.line"),
           add.contour.lines=show.lines)

    if (show.poly)
      plot(ply, add=TRUE, poly.args=list(border="black", lty=3))
    if (show.points)
      points(x=Data("data.pts")$x, y=Data("data.pts")$y, pch=19,
             cex=Data("cex.pts") / 2, col="black")
    if (build.poly) {
      v <- locator(type="o", col="black", bg="black", pch=22)
      loc.xy <- cbind(c(v$x, v$x[1]), c(v$y, v$y[1]))
      points(loc.xy, col="black", bg="black", pch=22)
      lines(loc.xy, col="black")

      ply.new <- as(as.data.frame(v), "gpc.poly")
      if (!is.null(ply))
        ply.new <- intersect(ply, ply.new)
      if (area.poly(ply.new) == 0) {
        msg <- "The resulting polygon is invalid."
        tkmessageBox(icon="warning", message=msg, title="Polygon Discarded",
                     parent=tt)
        ply.new <- NULL
      }

      if (inherits(ply.new, "gpc.poly")) {
        ply.list <- if (is.null(Data("polys"))) list() else Data("polys")
        ply.name <- NamePolygon(old=names(ply.list))
        ply.list[[ply.name]] <- ply.new

        if (type == "p") {
          pts <- get.pts(ply.new)
          logic <- rep(TRUE, nrow(dat))
          for (i in seq(along=pts)) {
              is.in <-  point.in.polygon(point.x=dat$x, point.y=dat$y,
                                         pol.x=pts[[i]]$x, pol.y=pts[[i]]$y) > 0
              is.in <- if (pts[[i]]$hole) !is.in else is.in
              logic <- logic & is.in
          }
          if (any(logic)) {
            points(dat$x[logic], dat$y[logic], col="red",
                   cex=Data("cex.pts"), pch=20)
            Data("polys", ply.list)
            Data("polys", ply.name, which.attr="data.poly")
            Data("data.pts", NULL)
            Data("data.grd", NULL)
          } else {
            msg <- "No data points fall within the given polygon."
            tkmessageBox(icon="warning", message=msg, title="Polygon Discarded",
                         parent=tt)
          }
        } else if (type == "l") {
          cutout <- CutoutPolygon(dat, ply.new)
          if (!is.null(cutout)) {
            Data("polys", ply.list)
            Data("polys", ply.name, which.attr="crop.poly")
            Data("data.grd", NULL)
          }
        }
      }
    }
    tkconfigure(tt, cursor="arrow")
    tkfocus(tt)
  }

  # Plot 3d surface data

  CallPlot3d <- function() {
    CallProcessData(interpolate=TRUE)

    if (is.null(Data("data.grd")))
      return()

    dat <- Data("data.grd")
    pts <- NULL
    if (Data("show.points"))
      pts <- Data("data.pts")
    lim <- Data("lim.axes")

    tkconfigure(tt, cursor="watch")
    Plot3d(x=dat, px=pts, xlim=lim$x, ylim=lim$y, zlim=lim$z,
           vasp=Data("asp.zx"), hasp=Data("asp.yx"),
           width=Data("width"), cex.pts=Data("cex.pts"),
           nlevels=Data("nlevels"), color.palette=Data("color.palette"))
    tkconfigure(tt, cursor="arrow")
    tkfocus(tt)
  }

  # Set the height of (default-sized) characters in inches.

  SetCsi <- function() {
    if (is.null(Data("csi"))) {
      x11(pointsize=12)
      Data("csi", par("csi"))
      dev.off()
    }
  }

  # Call view data for state variable data

  CallViewData <- function() {
    CallProcessData()
    if (is.null(Data("data.pts")))
      return()

    tkconfigure(tt, cursor="watch")

    vars <- Data("vars")
    cols <- Data("cols")

    lst <- list(x="x-coordinate", y="y-coordinate", z="z-coordinate",
                vx="x-vector", vy="y-vector")
    state.vars <- lst[names(lst) %in% names(vars)]
    state.idxs <- sapply(names(state.vars), function(i) vars[[i]])
    
    d <- Data("data.pts")[, names(state.vars)]
    
    cols <- cols[state.idxs]
    
    nams <- sapply(state.vars, function(i) i)
    fmts <- sapply(cols, function(i) ifelse(is.null(i$format), NA, i$format))
    ViewData(d, col.names=nams, col.formats=fmts, parent=tt)
    
    tkconfigure(tt, cursor="arrow")
    tkfocus(tt)
  }

  # Call process data

  CallProcessData <- function(interpolate=FALSE) {
    vars <- Data("vars")
    var.names <- names(vars)
    if (!all(c("x", "y") %in% var.names) || is.null(Data("data.raw"))) {
      Data("data.pts", NULL)
      Data("data.grd", NULL)
      return()
    }

    tkconfigure(tt, cursor="watch")

    # Process points

    if (is.null(Data("data.pts"))) {
      cols <- Data("cols")

      Eval <- function(v) {
        if (is.null(v)) NULL else EvalFunction(cols[[v]]$fun, cols)
      }
      lst <- lapply(var.names, function(i) Eval(vars[[i]]))
      len <- sapply(lst, function(i) length(i))
      max.len <- max(len)

      d <- as.data.frame(matrix(NA, nrow=max.len, ncol=length(lst)))
      names(d) <- var.names
      for (i in seq(along=lst))
        d[[i]] <- c(lst[[i]], rep(NA, max.len - len[i]))

      query.fun <- Data("query.fun")
      if (is.null(query.fun)) {
        coerce.rows <- NULL
      } else {
        coerce.rows <- EvalFunction(query.fun, cols)
      }
      
      if (!is.null(vars$x)) {
        ply <- Data("polys", which.attr="data.poly")
        if (!is.null(ply))
          ply <- Data(c("polys", ply))
      } else {
        ply <- NULL
      }

      data.pts <- ProcessData(d, type="p", coerce.rows=coerce.rows, ply=ply)
      Data("data.pts", data.pts)
      Data("data.grd", NULL)
    }

    if (is.null(Data("data.pts"))) {
      tkconfigure(tt, cursor="arrow")
      return()
    }

    # Process grid

    if (is.null(Data("data.grd")) && interpolate) {
      ply <- Data("polys", which.attr="crop.poly")
      if (!is.null(ply))
        ply <- Data("polys")[[ply]]
      grid.res <- Data("grid.res")
      grid.mba <- Data("grid.mba")
      data.grd <- ProcessData(Data("data.pts"), type="g", ply=ply,
                              grid.res=grid.res, grid.mba=grid.mba)
      Data("data.grd", data.grd)
    }

    tkconfigure(tt, cursor="arrow")
  }
  
  # Build query
  
  BuildQuery <- function() {
    if (is.null(Data("data.raw")))
      return()
    
    n <- nrow(Data("data.raw"))
    if (n == 0)
      return()
    
    cols <- Data("cols")
    old.fun <- Data("query.fun")
    new.fun <- EditFunction(cols, fun=old.fun, value.length=n,
                            value.class="logical", 
                            win.title="Edit Query", parent=tt)
    if (is.null(new.fun))
      return()
    if (new.fun == "")
      new.fun <- NULL
    Data("query.fun", new.fun)
    Data("data.pts", NULL)
    Data("data.grd", NULL)
  }
  
  # Clear query
  
  ClearQuery <- function() {
    if (!is.null(Data("query.fun"))) {
      Data("query.fun", NULL)
      Data("data.pts", NULL)
      Data("data.grd", NULL)
    }
  }
  
  
  # Main program

  # Load required R packages
  LoadPackages()

  # Warn if using Windows OS and running in MDI mode
  if (.Platform$OS.type == "windows" && getIdentification() == "RGui")
    message("\n\n    You are running R in MDI mode which *may* interfere\n",
            "    with the functionality of the graphical user interface.\n",
            "    It is recommended to use R in SDI mode which can be\n",
            "    set in the command line or by clicking in the Menu:\n",
            "    Edit - GUI Preferences: SDI, then Save and restart R.\n\n")

  # Establish default directories
  if ("package:RSurvey" %in% search())
    path <- system.file("RSurvey-ex", package="RSurvey")
  else
    path <- getwd()
  if (is.null(Data("default.dir")))
    Data("default.dir", path)

  if ("package:RSurvey" %in% search())
    image.path <- system.file("images", package="RSurvey")
  else
    image.path <- file.path(path, "inst", "images")

  # Set options
  SetCsi()
  options(digits.secs=3)
  options(help_type="html")
  shown.construct.polygon.msgbox <- TRUE

  # Assign variables linked to Tk entry widgets
  tt.done.var <- tclVar(0)

  # Package version number
  f <- "DESCRIPTION"
  if ("package:RSurvey" %in% search())
    f <- system.file("DESCRIPTION", package="RSurvey")
  ver <- scan(f, what="character", skip=1, nlines=1, quiet=TRUE)[2]
  Data("ver", paste("RSurvey", ver))

  # Open GUI
  tclServiceMode(FALSE)
  tt <- tktoplevel()
  tkwm.geometry(tt, Data("win.loc"))
  tktitle(tt) <- Data("ver")
  tkwm.resizable(tt, 1, 0)

  # Top menu
  top.menu <- tkmenu(tt, tearoff=0)

  # File menu

  menu.file <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="File", menu=menu.file, underline=0)

  tkadd(menu.file, "command", label="New project", accelerator="Ctrl+N",
        command=ClearObjs)
  tkadd(menu.file, "command", label="Open project\u2026", accelerator="Ctrl+O",
        command=OpenProj)
  tkadd(menu.file, "command", label="Save project", accelerator="Ctrl+S",
        command=SaveProj)
  tkadd(menu.file, "command", label="Save project as\u2026",
        accelerator="Shift+Ctrl+S", command=SaveProjAs)

  tkadd(menu.file, "separator")
  
  menu.file.import <- tkmenu(tt, tearoff=0)
  tkadd(menu.file.import, "command", label="Text file\u2026",
        command=CallImportData)
  tkadd(menu.file.import, "command", label="R data set\u2026",
        command=function() print("notyet"))
  tkadd(menu.file, "cascade", label="Import point data from", 
        menu=menu.file.import)
  
  menu.file.export <- tkmenu(tt, tearoff=0)
  tkadd(menu.file.export, "command", label="Text file\u2026",
        command=function() CallExportData("text"))
  tkadd(menu.file.export, "command", label="Shapefile\u2026",
        command=function() CallExportData("shape"))
  tkadd(menu.file, "cascade", label="Export point data as", 
        menu=menu.file.export)
  tkadd(menu.file, "command", label="Export grid data as\u2026",
        command=function() CallExportData("grid"))

  tkadd(menu.file, "separator")
  menu.file.save <- tkmenu(tt, tearoff=0)
  tkadd(menu.file.save, "command", label="R graphic\u2026", accelerator="Ctrl+R",
        command=SaveRDevice)
  tkadd(menu.file.save, "command", label="RGL graphic\u2026",
        command=SaveRGLDevice)
  tkadd(menu.file, "cascade", label="Save graph from", menu=menu.file.save)

  tkadd(menu.file, "separator")
  tkadd(menu.file, "command", label="Exit",
        command=CloseGUI)

  # Edit menu

  menu.edit <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Edit", menu=menu.edit, underline=0)

  tkadd(menu.edit, "command", label="Manage data\u2026",
        command=CallManageData)
  
  tkadd(menu.edit, "separator")
  tkadd(menu.edit, "command", label="Edit query\u2026",
        command=BuildQuery)
  tkadd(menu.edit, "command", label="Clear query",
        command=ClearQuery)

  tkadd(menu.edit, "separator")
  tkadd(menu.edit, "command", label="Set sort order\u2026",
        command=function() {
          col.ids <- sapply(Data("cols"), function(i) i$id)
          sort.on.old <- Data(c("vars", "sort.on"))
          sort.on.new <- SetSortOrder(col.ids, sort.on.old, parent=tt)
          if (!identical(sort.on.old, sort.on.new)) {
            Data(c("vars", "sort.on"), sort.on.new)
            Data("data.pts", NULL)
            Data("data.grd", NULL)
          }
        })
  tkadd(menu.edit, "command", label="Clear sort order",
        command=function() {
          Data(c("vars", "sort.on"), NULL)
        })

  tkadd(menu.edit, "separator")
  tkadd(menu.edit, "command", label="Interpolation preferences",
        command=function() {
          SetPreferences(tt)
        })
  
  tkadd(menu.edit, "separator")
  tkadd(menu.edit, "command", label="View processed data",
        command=CallViewData)

  # Polygon menu

  menu.poly <- tkmenu(tt, tearoff=0)

  tkadd(top.menu, "cascade", label="Polygon", menu=menu.poly, underline=0)

  tkadd(menu.poly, "command", label="Manage polygons\u2026",
        command=CallManagePolygons)
  tkadd(menu.poly, "separator")
  tkadd(menu.poly, "command", label="Set polygon limits\u2026",
        command=CallSetPolygonLimits)
  tkadd(menu.poly, "command", label="Clear polygon limits",
        command=function() {
          Data("polys", NULL, which.attr="data.poly")
          Data("polys", NULL, which.attr="crop.poly")
          Data("data.pts", NULL)
          Data("data.grd", NULL)
        })
  tkadd(menu.poly, "separator")

  menu.poly.con <- tkmenu(tt, tearoff=0)
  tkadd(menu.poly.con, "command", label="Boundary defining data limits",
        command=function() {
          ConstructPolygon(type="p")
        })
  tkadd(menu.poly.con, "command", label="Crop region for interpolated surface",
        command=function() {
          ConstructPolygon(type="l")
        })
  tkadd(menu.poly, "cascade", label="Build", menu=menu.poly.con)
  tkadd(menu.poly, "command", label="Autocrop region\u2026",
        command=CallAutocropRegion)

  # Graph menu

  menu.graph <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Graph", menu=menu.graph, underline=0)
  
  tkadd(menu.graph, "command", label="Histogram\u2026", 
        command=function() {
          CallProcessData()
          if (is.null(Data("data.pts")))
            return()
          d <- Data("data.pts")
          d <- d[, names(d) != "sort.on"]
          lst <- list(x="x-coordinate", y="y-coordinate", z="z-coordinate", 
                      vx="x-vector", vy="y-vector")
          var.names <- vapply(names(d), function(i) lst[[i]], "")
          BuildHistogram(d, var.names=var.names, parent=tt)
        })
  tkadd(menu.graph, "command", label="Scatterplot",
        command=function() {
          CallPlot2d(type="p")
        })
  tkadd(menu.graph, "command", label="2D interpolated map",
        command=function() {
          type <- if (Data("img.contour")) "g" else "l"
          CallPlot2d(type=type)
        })
  tkadd(menu.graph, "command", label="3D interpolated map",
        command=CallPlot3d)
  
  tkadd(menu.graph, "separator")
  tkadd(menu.graph, "command", label="Set axes limits\u2026",
        command=function() {
          lim <- SetAxesLimits(Data("lim.axes"), tt)
          Data("lim.axes", lim)
        })
  tkadd(menu.graph, "command", label="Clear axes limits",
        command=function() {
          Data("lim.axes", NULL)
        })
  
  tkadd(menu.graph, "separator")
  tkadd(menu.graph, "command", label="Configuration",
        command=function() {
          SetConfiguration(tt)
        })

  tkadd(menu.graph, "command", label="Choose color palette\u2026",
        command=function() {
          pal <- colorspace::choose_palette(pal=Data("color.palette"), 
                                            n=Data("nlevels"), parent=tt)
          if (!is.null(pal))
            Data("color.palette", pal)
        })

  tkadd(menu.graph, "separator")
  tkadd(menu.graph, "command", label="Close all graphs", accelerator="Ctrl+F4",
        command=CloseDevices)

  # Help menu

  menu.help <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Help", menu=menu.help, underline=0)
  tkadd(menu.help, "command", label="Help",
        command=function() {
          print(help("OpenRSurvey", package="RSurvey"))
        })
  tkadd(menu.help, "command", label="Functions",
        command=function() {
          help(package="RSurvey")
        })
  
  tkadd(menu.help, "separator")
  menu.help.rep <- tkmenu(tt, tearoff=0)
  tkadd(menu.help.rep, "command", label="CRAN",
        command=function() {
          browseURL("http://cran.r-project.org/web/packages/RSurvey/")
        })
  tkadd(menu.help.rep, "command", label="GitHub",
        command=function() {
          browseURL("https://github.com/jfisher-usgs/RSurvey")
        })
  tkadd(menu.help, "cascade", label="Repository on  ", menu=menu.help.rep)
  
  tkadd(menu.help, "separator")
  tkadd(menu.help, "command", label="About",
        command=AboutPackage)
  
  if (!("RSurvey" %in% .packages())) {
      tkadd(menu.help, "separator")
      tkadd(menu.help, "command", label="Restore R session",
            command=function() {
              CloseGUI()
              Data("data.pts", NULL)
              Data("data.grd", NULL)
              RestoreSession(file.path(getwd(), "R"), save.objs="Data",
                             fun.call="OpenRSurvey")
            })
  }

  # Finalize top menu

  tkconfigure(tt, menu=top.menu)

  # Frame 0, toolbar with command buttons

  import.var  <- tclVar()
  save.var    <- tclVar()
  data.var    <- tclVar()
  polygon.var <- tclVar()
  globe.var   <- tclVar()
  config.var  <- tclVar()
  axes.var    <- tclVar()
  close.var   <- tclVar()

  frame0 <- ttkframe(tt, relief="flat", borderwidth=2)
  tkpack(frame0, side="top", fill="x")

  tkimage.create("photo", import.var, format="GIF",
                 file=file.path(image.path, "import.gif"))
  tkimage.create("photo", save.var, format="GIF",
                 file=file.path(image.path, "save.gif"))
  tkimage.create("photo", data.var, format="GIF",
                 file=file.path(image.path, "data.gif"))
  tkimage.create("photo", polygon.var, format="GIF",
                 file=file.path(image.path, "polygon.gif"))
  tkimage.create("photo", config.var, format="GIF",
                 file=file.path(image.path, "config.gif"))
  tkimage.create("photo", axes.var, format="GIF",
                 file=file.path(image.path, "axes.gif"))
  tkimage.create("photo", close.var, format="GIF",
                 file=file.path(image.path, "close.gif"))

  frame0.but.1  <- tkbutton(frame0, relief="flat", overrelief="raised",
                            borderwidth=1, image=import.var,
                            command=CallImportData)
  frame0.but.2  <- tkbutton(frame0, relief="flat", overrelief="raised",
                            borderwidth=1, image=save.var,
                            command=SaveProj)
  frame0.but.3  <- tkbutton(frame0, relief="flat", overrelief="raised",
                            borderwidth=1, image=data.var,
                            command=CallManageData)
  frame0.but.4  <- tkbutton(frame0, relief="flat", overrelief="raised",
                            borderwidth=1, image=polygon.var,
                            command=CallManagePolygons)
  frame0.but.5  <- tkbutton(frame0, relief="flat", overrelief="raised",
                            borderwidth=1, image=config.var,
                            command=function() SetConfiguration(tt))
  frame0.but.6  <- tkbutton(frame0, relief="flat", overrelief="raised",
                            borderwidth=1, image=axes.var,
                            command=function() {
                             lim <- SetAxesLimits(Data("lim.axes"), tt)
                             Data("lim.axes", lim)
                           })
  frame0.but.7  <- tkbutton(frame0, relief="flat", overrelief="raised",
                            borderwidth=1, image=close.var,
                            command=CloseDevices)

  tkpack(frame0.but.1, frame0.but.2, frame0.but.3, frame0.but.4, frame0.but.5,
         frame0.but.6, frame0.but.7, side="left")

  separator <- ttkseparator(tt, orient="horizontal")
  tkpack(separator, fill="x")

  # Frame 1, variables

  frame1 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                          text="Set variables")

  frame1.lab.1.1 <- ttklabel(frame1, text="x-coordinate")
  frame1.lab.2.1 <- ttklabel(frame1, text="y-coordinate")
  frame1.lab.3.1 <- ttklabel(frame1, text="z-coordinate")
  frame1.lab.4.1 <- ttklabel(frame1, text="x-vector")
  frame1.lab.5.1 <- ttklabel(frame1, text="y-vector")

  frame1.box.1.2 <- ttkcombobox(frame1, state="readonly")
  frame1.box.2.2 <- ttkcombobox(frame1, state="readonly")
  frame1.box.3.2 <- ttkcombobox(frame1, state="readonly")
  frame1.box.4.2 <- ttkcombobox(frame1, state="readonly")
  frame1.box.5.2 <- ttkcombobox(frame1, state="readonly")

  tkgrid(frame1.lab.1.1, frame1.box.1.2, pady=c(0, 4))
  tkgrid(frame1.lab.2.1, frame1.box.2.2, pady=c(0, 4))
  tkgrid(frame1.lab.3.1, frame1.box.3.2, pady=c(0, 4))
  tkgrid(frame1.lab.4.1, frame1.box.4.2, pady=c(0, 4))
  tkgrid(frame1.lab.5.1, frame1.box.5.2)

  tkgrid.configure(frame1.lab.1.1, frame1.lab.2.1, frame1.lab.3.1,
                   frame1.lab.4.1, frame1.lab.5.1, sticky="w", padx=c(0, 2))

  tkgrid.configure(frame1.box.1.2, frame1.box.2.2, frame1.box.3.2,
                   frame1.box.4.2, frame1.box.5.2, sticky="we")
  tkgrid.configure(frame1.box.1.2, frame1.box.2.2, frame1.box.3.2,
                   frame1.box.4.2)

  tkgrid.columnconfigure(frame1, 1, weight=1, minsize=25)

  tkpack(frame1, fill="x", expand=TRUE, ipadx=0, ipady=0, padx=10, pady=5)

  # Frame 2, plotting buttons

  frame2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                          text="Graph variables")

  frame2.but.1.1 <- ttkbutton(frame2, width=10, text="Scatter",
                              command=function() {
                                CallPlot2d(type="p")
                              })
  frame2.but.1.2 <- ttkbutton(frame2, width=10, text="2D Map",
                              command=function() {
                                type <- if (Data("img.contour")) "g" else "l"
                                CallPlot2d(type=type)
                              })
  frame2.but.1.3 <- ttkbutton(frame2, width=10, text="3D Map",
                              command=CallPlot3d)

  tkgrid(frame2.but.1.1, frame2.but.1.2, frame2.but.1.3, pady=c(0, 4))
  tkgrid.configure(frame2.but.1.2, padx=4)

  tcl("grid", "anchor", frame2, "center")

  tkpack(frame2, fill="x", ipadx=0, ipady=0, expand=TRUE,
         padx=10, pady=c(0, 10))

  # Set variables

  SetVars()

  # Bind events

  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", CloseGUI)

  tkbind(tt, "<Control-n>", ClearObjs)
  tkbind(tt, "<Control-o>", OpenProj)
  tkbind(tt, "<Control-s>", SaveProj)
  tkbind(tt, "<Shift-Control-S>", SaveProjAs)
  tkbind(tt, "<Control-r>", SaveRDevice)
  tkbind(tt, "<Control-F4>", CloseDevices)

  tkbind(frame1.box.1.2, "<<ComboboxSelected>>", RefreshVars)
  tkbind(frame1.box.2.2, "<<ComboboxSelected>>", RefreshVars)
  tkbind(frame1.box.3.2, "<<ComboboxSelected>>", RefreshVars)
  tkbind(frame1.box.4.2, "<<ComboboxSelected>>", RefreshVars)
  tkbind(frame1.box.5.2, "<<ComboboxSelected>>", RefreshVars)

  # GUI closure

  tkfocus(force=tt)
  invisible()
}
