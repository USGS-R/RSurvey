StartGui <- function() {


  # close gui
  CloseGUI <- function() {
    if (as.integer(tclvalue(tt.done.var)) > 0)
      return()
    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE))
    geo <- unlist(strsplit(as.character(tkwm.geometry(tt)), "\\+"))
    tkdestroy(tt)
    Data("win.loc", paste0("+", as.integer(geo[2]), "+", as.integer(geo[3])))
    tclvalue(tt.done.var) <- 1
    CloseDevices()
  }


  # open binary project file
  OpenProj <- function() {
    file <- GetFile(cmd="Open", exts="RData", win.title="Open Project File", parent=tt)
    if (is.null(file) || file.access(file, mode=0) == -1L) return()
    if (ClearObjs() == "cancel") return()
    ans <- try(load(file=file, envir=environment(OpenProj)), silent=TRUE)
    if (inherits(ans, "try-error")) {
      tkmessageBox(icon="error", detail=ans, title="Error", type="ok", parent=tt,
                   message="Not a valid project file.")
      return()
    }
    Data(replace.all=get(ans, envir=environment(OpenProj)))
    Data("proj.file", file)
    SetCsi()
    SetVars()
  }


  # save binary project file
  SaveProj <- function() {
    if (!is.null(Data("proj.file")) &&
        file.access(Data("proj.file"), mode=0) != 0) {
      Data("proj.file", NULL)
    }
    if (is.null(Data("proj.file"))) {
      file <- GetFile(cmd="Save As", exts="RData", win.title="Save Project As",
                      defaultextension="RData", parent=tt)
      if (!is.null(file)) {
        Data("proj.file", file)
        Data("default.dir", attr(file, "directory"))
      }
    }
    if (!is.null(Data("proj.file"))) {
      csi <- Data("csi")
      Data("csi", NULL)
      file <- Data("proj.file")
      obj.name <- sub("[.][^.]*$", "", basename(file))
      assign(obj.name, Data(), envir=environment(SaveProj))
      save(list=obj.name, file=file, compress=TRUE)
      Data("csi", csi)
    }
  }


  # save a new binary project file
  SaveProjAs <- function() {
    Data("proj.file", NULL)
    SaveProj()
  }


  # clear objects
  ClearObjs <- function() {
    if (is.null(Data("proj.file")))
      ans <- "no"
    else
      ans <- as.character(tkmessageBox(icon="question", title="Warning",
                                       type="yesnocancel", parent=tt,
                                       message="Save the existing project?"))
    if (ans == "cancel") {
      return(ans)
    } else if (ans == "yes") {
      SaveProj()
    }
    Data(clear.proj=TRUE)
    SetVars()
    return(ans)
  }


  # write data
  WriteData <- function(file.type) {
    if (is.null(Data("cols"))) return()
    is.coordinate <- !is.null(Data("vars")$x) & !is.null(Data("vars")$y)
    if (!is.coordinate & file.type %in% c("shp", "grd")) return()
    if (file.type == "grd") {
      CallProcessData(interpolate=TRUE)
      d <- Data("data.grd")
      if (is.null(d)) return()
      file <- GetFile(cmd="Save As", exts="rda", file=NULL,
                      win.title="Save Grid Data As", defaultextension="rda")
      if (is.null(file)) return()
      save(d, file=file)
    } else {
      CallProcessData()
      ExportData(file.type=file.type, parent=tt)
    }
  }


  # read data
  ReadData <- function(file.type) {
    if (file.type == "txt") {
      ImportText(tt)
    } else {

      if (file.type == "xlsx") {
        ans <- ImportSpreadsheet(parent=tt)
        d <- ans$d
        src <- ans$src
      } else if (file.type == "rda") {
        file <- GetFile(cmd="Open", exts="rda", win.title="Open R Data File", parent=tt)
        if (is.null(file)) return()
        d <- local({d.name <- load(file=file); return(eval(parse(text=d.name[1])))})
        if (!inherits(d, c("data.frame", "matrix"))) {
          tkmessageBox(icon="error", title="Error", type="ok", parent=tt,
                       message="R data set is not a valid object class.")
          return()
        }
        src <- c(pathname=file[1], accessed=format(Sys.time()))
      } else if (file.type == "rpackage") {
        valid.classes <- c("data.frame", "matrix")
        d <- ImportPackage(valid.classes, parent=tt)
        src <- d$src
        d <- d$d
      }

      if (is.null(d) || nrow(d) == 0 || ncol(d) == 0) return()
      if (!is.null(Data("cols"))) {
        ans <- as.character(tkmessageBox(icon="warning", title="Warning",
                                         type="okcancel", parent=tt,
                                         message="This action will delete existing data."))
        if (ans == "cancel") return()
      }

      m <- nrow(d)
      n <- ncol(d)

      rows <- rownames(d)
      if (is.null(rows)) rows <- seq_len(m)
      rows.int <- as.integer(rows)
      is.int <- is.integer(rows.int) && !anyDuplicated(rows.int)
      rows <- if (is.int) rows.int else rows

      col.names <- colnames(d)

      if (inherits(d, "matrix")) {
        rownames(d) <- NULL
        colnames(d) <- NULL
        d <- split(d, rep(seq_len(ncol(d)), each=nrow(d)))
      } else if (inherits(d, "data.frame")) {
        d <- as.list(d)
      }

      ids <- col.names
      matched <- lapply(unique(ids), function(i) which(ids %in% i)[-1])
      names(matched) <- unique(ids)
      for (i in seq_along(matched))
        ids[matched[[i]]] <- paste0(names(matched[i]), " (",
                                    seq_along(matched[[i]]), ")")
      names(d) <- paste0("V", seq_len(n))

      cols <- list()
      for (i in seq_len(n)) {
        cols[[i]] <- list()
        cols[[i]]$id      <- ids[i]
        cols[[i]]$name    <- col.names[i]
        cols[[i]]$format  <- ""
        cols[[i]]$class   <- class(d[[i]])
        cols[[i]]$index   <- i
        cols[[i]]$fun     <- paste0("\"", ids[i], "\"")
        cols[[i]]$sample  <- na.omit(d[[i]])[1]
        cols[[i]]$summary <- summary(d[[i]])
      }

      Data(clear.data=TRUE)
      Data("comment", comment(d))
      Data("data.raw", d)
      Data("rows", rows)
      Data("cols", cols)
      Data("import", list(source=src))
    }
    EstablishDefaultVars()
    SetVars()
  }


  # get numeric columns
  GetNumericCols <- function(cols) {
    FUN <- function(i) any(c("numeric", "integer") %in% i$class)
    is.num <- vapply(cols, FUN, TRUE)
    return(which(is.num))
  }


  # establish defaults for x-, y-, and z-coordinate variables
  EstablishDefaultVars <- function() {
    vars <- list()
    idxs.n <- GetNumericCols(Data("cols"))
    for (i in seq_along(idxs.n)) {
      if (is.null(vars$x)) {
        vars$x <- idxs.n[i]
      } else if (is.null(vars$y)) {
        vars$y <- idxs.n[i]
      } else if (is.null(vars$z)) {
        vars$z <- idxs.n[i]
      }
    }
    Data("vars", vars)
  }


  # toggle widget state
  ToggleState <- function(vars) {

    # menus
    src <- Data(c("import", "source"))
    is.src.pkg <- !is.null(src) && "package" %in% names(src)
    tkentryconfigure(menu.help, 1, state=ifelse(is.src.pkg, "normal", "disabled"))

    # buttons
    is.xy <- !is.null(vars$x) && !is.null(vars$y)
    tkconfigure(f2.but.1.1, state=ifelse(is.xy, "normal", "disabled"))
    is.2d <- is.xy && !is.null(vars$z)
    tkconfigure(f2.but.1.2, state=ifelse(is.2d, "normal", "disabled"))
    is.3d <- is.2d && is.rgl
    tkconfigure(f2.but.1.3, state=ifelse(is.3d, "normal", "disabled"))
  }


  # set variables
  SetVars <- function() {
    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE))

    tkset(f1.box.1.2, "")
    tkset(f1.box.2.2, "")
    tkset(f1.box.3.2, "")

    cols <- Data("cols")
    vars <- Data("vars")

    if (is.null(cols) | is.null(vars)) {
      tkconfigure(f1.box.1.2, value="")
      tkconfigure(f1.box.2.2, value="")
      tkconfigure(f1.box.3.2, value="")
      ToggleState(vars)
      if (is.null(cols)) return()
    }

    ids <- vapply(cols, function(i) i$id, "")
    idxs.n <- GetNumericCols(cols)
    vals.n <- c("", ids[idxs.n])
    tkconfigure(f1.box.1.2, value=vals.n)
    tkconfigure(f1.box.2.2, value=vals.n)
    tkconfigure(f1.box.3.2, value=vals.n)

    if (!is.null(vars$x)) tcl(f1.box.1.2, "current", which(vars$x  == idxs.n))
    if (!is.null(vars$y)) tcl(f1.box.2.2, "current", which(vars$y  == idxs.n))
    if (!is.null(vars$z)) tcl(f1.box.3.2, "current", which(vars$z  == idxs.n))

    ToggleState(vars)
  }


  # refresh variables
  RefreshVars <- function(item) {
    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE))

    idxs.n <- GetNumericCols(Data("cols"))

    idx.x  <- as.integer(tcl(f1.box.1.2, "current"))
    idx.y  <- as.integer(tcl(f1.box.2.2, "current"))
    idx.z  <- as.integer(tcl(f1.box.3.2, "current"))
    vars <- list()
    if (idx.x > 0) vars$x[1] <- idxs.n[idx.x]
    if (idx.y > 0) vars$y[1] <- idxs.n[idx.y]
    if (idx.z > 0) vars$z[1] <- idxs.n[idx.z]
    if (!identical(vars, Data("vars"))) {
      Data("vars", vars)
      Data("data.pts", NULL)
      Data("data.grd", NULL)
    }
    ToggleState(vars)
  }


  # manage variables
  CallManageVariables <- function() {
    ans <- ManageVariables(Data("cols"), Data("vars"), Data("query"),
                           Data("changelog"), tt)
    if (!is.null(ans) && (!identical(ans$cols, Data("cols")) |
                          !identical(ans$vars, Data("vars")))) {
      Data("cols", ans$cols)
      Data("vars", ans$vars)
      Data("query", ans$query)
      Data("changelog", ans$changelog)
      Data("data.pts", NULL)
      Data("data.grd", NULL)
      SetVars()
    }
  }


  # close graphic devices
  CloseDevices <- function() {
    graphics.off()
    if (is.rgl) {
      while (rgl::rgl.cur() != 0)
        rgl::rgl.close()
    }
  }


  # save r graphic devices
  SaveRDevice <- function() {
    if (is.null(dev.list())) return()
    exts <- c("eps", "png", "jpg", "jpeg", "pdf", "bmp", "tif", "tiff")
    file <- GetFile(cmd="Save As", exts=exts, win.title="Save R Graphic As",
                    defaultextension="eps", parent=tt)
    if (is.null(file)) return()
    savePlot(filename=file, type=attr(file, "extension"))
  }


  # save rgl graphic devices
  SaveRGLDevice <- function() {
    if (!is.rgl || rgl::rgl.cur() == 0) return()
    file <- GetFile(cmd="Save As", exts=c("png", "eps", "pdf"),
                    win.title="Save RGL Graphic As", defaultextension="png",
                    parent=tt)
    if (is.null(file)) return()
    if (attr(file, "extension") == "png")
      rgl::rgl.snapshot(filename=file, fmt=attr(file, "extension"))
    else
      rgl::rgl.postscript(filename=file, fmt=attr(file, "extension"))
  }


  # session information
  SessionInfo <- function() {
    txt <- paste(c(capture.output(print(sessionInfo(), locale=TRUE)), ""), collapse="\n")
    EditText(txt, read.only=TRUE, win.title="Session Information",
             is.fixed.width.font=FALSE, parent=tt)
  }


  # about package
  AboutPackage <- function() {
    lib <- ifelse("package:RSurvey" %in% search(), system.file(package="RSurvey"), getwd())
    ver <- read.dcf(file.path(lib, "DESCRIPTION"), "Version")
    yr  <- sub("-.*", "", read.dcf(file.path(lib, "DESCRIPTION"), "Packaged"))
    msg <- sprintf("RSurvey package version %s (%s)", ver, yr)
    tkmessageBox(icon="info", message=msg, title="Information", type="ok", parent=tt)
  }


  # manage polygons
  CallManagePolygons <- function() {
    old.polys <- Data("polys")
    old.poly.data <- Data("poly.data")
    old.poly.crop <- Data("poly.crop")
    ans <- ManagePolygons(Data("polys"), Data("poly.data"), Data("poly.crop"), parent=tt)

    new.polys <- ans$polys
    new.poly.data <- ans$poly.data
    new.poly.crop <- ans$poly.crop
    if (is.null(new.polys) || identical(new.polys, old.polys)) return()

    old <- if (is.null(old.poly.data)) NULL else old.polys[[old.poly.data]]
    new <- if (is.null(new.poly.data)) NULL else new.polys[[new.poly.data]]
    if (!identical(new, old)) {
      Data("data.pts",  NULL)
      Data("data.grd",  NULL)
    }

    old <- if (is.null(old.poly.crop)) NULL else old.polys[[old.poly.crop]]
    new <- if (is.null(new.poly.crop)) NULL else new.polys[[new.poly.crop]]
    if (!identical(new, old)) Data("data.grd",  NULL)

    Data("polys", new.polys)
    Data("poly.data", new.poly.data)
    Data("poly.crop", new.poly.crop)
  }


  # set polygon range and limit
  CallSetPolygonLimits <- function() {
    polys <- Data("polys")
    old.poly.data <- Data("poly.data")
    old.poly.crop <- Data("poly.crop")
    ans <- SetPolygonLimits(names(polys), old.poly.data, old.poly.crop, tt)
    if (is.null(ans)) return()
    new.poly.data <- ans$poly.data
    new.poly.crop <- ans$poly.crop
    if (!identical(new.poly.data, old.poly.data)) {
      Data("data.pts", NULL)
      Data("data.grd", NULL)
    }
    if (!identical(new.poly.crop, old.poly.crop)) Data("data.grd", NULL)
    Data("poly.data", new.poly.data)
    Data("poly.crop", new.poly.crop)
  }


  # construct polygon
  ConstructPolygon <- function(type) {
    if (is.null(Data("data.raw"))) return()
    msg <- paste("After the graph has been created, use the mouse to identify",
                 "the vertices of the polygon. The identification process can",
                 "be terminated by clicking the second button and selecting",
                 "[Stop] from the menu, or from the [Stop] menu on the",
                 "graphics window.", sep="\n")
    if (shown.construct.polygon.msgbox)
      tkmessageBox(icon="info", message=msg, title="Build Polygon", type="ok", parent=tt)
    shown.construct.polygon.msgbox <<- FALSE
    CallPlot2d(type=type, build.poly=TRUE)
  }


  # autocrop polygon
  CallAutocropRegion <- function() {
    if (is.null(Data("data.raw"))) return()
    CallProcessData()
    d       <- Data("data.pts")
    xlab    <- Data("cols")[[Data("vars")$x]]$id
    ylab    <- Data("cols")[[Data("vars")$y]]$id
    zlab    <- Data("cols")[[Data("vars")$z]]$id
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
      if (!is.null(Data("polys"))) ply <- Data("polys")
      ply.name <- NamePolygon(old=names(ply))
      ply[[ply.name]] <- ply.new
      Data("polys", ply)
      Data("poly.crop", ply.name)
      Data("data.grd", NULL)
    }
  }


  # name polygon
  NamePolygon <- function(old=NULL, nam=NA){
    if (is.na(nam)) nam <- "New Polygon"
    idx <- 1
    chk <- nam
    while (chk %in% old) {
      chk <- paste0(nam, " (", idx, ")")
      idx <- idx + 1
    }
    chk
  }


  # plot point or 2d surface data
  CallPlot2d <- function(type, build.poly=FALSE) {
    if (type == "p")
      CallProcessData()
    else
      CallProcessData(interpolate=TRUE)
    tkconfigure(tt, cursor="watch")
    on.exit(tkconfigure(tt, cursor="arrow"))

    if (is.null(Data("data.grd")) && type %in% c("g", "l")) {
      return()
    } else if (is.null(Data("data.pts"))) {
      return()
    }

    ply <- if (type == "p") Data("poly.data") else Data("poly.crop")

    if (!is.null(ply) && !is.na(ply)) ply <- Data("polys")[[ply]]

    show.poly   <- Data("show.poly") && inherits(ply, "gpc.poly")
    show.lines  <- type %in% c("l", "g") && Data("show.lines")
    show.points <- type %in% c("l", "g") && Data("show.points")

    axis.side <- 1:2
    if (Data("show.2.axes")) axis.side <- 1:4

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

    # adjust axes limits for polygon
    lim <- Data("lim.axes")

    xlim <- lim$x
    if (is.null(xlim)) xlim <- c(NA, NA)
    ylim <- lim$y
    if (is.null(ylim)) ylim <- c(NA, NA)

    if (show.poly) {
      bbx <- bby <- NULL
      bb <- rgeos::get.bbox(ply)

      if (!is.na(xlim[1])) bb$x[1] <- xlim[1]
      if (!is.na(xlim[2])) bb$x[2] <- xlim[2]
      if (!is.na(ylim[1])) bb$y[1] <- ylim[1]
      if (!is.na(ylim[2])) bb$y[2] <- ylim[2]

      xy <- cbind(x=c(bb$x, rev(bb$x)), y=c(bb$y[c(1,1)], bb$y[c(2,2)]))
      bb <- rgeos::get.bbox(intersect(ply, as(xy, "gpc.poly")))
      bbx <- range(bb$x)
      bby <- range(bb$y)
      bbx <- extendrange(bbx, f=0.02)
      bby <- extendrange(bby, f=0.02)
      if (is.na(xlim[1]) && bbx[1] < xran[1]) lim$x[1] <- bbx[1]
      if (is.na(xlim[2]) && bbx[2] > xran[2]) lim$x[2] <- bbx[2]
      if (is.na(ylim[1]) && bby[1] < yran[1]) lim$y[1] <- bby[1]
      if (is.na(ylim[2]) && bby[2] > yran[2]) lim$y[2] <- bby[2]
    }

    ans <- try(Plot2d(dat, type=type, xlim=lim$x, ylim=lim$y, zlim=lim$z,
                      xlab=xlab, ylab=ylab, zlab=zlab, asp=Data("asp.yx"),
                      csi=Data("csi"), width=Data("width"), nlevels=nlevels,
                      cex.pts=Data("cex.pts"), rkey=Data("rkey"),
                      color.palette=Data("color.palette"),
                      axis.side=axis.side, minor.ticks=Data("minor.ticks"),
                      ticks.inside=Data("ticks.inside"),
                      rm.pnt.line=Data("rm.pnt.line"),
                      add.contour.lines=show.lines))
    if (inherits(ans, "try-error")) return()

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
      ply.new <- suppressWarnings(as(v, "gpc.poly"))
      if (!is.null(ply)) ply.new <- intersect(ply, ply.new)
      if (rgeos::area.poly(ply.new) == 0) {
        tkmessageBox(icon="warning", title="Polygon Discarded", parent=tt,
                     message="The resulting polygon is invalid.")
        ply.new <- NULL
      }

      if (inherits(ply.new, "gpc.poly")) {
        ply.list <- if (is.null(Data("polys"))) list() else Data("polys")
        ply.name <- NamePolygon(old=names(ply.list))
        ply.list[[ply.name]] <- ply.new

        if (type == "p") {
          pts <- rgeos::get.pts(ply.new)
          logic <- rep(TRUE, nrow(dat))
          for (i in seq_along(pts)) {
              is.in <-  point.in.polygon(point.x=dat$x, point.y=dat$y,
                                         pol.x=pts[[i]]$x, pol.y=pts[[i]]$y) > 0
              is.in <- if (pts[[i]]$hole) !is.in else is.in
              logic <- logic & is.in
          }
          if (any(logic)) {
            points(dat$x[logic], dat$y[logic], col="red",
                   cex=Data("cex.pts"), pch=20)
            Data("polys", ply.list)
            Data("poly.data", ply.name)
            Data("data.pts", NULL)
            Data("data.grd", NULL)
          } else {
            tkmessageBox(icon="warning", title="Polygon Discarded", parent=tt,
                         message="No data points fall within the given polygon.")
          }
        } else if (type == "l") {
          cutout <- CutoutPolygon(dat, ply.new)
          if (!is.null(cutout)) {
            Data("polys", ply.list)
            Data("poly.crop", ply.name)
            Data("data.grd", NULL)
          }
        }
      }
    }
    tkfocus(tt)
  }


  # plot 3d surface data
  CallPlot3d <- function() {
    CallProcessData(interpolate=TRUE)
    tkconfigure(tt, cursor="watch")
    on.exit(tkconfigure(tt, cursor="arrow"))
    if (is.null(Data("data.grd"))) return()
    dat <- Data("data.grd")
    pts <- NULL
    if (Data("show.points")) pts <- Data("data.pts")
    lim <- Data("lim.axes")
    try(Plot3d(x=dat, px=pts, xlim=lim$x, ylim=lim$y, zlim=lim$z,
               vasp=Data("asp.zx"), hasp=Data("asp.yx"),
               width=Data("width"), cex.pts=Data("cex.pts"),
               nlevels=Data("nlevels"), color.palette=Data("color.palette")))
    tkfocus(tt)
  }

  # Set the height of (default-sized) characters in inches.
  SetCsi <- function() {
    if (is.null(Data("csi"))) {
      dev.new(pointsize=12)
      Data("csi", par("csi"))
      dev.off()
    }
  }

  # Call edit data
  CallEditData <- function(read.only=TRUE, is.raw=TRUE, is.state=FALSE) {
    tkconfigure(tt, cursor="watch")
    on.exit(tkconfigure(tt, cursor="arrow"))
    if (read.only) {  # view processed data
      CallProcessData()
      if (is.null(Data("data.pts"))) return()



      cols <- Data("cols")
      vars <- Data("vars")



      col.names <- names(Data("data.pts"))
      col.formats <- vapply(col.names, function(i) cols[[vars[[i]]]]$format, "")



      EditData(Data("data.pts"), col.names=col.names, col.formats=col.formats,
               read.only=TRUE, win.title="View Processed Data", parent=tt)



    } else {  # edit raw data
      if (is.null(Data("data.raw"))) return()
      rows <- Data("rows")
      cols <- Data("cols")
      idxs <- na.omit(vapply(cols, function(i) i$index, 0L))
      col.names <- vapply(cols, function(i) i$id, "")[idxs]
      col.formats <- vapply(cols, function(i) i$format, "")[idxs]
      old.changelog <- Data("changelog")



      ans <- EditData(Data("data.raw")[idxs], col.names=col.names,
                      row.names=rows, col.formats=col.formats,
                      read.only=FALSE, changelog=old.changelog,
                      win.title="Edit Raw Data", parent=tt)
      if (is.null(ans)) return()

      tclServiceMode(FALSE)
      on.exit(tclServiceMode(TRUE), add=TRUE)

      Data("data.raw",  ans$d)
      Data("changelog", ans$changelog)

      if (is.null(old.changelog))
        new.changelog.rows <- seq_len(nrow(ans$changelog))
      else
        new.changelog.rows <- (nrow(old.changelog) + 1L):nrow(ans$changelog)
      changed.cols <- unique(ans$changelog[new.changelog.rows, "variable"])
      changed.idxs <- which(col.names %in% changed.cols)
      for (i in changed.idxs) {
        obj <- EvalFunction(cols[[i]]$fun, cols)
        cols[[i]]$summary <- summary(obj)
        cols[[i]]$sample <- na.omit(obj)[1]
      }
      Data("cols", cols)
      Data("data.pts", NULL)
      Data("data.grd", NULL)
    }
  }


  # call process data
  CallProcessData <- function(interpolate=FALSE) {
    tkconfigure(tt, cursor="watch")
    tclServiceMode(FALSE)
    on.exit(tkconfigure(tt, cursor="arrow"))
    on.exit(tclServiceMode(TRUE), add=TRUE)

    vars <- Data("vars")
    var.names <- names(vars)
    if (!all(c("x", "y") %in% var.names)) {
      Data("data.pts", NULL)
      Data("data.grd", NULL)
      return()
    }

    # process points
    if (is.null(Data("data.pts"))) {
      cols <- Data("cols")

      FUN <- function(v) {
        if (is.null(v)) NULL else EvalFunction(cols[[v]]$fun, cols)
      }
      d <- as.data.frame(lapply(var.names, function(i) FUN(vars[[i]])),
                         stringsAsFactors=FALSE)

      if (!is.null(Data("data.raw"))) {
        rows <- Data("rows")
        rows.int <- as.integer(rows)
        is.int <- is.integer(rows.int) && !anyDuplicated(rows.int)
        rownames(d) <- if (is.int) rows.int else rows
      }
      names(d) <- var.names

      query <- Data("query")
      if (is.null(query))
        coerce.rows <- NULL
      else
        coerce.rows <- EvalFunction(query, cols)

      if (!is.null(vars$x)) {
        ply <- Data("poly.data")
        if (!is.null(ply)) ply <- Data(c("polys", ply))
      } else {
        ply <- NULL
      }

      data.pts <- try(ProcessData(d, type="p", coerce.rows=coerce.rows, ply=ply))
      if (!inherits(data.pts, "try-error")) {
        Data("data.pts", data.pts)
        Data("data.grd", NULL)
      }
    }

    # process grid
    if (!is.null(Data("data.pts")) && is.null(Data("data.grd")) && interpolate) {
      ply <- Data("poly.crop")
      if (!is.null(ply)) ply <- Data("polys")[[ply]]
      grid.res <- Data("grid.res")
      grid.mba <- Data("grid.mba")
      data.grd <- try(ProcessData(Data("data.pts"), type="g", ply=ply,
                                  grid.res=grid.res, grid.mba=grid.mba))
      if (!inherits(data.grd, "try-error"))
        Data("data.grd", data.grd)
    }
  }


  # build query
  BuildQuery <- function() {
    if (is.null(Data("data.raw"))) return()
    m <- length(Data("data.raw")[[1]])
    if (m == 0) return()
    cols <- Data("cols")
    old.fun <- Data("query")
    file <- EditFunction(cols, fun=old.fun, value.length=m, value.class="logical",
                         win.title="Filter Data Records", parent=tt)
    if (is.null(file)) return()
    if (file$fun == "") Data("query", NULL) else Data("query", file$fun)
    Data("data.pts", NULL)
    Data("data.grd", NULL)
  }


  # clear query
  ClearQuery <- function() {
    if (!is.null(Data("query"))) {
      Data("query", NULL)
      Data("data.pts", NULL)
      Data("data.grd", NULL)
    }
  }


  # edit comment
  EditComment <- function() {
    txt <- EditText(Data("comment"), win.title="Comment", parent=tt)
    if (is.null(txt))
      return()
    if (length(txt) == 0 || (length(txt) == 1 & txt == ""))
      txt <- NULL
    Data("comment", txt)
  }


  # warn if using windows os and running in mdi mode
  if (.Platform$OS.type == "windows" && utils::getIdentification() == "RGui")
    message("\n\n    You are running R in MDI mode which *may* interfere\n",
            "    with the functionality of the graphical user interface.\n",
            "    It is recommended to use R in SDI mode which can be\n",
            "    set in the command line or by clicking in the Menu:\n",
            "    Edit - GUI Preferences: SDI, then Save and restart R.\n\n")

  # establish default directory
  if (is.null(Data("default.dir"))) Data("default.dir", getwd())

  # check if suggested packages are loaded
  is.xml        <- requireNamespace("XML",        quietly=TRUE)
  is.rgl        <- requireNamespace("rgl",        quietly=TRUE)
  is.rgdal      <- requireNamespace("rgdal",      quietly=TRUE)
  is.tripack    <- requireNamespace("tripack",    quietly=TRUE)
  is.colorspace <- requireNamespace("colorspace", quietly=TRUE)

  # set options
  SetCsi()
  options(help_type="html")
  shown.construct.polygon.msgbox <- TRUE

  # assign variables linked to Tk entry widgets
  import.var  <- tclVar()
  save.var    <- tclVar()
  manage.var  <- tclVar()
  polygon.var <- tclVar()
  config.var  <- tclVar()
  axes.var    <- tclVar()
  layout.var  <- tclVar(1)
  space2d.var <- tclVar(1)
  space3d.var <- tclVar(0)
  close.var   <- tclVar()
  plt.typ.var <- tclVar("Points")
  tt.done.var <- tclVar(0)

  # open GUI
  tclServiceMode(FALSE)
  tt <- tktoplevel()
  tkwm.geometry(tt, Data("win.loc"))
  tktitle(tt) <- "RSurvey"
  tkwm.resizable(tt, 1, 0)

  # top menu
  top.menu <- tkmenu(tt, tearoff=0)

  # file menu
  menu.file <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="File", menu=menu.file, underline=0)

  tkadd(menu.file, "command", label="New project", accelerator="Ctrl+n",
        command=ClearObjs)
  tkadd(menu.file, "command", label="Open project\u2026", accelerator="Ctrl+o",
        command=OpenProj)
  tkadd(menu.file, "command", label="Save project", accelerator="Ctrl+s",
        command=SaveProj)
  tkadd(menu.file, "command", label="Save project as\u2026",
        accelerator="Shift+Ctrl+s", command=SaveProjAs)

  tkadd(menu.file, "separator")
  menu.file.import <- tkmenu(tt, tearoff=0)
  tkadd(menu.file.import, "command", label="Text file or clipboard\u2026",
        command=function() ReadData("txt"))
  tkadd(menu.file.import, "command", label="XML spreadsheet file\u2026",
        state=ifelse(is.xml, "normal", "disabled"),
        command=function() ReadData("xlsx"))
  tkadd(menu.file.import, "command", label="R package\u2026",
        command=function() ReadData("rpackage"))
  tkadd(menu.file.import, "command", label="R data file\u2026",
        command=function() ReadData("rda"))
  tkadd(menu.file, "cascade", label="Import raw data from",
        menu=menu.file.import)
  menu.file.export <- tkmenu(tt, tearoff=0)
  tkadd(menu.file.export, "command", label="Text file\u2026",
        command=function() WriteData("txt"))
  tkadd(menu.file.export, "command", label="Shapefile\u2026",
        state=if (is.rgdal) "normal" else "disabled",
        command=function() WriteData("shp"))
  tkadd(menu.file.export, "command", label="R data file\u2026",
        command=function() WriteData("rda"))
  tkadd(menu.file, "cascade", label="Export point data as",
        menu=menu.file.export)
  tkadd(menu.file, "command", label="Export grid data as\u2026",
        command=function() WriteData("grd"))

  tkadd(menu.file, "separator")
  menu.file.save <- tkmenu(tt, tearoff=0)
  tkadd(menu.file.save, "command", label="R graphic\u2026", accelerator="Ctrl+r",
        command=SaveRDevice)
  tkadd(menu.file.save, "command", label="RGL graphic\u2026",
        command=SaveRGLDevice)
  tkadd(menu.file, "cascade", label="Save plot from", menu=menu.file.save)

  tkadd(menu.file, "separator")
  tkadd(menu.file, "command", label="Exit", command=CloseGUI)

  # edit menu
  menu.edit <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Edit", menu=menu.edit, underline=0)

  tkadd(menu.edit, "command", label="Manage variables\u2026",
        command=CallManageVariables)
  tkadd(menu.edit, "command", label="Raw data editor\u2026",
        command=function() CallEditData(read.only=FALSE))
  tkadd(menu.edit, "command", label="Comment\u2026", command=EditComment)

  tkadd(menu.edit, "separator")
  tkadd(menu.edit, "command", label="Filter data records\u2026",
        command=BuildQuery)
  tkadd(menu.edit, "command", label="Clear filter", command=ClearQuery)

  tkadd(menu.edit, "separator")
  tkadd(menu.edit, "command", label="Set sort order\u2026",
        command=function() {
          col.ids <- vapply(Data("cols"), function(i) i$id, "")
          sort.on.old <- Data(c("vars", "sort.on"))
          sort.on.new <- SetSortOrder(col.ids, sort.on.old, parent=tt)
          if (!identical(sort.on.old, sort.on.new)) {
            Data(c("vars", "sort.on"), sort.on.new)
            Data("data.pts", NULL)
            Data("data.grd", NULL)
          }
        })
  tkadd(menu.edit, "command", label="Clear sort order",
        command=function() Data(c("vars", "sort.on"), NULL))

  tkadd(menu.edit, "separator")
  tkadd(menu.edit, "command", label="Set interpolation method",
        command=function() SetInterpolation(tt))

  # view menu
  menu.view <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="View", menu=menu.view, underline=0)
  menu.view.raw <- tkmenu(tt, tearoff=0)
  tkadd(menu.view.raw, "command", label="All variables",
        command=function() print("notyet"), state="disabled")
  tkadd(menu.view.raw, "command", label="State variables",
        command=function() print("notyet"), state="disabled")
  tkadd(menu.view, "cascade", label="Raw data for", menu=menu.view.raw)
  menu.view.pr <- tkmenu(tt, tearoff=0)
  tkadd(menu.view.pr, "command", label="All variables",
        command=function() print("notyet"), state="disabled")
  tkadd(menu.view.pr, "command", label="State variables",
        command=function() CallEditData())
  tkadd(menu.view, "cascade", label="Processed data for", menu=menu.view.pr)

  # polygon menu
  menu.poly <- tkmenu(tt, tearoff=0)

  tkadd(top.menu, "cascade", label="Polygon", menu=menu.poly, underline=0)

  tkadd(menu.poly, "command", label="Manage polygons\u2026",
        command=CallManagePolygons)
  tkadd(menu.poly, "separator")
  tkadd(menu.poly, "command", label="Set polygon limits\u2026",
        command=CallSetPolygonLimits)
  tkadd(menu.poly, "command", label="Clear polygon limits",
        command=function() {
          Data("poly.data", NULL)
          Data("poly.crop", NULL)
          Data("data.pts",  NULL)
          Data("data.grd",  NULL)
        })
  tkadd(menu.poly, "separator")

  menu.poly.con <- tkmenu(tt, tearoff=0)
  tkadd(menu.poly.con, "command", label="Boundary defining data limits",
        command=function() ConstructPolygon(type="p"))
  tkadd(menu.poly.con, "command", label="Crop region for interpolated surface",
        command=function() ConstructPolygon(type="l"))
  tkadd(menu.poly, "cascade", label="Build", menu=menu.poly.con)
  tkadd(menu.poly, "command", label="Autocrop region\u2026",
        state=if (is.tripack) "normal" else "disabled",
        command=CallAutocropRegion)

  # plot menu
  menu.graph <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Plot", menu=menu.graph, underline=0)
  tkadd(menu.graph, "command", label="Scatter",
        command=function() CallPlot2d(type="p"))
  tkadd(menu.graph, "command", label="2D-interpolated map",
        command=function() {
          type <- if (Data("img.contour")) "g" else "l"
          CallPlot2d(type=type)
        })
  tkadd(menu.graph, "command", label="3D-interpolated map",
        state=ifelse(is.rgl, "normal", "disabled"),
        command=CallPlot3d)
  tkadd(menu.graph, "separator")
  tkadd(menu.graph, "command", label="Set axes limits\u2026",
        command=function() {
          lim <- SetAxesLimits(Data("lim.axes"), tt)
          Data("lim.axes", lim)
        })
  tkadd(menu.graph, "command", label="Clear axes limits",
        command=function() Data("lim.axes", NULL))
  tkadd(menu.graph, "separator")
  tkadd(menu.graph, "command", label="Configuration",
        command=function() SetConfiguration(tt))
  tkadd(menu.graph, "command", label="Choose color palette\u2026",
        state=if (is.colorspace) "normal" else "disabled",
        command=function() {
          pal <- colorspace::choose_palette(pal=Data("color.palette"),
                                            n=Data("nlevels"), parent=tt)
          if (!is.null(pal)) Data("color.palette", pal)
        })
  tkadd(menu.graph, "separator")
  tkadd(menu.graph, "command", label="Close all graphic devices",
        accelerator="Ctrl+F4", command=CloseDevices)

  # help menu
  menu.help <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Help", menu=menu.help, underline=0)
  tkadd(menu.help, "command", label="Documentation",
        command=function() help(package="RSurvey"))
  tkadd(menu.help, "command", label="Dataset",
        command=function() {
          src <- Data(c("import", "source"))
          print(help(src["dataset"], package=src["package"]))
        })

  tkadd(menu.help, "separator")
  menu.help.rep <- tkmenu(tt, tearoff=0)
  tkadd(menu.help.rep, "command", label="CRAN",
        command=function() browseURL("https://CRAN.R-project.org/package=RSurvey"))
  tkadd(menu.help.rep, "command", label="GitHub",
        command=function() browseURL("https://github.com/jfisher-usgs/RSurvey"))
  tkadd(menu.help, "cascade", label="Repository on  ", menu=menu.help.rep)

  tkadd(menu.help, "separator")
  tkadd(menu.help, "command", label="Session information", command=SessionInfo)
  tkadd(menu.help, "command", label="About", command=AboutPackage)

  if (!("RSurvey" %in% .packages())) {
      tkadd(menu.help, "separator")
      tkadd(menu.help, "command", label="Restore R session",
            command=function() {
              CloseGUI()
              Data("data.pts", NULL)
              Data("data.grd", NULL)
              RestoreSession(file.path(getwd(), "R"), save.objs="Data",
                             fun.call="StartGui")
            })
  }

  # finalize top menu
  tkconfigure(tt, menu=top.menu)

  # frame 0, toolbar with command buttons
  f0 <- ttkframe(tt, relief="flat", borderwidth=2)
  tkpack(f0, side="top", fill="x")

  if ("package:RSurvey" %in% search())
    img.path <- system.file("images", package="RSurvey")
  else
    img.path <- file.path(getwd(), "inst", "images")

  tkimage.create("photo", save.var, format="GIF",
                 file=file.path(img.path, "save.gif"))
  tkimage.create("photo", import.var, format="GIF",
                 file=file.path(img.path, "import.gif"))
  tkimage.create("photo", manage.var, format="GIF",
                 file=file.path(img.path, "manage.gif"))
  tkimage.create("photo", polygon.var, format="GIF",
                 file=file.path(img.path, "polygon.gif"))
  tkimage.create("photo", axes.var, format="GIF",
                 file=file.path(img.path, "axes.gif"))
  tkimage.create("photo", config.var, format="GIF",
                 file=file.path(img.path, "config.gif"))
  tkimage.create("photo", close.var, format="GIF",
                 file=file.path(img.path, "close.gif"))

  f0.but.1  <- tkbutton(f0, relief="flat", overrelief="raised", borderwidth=1,
                        image=save.var, command=SaveProj)
  f0.but.2  <- tkbutton(f0, relief="flat", overrelief="raised", borderwidth=1,
                        image=import.var, command=function() ReadData("txt"))
  f0.but.3  <- tkbutton(f0, relief="flat", overrelief="raised", borderwidth=1,
                        image=manage.var, command=CallManageVariables)
  f0.but.4  <- tkbutton(f0, relief="flat", overrelief="raised", borderwidth=1,
                        image=polygon.var, command=CallManagePolygons)
  f0.but.5  <- tkbutton(f0, relief="flat", overrelief="raised", borderwidth=1,
                        image=axes.var, command=function() {
                          Data("lim.axes", SetAxesLimits(Data("lim.axes"), tt))
                        })
  f0.but.6  <- tkbutton(f0, relief="flat", overrelief="raised", borderwidth=1,
                        image=config.var, command=function() SetConfiguration(tt))
  f0.but.7  <- tkbutton(f0, relief="flat", overrelief="raised", borderwidth=1,
                        image=close.var, command=CloseDevices)

  tkgrid(f0.but.1, f0.but.2, f0.but.3, f0.but.4, f0.but.5, f0.but.6, f0.but.7,
         sticky="w", padx=1)
  tkgrid.configure(f0.but.1, padx=c(5, 0))

  separator <- ttkseparator(tt, orient="horizontal")
  tkpack(separator, fill="x")

  # frame 1, variables
  f1 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                      text="Set state variables")

  f1.lab.1.1 <- ttklabel(f1, text="x")
  f1.lab.2.1 <- ttklabel(f1, text="y")
  f1.lab.3.1 <- ttklabel(f1, text="z")

  f1.box.1.2 <- ttkcombobox(f1, state="readonly")
  f1.box.2.2 <- ttkcombobox(f1, state="readonly")
  f1.box.3.2 <- ttkcombobox(f1, state="readonly")

  tkgrid(f1.lab.1.1, f1.box.1.2)
  tkgrid(f1.lab.2.1, f1.box.2.2, pady=4)
  tkgrid(f1.lab.3.1, f1.box.3.2)

  tkgrid.configure(f1.lab.1.1, f1.lab.2.1, f1.lab.3.1, sticky="w", padx=c(0, 2))
  tkgrid.configure(f1.box.1.2, f1.box.2.2, f1.box.3.2, sticky="we")

  tkgrid.columnconfigure(f1, 1, weight=1, minsize=25)

  tkpack(f1, fill="x", expand=TRUE, padx=10, pady=5)

  # frame 2, plotting buttons
  f2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                      text="Plot state variables")

  f2.but.1.1 <- ttkbutton(f2, width=10, text="Scatter",
                          command=function() CallPlot2d(type="p"))
  f2.but.1.2 <- ttkbutton(f2, width=10, text="2D Map",
                          command=function() {
                            CallPlot2d(type=if (Data("img.contour")) "g" else "l")
                          })
  f2.but.1.3 <- ttkbutton(f2, width=10, text="3D Map",
                          command=CallPlot3d)

  tkgrid(f2.but.1.1, f2.but.1.2, f2.but.1.3)
  tkgrid.configure(f2.but.1.2, padx=4)

  tkpack(f2, fill="x", expand=TRUE, padx=10, pady=c(0, 10))

  # set variables
  SetVars()

  # bind events
  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>",         CloseGUI)
  tkbind(tt, "<Control-n>",       ClearObjs)
  tkbind(tt, "<Control-o>",       OpenProj)
  tkbind(tt, "<Control-s>",       SaveProj)
  tkbind(tt, "<Shift-Control-S>", SaveProjAs)
  tkbind(tt, "<Control-r>",       SaveRDevice)
  tkbind(tt, "<Control-F4>",      CloseDevices)

  tkbind(f1.box.1.2, "<<ComboboxSelected>>", RefreshVars)
  tkbind(f1.box.2.2, "<<ComboboxSelected>>", RefreshVars)
  tkbind(f1.box.3.2, "<<ComboboxSelected>>", RefreshVars)

  # gui closure
  tkfocus(force=tt)
  invisible()
}
