#' GUI: Main Graphical User Interface
#'
#' Launches the main graphical user interface (\acronym{GUI}) for the \pkg{RSurvey} package.
#' May be used to specify coordinate variables, render plots, and access all other package functionality.
#'
#' @return Quaries and sets the \code{vars} list component of \code{\link{Data}}.
#'   The components of \code{vars} include:
#'     \item{x, y, z}{index number for the corresponding coordinate-dimension variable in \code{cols},
#'       see \code{\link{ManageVariables}} function for details.}
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords misc
#'
#' @import tcltk
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   LaunchGui()
#' }
#'

LaunchGui <- function() {


  # close gui
  CloseGUI <- function() {
    if (as.integer(tclvalue(tt.done.var)) > 0) return()
    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE))
    geo <- unlist(strsplit(as.character(tkwm.geometry(tt)), "\\+"))
    tkdestroy(tt)
    Data("win.loc", sprintf("+%s+%s", geo[2], geo[3]))
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
      msg <- "Not a valid project file."
      tkmessageBox(icon="error", message=msg, detail=ans, title="Error", type="ok", parent=tt)
      return()
    }
    Data(replace.all=get(ans, envir=environment(OpenProj)))
    Data("proj.file", file)
    SetVars()
  }


  # save binary project file
  SaveProj <- function() {
    if (!is.null(Data("proj.file")) && file.access(Data("proj.file"), mode=0) != 0) {
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
      file <- Data("proj.file")
      obj.name <- sub("[.][^.]*$", "", basename(file))
      assign(obj.name, Data(), envir=environment(SaveProj))
      save(list=obj.name, file=file, compress=TRUE)
    }
  }


  # save a new binary project file
  SaveProjAs <- function() {
    Data("proj.file", NULL)
    SaveProj()
  }


  # clear objects
  ClearObjs <- function() {
    if (!is.null(Data("proj.file"))) {
      msg <- "Save the existing project?"
      ans <- tkmessageBox(icon="question", message=msg, title="Warning", type="yesnocancel", parent=tt)
      ans <- as.character(ans)
    } else {
      ans <- "no"
    }
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
    if (!is.coordinate & file.type %in% c("shp", "rda")) return()
    ProcessData()
    ExportData(file.type=file.type, parent=tt)
  }


  # write raster
  WriteRaster <- function(file.type) {
    if (is.null(Data("cols"))) return()
    is.coordinate <- !is.null(Data("vars")$x) & !is.null(Data("vars")$y)
    if (!is.coordinate & file.type %in% c("tif", "rda")) return()
    ProcessData()
    r <- Data("data.grd")
    if (!inherits(r, "RasterLayer")) return()
    if (file.type == "txt") file.type <- c("tsv", "tab", "csv", "txt")
    file <- GetFile(cmd="Save As", exts=file.type, file=NULL,
                    win.title="Save Grid Data As", defaultextension="tif")
    if (is.null(file)) return()
    ext <- attr(file, "extension")
    if (ext == "tif") {
      raster::writeRaster(r, filename=file, format="GTiff", overwrite=TRUE, NAflag=-999)
    } else if (ext == "rda") {
      save(r, file=file)
    } else {
      if (ext == "csv") {
        sep <- ","
      } else if (ext %in% c("tsv", "tab")) {
         sep <- "\t"
      } else {
        sep <- " "
      }
      utils::write.table(raster::as.matrix(r), file=file, quote=FALSE, sep=sep,
                         row.names=FALSE, col.names=FALSE)
    }
  }


  # read data
  ReadData <- function(type) {
    if (type == "txt") {
      ImportText(tt)
    } else {

      valid.classes <- c("matrix", "data.frame", "tbl_df", "data.table", "SpatialPointsDataFrame")

      if (type == "xlsx") {
        ans <- ImportSpreadsheet(parent=tt)
        d <- ans$d
        src <- ans$src

      } else if (type == "shp") {
        file <- GetFile(cmd="Open", exts=c("shp", "shx", "dbf"),
                        win.title="Open Point Shapefile", parent=tt)
        if (is.null(file)) return()
        src <- c(pathname=file[1], accessed=format(Sys.time()))
        d <- rgdal::readOGR(dsn=attr(file, "directory"), layer=attr(file, "name"),
                            verbose=FALSE, stringsAsFactors=FALSE)

      } else if (type == "rda") {
        file <- GetFile(cmd="Open", exts="rda", win.title="Open R-Data File", parent=tt)
        if (is.null(file)) return()
        d <- local({d.name <- load(file=file); return(eval(parse(text=d.name[1])))})
        if (!inherits(d, valid.classes)) {
          msg <- "R dataset is not a valid object class."
          tkmessageBox(icon="error", message=msg, title="Error", type="ok", parent=tt)
          return()
        }
        src <- c(pathname=file[1], accessed=format(Sys.time()))

      } else if (type == "rpackage") {
        d <- ImportDataset(valid.classes, parent=tt)
        src <- d$src
        d <- d$d
      }

      if (inherits(d, "SpatialPointsDataFrame")) {
        crs.old <- Data("crs")
        crs.new <- d@proj4string
        if (is.na(rgdal::CRSargs(crs.new)) & !is.na(rgdal::CRSargs(crs.old))) {
          msg <- "Dataset has no CRS so the global and project-wide CRS will be used."
          ans <- tkmessageBox(icon="warning", message=msg, title="Warning", type="okcancel", parent=tt)
          if (as.character(ans) == "cancel") return()
          sp::proj4string(d) <- crs.old
          crs.new <- crs.old
        } else if (!is.na(rgdal::CRSargs(crs.new)) & !is.na(rgdal::CRSargs(crs.old))) {
          msg <- paste("The CRS for this dataset is different from the gloabl and project-wide CRS being used.",
                       "A transformation between datum(s) and conversion between projections will be made.")
          ans <- tkmessageBox(icon="warning", message=msg, title="Warning", type="okcancel", parent=tt)
          if (as.character(ans) == "cancel") return()
          d <- sp::spTransform(d, crs.old)
          crs.new <- crs.old
        }
        d <- cbind(sp::coordinates(d), d@data)
      } else {
        crs.new <- sp::CRS(as.character(NA))
      }

      if (is.null(d) || nrow(d) == 0 || ncol(d) == 0) return()
      if (!is.null(Data("cols"))) {
        msg <- "This action will delete existing data."
        ans <- tkmessageBox(icon="warning", message=msg, title="Warning", type="okcancel", parent=tt)
        if (as.character(ans) == "cancel") return()
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
        ids[matched[[i]]] <- sprintf("%s (%s)", names(matched[i]), seq_along(matched[[i]]))
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
        cols[[i]]$sample  <- stats::na.omit(d[[i]])[1]
        cols[[i]]$summary <- summary(d[[i]])
      }

      Data(clear.data=TRUE)
      Data("comment", comment(d))
      Data("data.raw", d)
      Data("rows", rows)
      Data("cols", cols)
      Data("import", list(source=src))
      Data("crs", crs.new)
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


  # establish defaults for x- and y-coordinate variables
  EstablishDefaultVars <- function() {
    vars <- list()
    idxs.n <- GetNumericCols(Data("cols"))
    for (i in seq_along(idxs.n)) {
      if (is.null(vars$x)) {
        vars$x <- idxs.n[i]
      } else if (is.null(vars$y)) {
        vars$y <- idxs.n[i]
      }
    }
    Data("vars", vars)
  }


  # set widget state
  SetState <- function() {
    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE))

    # button
    idx.x  <- as.integer(tcl(f1.box.1.2, "current"))
    idx.y  <- as.integer(tcl(f1.box.2.2, "current"))
    idx.z  <- as.integer(tcl(f1.box.3.2, "current"))

    is.xy  <- idx.x > 0 & idx.y > 0
    is.xyz <- is.xy & idx.z > 0
    is.pnt <- as.integer(tcl(f2.box.1.2, "current")) == 0
    is.r   <- as.character(tclvalue(device.var)) == "R"

    s <- ifelse(is.xyz | (is.xy & is.pnt & is.r), "normal", "disabled")
    tkconfigure(f2.but.1.1, state=s)

    # radiobutton
    tkconfigure(f3.rad.1.3, state=ifelse(is.pkg.rgl, "normal", "disabled"))
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
      SetState()
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

    SetState()
  }


  # refresh variables
  RefreshVars <- function(item) {
    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE))
    vars.old <- Data("vars")
    idxs.n <- GetNumericCols(Data("cols"))
    idx.x  <- as.integer(tcl(f1.box.1.2, "current"))
    idx.y  <- as.integer(tcl(f1.box.2.2, "current"))
    idx.z  <- as.integer(tcl(f1.box.3.2, "current"))
    vars.new <- list()
    if (idx.x > 0) vars.new$x[1] <- idxs.n[idx.x]
    if (idx.y > 0) vars.new$y[1] <- idxs.n[idx.y]
    if (idx.z > 0) vars.new$z[1] <- idxs.n[idx.z]
    if (!identical(vars.old, vars.new)) {
      if (!identical(vars.old$x, vars.new$x)) {
        Data(c("lim.axes", "x1.chk"), NULL)
        Data(c("lim.axes", "x2.chk"), NULL)
        Data(c("lim.axes", "x1"),     NULL)
        Data(c("lim.axes", "x2"),     NULL)
        Data(c("lim.axes", "x"),      NULL)
      }
      if (!identical(vars.old$y, vars.new$y)) {
        Data(c("lim.axes", "y1.chk"), NULL)
        Data(c("lim.axes", "y2.chk"), NULL)
        Data(c("lim.axes", "y1"),     NULL)
        Data(c("lim.axes", "y2"),     NULL)
        Data(c("lim.axes", "y"),      NULL)
      }
      if (!identical(vars.old$z, vars.new$z)) {
        Data(c("lim.axes", "z1.chk"), NULL)
        Data(c("lim.axes", "z2.chk"), NULL)
        Data(c("lim.axes", "z1"),     NULL)
        Data(c("lim.axes", "z2"),     NULL)
        Data(c("lim.axes", "z"),      NULL)
      }
      Data("vars", vars.new)
      Data("data.pts", NULL)
      Data("data.grd", NULL)
    }
    SetState()
  }


  # manage variables
  CallManageVariables <- function() {
    ans <- ManageVariables(Data("cols"), Data("vars"), Data("query"),
                           Data("changelog"), tt)
    if (!is.null(ans) && (!identical(ans$cols, Data("cols")) |
                          !identical(ans$vars, Data("vars")))) {
      Data("cols",      ans$cols)
      Data("vars",      ans$vars)
      Data("query",     ans$query)
      Data("changelog", ans$changelog)
      Data("data.pts",  NULL)
      Data("data.grd",  NULL)
      SetVars()
    }
  }


  # close graphic devices
  CloseDevices <- function() {
    grDevices::graphics.off()
    if (is.pkg.rgl) {while (rgl::rgl.cur() != 0) rgl::rgl.close()}
  }

  # save r graphics
  SaveRDevice <- function() {
    if (is.null(grDevices::dev.list())) return()
    exts <- c("eps", "png", "jpg", "jpeg", "pdf", "bmp", "tif", "tiff")
    file <- GetFile(cmd="Save As", exts=exts, win.title="Save 2D Graphic As",
                    defaultextension="eps", parent=tt)
    if (is.null(file)) return()
    grDevices::savePlot(filename=file, type=attr(file, "extension"))
  }


  # save rgl graphics
  SaveRGLDevice <- function() {
    if (!is.pkg.rgl || rgl::rgl.cur() == 0) return()
    file <- GetFile(cmd="Save As", exts=c("png", "eps", "pdf"),
                    win.title="Save 3D Graphic As", defaultextension="png", parent=tt)
    if (is.null(file)) return()
    if (attr(file, "extension") == "png")
      rgl::rgl.snapshot(filename=file, fmt=attr(file, "extension"))
    else
      rgl::rgl.postscript(filename=file, fmt=attr(file, "extension"))
  }

  # session information
  SessionInfo <- function() {
    txt <- paste(c(utils::capture.output(print(utils::sessionInfo(), locale=TRUE)), ""), collapse="\n")
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
    polys.old     <- Data("polys")
    poly.data.old <- Data("poly.data")
    ans <- ManagePolygons(Data("polys"), Data("poly.data"), Data("poly.crop"),
                          Data("crs"), parent=tt)
    if (is.null(ans$polys) || identical(ans$polys, polys.old)) return()
    old <- if (is.null(poly.data.old)) NULL else polys.old[[poly.data.old]]
    new <- if (is.null(ans$poly.data)) NULL else ans$polys[[ans$poly.data]]
    if (!identical(new, old)) {
      Data("data.pts", NULL)
      Data("data.grd", NULL)
    }
    Data("polys", if (length(ans$polys) == 0) NULL else ans$polys)
    Data("poly.data", ans$poly.data)
    Data("poly.crop", ans$poly.crop)
    Data("crs", ans$crs)
  }


  # set polygon range and limit
  CallSetPolygonLimits <- function() {
    polys <- Data("polys")
    poly.data.old <- Data("poly.data")
    poly.crop.old <- Data("poly.crop")
    ans <- SetPolygonLimits(names(polys), poly.data.old, poly.crop.old, tt)
    if (is.null(ans)) return()
    new.poly.data <- ans$poly.data
    new.poly.crop <- ans$poly.crop
    if (!identical(new.poly.data, poly.data.old)) {
      Data("data.pts", NULL)
      Data("data.grd", NULL)
    }
    Data("poly.data", new.poly.data)
    Data("poly.crop", new.poly.crop)
  }


  # create polygon interactively
  CreatePolygon <- function() {
    tclvalue(device.var) <- "R"
    PlotData()
    tkconfigure(tt, cursor="crosshair")
    on.exit(tkconfigure(tt, cursor="arrow"))
    v <- graphics::locator(type="o", col="black", bg="black", pch=22)
    loc.xy <- cbind(c(v$x, v$x[1]), c(v$y, v$y[1]))
    graphics::lines(loc.xy, col="black")
    ply <- raster::spPolygons(matrix(unlist(v), ncol=2), crs=Data("crs"))
    if (!inherits(ply, "SpatialPolygons")) return()
    polys <- if (is.null(Data("polys"))) list() else Data("polys")
    nam <- NamePolygon(old=names(polys))
    polys[[nam]] <- ply
    Data("polys", polys)
  }



  # view zoom
  ViewZoom <- function(zoom.direction, id=NULL) {
    tclvalue(device.var) <- "R"
    if (grDevices::dev.cur() == 1) return()
    if (zoom.direction == "0") {
      Data("lim.axes", NULL)
      PlotData()
      return()
    }
    if (is.null(id)) {
      f <- ifelse(zoom.direction == "+", -1, 1) * 0.2
      xlim <- grDevices::extendrange(graphics::par("usr")[1:2], f=f)
      ylim <- grDevices::extendrange(graphics::par("usr")[3:4], f=f)
    } else {
      tkconfigure(tt, cursor="crosshair")
      on.exit(tkconfigure(tt, cursor="arrow"))
      if (id == "point") {
        p <- unlist(graphics::locator(n=1, type="n"))
        dx <- diff(graphics::par("usr")[1:2]) / 2
        dy <- diff(graphics::par("usr")[3:4]) / 2
        f <- -0.2
        xlim <- grDevices::extendrange(c(p[1] - dx, p[1] + dx), f=f)
        ylim <- grDevices::extendrange(c(p[2] - dy, p[2] + dy), f=f)
      } else if (id == "bbox") {
        v <- graphics::locator(n=2, type="p", col="black", bg="black", pch=22)
        xlim <- sort(v$x)
        ylim <- sort(v$y)
      }
    }
    xlim <- signif(xlim, digits=6)
    ylim <- signif(ylim, digits=6)
    lim.old <- Data("lim.axes")
    lim.new <- list(x1=xlim[1], x1.chk=0, x2=xlim[2], x2.chk=0, x=xlim,
                    y1=ylim[1], y1.chk=0, y2=ylim[2], y2.chk=0, y=ylim)
    lim.new$z1     <- lim.old$z1
    lim.new$z1.chk <- lim.old$z1.chk
    lim.new$z2     <- lim.old$z2
    lim.new$z2.chk <- lim.old$z2.chk
    lim.new$z      <- lim.old$z
    Data("lim.axes", lim.new)
    PlotData()
  }


  # name polygon
  NamePolygon <- function(old=NULL, nam=NA){
    if (is.na(nam)) nam <- "New Polygon"
    idx <- 1
    chk <- nam
    while (chk %in% old) {
      chk <- sprintf("%s (%d)", nam, idx)
      idx <- idx + 1
    }
    chk
  }


  # plot data
  PlotData <- function(type) {
    plot.type <- paste(as.character(tcl(f2.box.1.2, "get")), collapse=" ")
    graphics.device <- if (missing(type)) as.character(tclvalue(device.var)) else type
    tkconfigure(tt, cursor="watch")
    on.exit(tkconfigure(tt, cursor="arrow"))
    ProcessData()

    if (is.null(Data("data.pts"))) return()
    if (is.null(Data("data.grd")) & plot.type != "Points") return()

    asp <- Data("asp.yx")
    lim <- Data("lim.axes")

    p <- if (plot.type == "Surface") NULL else Data("data.pts")
    if (!is.null(p) & is.null(Data("vars")$z)) p <- methods::as(p, "SpatialPoints")
    r <- if (plot.type == "Points") NULL else Data("data.grd")
    if (!is.null(r)) {
      ply <- if (is.null(Data("poly.crop"))) NULL else Data("polys")[[Data("poly.crop")]]
      if (!is.null(ply)) r <- raster::trim(raster::mask(r, ply))
    }

    if (graphics.device == "RGL") {
      ans <- try(Plot3d(r, p, xlim=lim$x, ylim=lim$y, zlim=lim$z, vasp=Data("asp.zx"),
                        hasp=asp, cex.pts=Data("cex.pts"), n=Data("nlevels"),
                        color.palette=Data("palette.grd")), silent=TRUE)
    } else {
      if (graphics.device == "R") {
        file <- NULL
      } else {
        file <- GetFile(cmd="Save As", exts=graphics.device, win.title="Save Graphics",
                        defaultextension=graphics.device, parent=tt)
        if (is.null(file)) return()
      }

      cex.pts <- Data("cex.pts")
      contour.lines <- if (Data("contour.lines")) list(col="#1F1F1F") else NULL
      inches <- if (Data("proportional")) c(0, 0.2 * cex.pts) else 0.03 * cex.pts

      is.z <- inherits(p, "SpatialPointsDataFrame")
      legend.loc <- if (is.z) Data("legend.loc") else NULL
      bg <- if (is.z) Data("palette.pts") else "#1F1F1FCB"

      if (plot.type == "Points")
        ans <- try(inlmisc::AddPoints(p, xlim=lim$x, ylim=lim$y, zlim=lim$z, inches=inches,
                                      bg=bg, fg="#FFFFFF40", legend.loc=legend.loc,
                                      make.intervals=Data("make.intervals"), add=FALSE,
                                      asp=asp, file=file,
                                      dms.tick=Data("dms.tick"), bg.lines=Data("bg.lines"),
                                      quantile.breaks=Data("quantile.breaks"),
                                      title=Data("legend.title"),
                                      subtitle=Data("legend.subtitle"),
                                      credit=Data("credit"), scale.loc=Data("scale.loc"),
                                      arrow.loc=Data("arrow.loc"),
                                      max.dev.dim=Data("max.dev.dim")),
                                      silent=TRUE)
      if (plot.type == "Surface")
        ans <- try(inlmisc::PlotMap(r, xlim=lim$x, ylim=lim$y, zlim=lim$z,
                                    n=Data("nlevels"), asp=asp, dms.tick=Data("dms.tick"),
                                    bg.lines=Data("bg.lines"), pal=Data("palette.grd"),
                                    contour.lines=contour.lines, file=file,
                                    useRaster=Data("useRaster"),
                                    scale.loc=Data("scale.loc"), arrow.loc=Data("arrow.loc"),
                                    draw.key=Data("draw.key"), max.dev.dim=Data("max.dev.dim"),
                                    credit=Data("credit"), explanation=Data("explanation")),
                                    silent=TRUE)

      if (plot.type == "Surface and points") {
        bg.neg <- if (Data("proportional")) "#999999B1" else NULL
        ans <- try(inlmisc::PlotMap(r, p, inches=inches, bg="#1F1F1FCB", bg.neg=bg.neg,
                                    fg="#FFFFFF40", legend.loc=NULL,
                                    xlim=lim$x, ylim=lim$y, zlim=lim$z,
                                    n=Data("nlevels"), asp=asp, dms.tick=Data("dms.tick"),
                                    bg.lines=Data("bg.lines"), pal=Data("palette.grd"),
                                    contour.lines=contour.lines, file=file,
                                    useRaster=Data("useRaster"),
                                    scale.loc=Data("scale.loc"), arrow.loc=Data("arrow.loc"),
                                    draw.key=Data("draw.key"), max.dev.dim=Data("max.dev.dim"),
                                    credit=Data("credit"), explanation=Data("explanation")),
                                    silent=TRUE)
      }
    }
    if (inherits(ans, "try-error"))
      tkmessageBox(icon="error", message="Plot routine failed.", detail=ans,
                   title="Error", type="ok", parent=tt)

    tkfocus(tt)
  }


  # call data editor
  CallEditData <- function(read.only=TRUE, is.all=TRUE, is.state=FALSE) {
    tkconfigure(tt, cursor="watch")
    on.exit(tkconfigure(tt, cursor="arrow"))

    if (!read.only && is.null(Data("data.raw"))) return()
    if (!is.all) {
      ProcessData()
      if (is.null(Data("data.pts"))) return()
    }

    vars <- Data("vars")
    cols <- Data("cols")
    rows <- Data("rows")

    if (is.null(cols)) return()
    if (is.state && length(vars) == 0) return()

    idxs <- seq_along(cols)
    if (!read.only) idxs <- idxs[!is.na(vapply(cols, function(i) i$index, 0L))]
    if (is.state) idxs <- unique(unlist(vars)[match(names(vars), c("x", "y", "z"))])
    col.nams <- vapply(cols, function(i) i$id,     "")[idxs]
    col.fmts <- vapply(cols, function(i) i$format, "")[idxs]
    col.idxs <- vapply(cols, function(i) i$index,  0 )[idxs]
    col.funs <- vapply(cols, function(i) i$fun,    "")[idxs]

    row.nams <- if (is.all) rows else rownames(Data("data.pts")@data)

    if (read.only) {
      idxs <- if (is.all) seq_along(rows) else match(row.nams, rows)
      l <- lapply(col.funs, function(i) EvalFunction(i, cols)[idxs])
      ans <- EditData(l, col.names=col.nams, row.names=row.nams, col.formats=col.fmts,
                      read.only=TRUE, win.title="Data Viewer", parent=tt)
    } else {
      ans <- EditData(Data("data.raw")[col.idxs], col.names=col.nams,
                      row.names=row.nams, col.formats=col.fmts,
                      read.only=FALSE, changelog=Data("changelog"),
                      win.title="Data Editor", parent=tt)
    }
    if (is.null(ans)) return()

    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE), add=TRUE)

    Data("data.raw",  ans$d)

    old.changelog <- Data("changelog")
    Data("changelog", ans$changelog)
    if (is.null(old.changelog))
      new.changelog.rows <- seq_len(nrow(ans$changelog))
    else
      new.changelog.rows <- (nrow(old.changelog) + 1L):nrow(ans$changelog)
    changed.cols <- unique(ans$changelog[new.changelog.rows, "variable"])
    changed.idxs <- which(col.nams %in% changed.cols)
    for (i in changed.idxs) {
      obj <- EvalFunction(cols[[i]]$fun, cols)
      cols[[i]]$summary <- summary(obj)
      cols[[i]]$sample <- stats::na.omit(obj)[1]
    }
    Data("cols", cols)
    Data("data.pts", NULL)
    Data("data.grd", NULL)
  }


  # process data
  ProcessData <- function() {
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

    # points
    if (is.null(Data("data.pts"))) {
      cols <- Data("cols")

      # construct data frame from coorinate variables
      FUN <- function(v) {
        if (is.null(v)) NULL else EvalFunction(cols[[v]]$fun, cols)
      }
      d <- lapply(var.names, function(i) FUN(vars[[i]]))
      class(d) <- "data.frame"
      colnames(d) <- var.names
      rownames(d) <- Data("rows")

      # account for missing z variable
      if (!"z" %in% var.names) d$z <- rep(as.numeric(NA), nrow(d))

      # filter records
      query <- Data("query")
      if (!is.null(query)) {
        is.filter <- EvalFunction(query, cols)
        is.filter[is.na(is.filter)] <- FALSE
        d <- d[is.filter, , drop=FALSE]
      }

      # sort records
      sort.on <- Data(c("vars", "sort.on"))
      if (!is.null(sort.on)) {
        sort.order <- order(EvalFunction(cols[[sort.on]]$fun, cols),
                            na.last=attr(sort.on, "na.last"),
                            decreasing=attr(sort.on, "decreasing"))
        if (!is.null(query)) sort.order <- sort.order[is.filter]
        d <- d[sort.order, , drop=FALSE]
      }

      # remove non-finite spatial coordinate values
      is.coord <- is.finite(d[, "x"]) & is.finite(d[, "y"])
      d <- d[is.coord, , drop=FALSE]

      # convert to spatial points
      sp::coordinates(d) <- ~ x + y
      ans <- try(sp::proj4string(d) <- Data("crs"), silent=TRUE)
      if (inherits(ans, "try-error")) {
        msg <- "Failed to set Coordinate Reference System."
        tkmessageBox(icon="error", message=msg, detail=ans,
                     title="Error", type="ok", parent=tt)
        sp::proj4string(d) <- sp::CRS(as.character(NA))
      }

      # points in polygon
      if (!is.null(Data("poly.data"))) {
        p <- Data("polys")[[Data("poly.data")]]
        if (!is.null(p)) d <- d[!is.na(sp::over(d, p)), , drop=FALSE]
      }

      if (nrow(d) > 0) {
        Data("data.pts", d)
        Data("data.grd", NULL)
      } else {
        msg <- "Range excludes all data points."
        tkmessageBox(icon="error", message=msg, title="Error", type="ok", parent=tt)
      }
    }

    # grid
    if (!is.null(Data("data.pts")) && is.null(Data("data.grd"))) {
      if (!"z" %in% var.names) return()
      if (all(is.na(Data("data.pts")$z))) {
        msg <- "Missing values for coordinate-variable 'z'."
        tkmessageBox(icon="error", message=msg, title="Error", type="ok", parent=tt)
        return()
      }
      x <- sp::coordinates(Data("data.pts"))[, 1]
      y <- sp::coordinates(Data("data.pts"))[, 2]
      z <- Data("data.pts")@data$z

      # build raster template
      grid <- Data("grid")
      if (!inherits(grid$opt, "integer") || !grid$opt %in% 1:3) grid$opt <- 1
      if (grid$opt == 3) {
        r <- grid$geo
      } else {
        xlim <- grDevices::extendrange(x)
        ylim <- grDevices::extendrange(y)
        if (grid$opt == 2) {
          xmod <- diff(xlim) %% grid$res[1]
          xadd <- ifelse(xmod == 0, 0, (grid$res[1] - xmod) / 2)
          xlim <- c(xlim[1] - xadd, xlim[2] + xadd)
          ymod <- diff(ylim) %% grid$res[2]
          yadd <- ifelse(ymod == 0, 0, (grid$res[2] - ymod) / 2)
          ylim <- c(ylim[1] - yadd, ylim[2] + yadd)
          ncols <- diff(xlim) %/% grid$res[1]
          nrows <- diff(ylim) %/% grid$res[2]
        } else {
          ncols <- 100
          nrows <- 100
        }
        r <- raster::raster(nrows=nrows, ncols=ncols, xmn=xlim[1], xmx=xlim[2],
                            ymn=ylim[1], ymx=ylim[2], crs=sp::CRS(as.character(NA)))
      }
      raster::crs(r) <- Data("crs")

      # interpolate
      xlen <- diff(c(raster::xmin(r), raster::xmax(r)))
      ylen <- diff(c(raster::ymin(r), raster::ymax(r)))
      m <- 1
      n <- 1
      if ((ylen / xlen) < 1)
        m <- 2
      else
        n <- 2
      ans <- try(MBA::mba.points(xyz=cbind(x, y, z)[!is.na(z), ],
                                 xy.est=sp::coordinates(r),
                                 n=n, m=m, h=11, verbose=FALSE)$xyz.est[, "z"], silent=TRUE)
      if (inherits(ans, "try-error")) {
        tkmessageBox(icon="error", message="Interpolation failed.", detail=ans,
                     title="Error", type="ok", parent=tt)
        r <- NULL
      } else {
        r[] <- ans
      }
      names(r) <- "z"

      Data("data.grd", r)
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
    if (file$fun == "")
      Data("query", NULL)
    else
      Data("query", file$fun)
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
    if (is.null(txt)) return()
    if (length(txt) == 0 || (length(txt) == 1 & txt == "")) txt <- NULL
    Data("comment", txt)
  }


  # build histogram
  CallBuildHistogram <- function() {
    tkconfigure(tt, cursor="watch")
    on.exit(tkconfigure(tt, cursor="arrow"))
    ProcessData()
    cols <- Data("cols")
    if (is.null(cols)) return()
    col.nams <- vapply(cols, function(i) i$id,  "")
    col.funs <- vapply(cols, function(i) i$fun, "")
    l <- lapply(col.funs, function(i) EvalFunction(i, cols))
    rows <- Data("rows")
    if (is.null(Data("data.pts")))
      idxs <- seq_along(rows)
    else
      idxs <- match(rownames(Data("data.pts")@data), rows)
    BuildHistogram(l, var.names=col.nams, processed.rec=idxs, parent=tt)
  }


  # web mapping
  PlotWebMap <- function() {
    tkconfigure(tt, cursor="watch")
    on.exit(tkconfigure(tt, cursor="arrow"))
    if (!requireNamespace("leaflet", quietly=TRUE)) return()
    crs.old <- Data("crs")
    if (is.na(rgdal::CRSargs(crs.old))) {
      msg <- "Data must be associated with a coordinate reference system."
      tkmessageBox(icon="info", message=msg, title="Information", type="ok", parent=tt)
      return()
    }
    ProcessData()
    crs.new <- sp::CRS("+init=epsg:4326")
    map <- leaflet::leaflet()
    map <- leaflet::addProviderTiles(map, "OpenStreetMap.Mapnik")
    opt <- leaflet::WMSTileOptions(format="image/png", transparent=TRUE)
    base.groups <- c("Open Street Map", "National Map")
    map <- leaflet::addTiles(map, group=base.groups[1])
    url <- "https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WmsServer?"
    txt <-  "USGS <a href='https://nationalmap.gov/'>The National Map</a>"
    map <- leaflet::addWMSTiles(map, url, options=opt, layers="0",
                                attribution=txt, group=base.groups[2])
    overlay.groups <- NULL

    # points
    pts <- Data("data.pts")
    if (!is.null(pts)) {
      grp <- "Points"
      rec <- rownames(pts@data)
      xyz <- as.data.frame(pts)
      txt <- sprintf("<b>Record:</b> %s<br/><b>x:</b> %s<br/><b>y:</b> %s<br/><b>z:</b> %s",
                     rec, xyz$x, xyz$y, xyz$z)
      pts <- sp::spTransform(pts, crs.new)
      opt <- leaflet::markerClusterOptions(showCoverageOnHover=FALSE)
      map <- leaflet::addCircleMarkers(map, data=pts, radius=10, popup=txt,
                                       clusterOptions=opt, weight=3, group=grp)
      overlay.groups <- c(overlay.groups, grp)
    }

    # polygons
    ply <- if (is.null(Data("poly.data"))) NULL else Data("polys")[[Data("poly.data")]]
    if (!is.null(ply)) {
      grp <- "Polygon (data limits)"
      ply <- sp::spTransform(ply, crs.new)
      map <- leaflet::addPolylines(map, data=ply, weight=1, color="#000000", group=grp)
      overlay.groups <- c(overlay.groups, grp)
    }
    ply <- if (is.null(Data("poly.crop"))) NULL else Data("polys")[[Data("poly.crop")]]
    if (!is.null(ply)) {
      grp <- "Polygon (crop region)"
      ply <- sp::spTransform(ply, crs.new)
      map <- leaflet::addPolylines(map, data=ply, weight=1, color="#000000", group=grp)
      overlay.groups <- c(overlay.groups, grp)
    }

    if (is.null(overlay.groups)) return()
    opt <- leaflet::layersControlOptions(collapsed=FALSE)
    map <- leaflet::addLayersControl(map, baseGroups=base.groups,
                                     overlayGroups=overlay.groups, options=opt)
    print(map)
  }


  # open new 2d device
  Open2d <- function() {
    if (grDevices::dev.cur() == 1) {
      grDevices::dev.new(width=7, height=7)
    } else {
      cin <- graphics::par("din")
      # TODO(JCF): read xpos and ypos from current dev and pass to dev.new
      grDevices::dev.new(width=cin[1], height=cin[2])
    }
  }


  # open new 3d device
  Open3d <- function() {
    if (rgl::rgl.cur() == 0) {
      rgl::open3d()
    } else {
      windowRect <- rgl::par3d("windowRect") + 25
      rgl::open3d(windowRect=windowRect)
    }
  }


  # warn if using windows os and running in mdi mode
  if (.Platform$OS.type == "windows" && utils::getIdentification() == "RGui")
    message("\n\n    You are running R in MDI mode which *may* interfere\n",
            "    with the functionality of the graphical user interface.\n",
            "    It is recommended to use R in SDI mode which can be\n",
            "    set in the command line or by clicking in the Menu:\n",
            "    Edit - GUI Preferences: SDI, then Save and restart R.\n\n")

  # initialize default directory
  if (is.null(Data("default.dir"))) Data("default.dir", getwd())

  # check if suggested packages are loaded
  is.pkg.xml <- requireNamespace("XML", quietly=TRUE)
  is.pkg.rgl <- requireNamespace("rgl", quietly=TRUE)

  # set options
  options(help_type="html")

  # assign variables linked to tk entry widgets
  import.var  <- tclVar()
  save.var    <- tclVar()
  manage.var  <- tclVar()
  polygon.var <- tclVar()
  config.var  <- tclVar()
  axes.var    <- tclVar()
  device.var  <- tclVar("R")
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
  tkadd(menu.file, "command", label="New project", accelerator="Ctrl+N", command=ClearObjs)
  tkadd(menu.file, "command", label="Open project\u2026", accelerator="Ctrl+O", command=OpenProj)
  tkadd(menu.file, "command", label="Save project", accelerator="Ctrl+S", command=SaveProj)
  tkadd(menu.file, "command", label="Save project as\u2026",  accelerator="Ctrl+Shift+S",
        command=SaveProjAs)
  tkadd(menu.file, "separator")
  menu.file.import <- tkmenu(tt, tearoff=0)
  tkadd(menu.file.import, "command", label="Text file or clipboard\u2026",
        command=function() ReadData("txt"))
  tkadd(menu.file.import, "command", label="XML-spreadsheet file\u2026",
        state=ifelse(is.pkg.xml, "normal", "disabled"),
        command=function() ReadData("xlsx"))
  tkadd(menu.file.import, "command", label="Shapefile\u2026",
        command=function() ReadData("shp"))
  tkadd(menu.file.import, "command", label="R-package dataset\u2026",
        command=function() ReadData("rpackage"))
  tkadd(menu.file.import, "command", label="R-data file\u2026",
        command=function() ReadData("rda"))
  tkadd(menu.file, "cascade", label="Import point data from", menu=menu.file.import)
  menu.file.export.pnt <- tkmenu(tt, tearoff=0)
  tkadd(menu.file.export.pnt, "command", label="Text file\u2026",
        command=function() WriteData("txt"))
  tkadd(menu.file.export.pnt, "command", label="Shapefile\u2026",
        command=function() WriteData("shp"))
  tkadd(menu.file.export.pnt, "command", label="R-data file\u2026",
        command=function() WriteData("rda"))
  tkadd(menu.file, "cascade", label="Export point data as", menu=menu.file.export.pnt)
  menu.file.export.grd <- tkmenu(tt, tearoff=0)
  tkadd(menu.file.export.grd, "command", label="Text file\u2026",
        command=function() WriteRaster("txt"))
  tkadd(menu.file.export.grd, "command", label="GeoTIFF\u2026",
        command=function() WriteRaster("tif"))
  tkadd(menu.file.export.grd, "command", label="R-data file\u2026",
        command=function() WriteRaster("rda"))
  tkadd(menu.file, "cascade", label="Export interpolated grid data as", menu=menu.file.export.grd)
  tkadd(menu.file, "separator")
  menu.file.graphics <- tkmenu(tt, tearoff=0)
  tkadd(menu.file.graphics, "command", label="PNG file\u2026", command=function() PlotData("png"))
  tkadd(menu.file.graphics, "command", label="PDF file\u2026", command=function() PlotData("pdf"))
  tkadd(menu.file, "cascade", label="Save graphics to a", menu=menu.file.graphics)
  menu.file.snapshot <- tkmenu(tt, tearoff=0)
  tkadd(menu.file.snapshot, "command", label="2D graphics device\u2026", accelerator="Ctrl+R",
        command=SaveRDevice)
  tkadd(menu.file.snapshot, "command", label="3D graphics device\u2026", command=SaveRGLDevice)
  tkadd(menu.file, "cascade", label="Save snapshot from active", menu=menu.file.snapshot)

  tkadd(menu.file, "separator")
  tkadd(menu.file, "command", label="Exit", command=CloseGUI)

  # edit menu
  menu.edit <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Edit", menu=menu.edit, underline=0)
  tkadd(menu.edit, "command", label="Coordinate reference system\u2026",
        command=function() {
          crs.old <- Data("crs")
          crs.new <- SetCrs(crs.old, parent=tt)
          if (!identical(crs.old, crs.new)) {
            Data("crs", crs.new)
            Data("data.pts", NULL)
            Data("data.grd", NULL)
          }
        })
  tkadd(menu.edit, "separator")
  tkadd(menu.edit, "command", label="Manage variables\u2026", command=CallManageVariables)
  tkadd(menu.edit, "command", label="Edit unprocessed data\u2026",
        command=function() CallEditData(read.only=FALSE))
  tkadd(menu.edit, "command", label="Comment\u2026", command=EditComment)
  tkadd(menu.edit, "separator")
  tkadd(menu.edit, "command", label="Filter data records\u2026", command=BuildQuery)
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
  tkadd(menu.edit, "command", label="Define interpolation grid\u2026",
        command=function() {
          grid.old <- Data("grid")
          grid.new <- DefineGrid(grid.old, tt)
          if (is.null(grid.new)) return()
          if (!identical(grid.old, grid.new)) {
            Data("grid", grid.new)
            Data("data.grd", NULL)
          }
        })

  # view menu
  menu.view <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="View", menu=menu.view, underline=0)
  menu.view.raw <- tkmenu(tt, tearoff=0)
  tkadd(menu.view.raw, "command", label="All variables\u2026",
        command=function() CallEditData(is.all=TRUE, is.state=FALSE))
  tkadd(menu.view.raw, "command", label="Coordinate variables\u2026",
        command=function() CallEditData(is.all=TRUE, is.state=TRUE))
  tkadd(menu.view, "cascade", label="All data records of", menu=menu.view.raw)
  menu.view.pr <- tkmenu(tt, tearoff=0)
  tkadd(menu.view.pr, "command", label="All variables\u2026",
        command=function() CallEditData(is.all=FALSE, is.state=FALSE))
  tkadd(menu.view.pr, "command", label="Coordinate variables\u2026",
        command=function() CallEditData(is.all=FALSE, is.state=TRUE))
  tkadd(menu.view, "cascade", label="Processed data records of", menu=menu.view.pr)

  # polygon menu
  menu.poly <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Polygon", menu=menu.poly, underline=0)
  tkadd(menu.poly, "command", label="Manage polygons\u2026", command=CallManagePolygons)
  tkadd(menu.poly, "separator")
  tkadd(menu.poly, "command", label="Interactively create a polygon\u2026", command=CreatePolygon)
  tkadd(menu.poly, "separator")
  tkadd(menu.poly, "command", label="Set polygon limits\u2026", command=CallSetPolygonLimits)
  tkadd(menu.poly, "command", label="Clear polygon limits",
        command=function() {
          Data("poly.data", NULL)
          Data("poly.crop", NULL)
          Data("data.pts",  NULL)
          Data("data.grd",  NULL)
        })

  # plot menu
  menu.plot <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Plot", menu=menu.plot, underline=0)
  tkadd(menu.plot, "command", label="Set axes limits\u2026",
        command=function() {
          lim <- SetAxesLimits(Data("lim.axes"), tt)
          Data("lim.axes", lim)
        })
  tkadd(menu.plot, "command", label="Clear axes limits",
        command=function() {
          Data("lim.axes", NULL)
        })

  tkadd(menu.plot, "separator")
  tkadd(menu.plot, "command", label="Fit all",  accelerator="Ctrl+0", command=function() ViewZoom("0"))
  tkadd(menu.plot, "command", label="Zoom in",  accelerator="Ctrl++", command=function() ViewZoom("+"))
  tkadd(menu.plot, "command", label="Zoom out", accelerator="Ctrl+-", command=function() ViewZoom("-"))
  menu.plot.axes <- tkmenu(tt, tearoff=0)
  tkadd(menu.plot.axes, "command", label="Zoom in on point\u2026",
        command=function() ViewZoom("+", id="point"))
  tkadd(menu.plot.axes, "command", label="Define bounding box\u2026",
        command=function() ViewZoom("+", id="bbox"))
  tkadd(menu.plot, "cascade", label="Interactively", menu=menu.plot.axes)

  tkadd(menu.plot, "separator")
  tkadd(menu.plot, "command", label="Configuration\u2026", command=function() SetConfiguration(tt))
  menu.plot.col <- tkmenu(tt, tearoff=0)
  tkadd(menu.plot.col, "command", label="Point data\u2026",
        command=function() {
          Pal <- colorspace::choose_palette(Data("palette.pts"), parent=tt)
          if (!is.null(Pal)) Data("palette.pts", Pal)
        })
  tkadd(menu.plot.col, "command", label="Gridded data\u2026",
        command=function() {
          n <- ifelse(is.null(Data("nlevels")), 200, Data("nlevels"))
          Pal <- colorspace::choose_palette(Data("palette.grd"), n, parent=tt)
          if (!is.null(Pal)) Data("palette.grd", Pal)
        })
  tkadd(menu.plot, "cascade", label="Color palette for", menu=menu.plot.col)
  tkadd(menu.plot, "command", label="Annotation\u2026", command=function() SetPlotAnnotation(tt))
  tkadd(menu.plot, "separator")
  tkadd(menu.plot, "command", label="Histogram\u2026", command=CallBuildHistogram)
  tkadd(menu.plot, "command", label="Web mapping", command=PlotWebMap)
  tkadd(menu.plot, "separator")
  menu.plot.new <- tkmenu(tt, tearoff=0)
  tkadd(menu.plot.new, "command", label="2D graphics", accelerator="Ctrl+F3", command=Open2d)
  tkadd(menu.plot.new, "command", label="3D graphics", command=Open3d)
  tkadd(menu.plot, "cascade", label="Open a new device for", menu=menu.plot.new)
  tkadd(menu.plot, "command", label="Close graphic devices", accelerator="Ctrl+F4",
        command=CloseDevices)

  # help menu
  menu.help <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Help", menu=menu.help, underline=0)
  tkadd(menu.help, "command", label="Documentation",
        command=function() utils::help(package="RSurvey"))

  tkadd(menu.help, "separator")
  menu.help.rep <- tkmenu(tt, tearoff=0)
  tkadd(menu.help.rep, "command", label="CRAN",
        command=function() utils::browseURL("https://CRAN.R-project.org/package=RSurvey"))
  tkadd(menu.help.rep, "command", label="GitHub",
        command=function() utils::browseURL("https://github.com/USGS-R/RSurvey"))
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
              RestoreSession(file.path(getwd(), "R"), save.objs="Data", fun.call="LaunchGui")
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
                      text="Coordinate variables")
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

  # frame 2, plot
  f2 <- tkframe(tt, relief="flat")
  f2.but.1.1 <- ttkbutton(f2, width=10, text="Plot", command=function() PlotData())
  f2.box.1.2 <- ttkcombobox(f2, state="readonly", textvariable=plt.typ.var,
                            values=c("Points", "Surface", "Surface and points"))
  tkgrid(f2.but.1.1, f2.box.1.2, pady=5)
  tkgrid.configure(f2.box.1.2, padx=c(5, 10), sticky="we")
  tkgrid.columnconfigure(f2, 1, weight=1, minsize=25)
  tkpack(f2, fill="x", expand=TRUE, padx=c(20, 10))

  # frame 3, graphics device
  f3 <- tkframe(tt, relief="flat")
  f3.lab.1.1 <- ttklabel(f3, text="Graphics display")
  f3.rad.1.2 <- ttkradiobutton(f3, variable=device.var, value="R", text="2D", command=SetState)
  f3.rad.1.3 <- ttkradiobutton(f3, variable=device.var, value="RGL", text="3D", command=SetState)
  tkgrid(f3.lab.1.1, f3.rad.1.2, f3.rad.1.3, pady=c(0, 10), sticky="e")
  tkgrid.configure(f3.lab.1.1, padx=c(0, 4))
  tkgrid.configure(f3.rad.1.2, padx=2)
  tkpack(f3, anchor="w", padx=c(30, 10))

  # set variables
  SetVars()

  # bind events
  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>",                      CloseGUI)
  tkbind(tt, "<Control-KeyPress-n>",           ClearObjs)
  tkbind(tt, "<Control-KeyPress-o>",           OpenProj)
  tkbind(tt, "<Control-KeyPress-s>",           SaveProj)
  tkbind(tt, "<Control-Shift-KeyPress-S>",     SaveProjAs)
  tkbind(tt, "<Control-KeyPress-r>",           SaveRDevice)
  tkbind(tt, "<Control-KeyPress-F3>",          Open2d)
  tkbind(tt, "<Control-KeyPress-F4>",          CloseDevices)
  tkbind(tt, "<Control-KeyPress-plus>",        function() ViewZoom("+"))
  tkbind(tt, "<Control-KeyPress-KP_Add>",      function() ViewZoom("+"))
  tkbind(tt, "<Control-KeyPress-minus>",       function() ViewZoom("-"))
  tkbind(tt, "<Control-KeyPress-KP_Subtract>", function() ViewZoom("-"))
  tkbind(tt, "<Control-KeyPress-0>",           function() ViewZoom("0"))
  tkbind(tt, "<Control-KeyPress-KP_0>",        function() ViewZoom("0"))

  tkbind(f1.box.1.2, "<<ComboboxSelected>>", RefreshVars)
  tkbind(f1.box.2.2, "<<ComboboxSelected>>", RefreshVars)
  tkbind(f1.box.3.2, "<<ComboboxSelected>>", RefreshVars)
  tkbind(f2.box.1.2, "<<ComboboxSelected>>", SetState)

  # gui closure
  tkfocus(force=tt)
  invisible()
}
