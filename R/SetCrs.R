SetCrs <- function(crs=sp::CRS(as.character(NA)), parent=NULL) {


  # check and save projection arguments
  SaveArgs <- function() {
    new.projargs <- as.character(tclvalue(projargs.var))
    if (new.projargs %in% c("", "NA")) new.projargs <- as.character(NA)
    ans <- rgdal::checkCRSArgs(new.projargs)
    if (ans[[1]] | is.na(new.projargs)) {
      rtn <<- sp::CRS(new.projargs)
      tclvalue(tt.done.var) <- 1
    } else {
      tkmessageBox(icon="error", message=ans[[2]], title="Error", type="ok", parent=tt)
      tkfocus(f1.ent.2.1)
    }
  }


  # copy entry to clipboard
  CopyEntry <- function() {
    txt <- as.character(tclvalue(projargs.var))
    cat(txt, file="clipboard")
  }


  # paste into entry from clipboard
  PasteEntry <- function() {
    cb <- try(scan(file="clipboard", what="character", sep="\n", quiet=TRUE), silent=TRUE)
    if (inherits(cb, "try-error")) return()
    tclvalue(projargs.var) <- cb
    tkfocus(f1.ent.2.1)
  }


 # clear entry
  ClearEntry <- function() {
    tclvalue(projargs.var) <- ""
    tkfocus(f1.ent.2.1)
  }


 # get existing crs
  GetCrs <- function(projargs=NULL) {
    if (is.null(projargs)) {
      file <- GetFile(cmd="Open", exts="prj", win.title="Open Projection File", parent=tt)
      if (is.null(file)) return()
      wkt <- readLines(file, n=1, warn=FALSE)
      projargs <- showP4(wkt)
    }
    tclvalue(projargs.var) <- projargs
    tkfocus(f1.ent.2.1)
  }


  # initialize return value
  rtn <- crs

  # assign the variables linked to tk widgets
  projargs.var <- tclVar(as.character(crs@projargs))
  tt.done.var  <- tclVar(0)

  # open gui
  tclServiceMode(FALSE)
  tt <- tktoplevel()
  if (!is.null(parent)) {
      tkwm.transient(tt, parent)
      geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
      tkwm.geometry(tt, paste0("+", as.integer(geo[2]) + 25,
                               "+", as.integer(geo[3]) + 25))
  }
  tktitle(tt) <- "Coordinate Reference System"
  tkwm.resizable(tt, 1, 0)

  # create menus
  top.menu <- tkmenu(tt, tearoff=0)

  menu.file <- tkmenu(tt, tearoff=0, relief="flat")
  tkadd(top.menu, "cascade", label="File", menu= menu.file, underline=0)
  tkadd(menu.file, "command", label="Import from projection file\u2026",
        command=function() GetCrs())

  menu.crs <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Common", menu=menu.crs, underline=0)

  tkadd(menu.crs, "command", label="Unknown projection", command=function() GetCrs("NA"))

  menu.crs.geo <- tkmenu(tt, tearoff=0)
  tkadd(menu.crs.geo, "command", label="World Geodetic System of 1984",
        command=function() GetCrs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  tkadd(menu.crs.geo, "command", label="North American Datum of 1983",
        command=function() GetCrs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
  tkadd(menu.crs.geo, "command", label="North American Datum of 1927",
        command=function() GetCrs("+proj=longlat +ellps=clrk66 +datum=NAD27 +no_defs"))
  tkadd(menu.crs, "cascade", label="Geographic coordinate systems", menu=menu.crs.geo)

  menu.crs.loc <- tkmenu(tt, tearoff=0)
  tkadd(menu.crs.loc, "command", label="North America Lambert Conformal Conic",
        command=function() GetCrs("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
  tkadd(menu.crs.loc, "command", label="North America Albers Equal Area Conic",
        command=function() GetCrs("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
  tkadd(menu.crs.loc, "command", label="Robinson",
        command=function() GetCrs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  tkadd(menu.crs.loc, "command", label="Mollweide",
        command=function() GetCrs("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  tkadd(menu.crs, "cascade", label="Local projected coordinate systems", menu=menu.crs.loc)

  tkconfigure(tt, menu=top.menu)

  # frame 0, ok and cancel buttons
  f0 <- tkframe(tt, relief="flat")

  f0.but.1 <- ttkbutton(f0, width=2, image=GetBitmapImage("copy"), command=CopyEntry)
  f0.but.2 <- ttkbutton(f0, width=2, image=GetBitmapImage("paste"), command=PasteEntry)
  f0.but.3 <- ttkbutton(f0, width=2, image=GetBitmapImage("delete"), command=ClearEntry)
  f0.but.5 <- ttkbutton(f0, width=12, text="OK", command=SaveArgs)
  f0.but.6 <- ttkbutton(f0, width=12, text="Cancel",
                        command=function() tclvalue(tt.done.var) <- 1)
  f0.but.7 <- ttkbutton(f0, width=12, text="Help",
                        command=function() {
                          print(help("SetCrs", package="RSurvey"))
                        })
  tkgrid(f0.but.1, f0.but.2, f0.but.3, "x", f0.but.5, f0.but.6, f0.but.7)
  tkgrid.columnconfigure(f0, 3, weight=1)
  tkgrid.configure(f0.but.1, f0.but.2, f0.but.3, sticky="n", padx=c(0, 2), pady=4)
  tkgrid.configure(f0.but.1, padx=c(10, 2))
  tkgrid.configure(f0.but.5, f0.but.6, f0.but.7, padx=c(4, 0), pady=c(15, 10))
  tkgrid.configure(f0.but.7, padx=c(4, 10))
  tkpack(f0, fill="x", side="bottom", anchor="e")

  # frame 1
  f1 <- ttkframe(tt, relief="flat")
  txt <- paste("The coordinate reference system (CRS) defines a map projection and datum.",
               "Using PROJ.4 arguments, define a global and project-wide CRS below.",
               "When a new dataset is created, or when data is loaded that has no CRS, the global CRS is used.",
               "If a loaded dataset has a different CRS, it is automatically reprojected to the global CRS.")
  f1.lab.1.1 <- ttklabel(f1, text=txt, wraplength=600)
  f1.ent.2.1 <- ttkentry(f1, textvariable=projargs.var)
  tkgrid(f1.lab.1.1, sticky="w", pady=c(10, 4))
  tkgrid(f1.ent.2.1, sticky="we", padx=c(0, 2))
  tkgrid.columnconfigure(f1, 0, weight=1, minsize=10)
  tkpack(f1, fill="both", expand="yes", padx=10)

  # bind events
  tclServiceMode(TRUE)
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  # gui control
  tkfocus(tt)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  return(rtn)
}
