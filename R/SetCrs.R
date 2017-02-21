#' GUI: Coordinate Reference System
#'
#' A graphical user interface (\acronym{GUI}) for specifying PROJ.4 arguments
#' associated with a coordinate reference system (CRS).
#' The arguments must be entered exactly as in the PROJ.4 documentation,
#' in particular there cannot be any white space in +<arg>=<value> strings,
#' and successive such strings can only be separated by blanks.
#'
#' @param crs CRS.
#'    Coordinate reference system described using PROJ.4 arguments.
#' @param parent tkwin.
#'   \acronym{GUI} parent window
#'
#' @return Returns an updated value of the \code{crs} argument.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link[sp]{CRS}}, \code{\link[rgdal]{checkCRSArgs}}
#'
#' @keywords misc
#'
#' @import tcltk
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   SetCrs("+init=epsg:4326")
#' }
#'

SetCrs <- function(crs=sp::CRS(as.character(NA)), parent=NULL) {


  # check and save projection arguments
  SaveArgs <- function() {
    new.projargs <- as.character(tclvalue(tkget(f1.txt.2.1, "1.0", "end-1c")))
    new.projargs <- gsub("\n", " ", new.projargs)
    if (new.projargs %in% c("", "NA")) new.projargs <- as.character(NA)
    ans <- rgdal::checkCRSArgs(new.projargs)
    if (ans[[1]] | is.na(new.projargs)) {
      rtn <<- sp::CRS(new.projargs)
      tclvalue(tt.done.var) <- 1
    } else {
      tkmessageBox(icon="error", message=ans[[2]], title="Error", type="ok", parent=tt)
      tkfocus(f1.txt.2.1)
    }
  }


 # get existing crs
  GetCrs <- function(projargs=NULL) {
    if (is.null(projargs)) {
      file <- GetFile(cmd="Open", exts="prj", win.title="Open Projection File", parent=tt)
      if (is.null(file)) return()
      wkt <- readLines(file, n=1, warn=FALSE)
      projargs <- rgdal::showP4(wkt)
    }
    ClearConsole()
    tkinsert(f1.txt.2.1, "end", projargs)
    tkfocus(f1.txt.2.1)
  }


  # edit menu functions
  EditUndo <- function() {
    tkfocus(f1.txt.2.1)
    try(tcl(f1.txt.2.1, "edit", "undo"), silent=TRUE)
  }
  EditRedo <- function() {
    tkfocus(f1.txt.2.1)
    try(tcl(f1.txt.2.1, "edit", "redo"), silent=TRUE)
  }
  EditCut <- function() {
    tkfocus(f1.txt.2.1)
    tcl("tk_textCut", f1.txt.2.1)
  }
  EditCopy <- function() {
    tkfocus(f1.txt.2.1)
    tcl("tk_textCopy", f1.txt.2.1)
  }
  EditPaste <- function() {
    tkfocus(f1.txt.2.1)
    tcl("tk_textPaste", f1.txt.2.1)
  }
  EditSelectAll <- function() {
    tkfocus(f1.txt.2.1)
    tktag.add(f1.txt.2.1, "sel", "1.0", "end")
  }
  ClearConsole <- function() {
    tcl(f1.txt.2.1, "delete", "1.0", "end")
    tkfocus(f1.txt.2.1)
  }


  # ensure valid crs argument
  if (!inherits(crs, "CRS")) crs <- sp::CRS(as.character(NA))

  # initialize return value
  rtn <- crs

  # assign the variables linked to tk widgets
  tt.done.var  <- tclVar(0)

  # open gui
  tclServiceMode(FALSE)
  tt <- tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    geo <- as.integer(geo[2:3]) + 25
    tkwm.geometry(tt, sprintf("+%s+%s", geo[1], geo[2]))
  }
  tktitle(tt) <- "Coordinate Reference System"
  tkwm.resizable(tt, 1, 0)

  # create menus
  top.menu <- tkmenu(tt, tearoff=0)

  menu.file <- tkmenu(tt, tearoff=0, relief="flat")
  tkadd(top.menu, "cascade", label="File", menu= menu.file, underline=0)
  tkadd(menu.file, "command", label="Import\u2026", command=function() GetCrs())

  menu.edit <- tkmenu(tt, tearoff=0, relief="flat")
  tkadd(top.menu, "cascade", label="Edit", menu=menu.edit, underline=0)
  tkadd(menu.edit, "command", label="Undo", accelerator="Ctrl+Z", command=EditUndo)
  tkadd(menu.edit, "command", label="Redo", accelerator="Ctrl+Y", command=EditRedo)
  tkadd(menu.edit, "separator")
  tkadd(menu.edit, "command", label="Cut", accelerator="Ctrl+X", command=EditCut)
  tkadd(menu.edit, "command", label="Copy", accelerator="Ctrl+C", command=EditCopy)
  tkadd(menu.edit, "command", label="Paste", accelerator="Ctrl+V", command=EditPaste)
  tkadd(menu.edit, "separator")
  tkadd(menu.edit, "command", label="Select all", accelerator="Ctrl+A", command=EditSelectAll)
  tkadd(menu.edit, "command", label="Clear console", accelerator="Ctrl+L", command=ClearConsole)

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

  f0.but.1 <- ttkbutton(f0, width=12, text="OK", command=SaveArgs)
  f0.but.2 <- ttkbutton(f0, width=12, text="Cancel",
                        command=function() tclvalue(tt.done.var) <- 1)
  f0.but.3 <- ttkbutton(f0, width=12, text="Help",
                        command=function() {
                          print(utils::help("SetCrs", package="RSurvey"))
                        })
  tkgrid("x", f0.but.1, f0.but.2, f0.but.3)
  tkgrid.columnconfigure(f0, 0, weight=1)
  tkgrid.configure(f0.but.1, f0.but.2, f0.but.3, padx=c(4, 0), pady=c(15, 10))
  tkgrid.configure(f0.but.3, padx=c(4, 10))
  tkpack(f0, fill="x", side="bottom", anchor="e")

  # frame 1
  f1 <- ttkframe(tt, relief="flat")
  txt <- paste("The coordinate reference system (CRS) defines a map projection and datum.",
               "Using PROJ.4 arguments, define a global and project-wide CRS below.",
               "When a new dataset is created, or when data is loaded that has no CRS, the global CRS is used.",
               "If a loaded dataset has a different CRS, it is automatically reprojected to the global CRS.")
  f1.lab.1.1 <- ttklabel(f1, text=txt, wraplength=500)
  f1.txt.2.1 <- tktext(f1, width=50, height=3, undo=1, wrap="word",
                       relief="flat", foreground="black", background="#FFFFFF",
                       borderwidth=1, font="TkTextFont", state="normal")
  tkgrid(f1.lab.1.1, sticky="w", pady=c(10, 4))
  tkgrid(f1.txt.2.1, sticky="we", padx=c(0, 2))
  tkgrid.columnconfigure(f1, 0, weight=1, minsize=10)
  tkpack(f1, fill="both", expand="yes", padx=10)

   tkinsert(f1.txt.2.1, "end", as.character(crs@projargs))

  # bind events
  tclServiceMode(TRUE)

  tkbind("Text", "<Control-KeyPress-z>", EditUndo)
  tkbind("Text", "<Control-KeyPress-y>", EditRedo)
  tkbind("Text", "<Control-KeyPress-v>", EditPaste)
  tkbind("Text", "<Control-KeyPress-l>", ClearConsole)
  tkbind("Text", "<Control-KeyPress-a>", EditSelectAll)

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
