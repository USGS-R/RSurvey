#' GUI: Polygon Limits
#'
#' A graphical user interface (\acronym{GUI}) for specifying polygon limits.
#'
#' @param poly.names character.
#'   Vector of polygon names
#' @param poly.data character.
#'   Name of the polygon that defines the data limits boundary.
#' @param poly.crop character.
#'   Name of the polygon that defines the crop region for interpolated data.
#' @param parent tkwin.
#'   \acronym{GUI} parent window
#'
#' @return Queries and sets the following components of \code{\link{Data}}:
#'   \item{credit}{mapping credit note}
#'   \item{explanation}{explanation of gridded-data values.}
#'   \item{legend.title}{title to be placed at the top of the points legend.}
#'   \item{legend.subtitle}{subtitle to be placed at the top of the points legend.}
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
#'   SetPolygonLimits(c("Polygon1", "Polygon2", "Polygon3"))
#' }
#'

SetPolygonLimits <- function(poly.names=NULL, poly.data=NULL, poly.crop=NULL, parent=NULL) {


  # save new polygons
  SaveNames <- function() {
    box1 <- as.character(tclvalue(data.var))
    if(box1 == "") box1 <- NULL
    box2 <- as.character(tclvalue(crop.var))
    if(box2 == "") box2 <- NULL
    rtn <<- list(poly.data=box1, poly.crop=box2)
    tclvalue(tt.done.var) <- 1
  }


  poly.names <- c("", poly.names)
  if (!is.null(poly.data) && !poly.data %in% poly.names) poly.data <- NULL
  if (!is.null(poly.crop) && !poly.crop %in% poly.names) poly.crop <- NULL

  rtn <- NULL

  # assign the variables linked to tk widgets
  names.var   <- tclVar("")
  data.var    <- tclVar("")
  crop.var    <- tclVar("")
  tt.done.var <- tclVar(0)

  # open gui
  tclServiceMode(FALSE)
  tt <- tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    geo <- as.integer(geo[2:3]) + 25
    tkwm.geometry(tt, sprintf("+%s+%s", geo[1], geo[2]))
  }
  tktitle(tt) <- "Polygon Limits"
  tkwm.resizable(tt, 1, 0)

  # frame 0
  f0 <- tkframe(tt, relief="flat")

  f0.but.2 <- ttkbutton(f0, width=12, text="OK", command=SaveNames)
  f0.but.3 <- ttkbutton(f0, width=12, text="Cancel",
                        command=function() tclvalue(tt.done.var) <- 1)
  f0.but.4 <- ttkbutton(f0, width=12, text="Help",
                        command=function() {
                          print(utils::help("SetPolygonLimits", package="RSurvey"))
                        })
  tkgrid("x", f0.but.2, f0.but.3, f0.but.4, sticky="se", pady=10, padx=c(4, 0))
  tkgrid.columnconfigure(f0, 0, weight=1)
  tkgrid.configure(f0.but.4, padx=c(4, 10))
  tkpack(f0, fill="x", side="bottom", anchor="e")

  # frame 1
  f1 <- ttkframe(tt, relief="flat")

  f1.lab.1.1 <- tklabel(f1, text="Boundary defining data limits")
  f1.lab.2.1 <- tklabel(f1, text="Crop region for interpolated surface")

  vals <- poly.names
  if (length(vals) == 1) vals <- paste0("{", vals, "}")

  f1.box.1.2 <- ttkcombobox(f1, state="readonly", textvariable=data.var, values=vals)
  f1.box.2.2 <- ttkcombobox(f1, state="readonly", textvariable=crop.var, values=vals)

  if (!is.null(poly.data)) tcl(f1.box.1.2, "current", match(poly.data, poly.names) - 1)
  if (!is.null(poly.crop)) tcl(f1.box.2.2, "current", match(poly.crop, poly.names) - 1)

  tkgrid(f1.lab.1.1, f1.box.1.2, pady=c(20, 10))
  tkgrid(f1.lab.2.1, f1.box.2.2, pady=c(0, 10))

  tkgrid.configure(f1.lab.1.1, f1.lab.2.1, padx=c(0, 4), sticky="w")
  tkgrid.configure(f1.box.1.2, f1.box.2.2, sticky="we")

  tkgrid.columnconfigure(f1, 1, weight=1, minsize=25)

  tkpack(f1, fill="x", expand=TRUE, padx=10)

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
