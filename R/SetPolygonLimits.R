SetPolygonLimits <- function(poly.names=NULL, poly.data=NULL, poly.crop=NULL,
                             parent=NULL) {
  # A GUI for specifying polygon limits.

  # Additional functions (subroutines)

  # Save new polygons

  SaveNames <- function() {
    box1 <- as.character(tclvalue(data.var))
    if(box1 == "")
      box1 <- NULL
    box2 <- as.character(tclvalue(crop.var))
    if(box2 == "")
      box2 <- NULL
    rtn <<- list(poly.data=box1, poly.crop=box2)
    tclvalue(tt.done.var) <- 1
  }


  # Main program

  poly.names <- c("", poly.names)
  if (!is.null(poly.data) && !(poly.data %in% poly.names))
    poly.data <- NULL
  if (!is.null(poly.crop) && !(poly.crop %in% poly.names))
    poly.crop <- NULL

  rtn <- NULL

  # Assign the variables linked to Tk widgets

  names.var <- tclVar("")
  data.var  <- tclVar("")
  crop.var  <- tclVar("")

  tt.done.var <- tclVar(0)

  # Open GUI

  tclServiceMode(FALSE)
  tt <- tktoplevel(padx=0, pady=0)

  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(geo[2]) + 25,
                            "+", as.integer(geo[3]) + 25, sep=""))
  }

  tktitle(tt) <- "Polygon Limits"

  tkwm.resizable(tt, 1, 0)

  # Frame 0

  frame0 <- tkframe(tt, relief="flat", padx=0, pady=0)

  frame0.but.1 <- ttkbutton(frame0, width=12, text="OK",
                            command=SaveNames)
  frame0.but.2 <- ttkbutton(frame0, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)

  tkgrid(frame0.but.1, frame0.but.2)

  tkgrid.configure(frame0.but.1, sticky="e", padx=c(4, 0), pady=c(5, 8))
  tkgrid.configure(frame0.but.2, sticky="w", padx=c(4, 8), pady=c(5, 8),
                   rowspan=2)

  tkpack(frame0, side="bottom", anchor="e")

  # Frame 1

  frame1 <- ttkframe(tt, relief="flat", borderwidth=5, padding=8)

  frame1.lab.1.1 <- tklabel(frame1, text="Boundary defining data limits")
  frame1.lab.2.1 <- tklabel(frame1, text="Crop region for interpolated surface")

  vals <- poly.names
  if (length(vals) == 1)
    vals <- paste("{", vals, "}", sep="")

  frame1.box.1.2 <- ttkcombobox(frame1, state="readonly",
                                textvariable=data.var, values=vals)
  frame1.box.2.2 <- ttkcombobox(frame1, state="readonly",
                                textvariable=crop.var, values=vals)

  if (!is.null(poly.data))
    tcl(frame1.box.1.2, "current", match(poly.data, poly.names) - 1)

  if (!is.null(poly.crop))
    tcl(frame1.box.2.2, "current", match(poly.crop, poly.names) - 1)

  tkgrid(frame1.lab.1.1, frame1.box.1.2, pady=0)
  tkgrid(frame1.lab.2.1, frame1.box.2.2, pady=5)

  tkgrid.configure(frame1.lab.1.1, frame1.lab.2.1, sticky="e")
  tkgrid.configure(frame1.box.1.2, frame1.box.2.2, sticky="we")

  tkgrid.columnconfigure(frame1, 1, weight=1, minsize=25)

  tkpack(frame1, fill="x", expand=TRUE, padx=2, pady=2)

  # GUI control

  tkfocus(tt)
  tkgrab(tt)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tclServiceMode(TRUE)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)

  if (!is.null(parent))
    tkfocus(parent)

  tclServiceMode(TRUE)

  rtn
}
