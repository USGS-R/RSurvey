AutocropRegion <- function(d, parent=NULL, ...) {


  # save polygon and exit gui
  SavePolygon <- function() {
    if (inherits(ply, "gpc.poly")) {
      rtn <<- ply
      tclvalue(tt.done.var) <- 1
    }
  }


  # run Autocrop function and plot results
  DrawPolygon <- function() {
    tkconfigure(tt, cursor="watch")
    on.exit(tkconfigure(tt, cursor="arrow"))

    max.len <- as.numeric(tclvalue(max.len.var))
    if (is.na(max.len) || max.len > default.len) {
      tclvalue(max.len.var) <- format(default.len)
      max.len <- default.len
    }

    ply <<- Autocrop(mesh, max.len)

    if (is.null(ply)) {
      msg <- "Autocrop failed, try increasing the maximum outer arc length."
      tkmessageBox(icon="warning", message=msg, title="Warning", type="ok", parent=tt)
    } else {
      if (is.null(dev) || dev.cur() == 1) DrawBasePlot()

      if (!is.null(old.ply))
        plot(old.ply, add=TRUE, poly.args=list(border="black", lty=3))
      plot(ply, add=TRUE, poly.args=list(border="red", lty=3))
      old.ply <<- ply
    }

    tkfocus(tt)
  }


  # draw base plot and points
  DrawBasePlot <- function() {
### do.call(Plot2d, append(list(x=d, type="p"), list(...)))
    dev <<- dev.cur()
  }


  # refresh plot
  RefreshPlot <- function() {
    if (is.null(dev)) return()
    dev.off(which=dev)
    DrawBasePlot()
    dev <<- dev.cur()
    ply <<- NULL
    old.ply <<- NULL
  }


  # require tripack package
  if (!requireNamespace("tripack", quietly=TRUE)) stop()

  # initialize parameters
  ply     <- NULL
  rtn     <- NULL
  dev     <- NULL
  old.ply <- NULL

  # construct mesh
  mesh <- tripack::tri.mesh(d$x, d$y, duplicate="remove")

  # convex hull and maximum outer arc length
  hull <- tripack::convex.hull(mesh)
  x1 <- hull$x
  y1 <- hull$y
  x2 <- c(x1[-1], x1[1])
  y2 <- c(y1[-1], y1[1])
  default.len <- max(sqrt((x2 - x1)^2 + (y2 - y1)^2))

  # assign the variables linked to Tk widgets
  max.len.var <- tclVar(format(default.len))
  tt.done.var <- tclVar(0)

  # open gui
  tclServiceMode(FALSE)
  tt <- tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste0("+", as.integer(geo[2]) + 25,
                             "+", as.integer(geo[3]) + 25))
  }
  tktitle(tt) <- "Autocrop Region"
  tkwm.resizable(tt, 1, 0)

  # frame 0 contains buttons
  f0 <- ttkframe(tt, relief="flat")

  f0.but.1 <- ttkbutton(f0, width=10, text="Build", command=DrawPolygon)
  f0.but.2 <- ttkbutton(f0, width=10, text="Refresh", command=RefreshPlot)

  f0.but.4 <- ttkbutton(f0, width=10, text="Save", command=SavePolygon)
  f0.but.5 <- ttkbutton(f0, width=10, text="Cancel",
                        command=function() tclvalue(tt.done.var) <- 1)
  f0.but.6 <- ttkbutton(f0, width=12, text="Help",
                        command=function() print(help("Autocrop", package="RSurvey")))

  tkgrid(f0.but.1, f0.but.2, "x", f0.but.4, f0.but.5, f0.but.6,
         pady=c(15, 10), padx=c(4, 0))

  tkgrid.columnconfigure(f0, 2, weight=1)
  tkgrid.configure(f0.but.1, padx=c(10, 0))
  tkgrid.configure(f0.but.2, padx=c(4, 25))
  tkgrid.configure(f0.but.6, padx=c(4, 10))
  tkpack(f0, fill="x", side="bottom", anchor="e")

  # frame 1 contains parameters
  f1 <- ttkframe(tt, relief="flat", borderwidth=10)

  f1.lab.1.1 <- ttklabel(f1, text="Maximum outer arc length")
  f1.ent.1.2 <- ttkentry(f1, width=20, textvariable=max.len.var)

  tkgrid(f1.lab.1.1, f1.ent.1.2, pady=c(10, 0))

  tkgrid.configure(f1.lab.1.1, sticky="e", padx=c(0, 2))
  tkgrid.configure(f1.ent.1.2, sticky="we")

  tkgrid.columnconfigure(f1, 1, weight=1, minsize=15)

  tcl("grid", "anchor", f1, "center")

  tkpack(f1, padx=50, fill="both")

  # bind events
  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)
  tkbind(f1.ent.1.2, "<KeyRelease>",
         function() {
           tclvalue(max.len.var) <- CheckEntry("numeric", tclvalue(max.len.var))
         })

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
