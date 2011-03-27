AutocropPolygon <- function(parent=NULL) {
  # A GUI for specify input parameters for the Autocrop function.

  # Additional functions (subroutines)

  # Run Autocrop function and plot results

  DrawPolygon <- function() {
    tkconfigure(tt, cursor="watch")

    max.len <- as.numeric(tclvalue(max.len.var))
    max.itr <- as.integer(tclvalue(max.itr.var))

    if (is.na(max.len) || max.len > default.len) {
      tclvalue(max.len.var) <- format(default.len)
      max.len <- default.len
    }
    if (is.na(max.itr)) {
      tclvalue(max.itr.var) <- format(default.itr)
      max.itr <- default.itr
    }

    ply <<- Autocrop(mesh, max.len, max.itr)

    if (is.null(ply)) {
      msg <- "Autocrop failed, try increasing the maximum outer arc length."
      tkmessageBox(icon="warning", message=msg, title="Warning", type="ok",
                   parent=tt)
    } else {
      if (is.null(dev) || dev.cur() == 1)
        DrawBasePlot()

      if (!is.null(old.ply))
        plot(old.ply, add=TRUE, poly.args=list(border="black", lty=3))
      plot(ply, add=TRUE, poly.args=list(border="red", lty=3))
      old.ply <<- ply
    }

    tkfocus(tt)
    tkconfigure(tt, cursor="arrow")
  }

  # Draw base plot and points

  DrawBasePlot <- function() {
    PlotSurface2d(dat, type="p", xlab=xlab, ylab=ylab, zlab=zlab, asp=asp,
                  csi=csi, width=width, nlevels=nlevels, cex.pts=cex.pts,
                  rkey=rkey)
    dev <<- dev.cur()
  }

  # Refresh plot

  RefreshPlot <- function() {
    if (is.null(dev))
      return()

    dev.off(which=dev)
    DrawBasePlot()

    dev <<- dev.cur()
    ply <<- NULL
    old.ply <<- NULL
  }


  # Main program

  # Initialize parameters

  dat  <- Data("data.pts")
  vars <- Data("vars")
  cols <- Data("cols")

  xlab <- cols[[vars$x]]$id
  ylab <- cols[[vars$y]]$id
  zlab <- cols[[vars$z]]$id

  asp <- Data("asp.yx")
  csi <- Data("csi")
  width <- Data("width")
  nlevels <- Data("nlevels")
  cex.pts <- Data("cex.pts")
  rkey <- Data("rkey")

  dat$vx <- NULL
  dat$vy <- NULL

  ply <- NULL
  rtn <- NULL
  dev <- NULL
  old.ply <- NULL

  # Construct mesh

  mesh <- tri.mesh(dat$x, dat$y, duplicate="remove")

  # Convex hull and maximum outer arc length

  hull <- convex.hull(mesh)
  x1 <- hull$x
  y1 <- hull$y
  x2 <- c(x1[-1], x1[1])
  y2 <- c(y1[-1], y1[1])
  default.len <- max(sqrt((x2 - x1)^2 + (y2 - y1)^2))
  default.itr <- 10000

  # Assign the variables linked to Tk widgets

  max.len.var <- tclVar(format(default.len))
  max.itr.var <- tclVar(format(default.itr))

  tt.done.var <- tclVar(0)

  # Open GUI

  tclServiceMode(FALSE)
  tt <- tktoplevel(padx=0, pady=0)
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    tmp <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(tmp[2]) + 25,
                            "+", as.integer(tmp[3]) + 25, sep=""))
  }
  tktitle(tt) <- "Autocrop Region"
  tkwm.resizable(tt, 1, 0)

  # Frame 0 contains buttons

  frame0 <- ttkframe(tt, relief="flat", borderwidth=2)

  frame0.but.2.1 <- ttkbutton(frame0, width=10, text="Build",
                              command=DrawPolygon)

  frame0.but.2.2 <- ttkbutton(frame0, width=10, text="Refresh",
                              command=RefreshPlot)
  frame0.but.2.3 <- ttkbutton(frame0, width=10, text="Save",
                              command=function() {
                                if (inherits(ply, "gpc.poly")) {
                                  rtn <<- ply
                                  tclvalue(tt.done.var) <- 1
                                }
                              })
  frame0.but.2.4 <- ttkbutton(frame0, width=10, text="Cancel",
                              command=function() tclvalue(tt.done.var) <- 1)

  tkgrid(frame0.but.2.1, frame0.but.2.2, frame0.but.2.3, frame0.but.2.4,
         pady=c(6, 8), padx=c(4, 0))

  tkgrid.configure(frame0.but.2.2, padx=4)
  tkgrid.configure(frame0.but.2.4, padx=c(4, 8))

  tcl("grid", "anchor", frame0, "center")

  tkpack(frame0, side="bottom", anchor="e")

  # Frame 1 contains parameters

  frame1 <- ttkframe(tt, relief="flat", borderwidth=5, padding=8)

  frame1.lab.1.1 <- ttklabel(frame1, text="Maximum arc length")
  frame1.lab.2.1 <- ttklabel(frame1, text="Maximum number of iterations")

  frame1.ent.1.2 <- ttkentry(frame1, width=20, textvariable=max.len.var)
  frame1.ent.2.2 <- ttkentry(frame1, width=20, textvariable=max.itr.var)

  tkbind(frame1.ent.1.2, "<KeyRelease>",
         function() {
           tclvalue(max.len.var) <- CheckEntry("numeric", tclvalue(max.len.var))
         })
  tkbind(frame1.ent.1.2, "<KeyRelease>",
         function() {
           tclvalue(max.itr.var) <- CheckEntry("integer", tclvalue(max.itr.var))
         })

  tkgrid(frame1.lab.1.1, frame1.ent.1.2, padx=2, pady=2)
  tkgrid(frame1.lab.2.1, frame1.ent.2.2, padx=2, pady=2)

  tkgrid.configure(frame1.lab.1.1, sticky="e")
  tkgrid.configure(frame1.lab.2.1, sticky="e")

  tkgrid.configure(frame1.ent.1.2, sticky="we")
  tkgrid.configure(frame1.ent.2.2, sticky="we")

  tkgrid.columnconfigure(frame1, 1, weight=1, minsize=15)

  tcl("grid", "anchor", frame1, "center")

  tkpack(frame1, fill="both")

  # GUI control

  tkfocus(tt)
  tkgrab(tt)
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tclServiceMode(TRUE)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  rtn
}
