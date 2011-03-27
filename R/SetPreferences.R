SetPreferences <- function(parent=NULL) {
  # A GUI for specifying the interpolation parameters.

  # Additional functions (subroutines)

  # Update parameter values

  UpdatePar <- function() {
    vars <- c("grid.dx", "grid.dy", "mba.n", "mba.m", "mba.h")

    old <- sapply(vars, function(i) Data(i))

    tmp <- as.numeric(tclvalue(grid.dx.var))
    Data("grid.dx", if (is.na(tmp)) NULL else tmp)

    tmp <- as.numeric(tclvalue(grid.dy.var))
    Data("grid.dy", if (is.na(tmp)) NULL else tmp)

    tmp <- as.integer(tclvalue(mba.n.var))
    Data("mba.n", if (is.na(tmp)) NULL else tmp)

    tmp <- as.integer(tclvalue(mba.m.var))
    Data("mba.m", if (is.na(tmp)) NULL else tmp)

    tmp <- as.integer(tclvalue(mba.h.var))
    Data("mba.h", if (is.na(tmp)) NULL else tmp)

    new <- sapply(vars, function(i) Data(i))

    if (!identical(old, new))
      Data("data.grd", NULL)

    tclvalue(tt.done.var) <- 1
  }


  # Main program

  # Assign the variables linked to Tk widgets

  grid.dx.var <- tclVar()
  grid.dy.var <- tclVar()
  mba.n.var   <- tclVar()
  mba.m.var   <- tclVar()
  mba.h.var   <- tclVar()

  if (!is.null(Data("grid.dx")))
    tclvalue(grid.dx.var) <- Data("grid.dx")
  if (!is.null(Data("grid.dy")))
    tclvalue(grid.dy.var) <- Data("grid.dy")

  if (!is.null(Data("mba.n")))
    tclvalue(mba.n.var) <- Data("mba.n")
  if (!is.null(Data("mba.m")))
    tclvalue(mba.m.var) <- Data("mba.m")
  if (!is.null(Data("mba.h")))
    tclvalue(mba.h.var) <- Data("mba.h")

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
  tktitle(tt) <- "Preferences"

  tkwm.resizable(tt, 1, 0)

  # Frame 0 contains ok and cancel buttons

  frame0 <- tkframe(tt, relief="flat", padx=0, pady=0)

  frame0.but.1 <- ttkbutton(frame0, width=12, text="OK",
                            command=UpdatePar)
  frame0.but.2 <- ttkbutton(frame0, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)

  tkgrid(frame0.but.1, frame0.but.2)

  tkgrid.configure(frame0.but.1, sticky="e", padx=c(4, 0), pady=c(5, 8))
  tkgrid.configure(frame0.but.2, sticky="w", padx=c(4, 8), pady=c(5, 8),
                   rowspan=2)

  tkpack(frame0, side="bottom", anchor="e")

  # Frame 1 contains interpolation parameteres

  frame1 <- ttkframe(tt, relief="flat", borderwidth=5, padding=8)

  txt <- "Interpolated-grid spacing along the x axis"
  frame1.lab.1.1 <- ttklabel(frame1, text=txt)
  txt <- "Interpolated-grid spacing along the y axis"
  frame1.lab.2.1 <- ttklabel(frame1, text=txt)

  frame1.ent.1.2 <- ttkentry(frame1, width=15, textvariable=grid.dx.var)
  frame1.ent.2.2 <- ttkentry(frame1, width=15, textvariable=grid.dy.var)

  tkbind(frame1.ent.1.2, "<KeyRelease>",
         function() {
           tclvalue(grid.dx.var) <- CheckEntry("numeric", tclvalue(grid.dx.var))
         })
  tkbind(frame1.ent.1.2, "<KeyRelease>",
         function() {
           tclvalue(grid.dy.var) <- CheckEntry("numeric", tclvalue(grid.dy.var))
         })

  tkgrid(frame1.lab.1.1, frame1.ent.1.2, padx=1, pady=1)
  tkgrid(frame1.lab.2.1, frame1.ent.2.2, padx=1, pady=1)

  tkgrid.configure(frame1.lab.1.1, frame1.lab.2.1, sticky="e")
  tkgrid.configure(frame1.ent.1.2, frame1.ent.2.2, sticky="we")

  tkgrid.columnconfigure(frame1, 1, weight=1, minsize=20)

  tkpack(frame1, fill="both", expand="yes", padx=20, pady=2)

  # Frame 2 contains MBA input parameters

  frame2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                          text="Multilevel B-spline approximation")

  txt <- "Initial size of the spline space along the x axis"
  frame2.lab.1.1 <- ttklabel(frame2, text=txt)
  txt <- "Initial size of the spline space along the y axis"
  frame2.lab.2.1 <- ttklabel(frame2, text=txt)
  txt <- "Number of levels in the hierarchical construction"
  frame2.lab.3.1 <- ttklabel(frame2, text=txt)

  frame2.ent.1.2 <- ttkentry(frame2, width=15, textvariable=mba.n.var)
  frame2.ent.2.2 <- ttkentry(frame2, width=15, textvariable=mba.m.var)
  frame2.ent.3.2 <- ttkentry(frame2, width=15, textvariable=mba.h.var)

  tkgrid(frame2.lab.1.1, frame2.ent.1.2, padx=1, pady=1)
  tkgrid(frame2.lab.2.1, frame2.ent.2.2, padx=1, pady=1)
  tkgrid(frame2.lab.3.1, frame2.ent.3.2, padx=1, pady=1)

  tkgrid.configure(frame2.lab.1.1, frame2.lab.2.1, frame2.lab.3.1, sticky="e")
  tkgrid.configure(frame2.ent.1.2, frame2.ent.2.2, frame2.ent.3.2, sticky="we")

  tkgrid.columnconfigure(frame2, 1, weight=1, minsize=20)

  tkpack(frame2, fill="both", expand="yes", padx=10, pady=c(0, 10))

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
}
