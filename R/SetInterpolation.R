SetInterpolation <- function(parent=NULL) {


  # update parameter values
  UpdatePar <- function() {
    vars <- c("grid.res", "grid.mba")

    old <- sapply(vars, function(i) Data(i))

    grid.res <- list(x=as.numeric(tclvalue(grid.dx.var)),
                     y=as.numeric(tclvalue(grid.dy.var)))
    if (all(vapply(grid.res, function(i) is.na(i), TRUE)))
      Data("grid.res", NULL)
    else
      Data("grid.res", grid.res)

    grid.mba <- list(n=as.integer(tclvalue(mba.n.var)),
                     m=as.integer(tclvalue(mba.m.var)),
                     h=as.integer(tclvalue(mba.h.var)))
    if (all(vapply(grid.mba, function(i) is.na(i), TRUE)))
      Data("grid.mba", NULL)
    else
      Data("grid.mba", grid.mba)

    new <- sapply(vars, function(i) Data(i))

    if (!identical(old, new)) Data("data.grd", NULL)

    tclvalue(tt.done.var) <- 1
  }


  # assign the variables linked to tk widgets
  grid.dx.var <- tclVar()
  grid.dy.var <- tclVar()
  mba.n.var   <- tclVar()
  mba.m.var   <- tclVar()
  mba.h.var   <- tclVar()

  grid.res <- Data("grid.res")
  if (!is.na(grid.res$x)) tclvalue(grid.dx.var) <- as.numeric(grid.res$x)
  if (!is.na(grid.res$y)) tclvalue(grid.dy.var) <- as.numeric(grid.res$y)

  grid.mba <- Data("grid.mba")
  if (!is.na(grid.mba$n)) tclvalue(mba.n.var) <- as.integer(grid.mba$n)
  if (!is.na(grid.mba$m)) tclvalue(mba.m.var) <- as.integer(grid.mba$m)
  if (!is.na(grid.mba$h)) tclvalue(mba.h.var) <- as.integer(grid.mba$h)

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
  tktitle(tt) <- "Interpolation Options"
  tkwm.resizable(tt, 1, 0)

  # frame 0, ok and cancel buttons
  f0 <- tkframe(tt, relief="flat")

  f0.but.2 <- ttkbutton(f0, width=12, text="OK", command=UpdatePar)
  f0.but.3 <- ttkbutton(f0, width=12, text="Cancel",
                        command=function() tclvalue(tt.done.var) <- 1)

  f0.but.4 <- ttkbutton(f0, width=12, text="Help",
                        command=function() {
                          print(help("SetInterpolation", package="RSurvey"))
                        })
  tkgrid("x", f0.but.2, f0.but.3, f0.but.4, pady=c(15, 10), padx=c(4, 0))
  tkgrid.columnconfigure(f0, 0, weight=1)
  tkgrid.configure(f0.but.4, padx=c(4, 10))
  tkpack(f0, fill="x", side="bottom", anchor="e")

  # frame 1, interpolation parameteres
  f1 <- ttkframe(tt, relief="flat")

  f1.lab.1.1 <- ttklabel(f1, text="Interpolated-grid spacing along the x-axis")
  f1.lab.2.1 <- ttklabel(f1, text="Interpolated-grid spacing along the y-axis")

  f1.ent.1.2 <- ttkentry(f1, width=15, textvariable=grid.dx.var)
  f1.ent.2.2 <- ttkentry(f1, width=15, textvariable=grid.dy.var)

  tkgrid(f1.lab.1.1, f1.ent.1.2, pady=c(20, 4))
  tkgrid(f1.lab.2.1, f1.ent.2.2, pady=c(0, 10))

  tkgrid.configure(f1.lab.1.1, f1.lab.2.1, sticky="w", padx=c(0, 2))
  tkgrid.configure(f1.ent.1.2, f1.ent.2.2, sticky="we")

  tkgrid.columnconfigure(f1, 1, weight=1, minsize=20)

  tkpack(f1, fill="both", expand="yes", padx=30)

  # frame 2, mba input parameters
  f2 <- ttklabelframe(tt, relief="flat", borderwidth=10, padding=0,
                      text="Multilevel B-spline approximation")

  f2.lab.1.1 <- ttklabel(f2, text="Initial size of the spline space along the x-axis")
  f2.lab.2.1 <- ttklabel(f2, text="Initial size of the spline space along the y-axis")
  f2.lab.3.1 <- ttklabel(f2, text="Number of levels in the hierarchical construction")

  f2.ent.1.2 <- ttkentry(f2, width=15, textvariable=mba.n.var)
  f2.ent.2.2 <- ttkentry(f2, width=15, textvariable=mba.m.var)
  f2.ent.3.2 <- ttkentry(f2, width=15, textvariable=mba.h.var)

  tkgrid(f2.lab.1.1, f2.ent.1.2, pady=c(0, 4))
  tkgrid(f2.lab.2.1, f2.ent.2.2, pady=c(0, 4))
  tkgrid(f2.lab.3.1, f2.ent.3.2)

  tkgrid.configure(f2.lab.1.1, f2.lab.2.1, f2.lab.3.1, sticky="w", padx=c(0, 2))
  tkgrid.configure(f2.ent.1.2, f2.ent.2.2, f2.ent.3.2, sticky="we")

  tkgrid.columnconfigure(f2, 1, weight=1, minsize=20)

  tkpack(f2, fill="both", expand="yes", padx=10, pady=c(0, 0))

  # bind events
  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tkbind(f1.ent.1.2, "<KeyRelease>",
         function() {
           tclvalue(grid.dx.var) <- CheckEntry("numeric", tclvalue(grid.dx.var))
         })
  tkbind(f1.ent.1.2, "<KeyRelease>",
         function() {
           tclvalue(grid.dy.var) <- CheckEntry("numeric", tclvalue(grid.dy.var))
         })

  # gui control
  tkfocus(tt)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)
}
