DefineInterpGrid <- function(grid.res=NULL, grid.geo=NULL, parent=NULL) {


  # update parameter values
  UpdatePar <- function() {
    opt <- as.integer(tclvalue(opt.var))
    if (opt == 1 || opt == 2) grid.geo <- NULL
    if (opt == 1 || opt == 3) grid.res <- NULL

    if (opt == 2) {
     grid.res <- c(as.numeric(tclvalue(xres.var)), as.numeric(tclvalue(yres.var)))
      if (any(is.na(grid.res))) {
        tkmessageBox(icon="error", title="Error", type="ok", parent=tt,
                     message="All grid spacing fields are required.")
        return()
      }
    } else if (opt == 3) {
      nrows <- as.integer(tclvalue(nrow.var))
      ncols <- as.integer(tclvalue(ncol.var))
      xmn   <- as.numeric(tclvalue(xmin.var))
      xmx   <- as.numeric(tclvalue(xmax.var))
      ymn   <- as.numeric(tclvalue(ymin.var))
      ymx   <- as.numeric(tclvalue(ymax.var))
      if (any(is.na(c(nrows, ncols, xmn, xmx, ymn, ymx)))) {
        tkmessageBox(icon="error", title="Error", type="ok", parent=tt,
                     message="All grid geometry fields are required.")
        return()
      }
      ans <- try(raster::raster(nrows=nrows, ncols=ncols,
                                     xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx,
                                     crs=sp::CRS(as.character(NA)), vals=NULL),
                                     silent=TRUE)
      if (inherits(ans, "try-error")) {
        tkmessageBox(icon="error", detail=ans, title="Error", type="ok", parent=tt,
                     message="Problem with grid geometry.")
        return()
      }
      grid.geo <- ans
    }
    rtn <<- list(grid.res=grid.res, grid.geo=grid.geo)
    tclvalue(tt.done.var) <- 1
  }


  # toggle widget state
  ToggleState <- function() {

    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE))

    opt <- as.integer(tclvalue(opt.var))

    s <- ifelse(opt == 2, "normal", "disabled")
    tkconfigure(f2.lab.1.1, state=s)
    tkconfigure(f2.lab.2.1, state=s)

    s <- ifelse(opt == 2, "normal", "readonly")
    tkconfigure(f2.ent.1.2, state=s)
    tkconfigure(f2.ent.2.2, state=s)

    s <- ifelse(opt == 3, "normal", "disabled")
    tkconfigure(f2.lab.3.1, state=s)
    tkconfigure(f2.lab.4.1, state=s)
    tkconfigure(f2.lab.5.1, state=s)
    tkconfigure(f2.lab.6.1, state=s)
    tkconfigure(f2.lab.7.1, state=s)
    tkconfigure(f2.lab.8.1, state=s)

    s <- ifelse(opt == 3, "normal", "readonly")
    tkconfigure(f2.ent.3.2, state=s)
    tkconfigure(f2.ent.4.2, state=s)
    tkconfigure(f2.ent.5.2, state=s)
    tkconfigure(f2.ent.6.2, state=s)
    tkconfigure(f2.ent.7.2, state=s)
    tkconfigure(f2.ent.8.2, state=s)
  }


  rtn <- NULL

  # assign variables linked to tk widgets
  opt.var     <- tclVar(1)
  xres.var    <- tclVar()
  yres.var    <- tclVar()
  ncol.var    <- tclVar()
  nrow.var    <- tclVar()
  xmin.var    <- tclVar()
  xmax.var    <- tclVar()
  ymin.var    <- tclVar()
  ymax.var    <- tclVar()
  tt.done.var <- tclVar(0)

  if (!is.null(grid.res) && inherits(grid.res, "numeric") && length(grid.res) == 2) {
    tclvalue(xres.var) <- grid.res[1]
    tclvalue(yres.var) <- grid.res[2]
    tclvalue(opt.var) <- 2
  }

  if (!is.null(grid.geo) && inherits(grid.geo, "RasterLayer")) {
    tclvalue(ncol.var) <- raster::ncol(grid.geo)
    tclvalue(nrow.var) <- raster::nrow(grid.geo)
    tclvalue(xmin.var) <- raster::xmin(grid.geo)
    tclvalue(xmax.var) <- raster::xmax(grid.geo)
    tclvalue(ymin.var) <- raster::ymin(grid.geo)
    tclvalue(ymax.var) <- raster::ymax(grid.geo)
    tclvalue(opt.var) <- 3
  }

  # open gui
  tclServiceMode(FALSE)
  tt <- tktoplevel()
  if (!is.null(parent)) {
      tkwm.transient(tt, parent)
      geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
      tkwm.geometry(tt, paste0("+", as.integer(geo[2]) + 25,
                               "+", as.integer(geo[3]) + 25))
  }
  tktitle(tt) <- "Interpolation Grid"
  tkwm.resizable(tt, 1, 0)

  # frame 0, ok and cancel buttons
  f0 <- tkframe(tt, relief="flat")

  f0.but.2 <- ttkbutton(f0, width=12, text="OK", command=UpdatePar)
  f0.but.3 <- ttkbutton(f0, width=12, text="Cancel",
                        command=function() tclvalue(tt.done.var) <- 1)

  f0.but.4 <- ttkbutton(f0, width=12, text="Help",
                        command=function() {
                          print(help("DefineInterpGrid", package="RSurvey"))
                        })
  tkgrid("x", f0.but.2, f0.but.3, f0.but.4, pady=c(15, 10), padx=c(4, 0))
  tkgrid.columnconfigure(f0, 0, weight=1)
  tkgrid.configure(f0.but.4, padx=c(4, 10))
  tkpack(f0, fill="x", side="bottom", anchor="e")

  # frame 1, grid options
  f1 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                      text="Define grid using")
  f1.rbt.1.1 <- ttkradiobutton(f1, variable=opt.var, value=1, text="Defaults",
                               command=ToggleState)
  f1.rbt.1.2 <- ttkradiobutton(f1, variable=opt.var, value=2, text="Cell resolution",
                               command=ToggleState)
  f1.rbt.1.3 <- ttkradiobutton(f1, variable=opt.var, value=3, text="Explicit geometry",
                               command=ToggleState)
  tkgrid(f1.rbt.1.1, f1.rbt.1.2, f1.rbt.1.3)
  tkgrid.configure(f1.rbt.1.2, padx=10)
  tkpack(f1, fill="x", padx=10, pady=10)

  # frame 2
  f2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                      text="Grid parameters")

  f2.lab.1.1 <- ttklabel(f2, text="Grid spacing along x-axis")
  f2.lab.2.1 <- ttklabel(f2, text="Grid spacing along y-axis")
  f2.lab.3.1 <- ttklabel(f2, text="Number of intervals along x-axis")
  f2.lab.4.1 <- ttklabel(f2, text="Number of intervals along y-axis")
  f2.lab.5.1 <- ttklabel(f2, text="Minimum x-coordinate")
  f2.lab.6.1 <- ttklabel(f2, text="Maximum x-coordinate")
  f2.lab.7.1 <- ttklabel(f2, text="Minimum y-coordinate")
  f2.lab.8.1 <- ttklabel(f2, text="Maximum y-coordinate")

  f2.ent.1.2 <- ttkentry(f2, width=15, textvariable=xres.var)
  f2.ent.2.2 <- ttkentry(f2, width=15, textvariable=yres.var)
  f2.ent.3.2 <- ttkentry(f2, width=15, textvariable=ncol.var)
  f2.ent.4.2 <- ttkentry(f2, width=15, textvariable=nrow.var)
  f2.ent.5.2 <- ttkentry(f2, width=15, textvariable=xmin.var)
  f2.ent.6.2 <- ttkentry(f2, width=15, textvariable=xmax.var)
  f2.ent.7.2 <- ttkentry(f2, width=15, textvariable=ymin.var)
  f2.ent.8.2 <- ttkentry(f2, width=15, textvariable=ymax.var)

  tkgrid(f2.lab.1.1, f2.ent.1.2, pady=c(0, 4))
  tkgrid(f2.lab.2.1, f2.ent.2.2, pady=c(0, 12))
  tkgrid(f2.lab.3.1, f2.ent.3.2, pady=c(0, 4))
  tkgrid(f2.lab.4.1, f2.ent.4.2, pady=c(0, 4))
  tkgrid(f2.lab.5.1, f2.ent.5.2, pady=c(0, 4))
  tkgrid(f2.lab.6.1, f2.ent.6.2, pady=c(0, 4))
  tkgrid(f2.lab.7.1, f2.ent.7.2, pady=c(0, 4))
  tkgrid(f2.lab.8.1, f2.ent.8.2, pady=c(0, 4))

  tkgrid.configure(f2.lab.1.1, f2.lab.2.1, f2.lab.3.1, f2.lab.4.1,
                   f2.lab.5.1, f2.lab.6.1, f2.lab.7.1, f2.lab.8.1,
                   sticky="w", padx=c(0, 2))
  tkgrid.configure(f2.ent.1.2, f2.ent.2.2, f2.ent.3.2, f2.ent.4.2,
                   f2.ent.5.2, f2.ent.6.2, f2.ent.7.2, f2.ent.8.2,
                   sticky="we")

  tkgrid.columnconfigure(f2, 1, weight=1, minsize=20)
  tkpack(f2, fill="both", expand="yes", padx=10)

  # bind events
  tclServiceMode(TRUE)
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tkbind(f2.ent.1.2, "<KeyRelease>",
         function() {tclvalue(xres.var) <- CheckEntry("numeric", tclvalue(xres.var))})
  tkbind(f2.ent.1.2, "<KeyRelease>",
         function() {tclvalue(yres.var) <- CheckEntry("numeric", tclvalue(yres.var))})
  tkbind(f2.ent.3.2, "<KeyRelease>",
         function() {tclvalue(ncol.var) <- CheckEntry("integer", tclvalue(ncol.var))})
  tkbind(f2.ent.4.2, "<KeyRelease>",
         function() {tclvalue(nrow.var) <- CheckEntry("integer", tclvalue(nrow.var))})
  tkbind(f2.ent.5.2, "<KeyRelease>",
         function() {tclvalue(xmin.var) <- CheckEntry("numeric", tclvalue(xmin.var))})
  tkbind(f2.ent.6.2, "<KeyRelease>",
         function() {tclvalue(xmax.var) <- CheckEntry("numeric", tclvalue(xmax.var))})
  tkbind(f2.ent.7.2, "<KeyRelease>",
         function() {tclvalue(ymin.var) <- CheckEntry("numeric", tclvalue(ymin.var))})
  tkbind(f2.ent.8.2, "<KeyRelease>",
         function() {tclvalue(ymax.var) <- CheckEntry("numeric", tclvalue(ymax.var))})

  # gui control
  ToggleState()

  tkfocus(tt)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  return(rtn)
}
