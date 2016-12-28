SetAxesLimits <- function(lim=NULL, parent=NULL) {


  # update limits
  UpdateLimits <- function() {
    d <- list()

    d$x1 <- as.numeric(tclvalue(x1.var))
    d$x2 <- as.numeric(tclvalue(x2.var))
    d$y1 <- as.numeric(tclvalue(y1.var))
    d$y2 <- as.numeric(tclvalue(y2.var))
    d$z1 <- as.numeric(tclvalue(z1.var))
    d$z2 <- as.numeric(tclvalue(z2.var))

    if (is.na(d$x1))
      d$x1 <- d$x1.chk <- NULL
    else
      d$x1.chk <- as.integer(tclvalue(x1.chk.var))
    if (is.na(d$x2))
      d$x2 <- d$x2.chk <- NULL
    else
      d$x2.chk <- as.integer(tclvalue(x2.chk.var))
    if (is.na(d$y1))
      d$y1 <- d$y1.chk <- NULL
    else
      d$y1.chk <- as.integer(tclvalue(y1.chk.var))
    if (is.na(d$y2))
      d$y2 <- d$y2.chk <- NULL
    else
      d$y2.chk <- as.integer(tclvalue(y2.chk.var))
    if (is.na(d$z1))
      d$z1 <- d$z1.chk <- NULL
    else
      d$z1.chk <- as.integer(tclvalue(z1.chk.var))
    if (is.na(d$z2))
      d$z2 <- d$z2.chk <- NULL
    else
      d$z2.chk <- as.integer(tclvalue(z2.chk.var))

    d$x <- c(if (!is.null(d$x1) && !d$x1.chk) d$x1 else NA,
             if (!is.null(d$x2) && !d$x2.chk) d$x2 else NA)
    d$y <- c(if (!is.null(d$y1) && !d$y1.chk) d$y1 else NA,
             if (!is.null(d$y2) && !d$y2.chk) d$y2 else NA)
    d$z <- c(if (!is.null(d$z1) && !d$z1.chk) d$z1 else NA,
             if (!is.null(d$z2) && !d$z2.chk) d$z2 else NA)

    tclvalue(tt.done.var) <- 1
    new <<- d
  }


  new <- lim

  # assign variables linked to tk widgets
  if (is.null(lim)) lim <- list()

  x1.var <- if (is.null(lim$x1)) tclVar() else tclVar(lim$x1)
  x2.var <- if (is.null(lim$x2)) tclVar() else tclVar(lim$x2)
  y1.var <- if (is.null(lim$y1)) tclVar() else tclVar(lim$y1)
  y2.var <- if (is.null(lim$y2)) tclVar() else tclVar(lim$y2)
  z1.var <- if (is.null(lim$z1)) tclVar() else tclVar(lim$z1)
  z2.var <- if (is.null(lim$z2)) tclVar() else tclVar(lim$z2)

  x1.chk <- if (is.null(lim$x1)) 1 else lim$x1.chk
  x2.chk <- if (is.null(lim$x2)) 1 else lim$x2.chk
  y1.chk <- if (is.null(lim$y1)) 1 else lim$y1.chk
  y2.chk <- if (is.null(lim$y2)) 1 else lim$y2.chk
  z1.chk <- if (is.null(lim$z1)) 1 else lim$z1.chk
  z2.chk <- if (is.null(lim$z2)) 1 else lim$z2.chk

  x1.chk.var <- tclVar(x1.chk)
  x2.chk.var <- tclVar(x2.chk)
  y1.chk.var <- tclVar(y1.chk)
  y2.chk.var <- tclVar(y2.chk)
  z1.chk.var <- tclVar(z1.chk)
  z2.chk.var <- tclVar(z2.chk)

  x1.sta.var <- if (x1.chk) tclVar("disabled") else tclVar("normal")
  x2.sta.var <- if (x2.chk) tclVar("disabled") else tclVar("normal")
  y1.sta.var <- if (y1.chk) tclVar("disabled") else tclVar("normal")
  y2.sta.var <- if (y2.chk) tclVar("disabled") else tclVar("normal")
  z1.sta.var <- if (z1.chk) tclVar("disabled") else tclVar("normal")
  z2.sta.var <- if (z2.chk) tclVar("disabled") else tclVar("normal")

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
  tktitle(tt) <- "Axes Limits"
  tkwm.resizable(tt, 1, 0)

  # frame 0 contains ok and cancel buttons
  f0 <- tkframe(tt, relief="flat")
  f0.but.2 <- ttkbutton(f0, width=12, text="OK", command=UpdateLimits)
  f0.but.3 <- ttkbutton(f0, width=12, text="Cancel",
                        command=function() tclvalue(tt.done.var) <- 1)
  f0.but.4 <- ttkbutton(f0, width=12, text="Help",
                        command=function() {
                          print(help("SetAxesLimits", package="RSurvey"))
                        })
  tkgrid("x", f0.but.2, f0.but.3, f0.but.4, sticky="se", pady=10, padx=c(4, 0))
  tkgrid.columnconfigure(f0, 0, weight=1)
  tkgrid.configure(f0.but.4, padx=c(4, 10))
  tkpack(f0, fill="x", side="bottom", anchor="e")

  # notebook with tabs
  nb <- ttknotebook(tt)

  # frame 1 contains x-axis limits
  f1 <- ttkframe(nb, relief="flat", padding=10, borderwidth=2)
  tkadd(nb, f1, text="      x      ")

  f1.lab.1.1 <- ttklabel(f1, text="Minimum")
  f1.lab.2.1 <- ttklabel(f1, text="Maximum")

  f1.ent.1.2 <- ttkentry(f1, textvariable=x1.var, state=tclvalue(x1.sta.var))
  f1.ent.2.2 <- ttkentry(f1, textvariable=x2.var, state=tclvalue(x2.sta.var))

  f1.chk.1.3 <- ttkcheckbutton(f1, variable=x1.chk.var, text="Auto",
                               command=function() {
                                 if (as.integer(tclvalue(x1.chk.var))) {
                                   tclvalue(x1.sta.var) <- "disabled"
                                 } else {
                                   tclvalue(x1.sta.var) <- "normal"
                                 }
                                 tkconfigure(f1.ent.1.2, state=tclvalue(x1.sta.var))
                                 tkfocus(f1.ent.1.2)
                               })
  f1.chk.2.3 <- ttkcheckbutton(f1, variable=x2.chk.var, text="Auto",
                               command=function() {
                                 if (as.integer(tclvalue(x2.chk.var))) {
                                   tclvalue(x2.sta.var) <- "disabled"
                                 } else {
                                   tclvalue(x2.sta.var) <- "normal"
                                 }
                                 tkconfigure(f1.ent.2.2, state=tclvalue(x2.sta.var))
                                 tkfocus(f1.ent.2.2)
                               })

  tkgrid(f1.lab.1.1, f1.ent.1.2, f1.chk.1.3, padx=1, pady=2)
  tkgrid(f1.lab.2.1, f1.ent.2.2, f1.chk.2.3, padx=1, pady=2)

  tkgrid.configure(f1.lab.1.1, f1.lab.2.1, sticky="e")

  tkgrid.configure(f1.ent.1.2, f1.ent.2.2, sticky="we")

  tkgrid.columnconfigure(f1, 1, weight=1, minsize=25)

  tcl("grid", "anchor", f1, "center")

  # frame 2 contains y-axis limits
  f2 <- ttkframe(nb, relief="flat", padding=10, borderwidth=2)
  tkadd(nb, f2, text="      y      ")

  f2.lab.1.1 <- ttklabel(f2, text="Minimum")
  f2.lab.2.1 <- ttklabel(f2, text="Maximum")

  f2.ent.1.2 <- ttkentry(f2, textvariable=y1.var, state=tclvalue(y1.sta.var))
  f2.ent.2.2 <- ttkentry(f2, textvariable=y2.var, state=tclvalue(y2.sta.var))

  f2.chk.1.3 <- ttkcheckbutton(f2, variable=y1.chk.var, text="Auto",
                               command=function() {
                                 if (as.integer(tclvalue(y1.chk.var))) {
                                   tclvalue(y1.sta.var) <- "disabled"
                                 } else {
                                   tclvalue(y1.sta.var) <- "normal"
                                 }
                                 tkconfigure(f2.ent.1.2, state=tclvalue(y1.sta.var))
                                 tkfocus(f2.ent.1.2)
                               })
  f2.chk.2.3 <- ttkcheckbutton(f2, variable=y2.chk.var, text="Auto",
                               command=function() {
                                 if (as.integer(tclvalue(y2.chk.var)))
                                   tclvalue(y2.sta.var) <- "disabled"
                                 else
                                   tclvalue(y2.sta.var) <- "normal"
                                 tkconfigure(f2.ent.2.2, state=tclvalue(y2.sta.var))
                                 tkfocus(f2.ent.2.2)
                               })

  tkgrid(f2.lab.1.1, f2.ent.1.2, f2.chk.1.3, padx=1, pady=2)
  tkgrid(f2.lab.2.1, f2.ent.2.2, f2.chk.2.3, padx=1, pady=2)

  tkgrid.configure(f2.lab.1.1, f2.lab.2.1, sticky="e")
  tkgrid.configure(f2.ent.1.2, f2.ent.2.2, sticky="we")

  tkgrid.columnconfigure(f2, 1, weight=1, minsize=25)

  tcl("grid", "anchor", f2, "center")

  # frame 3 contains z-axis limits
  f3 <- ttkframe(nb, relief="flat", padding=10, borderwidth=2)
  tkadd(nb, f3, text="      z      ")

  f3.lab.1.1 <- ttklabel(f3, text="Minimum")
  f3.lab.2.1 <- ttklabel(f3, text="Maximum")

  f3.ent.1.2 <- ttkentry(f3, textvariable=z1.var, state=tclvalue(z1.sta.var))
  f3.ent.2.2 <- ttkentry(f3, textvariable=z2.var, state=tclvalue(z2.sta.var))

  f3.chk.1.3 <- ttkcheckbutton(f3, variable=z1.chk.var, text="Auto",
                               command=function() {
                                 if (as.integer(tclvalue(z1.chk.var)))
                                   tclvalue(z1.sta.var) <- "disabled"
                                 else
                                   tclvalue(z1.sta.var) <- "normal"
                                 tkconfigure(f3.ent.1.2, state=tclvalue(z1.sta.var))
                                 tkfocus(f3.ent.1.2)
                               })
  f3.chk.2.3 <- ttkcheckbutton(f3, variable=z2.chk.var, text="Auto",
                               command=function() {
                                 if (as.integer(tclvalue(z2.chk.var)))
                                   tclvalue(z2.sta.var) <- "disabled"
                                 else
                                   tclvalue(z2.sta.var) <- "normal"
                                 tkconfigure(f3.ent.2.2, state=tclvalue(z2.sta.var))
                                 tkfocus(f3.ent.2.2)
                               })

  tkgrid(f3.lab.1.1, f3.ent.1.2, f3.chk.1.3, padx=1, pady=2)
  tkgrid(f3.lab.2.1, f3.ent.2.2, f3.chk.2.3, padx=1, pady=2)

  tkgrid.configure(f3.lab.1.1, f3.lab.2.1, sticky="w")
  tkgrid.configure(f3.ent.1.2, f3.ent.2.2, sticky="we")

  tkgrid.columnconfigure(f3, 1, weight=1, minsize=25)

  tcl("grid", "anchor", f3, "center")

  # insert notebook
  tkpack(nb, fill="x", expand=TRUE, padx=10, pady=10)

  # bind events
  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tkbind(f1.ent.1.2, "<KeyRelease>",
         function() {
           tclvalue(x1.var) <- CheckEntry("numeric", tclvalue(x1.var))
         })
  tkbind(f1.ent.2.2, "<KeyRelease>",
         function() {
           tclvalue(x2.var) <- CheckEntry("numeric", tclvalue(x2.var))
         })

  tkbind(f2.ent.1.2, "<KeyRelease>",
         function() {
           tclvalue(y1.var) <- CheckEntry("numeric", tclvalue(y1.var))
         })
  tkbind(f2.ent.2.2, "<KeyRelease>",
         function() {
           tclvalue(y2.var) <- CheckEntry("numeric", tclvalue(y2.var))
         })

  tkbind(f3.ent.1.2, "<KeyRelease>",
         function() {
           tclvalue(z1.var) <- CheckEntry("numeric", tclvalue(z1.var))
         })
  tkbind(f3.ent.2.2, "<KeyRelease>",
         function() {
           tclvalue(z2.var) <- CheckEntry("numeric", tclvalue(z2.var))
         })

  # gui control
  tkfocus(tt)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  return(new)
}
