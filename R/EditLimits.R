EditLimits <- function(lim=NULL, win.title="Limits", parent=NULL) {
  # A GUI for specifying data and axis limits.

  # Additional functions (subroutines)

  # Update limits

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

    t1.str <- paste(tclvalue(t1d.var), " ",
                    as.integer(tclvalue(t1h.var)), ":",
                    as.integer(tclvalue(t1m.var)), ":",
                    as.numeric(tclvalue(t1s.var)), sep="")
    t2.str <- paste(tclvalue(t2d.var), " ",
                    as.integer(tclvalue(t2h.var)), ":",
                    as.integer(tclvalue(t2m.var)), ":",
                    as.numeric(tclvalue(t2s.var)), sep="")

    t1 <- strptime(t1.str, "%Y-%m-%d %H:%M:%OS")
    t2 <- strptime(t2.str, "%Y-%m-%d %H:%M:%OS")

    d$t1 <- NA
    if (inherits(t1, "POSIXt"))
      d$t1 <- t1
    d$t2 <- NA
    if (inherits(t2, "POSIXt"))
      d$t2 <- t2

    if (is.na(d$t1))
      d$t1 <- d$t1.chk <- NULL
    else
      d$t1.chk <- as.integer(tclvalue(t1.chk.var))
    if (is.na(d$t2))
      d$t2 <- d$t2.chk <- NULL
    else
      d$t2.chk <- as.integer(tclvalue(t2.chk.var))

    origin <- as.POSIXct("1970-01-01 00:00:00.0")

    if (is.null(d$t1) || d$t1.chk)
      t1.str <- NA
    if (is.null(d$t2) || d$t2.chk)
      t2.str <- NA
    d$t <- as.POSIXct(c(t1.str, t2.str), origin=origin)

    tclvalue(tt.done.var) <- 1
    new <<- d
  }


  # Main program

  new <- lim

  # Assign variables linked to Tk widgets

  if (is.null(lim))
    lim <- list()

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

  if (is.null(lim$t1)) {
    t1d.var <- tclVar()
    t1h.var <- tclVar()
    t1m.var <- tclVar()
    t1s.var <- tclVar()
  } else {
    t1d.var <- tclVar(format(lim$t1, format="%Y-%m-%d"))
    t1h.var <- tclVar(as.character(as.integer(format(lim$t1, format="%H" ))))
    t1m.var <- tclVar(as.character(as.integer(format(lim$t1, format="%M" ))))
    t1s.var <- tclVar(as.character(as.numeric(format(lim$t1, format="%OS"))))
  }
  if (is.null(lim$t2)) {
    t2d.var <- tclVar()
    t2h.var <- tclVar()
    t2m.var <- tclVar()
    t2s.var <- tclVar()
  } else {
    t2d.var <- tclVar(format(lim$t2, format="%Y-%m-%d"))
    t2h.var <- tclVar(as.character(as.integer(format(lim$t2, format="%H" ))))
    t2m.var <- tclVar(as.character(as.integer(format(lim$t2, format="%M" ))))
    t2s.var <- tclVar(as.character(as.numeric(format(lim$t2, format="%OS"))))
  }

  t1.chk <- if (is.null(lim$t1)) 1 else lim$t1.chk
  t2.chk <- if (is.null(lim$t2)) 1 else lim$t2.chk

  t1.chk.var <- tclVar(t1.chk)
  t2.chk.var <- tclVar(t2.chk)

  t1.sta.var <- if (t1.chk) tclVar("disabled") else tclVar("normal")
  t2.sta.var <- if (t2.chk) tclVar("disabled") else tclVar("normal")

  tt.done.var <- tclVar(0)

  # Open GUI

  tclServiceMode(FALSE)
  tt <- tktoplevel(padx=5, pady=5)

  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(geo[2]) + 25,
                            "+", as.integer(geo[3]) + 25, sep=""))
  }
  tktitle(tt) <- win.title
  tkwm.resizable(tt, 1, 0)

  # Notebook with tabs

  nb <- ttknotebook(tt)

  # Frame 0 contains x-axis limits

  frame0 <- ttkframe(nb, relief="flat", padding=10, borderwidth=2)
  tkadd(nb, frame0, text="      x      ")

  frame0.lab.1.1 <- ttklabel(frame0, text="Minimum")
  frame0.lab.2.1 <- ttklabel(frame0, text="Maximum")

  frame0.ent.1.2 <- ttkentry(frame0, textvariable=x1.var,
                             state=tclvalue(x1.sta.var))
  frame0.ent.2.2 <- ttkentry(frame0, textvariable=x2.var,
                             state=tclvalue(x2.sta.var))

  frame0.chk.1.3 <- ttkcheckbutton(frame0, variable=x1.chk.var, text="Auto",
                    command=function() {
                      if (as.integer(tclvalue(x1.chk.var))) {
                        tclvalue(x1.sta.var) <- "disabled"
                      } else {
                        tclvalue(x1.sta.var) <- "normal"
                      }
                      tkconfigure(frame0.ent.1.2, state=tclvalue(x1.sta.var))
                      tkfocus(frame0.ent.1.2)
                    })
  frame0.chk.2.3 <- ttkcheckbutton(frame0, variable=x2.chk.var, text="Auto",
                    command=function() {
                      if (as.integer(tclvalue(x2.chk.var))) {
                        tclvalue(x2.sta.var) <- "disabled"
                      } else {
                        tclvalue(x2.sta.var) <- "normal"
                      }
                      tkconfigure(frame0.ent.2.2, state=tclvalue(x2.sta.var))
                      tkfocus(frame0.ent.2.2)
                    })

  tkgrid(frame0.lab.1.1, frame0.ent.1.2, frame0.chk.1.3, padx=1, pady=2)
  tkgrid(frame0.lab.2.1, frame0.ent.2.2, frame0.chk.2.3, padx=1, pady=2)

  tkgrid.configure(frame0.lab.1.1, frame0.lab.2.1, sticky="e")

  tkgrid.configure(frame0.ent.1.2, frame0.ent.2.2, sticky="we")

  tkgrid.columnconfigure(frame0, 1, weight=1, minsize=25)

  tcl("grid", "anchor", frame0, "center")

  # Frame 1 contains y-axis limits

  frame1 <- ttkframe(nb, relief="flat", padding=10, borderwidth=2)
  tkadd(nb, frame1, text="      y      ")

  frame1.lab.1.1 <- ttklabel(frame1, text="Minimum")
  frame1.lab.2.1 <- ttklabel(frame1, text="Maximum")

  frame1.ent.1.2 <- ttkentry(frame1, textvariable=y1.var,
                             state=tclvalue(y1.sta.var))
  frame1.ent.2.2 <- ttkentry(frame1, textvariable=y2.var,
                             state=tclvalue(y2.sta.var))

  frame1.chk.1.3 <- ttkcheckbutton(frame1, variable=y1.chk.var, text="Auto",
                    command=function() {
                      if (as.integer(tclvalue(y1.chk.var))) {
                        tclvalue(y1.sta.var) <- "disabled"
                      } else {
                        tclvalue(y1.sta.var) <- "normal"
                      }
                      tkconfigure(frame1.ent.1.2, state=tclvalue(y1.sta.var))
                      tkfocus(frame1.ent.1.2)
                    })
  frame1.chk.2.3 <- ttkcheckbutton(frame1, variable=y2.chk.var, text="Auto",
                    command=function() {
                      if (as.integer(tclvalue(y2.chk.var)))
                        tclvalue(y2.sta.var) <- "disabled"
                      else
                        tclvalue(y2.sta.var) <- "normal"
                      tkconfigure(frame1.ent.2.2, state=tclvalue(y2.sta.var))
                      tkfocus(frame1.ent.2.2)
                    })

  tkgrid(frame1.lab.1.1, frame1.ent.1.2, frame1.chk.1.3, padx=1, pady=2)
  tkgrid(frame1.lab.2.1, frame1.ent.2.2, frame1.chk.2.3, padx=1, pady=2)

  tkgrid.configure(frame1.lab.1.1, frame1.lab.2.1, sticky="e")
  tkgrid.configure(frame1.ent.1.2, frame1.ent.2.2, sticky="we")

  tkgrid.columnconfigure(frame1, 1, weight=1, minsize=25)

  tcl("grid", "anchor", frame1, "center")

  # Frame 2 contains z-axis limits

  frame2 <- ttkframe(nb, relief="flat", padding=10, borderwidth=2)
  tkadd(nb, frame2, text="      z      ")

  frame2.lab.1.1 <- ttklabel(frame2, text="Minimum")
  frame2.lab.2.1 <- ttklabel(frame2, text="Maximum")

  frame2.ent.1.2 <- ttkentry(frame2, textvariable=z1.var,
                             state=tclvalue(z1.sta.var))
  frame2.ent.2.2 <- ttkentry(frame2, textvariable=z2.var,
                             state=tclvalue(z2.sta.var))

  frame2.chk.1.3 <- ttkcheckbutton(frame2, variable=z1.chk.var, text="Auto",
                    command=function() {
                      if (as.integer(tclvalue(z1.chk.var)))
                        tclvalue(z1.sta.var) <- "disabled"
                      else
                        tclvalue(z1.sta.var) <- "normal"
                      tkconfigure(frame2.ent.1.2, state=tclvalue(z1.sta.var))
                      tkfocus(frame2.ent.1.2)
                    })
  frame2.chk.2.3 <- ttkcheckbutton(frame2, variable=z2.chk.var, text="Auto",
                    command=function() {
                      if (as.integer(tclvalue(z2.chk.var)))
                        tclvalue(z2.sta.var) <- "disabled"
                      else
                        tclvalue(z2.sta.var) <- "normal"
                      tkconfigure(frame2.ent.2.2, state=tclvalue(z2.sta.var))
                      tkfocus(frame2.ent.2.2)
                    })

  tkgrid(frame2.lab.1.1, frame2.ent.1.2, frame2.chk.1.3, padx=1, pady=2)
  tkgrid(frame2.lab.2.1, frame2.ent.2.2, frame2.chk.2.3, padx=1, pady=2)

  tkgrid.configure(frame2.lab.1.1, sticky="e")
  tkgrid.configure(frame2.lab.2.1, sticky="e")

  tkgrid.configure(frame2.ent.1.2, sticky="we")
  tkgrid.configure(frame2.ent.2.2, sticky="we")

  tkgrid.columnconfigure(frame2, 1, weight=1, minsize=25)

  tcl("grid", "anchor", frame2, "center")

  # Frame 3 contains t-axis limits

  frame3 <- ttkframe(nb, relief="flat", padding=10, borderwidth=2)
  tkadd(nb, frame3, text="      t      ")

  fg <- "#414042"
  frame3.lab.1.1 <- ttklabel(frame3, text="Minimum")
  frame3.lab.2.1 <- ttklabel(frame3, text="Maximum")
  frame3.lab.3.1 <- ttklabel(frame3, foreground=fg, text="e.g.")
  frame3.lab.3.2 <- ttklabel(frame3, foreground=fg, text="2010-06-27")
  frame3.lab.3.3 <- ttklabel(frame3, foreground=fg, text="13")
  frame3.lab.3.4 <- ttklabel(frame3, foreground=fg, text="42")
  frame3.lab.3.5 <- ttklabel(frame3, foreground=fg, text="6.321")

  frame3.ent.1.2 <- ttkentry(frame3, width=11, textvariable=t1d.var,
                             state=tclvalue(t1.sta.var))
  frame3.ent.1.3 <- ttkentry(frame3, width= 3, textvariable=t1h.var,
                             state=tclvalue(t1.sta.var))
  frame3.ent.1.4 <- ttkentry(frame3, width= 3, textvariable=t1m.var,
                             state=tclvalue(t1.sta.var))
  frame3.ent.1.5 <- ttkentry(frame3, width= 5, textvariable=t1s.var,
                             state=tclvalue(t1.sta.var))

  frame3.ent.2.2 <- ttkentry(frame3, width=11, textvariable=t2d.var,
                             state=tclvalue(t2.sta.var))
  frame3.ent.2.3 <- ttkentry(frame3, width= 3, textvariable=t2h.var,
                             state=tclvalue(t2.sta.var))
  frame3.ent.2.4 <- ttkentry(frame3, width= 3, textvariable=t2m.var,
                             state=tclvalue(t2.sta.var))
  frame3.ent.2.5 <- ttkentry(frame3, width= 5, textvariable=t2s.var,
                             state=tclvalue(t2.sta.var))

  frame3.chk.1.6 <- ttkcheckbutton(frame3, variable=t1.chk.var, text="Auto",
                    command=function() {
                      if (as.integer(tclvalue(t1.chk.var)))
                        tclvalue(t1.sta.var) <- "disabled"
                      else
                        tclvalue(t1.sta.var) <- "normal"
                      tkconfigure(frame3.ent.1.2, state=tclvalue(t1.sta.var))
                      tkconfigure(frame3.ent.1.3, state=tclvalue(t1.sta.var))
                      tkconfigure(frame3.ent.1.4, state=tclvalue(t1.sta.var))
                      tkconfigure(frame3.ent.1.5, state=tclvalue(t1.sta.var))
                      tkfocus(frame3.ent.1.2)
                    })
  frame3.chk.2.6 <- ttkcheckbutton(frame3, variable=t2.chk.var, text="Auto",
                    command=function() {
                      if (as.integer(tclvalue(t2.chk.var)))
                        tclvalue(t2.sta.var) <- "disabled"
                      else
                        tclvalue(t2.sta.var) <- "normal"
                      tkconfigure(frame3.ent.2.2, state=tclvalue(t2.sta.var))
                      tkconfigure(frame3.ent.2.3, state=tclvalue(t2.sta.var))
                      tkconfigure(frame3.ent.2.4, state=tclvalue(t2.sta.var))
                      tkconfigure(frame3.ent.2.5, state=tclvalue(t2.sta.var))
                      tkfocus(frame3.ent.2.2)
                    })

  tkgrid(frame3.lab.1.1, frame3.ent.1.2, frame3.ent.1.3,
         frame3.ent.1.4, frame3.ent.1.5, frame3.chk.1.6, padx=1, pady=2)
  tkgrid(frame3.lab.2.1, frame3.ent.2.2, frame3.ent.2.3,
         frame3.ent.2.4, frame3.ent.2.5, frame3.chk.2.6, padx=1, pady=2)
  tkgrid(frame3.lab.3.1, frame3.lab.3.2, frame3.lab.3.3,
         frame3.lab.3.4, frame3.lab.3.5, "x", padx=1, pady=2)

  tkgrid.configure(frame3.lab.1.1, frame3.lab.2.1, frame3.lab.3.1, sticky="e")
  tkgrid.configure(frame3.lab.3.2, frame3.lab.3.3,
                   frame3.lab.3.4, frame3.lab.3.5, sticky="w")

  tkgrid.configure(frame3.ent.1.2, frame3.ent.1.3, frame3.ent.1.4,
                   frame3.ent.1.5, frame3.ent.2.2, frame3.ent.2.3,
                   frame3.ent.2.4, frame3.ent.2.5, sticky="we")

  tkgrid.columnconfigure(frame3, 1, weight=5, minsize=13)
  tkgrid.columnconfigure(frame3, 2, weight=2, minsize=3)
  tkgrid.columnconfigure(frame3, 3, weight=2, minsize=3)
  tkgrid.columnconfigure(frame3, 4, weight=3, minsize=6)

  tcl("grid", "anchor", frame3, "center")

  # Insert notebook

  tkpack(nb, fill="x", expand=TRUE, padx=5, pady=c(5, 0))

  # Frame 4 contains ok and cancel buttons

  frame4 <- tkframe(tt, relief="flat", padx=0, pady=0)

  frame4.but.1 <- ttkbutton(frame4, width=12, text="OK",
                            command=UpdateLimits)
  frame4.but.2 <- ttkbutton(frame4, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)

  tkgrid(frame4.but.1, frame4.but.2, padx=2, pady=c(10, 5))
  tkpack(frame4, anchor="e", padx=5)

  # Bind events

  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tkbind(frame0.ent.1.2, "<KeyRelease>",
         function() {
           tclvalue(x1.var) <- CheckEntry("numeric", tclvalue(x1.var))
         })
  tkbind(frame0.ent.2.2, "<KeyRelease>",
         function() {
           tclvalue(x2.var) <- CheckEntry("numeric", tclvalue(x2.var))
         })

  tkbind(frame1.ent.1.2, "<KeyRelease>",
         function() {
           tclvalue(y1.var) <- CheckEntry("numeric", tclvalue(y1.var))
         })
  tkbind(frame1.ent.2.2, "<KeyRelease>",
         function() {
           tclvalue(y2.var) <- CheckEntry("numeric", tclvalue(y2.var))
         })

  tkbind(frame2.ent.1.2, "<KeyRelease>",
         function() {
           tclvalue(z1.var) <- CheckEntry("numeric", tclvalue(z1.var))
         })
  tkbind(frame2.ent.2.2, "<KeyRelease>",
         function() {
           tclvalue(z2.var) <- CheckEntry("numeric", tclvalue(z2.var))
         })

  tkbind(frame3.ent.1.2, "<KeyRelease>",
         function() {
           tclvalue(t1d.var) <- CheckEntry("date", tclvalue(t1d.var))
         })
  tkbind(frame3.ent.1.3, "<KeyRelease>",
         function() {
           tclvalue(t1h.var) <- CheckEntry("hour", tclvalue(t1h.var))
         })
  tkbind(frame3.ent.1.4, "<KeyRelease>",
         function() {
           tclvalue(t1m.var) <- CheckEntry("minute", tclvalue(t1m.var))
         })
  tkbind(frame3.ent.1.5, "<KeyRelease>",
         function() {
           tclvalue(t1s.var) <- CheckEntry("second", tclvalue(t1s.var))
         })

  tkbind(frame3.ent.2.2, "<KeyRelease>",
         function() {
           tclvalue(t2d.var) <- CheckEntry("date", tclvalue(t2d.var))
         })
  tkbind(frame3.ent.2.3, "<KeyRelease>",
         function() {
           tclvalue(t2h.var) <- CheckEntry("hour", tclvalue(t2h.var))
         })
  tkbind(frame3.ent.2.4, "<KeyRelease>",
         function() {
           tclvalue(t2m.var) <- CheckEntry("minute", tclvalue(t2m.var))
         })
  tkbind(frame3.ent.2.5, "<KeyRelease>",
         function() {
           tclvalue(t2s.var) <- CheckEntry("second", tclvalue(t2s.var))
         })

  # GUI control

  tkfocus(tt)
  tkgrab(tt)

  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  new
}
