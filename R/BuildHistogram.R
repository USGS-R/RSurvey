# A GUI for building histograms.

BuildHistogram <- function(d, var.names=NULL, var.default=1L, parent=NULL) {

  ## Additional functions (subroutines)

  # Calculate and plot histogram

  CalcHist <- function(draw.plot=TRUE) {
    idx <- as.integer(tcl(frame1.box.1.2, "current")) + 1L
    x <- d[, idx]
    xlab <- var.names[idx]

    type <- as.integer(tclvalue(breaks.var))
    if (type == 1L) {
      idx <- as.integer(tcl(frame2.box.2.1, "current")) + 1L
      breaks <- fun.names[idx]
    } else if (type == 2L) {
      breaks <- as.integer(tclvalue(single.var))
    } else if (type == 3L) {
      s <- as.character(tclvalue(vector.var))
      str.split <- unlist(strsplit(s, "[[:space:]]"))
      num.split <- suppressWarnings(as.numeric(str.split))
      breaks <- num.split[!is.na(num.split)]
      if (length(breaks) == 0)
        return()
    } else if (type == 4L) {
      seq.from <- as.numeric(tclvalue(from.var))
      seq.to   <- as.numeric(tclvalue(to.var))
      seq.by   <- as.numeric(tclvalue(by.var))
      if (any(is.na(c(seq.from, seq.to, seq.by))))
        return()
      breaks <- try(seq(seq.from, seq.to, seq.by), silent=TRUE)
      if (inherits(breaks, "try-error"))
        return()
    }

    right <- as.logical(as.integer(tclvalue(right.var)))
    freq <- as.logical(as.integer(tclvalue(freq.var)))

    obj <- try(hist(x, breaks=breaks, right=right, plot=FALSE))
    if (inherits(obj, "try-error"))
      return()

    if (draw.plot) {
      if (dev.cur() == dev) {
        x11()
        par(mar=c(5, 5, 2, 2) + 0.1)
      }
      plot(obj, col="light grey", freq=freq, main=NULL, xlab=xlab)

    } else {
      obj$xname <- xlab
      n <- max(vapply(obj, length, 0L))
      for (i in names(obj)) {
        obj[[i]] <- format(obj[[i]])
        length(obj[[i]]) <- n
        obj[[i]][is.na(obj[[i]])] <- ""
      }
      obj <- as.data.frame(do.call(cbind, obj))
      EditData(obj, col.names=names(obj), read.only=TRUE,
               win.title="Histogram Data", parent=tt)
    }
  }

  # Adjust scale
  AdjustScale <- function(x) {
    idx <- as.integer(tcl(frame1.box.1.2, "current")) + 1L
    breaks <- as.integer(x * (maxs[idx] - 1) + 1)
    if (breaks != as.integer(tclvalue(single.var))) {
      tclvalue(single.var) <- breaks
      PlotHist()
    }
  }

  # Plot Histogram
  PlotHist <- function() {
    if (dev.cur() > dev)
      CalcHist()
  }

  # Toggle state on break options

  ToggleState <- function() {
    type <- as.integer(tclvalue(breaks.var))
    states <- rep(FALSE, 4)
    states[type] <- TRUE

    tclServiceMode(FALSE)
    s <- if (states[1]) "readonly" else "disabled"
    tkconfigure(frame2.box.2.1, state=s)
    s <- if (states[2]) "!disabled" else "disabled"
    tcl(frame2.scl.4.1, "state", s)
    s <- if (states[2]) "normal" else "disabled"
    tkconfigure(frame2.ent.4.3, state=s)
    s <- if (states[3]) "normal" else "disabled"
    tkconfigure(frame2.ent.6.1, state=s)
    s <- if (states[4]) "normal" else "disabled"
    tkconfigure(frame2.lab.8.1,  state=s)
    tkconfigure(frame2.lab.9.1,  state=s)
    tkconfigure(frame2.lab.10.1, state=s)
    tkconfigure(frame2.ent.8.2,  state=s)
    tkconfigure(frame2.ent.9.2,  state=s)
    tkconfigure(frame2.ent.10.2, state=s)
    tclServiceMode(TRUE)

    if (states[1]) {
      tkfocus(frame2.box.2.1)
    } else if (states[2]) {
      tkfocus(frame2.ent.4.3)
    } else if (states[3]) {
      tkfocus(frame2.ent.6.1)
    } else if (states[4]) {
      tkfocus(frame2.ent.8.2)
    }

    PlotHist()
  }

  ## Main program

  # Check input arguments

  if (!is.matrix(d) & !is.data.frame(d) & !is.vector(d))
    stop()
  if (is.vector(d))
    d <- as.matrix(d)

  if (is.null(var.names)) {
    var.names <- colnames(d)
    if (is.null(var.names))
      var.names <- paste0("Unknown (", 1:ncol(d), ")")
  }

  if (is.character(var.default)) {
    var.default <- which(var.default == var.names)
    if (length(var.default) == 0 || !var.default %in% 1:ncol(d))
      var.default <- 1L
  }
  if (!is.integer(var.default) || !var.default %in% 1:ncol(d))
    stop()

  # Set limits and default value
  maxs <- as.vector(apply(d, 2, function(i) length(unique(i))))
  maxs[maxs > 100] <- 100
  maxs[maxs <  10] <-  10
  defs <- as.vector(apply(d, 2, function(i) length(hist(i, plot=FALSE)$breaks)))
  xdef <- (defs[var.default] - 1) / (maxs[var.default] - 1)

  # Initialize device
  dev <- dev.cur()

  # Assign the variables linked to Tk widgets

  fun.names <- c("sturges", "scott", "freedman-diaconis")

  breaks.var <- tclVar(1L)
  single.var <- tclVar(defs[var.default])
  scale.var <- tclVar(xdef)

  vector.var <- tclVar()
  from.var <- tclVar()
  to.var <- tclVar()
  by.var <- tclVar()
  right.var <- tclVar(TRUE)
  freq.var <- tclVar(TRUE)

  tt.done.var <- tclVar(0)

  # Open GUI

  tclServiceMode(FALSE)
  tt <- tktoplevel()

  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste0("+", as.integer(geo[2]) + 25,
                             "+", as.integer(geo[3]) + 25))
  }
  tktitle(tt) <- "Histogram"
  tkwm.resizable(tt, 1, 0)

  # Frame 0

  frame0 <- ttkframe(tt, relief="flat")

  frame0.but.1 <- ttkbutton(frame0, width=12, text="View",
                            command=function() CalcHist(draw.plot=FALSE))
  frame0.but.2 <- ttkbutton(frame0, width=12, text="Plot",
                            command=function() CalcHist())
  frame0.but.3 <- ttkbutton(frame0, width=12, text="Close",
                            command=function() tclvalue(tt.done.var) <- 1)
  frame0.but.4 <- ttkbutton(frame0, width=12, text="Help",
                            command=function() {
                              print(help("BuildHistogram", package="RSurvey"))
                            })
  tkgrid(frame0.but.1, frame0.but.2, frame0.but.3, frame0.but.4,
         pady=10, padx=c(0, 4))
  tkgrid.configure(frame0.but.4, padx=c(0, 10))

  tkpack(frame0, side="bottom", anchor="e")

  # Frame 1

  frame1 <- ttkframe(tt, relief="flat")

  frame1.lab.1.1 <- ttklabel(frame1, text="Variable")
  frame1.box.1.2 <- ttkcombobox(frame1, state="readonly")
  tkgrid(frame1.lab.1.1, frame1.box.1.2, pady=c(10, 0))

  tkgrid.configure(frame1.lab.1.1, sticky="e", padx=c(10, 2))
  tkgrid.configure(frame1.box.1.2, sticky="we", padx=c(0, 10))

  tkconfigure(frame1.box.1.2, value=var.names)
  tcl(frame1.box.1.2, "current", var.default - 1L)

  tkgrid.columnconfigure(frame1, 1, weight=1, minsize=25)
  tkpack(frame1, fill="x", expand=TRUE, ipadx=0, ipady=0, padx=10, pady=5)

  # Frame 2

  frame2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                          text="Breaks")

  txt <- "A function to compute the number of cells for the histogram"
  frame2.rbt.1.1 <- ttkradiobutton(frame2, variable=breaks.var, value=1L,
                                  text=txt, command=ToggleState)
  frame2.box.2.1 <- ttkcombobox(frame2, state="readonly")
  tkconfigure(frame2.box.2.1, value=fun.names)
  tcl(frame2.box.2.1, "current", 0)

  txt <- "A single number giving the suggested number of cells"
  frame2.rbt.3.1 <- ttkradiobutton(frame2, variable=breaks.var, value=2L,
                                  text=txt, command=ToggleState)
  frame2.scl.4.1 <- tkwidget(frame2, "ttk::scale", from=0, to=1,
                             orient="horizontal", variable=scale.var,
                             command=function(...) {
                               AdjustScale(x=as.numeric(...))
                             })
  frame2.ent.4.3 <- ttkentry(frame2, width=4, textvariable=single.var)

  txt <- "A vector giving the breakpoints between cells"
  frame2.rbt.5.1 <- ttkradiobutton(frame2, variable=breaks.var, value=3L,
                                  text=txt, command=ToggleState)
  frame2.ent.6.1 <- ttkentry(frame2, width=15, textvariable=vector.var)

  txt <- "A sequence giving the breakpoints between cells"
  frame2.rbt.7.1 <- ttkradiobutton(frame2, variable=breaks.var, value=4L,
                                  text=txt, command=ToggleState)

  frame2.lab.8.1  <- ttklabel(frame2, text="Starting value")
  frame2.lab.9.1  <- ttklabel(frame2, text="Ending value")
  frame2.lab.10.1 <- ttklabel(frame2, text="Increment")

  frame2.ent.8.2  <- ttkentry(frame2, width=10, textvariable=from.var)
  frame2.ent.9.2  <- ttkentry(frame2, width=10, textvariable=to.var)
  frame2.ent.10.2 <- ttkentry(frame2, width=10, textvariable=by.var)

  tkgrid(frame2.rbt.1.1, sticky="w", columnspan=3)
  tkgrid(frame2.box.2.1, padx=c(20, 0), pady=c(0, 10), sticky="we",
         columnspan=3)
  tkgrid(frame2.rbt.3.1, sticky="w", columnspan=3)
  tkgrid(frame2.scl.4.1, "x", frame2.ent.4.3, pady=c(0, 5), sticky="we")
  tkgrid(frame2.rbt.5.1, sticky="w", columnspan=3)
  tkgrid(frame2.ent.6.1, padx=c(20, 0), pady=c(0, 10), sticky="we",
         columnspan=3)
  tkgrid(frame2.rbt.7.1, sticky="w", columnspan=3)
  tkgrid(frame2.lab.8.1, frame2.ent.8.2)
  tkgrid(frame2.lab.9.1, frame2.ent.9.2)
  tkgrid(frame2.lab.10.1, frame2.ent.10.2)

  tkgrid.configure(frame2.scl.4.1, columnspan=2, padx=c(20, 4))
  tkgrid.configure(frame2.lab.8.1, frame2.lab.9.1, frame2.lab.10.1,
                   padx=c(40, 4), sticky="w")
  tkgrid.configure(frame2.ent.8.2, frame2.ent.9.2, frame2.ent.10.2,
                   columnspan=2, sticky="we")
  tkgrid.configure(frame2.ent.9.2, pady=2)
  tkgrid.configure(frame2.ent.8.2, frame2.ent.9.2, frame2.ent.10.2,
                   padx=c(0,150))

  tkgrid.columnconfigure(frame2, 1, weight=1, minsize=50)
  tkpack(frame2, fill="x", expand=TRUE, ipadx=0, ipady=0, padx=10, pady=5)

  # Frame 3

  frame3 <- ttkframe(tt, relief="flat")

  txt <- "The histogram cells are right-closed (left-open) intervals"
  frame3.chk.1 <- ttkcheckbutton(frame3, text=txt, variable=right.var,
                                 command=PlotHist)
  tkgrid(frame3.chk.1, sticky="w")
  tkpack(frame3, fill="x", expand=TRUE, padx=20, pady=5)

  # Frame 4

  frame4 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                          text="Axis scaling")
  frame4.rbt.1.1 <- ttkradiobutton(frame4, variable=freq.var, value=TRUE,
                                   text="Frequences (counts component)",
                                   command=PlotHist)
  frame4.rbt.1.2 <- ttkradiobutton(frame4, variable=freq.var, value=FALSE,
                                   text="Probability densities",
                                   command=PlotHist)
  tkgrid(frame4.rbt.1.1, frame4.rbt.1.2)
  tkgrid.configure(frame4.rbt.1.1, padx=c(0, 10))
  tkpack(frame4, fill="x", expand=TRUE, ipadx=0, ipady=0, padx=10, pady=5)

  # Bind events

  tclServiceMode(TRUE)

  tkbind(frame1.box.1.2, "<<ComboboxSelected>>",
         function() {
           idx <- as.integer(tcl(frame1.box.1.2, "current")) + 1L
           x <- as.numeric(tclvalue(scale.var))
           tclvalue(single.var) <- as.integer(x * (maxs[idx] - 1) + 1)
           PlotHist()
         })
  tkbind(frame2.box.2.1, "<<ComboboxSelected>>", PlotHist)

  tkbind(frame2.ent.4.3, "<KeyRelease>",
         function() {
           idx <- as.integer(tcl(frame1.box.1.2, "current")) + 1L
           ent <- as.integer(CheckEntry("integer", tclvalue(single.var)))
           if (ent == "") {
             ent <- 1L
           } else if (as.integer(ent) > maxs[idx]) {
             ent <- maxs[idx]
           }
           tclvalue(single.var) <- ent
           tclvalue(scale.var) <- (as.integer(ent) - 1) / (maxs[idx] - 1)
           PlotHist()
         })
  tkbind(frame2.ent.8.2, "<KeyRelease>",
         function() {
           tclvalue(from.var) <- CheckEntry("numeric", tclvalue(from.var))
         })
  tkbind(frame2.ent.9.2, "<KeyRelease>",
         function() {
           tclvalue(to.var) <- CheckEntry("numeric", tclvalue(to.var))
         })
  tkbind(frame2.ent.10.2, "<KeyRelease>",
         function() {
           tclvalue(by.var) <- CheckEntry("numeric", tclvalue(by.var))
         })

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  # GUI control

  ToggleState()

  tkfocus(tt)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)

  if (!is.null(parent))
    tkfocus(parent)

  tclServiceMode(TRUE)
  invisible()
}
