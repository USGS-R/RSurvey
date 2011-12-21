ChoosePalette <- function(pal, n=5L, parent=NULL) {
# A GUI for selecting a color palette.

  # Additional functions (subroutines)

  # Save palette and quit

  SavePalette <- function() {
    pal <- GetPalette()
    pal.cols <- pal(n)
    if (any(is.na(pal.cols))) {
      msg <- "Palette can not be translated to valid RGB values, try again."
      tkmessageBox(icon="error", message=msg, title="Palette Error",
                   parent=tt)
      return()
    } else {
      pal.rtn <<- pal
    }
    tclvalue(tt.done.var) <- 1
  }

  # Scale change

  ScaleChange <- function(x, v, x.ent.var) {
    if (x == get(v))
      return
    assign(v, x, inherits=TRUE)
    fmt <- ifelse(v %in% c("p1", "p2"), "%.1f", "%.0f")
    tclvalue(x.ent.var) <- sprintf(fmt, x)
    UpdatePalette()
  }

  # Entry change

  EntryChange <- function(v, x.lim, x.ent.var, x.scl.var) {
    x <- suppressWarnings(as.integer(tclvalue(x.ent.var)))
    if (is.na(x))
      return()
    if (x < x.lim[1]) {
      tclvalue(x.ent.var) <- x.lim[1]
      x <- x.lim[1]
    } else if (x > x.lim[2]) {
      tclvalue(x.ent.var) <- x.lim[2]
      x <- x.lim[2]
    }
    assign(v, x, inherits=TRUE)
    tclvalue(x.scl.var) <- x
    UpdatePalette()
  }

  # Get color palette as function of n

  GetPalette <- function() {
    type <- as.character(tclvalue(nature.var))
    if (type == "Qualitative") {
      f <- rainbow_hcl
      formals(f) <- eval(substitute(alist(n=, c=d1, l=d2, start=d3, end=d4,
                                          gamma=NULL, fixup=TRUE, ...=),
                                    list(d1=c1, d2=l1, d3=h1, d4=h2)))
    } else if (type == "Sequential (single hue)") {
      f <- sequential_hcl
      formals(f) <- eval(substitute(alist(n=, h=d1, c.=d2, l=d3, power=d4,
                                          gamma=NULL, fixup=TRUE, ...=),
                                    list(d1=h1, d2=c(c1, c2), d3=c(l1, l2),
                                         d4=p1)))
    } else if (type == "Sequential (multiple hues)") {
      f <- heat_hcl
      formals(f) <- eval(substitute(alist(n=, h=d1, c.=d2, l=d3, power=d4,
                                          gamma=NULL, fixup=TRUE, ...=),
                                    list(d1=c(h1, h2), d2=c(c1, c2),
                                         d3=c(l1, l2), d4=c(p1, p2))))
    } else if (type == "Diverging") {
      f <- diverge_hcl
      formals(f) <- eval(substitute(alist(n=, h=d1, c=d2, l=d3, power=d4,
                                          gamma=NULL, fixup=TRUE, ...=),
                                    list(d1=c(h1, h2), d2=c1, d3=c(l1, l2),
                                         d4=p1)))
    }
    f
  }

  # Update palette

  UpdatePalette <- function() {
    pal <- GetPalette()
    tcl(frame5.cvs, "delete", "pal")
    pal.cols <- pal(n)
    if (any(is.na(pal.cols)))
      return()
    dx <- (cvs.width - 1) / n
    x2 <- 1
    y1 <- 1
    y2 <- cvs.height
    for (i in pal.cols) {
      x1 <- x2
      x2 <- x1 + dx
      pts <- .Tcl.args(c(x1, y1, x2, y1, x2, y2, x1, y2))
      tkcreate(frame5.cvs, "polygon", pts, fill=i, tag="pal")
    }
  }

  # Update data type

  UpdateDataType <- function() {
    type <- as.character(tclvalue(nature.var))

    if (type == "Qualitative") {
      is.normal <- c(TRUE, FALSE, FALSE, FALSE, FALSE)
    } else if (type == "Sequential (single hue)") {
      is.normal <- c(FALSE, TRUE, TRUE, TRUE, FALSE)
    } else if (type == "Sequential (multiple hues)") {
      is.normal <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
    } else if (type == "Diverging") {
      is.normal <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
    }

    s <- ifelse(is.normal, "normal", "disabled")
    tkconfigure(frame3.lab.2.1, state=s[1])
    tkconfigure(frame3.lab.4.1, state=s[2])
    tkconfigure(frame3.lab.6.1, state=s[3])
    tkconfigure(frame3.lab.7.1, state=s[4])
    tkconfigure(frame3.lab.8.1, state=s[5])
    tkconfigure(frame3.ent.2.3, state=s[1])
    tkconfigure(frame3.ent.4.3, state=s[2])
    tkconfigure(frame3.ent.6.3, state=s[3])
    tkconfigure(frame3.ent.7.3, state=s[4])
    tkconfigure(frame3.ent.8.3, state=s[5])

    s <- ifelse(is.normal, "!disabled", "disabled")
    tcl(frame3.scl.2.2, "state", s[1])
    tcl(frame3.scl.4.2, "state", s[2])
    tcl(frame3.scl.6.2, "state", s[3])
    tcl(frame3.scl.7.2, "state", s[4])
    tcl(frame3.scl.8.2, "state", s[5])

    UpdatePalette()
  }










  # Main program

  # Initialize return palette

  pal.rtn <- NULL

  # Initialize default palette

  if (missing(pal) || is.null(pal)) {
    nature <- "Sequential (multiple hues)"
    h1 <- 0
    h2 <- 90
    c1 <- 100
    c2 <- 30
    l1 <- 50
    l2 <- 90
    p1 <- 0.2
    p2 <- 1.0
  } else {
    arg <- formals(pal)
    h1 <- h2 <- c1 <- c2 <- l1 <- l2 <- p1 <- p2 <- 0
    what <- c("numeric", "integer")
    a <- c("c", "l", "start", "end")
    if (all(sapply(a, function(i) inherits(arg[[i]], what)))) {
      nature <- "Qualitative"
      h1 <- arg$start
      h2 <- arg$end
      c1 <- arg$c
      l1 <- arg$l
    }
    a <- c("h", "c", "l", "power")
    if (all(sapply(a, function(i) inherits(arg[[i]], what)))) {
      nature <- "Diverging"
      h1 <- arg$h[1]
      h2 <- arg$h[2]
      c1 <- arg$c
      l1 <- arg$l[1]
      l2 <- arg$l[2]
      p1 <- arg$power
    }
    a <- c("h", "c.", "l", "power")
    if (all(sapply(a, function(i) inherits(arg[[i]], what)))) {
      if (length(arg$h) == 1 && length(arg$p) == 1) {
        nature <- "Sequential (single hue)"
        h1 <- arg$h
        c1 <- arg$c.[1]
        c2 <- arg$c.[2]
        l1 <- arg$l[1]
        l2 <- arg$l[2]
        p1 <- arg$power
      } else {
        nature <- "Sequential (multiple hues)"
        h1 <- arg$h[1]
        h2 <- arg$h[2]
        c1 <- arg$c.[1]
        c2 <- arg$c.[2]
        l1 <- arg$l[1]
        l2 <- arg$l[2]
        p1 <- arg$power[1]
        p2 <- arg$power[2]
      }
    }
  }

  # Set limits for palette attributes

  n.lim <- c(1, 50)
  h.lim <- c(0, 360)
  c.lim <- c(0, 100)
  l.lim <- c(0, 100)
  p.lim <- c(0, 5)

  # Set dimensions on palette canvas

  cvs.width <- 350
  cvs.height <- 25

  # Assign additional variables linked to Tk widgets

  nature.var <- tclVar(nature)

  n.scl.var <- tclVar(n)
  n.ent.var <- tclVar(n)

  h1.scl.var <- tclVar(h1)
  h1.ent.var <- tclVar(h1)
  h2.scl.var <- tclVar(h2)
  h2.ent.var <- tclVar(h2)

  c1.scl.var <- tclVar(c1)
  c1.ent.var <- tclVar(c1)
  c2.scl.var <- tclVar(c2)
  c2.ent.var <- tclVar(c2)

  l1.scl.var <- tclVar(l1)
  l1.ent.var <- tclVar(l1)
  l2.scl.var <- tclVar(l2)
  l2.ent.var <- tclVar(l2)

  p1.scl.var <- tclVar(p1)
  p1.ent.var <- tclVar(p1)
  p2.scl.var <- tclVar(p2)
  p2.ent.var <- tclVar(p2)

  tt.done.var <- tclVar(0)

  # Open GUI

  tclServiceMode(FALSE)

  tt <- tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(geo[2]) + 25,
                            "+", as.integer(geo[3]) + 25, sep=""))
  }
  tkwm.resizable(tt, 0, 0)
  tktitle(tt) <- "Choose Palette"

  # Frame 0, ok and cancel buttons

  frame0 <- ttkframe(tt, relief="flat")

  frame0.but.1 <- ttkbutton(frame0, width=12, text="OK", command=SavePalette)
  frame0.but.2 <- ttkbutton(frame0, width=12, text="Cancel",
                            command=function() {
                              pal.rtn <<- NULL
                              tclvalue(tt.done.var) <- 1
                            })

  tkgrid(frame0.but.1, frame0.but.2, pady=c(10, 10))

  tkgrid.configure(frame0.but.1, sticky="e")
  tkgrid.configure(frame0.but.2, sticky="w", padx=c(4, 10))

  tkpack(frame0, side="bottom", anchor="e")

  # Frame 1, choose nature of data

  frame1 <- ttkframe(tt, relief="flat")
  frame1.lab.1 <- ttklabel(frame1, text="The nature of your data")
  frame1.box.2 <- ttkcombobox(frame1, state="readonly", textvariable=nature.var,
                              values=c("Qualitative", "Sequential (single hue)",
                                       "Sequential (multiple hues)", "Diverging"))

  tkgrid(frame1.lab.1, frame1.box.2, pady=10)
  tkgrid.configure(frame1.lab.1, padx=c(10, 2))
  tkgrid.configure(frame1.box.2, padx=c(0, 10), sticky="we")

  tkgrid.columnconfigure(frame1, 1, weight=1)

  tkpack(frame1, fill="x")

  # Frame 2, default color schemes

  frame2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                          text="Default color schemes")
  frame2.cvs.1.1 <- tkcanvas(frame2, relief="flat", width=50, height=50,
                             background="white", confine=TRUE, closeenough=0,
                             borderwidth=0, highlightthickness=0)
  tkgrid(frame2.cvs.1.1, sticky="we")
  tkgrid.columnconfigure(frame2, 0, weight=1)
  tkpack(frame2, fill="x", padx=10)

  # Frame 3, color description

  txt <- "Color description: hue, croma, luminance, power"
  frame3 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5, text=txt)

  frame3.lab.1.1 <- ttklabel(frame3, text="H1", width=2)
  frame3.lab.2.1 <- ttklabel(frame3, text="H2", width=2)
  frame3.lab.3.1 <- ttklabel(frame3, text="C1", width=2)
  frame3.lab.4.1 <- ttklabel(frame3, text="C2", width=2)
  frame3.lab.5.1 <- ttklabel(frame3, text="L1", width=2)
  frame3.lab.6.1 <- ttklabel(frame3, text="L2", width=2)
  frame3.lab.7.1 <- ttklabel(frame3, text="P1", width=2)
  frame3.lab.8.1 <- ttklabel(frame3, text="P2", width=2)

  frame3.ent.1.3 <- ttkentry(frame3, textvariable=h1.ent.var, width=4)
  frame3.ent.2.3 <- ttkentry(frame3, textvariable=h2.ent.var, width=4)
  frame3.ent.3.3 <- ttkentry(frame3, textvariable=c1.ent.var, width=4)
  frame3.ent.4.3 <- ttkentry(frame3, textvariable=c2.ent.var, width=4)
  frame3.ent.5.3 <- ttkentry(frame3, textvariable=l1.ent.var, width=4)
  frame3.ent.6.3 <- ttkentry(frame3, textvariable=l2.ent.var, width=4)
  frame3.ent.7.3 <- ttkentry(frame3, textvariable=p1.ent.var, width=4)
  frame3.ent.8.3 <- ttkentry(frame3, textvariable=p2.ent.var, width=4)

  frame3.scl.1.2 <- tkwidget(frame3, "ttk::scale", from=h.lim[1], to=h.lim[2],
                             orient="horizontal", value=h1, variable=h1.scl.var,
                             command=function(...) {
                               ScaleChange(x=round(as.numeric(...)), v="h1",
                                           x.ent.var=h1.ent.var)
                             })
  frame3.scl.2.2 <- tkwidget(frame3, "ttk::scale", from=h.lim[1], to=h.lim[2],
                             orient="horizontal", value=h2, variable=h2.scl.var,
                             command=function(...) {
                               ScaleChange(x=round(as.numeric(...)), v="h2",
                                           x.ent.var=h2.ent.var)
                             })
  frame3.scl.3.2 <- tkwidget(frame3, "ttk::scale", from=c.lim[1], to=c.lim[2],
                             orient="horizontal", value=c1, variable=c1.scl.var,
                             command=function(...) {
                               ScaleChange(x=round(as.numeric(...)), v="c1",
                                           x.ent.var=c1.ent.var)
                             })
  frame3.scl.4.2 <- tkwidget(frame3, "ttk::scale", from=c.lim[1], to=c.lim[2],
                             orient="horizontal", value=c2, variable=c2.scl.var,
                             command=function(...) {
                               ScaleChange(x=round(as.numeric(...)), v="c2",
                                           x.ent.var=c2.ent.var)
                             })
  frame3.scl.5.2 <- tkwidget(frame3, "ttk::scale", from=l.lim[1], to=l.lim[2],
                             orient="horizontal", value=l1, variable=l1.scl.var,
                             command=function(...) {
                               ScaleChange(x=round(as.numeric(...)), v="l1",
                                           x.ent.var=l1.ent.var)
                             })
  frame3.scl.6.2 <- tkwidget(frame3, "ttk::scale", from=l.lim[1], to=l.lim[2],
                             orient="horizontal", value=l2, variable=l2.scl.var,
                             command=function(...) {
                               ScaleChange(x=round(as.numeric(...)), v="l2",
                                           x.ent.var=l2.ent.var)
                             })
  frame3.scl.7.2 <- tkwidget(frame3, "ttk::scale", from=p.lim[1], to=p.lim[2],
                             orient="horizontal", value=p1, variable=p1.scl.var,
                             command=function(...) {
                               ScaleChange(x=as.numeric(...), v="p1",
                                           x.ent.var=p1.ent.var)
                             })
  frame3.scl.8.2 <- tkwidget(frame3, "ttk::scale", from=p.lim[1], to=p.lim[2],
                             orient="horizontal", value=p2, variable=p2.scl.var,
                             command=function(...) {
                               ScaleChange(x=as.numeric(...), v="p2",
                                           x.ent.var=p2.ent.var)
                             })

  tkgrid(frame3.lab.1.1, frame3.scl.1.2, frame3.ent.1.3, pady=c(0, 5))
  tkgrid(frame3.lab.2.1, frame3.scl.2.2, frame3.ent.2.3, pady=c(0, 5))
  tkgrid(frame3.lab.3.1, frame3.scl.3.2, frame3.ent.3.3, pady=c(0, 5))
  tkgrid(frame3.lab.4.1, frame3.scl.4.2, frame3.ent.4.3, pady=c(0, 5))
  tkgrid(frame3.lab.5.1, frame3.scl.5.2, frame3.ent.5.3, pady=c(0, 5))
  tkgrid(frame3.lab.6.1, frame3.scl.6.2, frame3.ent.6.3, pady=c(0, 5))
  tkgrid(frame3.lab.7.1, frame3.scl.7.2, frame3.ent.7.3, pady=c(0, 5))
  tkgrid(frame3.lab.8.1, frame3.scl.8.2, frame3.ent.8.3)

  tkgrid.configure(frame3.scl.1.2, frame3.scl.2.2, frame3.scl.3.2,
                   frame3.scl.4.2, frame3.scl.5.2, frame3.scl.6.2,
                   frame3.scl.7.2, frame3.scl.8.2,
                   sticky="we", padx=c(4, 10))

  tkgrid.columnconfigure(frame3, 1, weight=1)

  tkpack(frame3, fill="x", padx=10, pady=10)

  # Frame 4, number of colors in palette

  txt <- "Number of colors in palette (does not affect returned palette)"
  frame4 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5, text=txt)

  frame4.lab.1 <- ttklabel(frame4, text="n", width=2)
  frame4.ent.3 <- ttkentry(frame4, textvariable=n.ent.var, width=4)
  frame4.scl.2 <- tkwidget(frame4, "ttk::scale", from=n.lim[1], to=n.lim[2],
                           orient="horizontal", value=n, variable=n.scl.var,
                           command=function(...) {
                             ScaleChange(x=round(as.numeric(...)), v="n",
                                         x.ent.var=n.ent.var)
                           })

  tkgrid(frame4.lab.1, frame4.scl.2, frame4.ent.3)
  tkgrid.configure(frame4.scl.2, sticky="we", padx=c(4, 10))
  tkgrid.columnconfigure(frame4, 1, weight=1)

  tkpack(frame4, fill="x", padx=10)

 # Frame 5, color palette

  frame5 <- ttkframe(tt, relief="flat")
  frame5.cvs <- tkcanvas(frame5, relief="flat",
                         width=cvs.width + 1, height=cvs.height + 1,
                         background="black", confine=TRUE, closeenough=0,
                         borderwidth=0, highlightthickness=0)
  tkgrid(frame5.cvs, padx=10, pady=c(10, 0))
  tkpack(frame5)

  # Initial commands

  UpdatePalette()
  UpdateDataType()

  # Bind events

  tclServiceMode(TRUE)

  tkbind(frame1.box.2, "<<ComboboxSelected>>", UpdateDataType)

  tkbind(frame3.ent.1.3, "<KeyRelease>",
         function() EntryChange("h1", h.lim, h1.ent.var, h1.scl.var))
  tkbind(frame3.ent.2.3, "<KeyRelease>",
         function() EntryChange("h2", h.lim, h2.ent.var, h2.scl.var))
  tkbind(frame3.ent.3.3, "<KeyRelease>",
         function() EntryChange("c1", c.lim, c1.ent.var, c1.scl.var))
  tkbind(frame3.ent.4.3, "<KeyRelease>",
         function() EntryChange("c2", c.lim, c2.ent.var, c2.scl.var))
  tkbind(frame3.ent.5.3, "<KeyRelease>",
         function() EntryChange("l1", l.lim, l1.ent.var, l1.scl.var))
  tkbind(frame3.ent.6.3, "<KeyRelease>",
         function() EntryChange("l2", l.lim, l2.ent.var, l2.scl.var))
  tkbind(frame3.ent.7.3, "<KeyRelease>",
         function() EntryChange("p1", p.lim, p1.ent.var, p1.scl.var))
  tkbind(frame3.ent.8.3, "<KeyRelease>",
         function() EntryChange("p2", p.lim, p2.ent.var, p2.scl.var))

  tkbind(frame4.ent.3, "<KeyRelease>",
         function() EntryChange("n", n.lim, n.ent.var, n.scl.var))

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  # GUI control

  tkfocus(tt)
  tkgrab(tt)

  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  invisible(pal.rtn)
}
