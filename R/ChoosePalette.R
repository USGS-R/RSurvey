ChoosePalette <- function(n=25, H=c(0, 90), C=c(100, 30), L=c(50, 90),
                          power=c(0.2, 1), parent=NULL) {
# A GUI for selecting a color palette.







  # Additional functions (subroutines)

  # Save palette and quit

  SavePalette <- function() {

    tclvalue(tt.done.var) <- 1
  }

  # Scale color space coordinates

  ScaleN <- function(...) {
    val <- as.integer(...)
    if (val == n)
      return
    n <<- val
    tclvalue(n.scl.var) <- n
    tclvalue(n.ent.var) <- sprintf("%.0f", n)
    UpdatePalette()
  }
  ScaleH1 <- function(...) {
    val <- as.integer(...)
    if (val == h1)
      return
    h1 <<- val
    tclvalue(h1.scl.var) <- h1
    tclvalue(h1.ent.var) <- sprintf("%.0f", h1)
    UpdatePalette()
  }
  ScaleH2 <- function(...) {
    val <- as.integer(...)
    if (val == h2)
      return
    h2 <<- val
    tclvalue(h2.scl.var) <- h2
    tclvalue(h2.ent.var) <- sprintf("%.0f", h2)
    UpdatePalette()
  }
  ScaleC1 <- function(...) {
    val <- as.integer(...)
    if (val == c1)
      return
    c1 <<- val
    tclvalue(c1.scl.var) <- c1
    tclvalue(c1.ent.var) <- sprintf("%.0f", c1)
    UpdatePalette()
  }
  ScaleC2 <- function(...) {
    val <- as.integer(...)
    if (val == c2)
      return
    c2 <<- val
    tclvalue(c2.scl.var) <- c2
    tclvalue(c2.ent.var) <- sprintf("%.0f", c2)
    UpdatePalette()
  }
  ScaleL1 <- function(...) {
    val <- as.integer(...)
    if (val == l1)
      return
    l1 <<- val
    tclvalue(l1.scl.var) <- l1
    tclvalue(l1.ent.var) <- sprintf("%.0f", l1)
    UpdatePalette()
  }
  ScaleL2 <- function(...) {
    val <- as.integer(...)
    if (val == l2)
      return
    l2 <<- val
    tclvalue(l2.scl.var) <- l2
    tclvalue(l2.ent.var) <- sprintf("%.0f", l2)
    UpdatePalette()
  }
  ScaleP1 <- function(...) {
    p1 <<- as.numeric(...)
    tclvalue(p1.scl.var) <- p1
    tclvalue(p1.ent.var) <- sprintf("%.1f", p1)
    UpdatePalette()
  }
  ScaleP2 <- function(...) {
    p2 <<- as.numeric(...)
    tclvalue(p2.scl.var) <- p2
    tclvalue(p2.ent.var) <- sprintf("%.1f", p2)
    UpdatePalette()
  }

  # Get color palette as function of n

  GetPalette <- function() {
    type <- as.character(tclvalue(nature.var))
    if (type == "Qualitative") {
      f <- function(n, c, l, start, end) {
             rainbow_hcl(n, c=c, l=l, start=start, end=end)
           }
      formals(f) <- alist(n=, c=c1, l=l1, start=h1, end=h2)
    } else if (type == "Sequential (single hue)") {
      f <- function(n, h, c., l, power) {
             sequential_hcl(n, h=h, c.=c., l=l, power=power)
           }
      formals(f) <- alist(n=, h=h1, c.=c(c1, c2), l=c(l1, l2), power=p1)
    } else if (type == "Sequential (multiple hues)") {
      f <- function(n, h, c., l, power) {
             heat_hcl(n, h=h, c.=c., l=l, power=power)
           }
      formals(f) <- alist(n=, h=c(h1, h2), c.=c(c1, c2), l=c(l1, l2),
                          power=c(p1, p2))
    } else {
      f <- function(n, h, c, l, power) {
             diverge_hcl(n, h=h, c=c, l=l, power=power)
           }
      formals(f) <- alist(n=, h=c(h1, h2), c=c1, l=c(l1, l2), power=p1)
    }
    f
  }

  # Update palette

  UpdatePalette <- function() {
    pal <<- GetPalette()
    tcl(frame5.cvs, "delete", "pal")
    dx <- (cvs.width - 1) / n
    x2 <- 1
    y1 <- 1
    y2 <- cvs.height
    for (i in pal(n)) {
      x1 <- x2
      x2 <- x1 + dx
      pts <- .Tcl.args(c(x1, y1, x2, y1, x2, y2, x1, y2))
      tkcreate(frame5.cvs, "polygon", pts, fill=i, tag="pal")
    }
  }










  # Main program

  pal <- NULL




  n <- 20

  h1 <- H[1]
  h2 <- H[2]
  c1 <- C[1]
  c2 <- C[2]
  l1 <- L[1]
  l2 <- L[2]
  p1 <- power[1]
  p2 <- power[2]






  cvs.width <- 350
  cvs.height <- 25



  # Assign additional variables linked to Tk widgets

  nature.var <- tclVar("Sequential (multiple hues)")

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

  colorblind.safe.var <- tclVar(FALSE)
  print.friendly.var  <- tclVar(FALSE)
  photocoy.able.var   <- tclVar(FALSE)

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

  frame2.cvs.1.1 <- tkcanvas(frame2, relief="flat", width= 50, height=50,
                             background="white", confine=TRUE, closeenough=0,
                             borderwidth=0, highlightthickness=0)

  frame2.chk.2.1 <- ttkcheckbutton(frame2, variable=colorblind.safe.var,
                                   text="Colorblind safe",
                                   command=function() print("notyet"))
  frame2.chk.2.2 <- ttkcheckbutton(frame2, variable=print.friendly.var,
                                   text="Print friendly",
                                   command=function() print("notyet"))
  frame2.chk.2.3 <- ttkcheckbutton(frame2, variable=photocoy.able.var,
                                   text="Photocopy-able",
                                   command=function() print("notyet"))

  tkgrid(frame2.cvs.1.1, sticky="we", columnspan=4)
  tkgrid("x", frame2.chk.2.1, frame2.chk.2.2, frame2.chk.2.3,
         padx=c(5, 0), pady=c(2, 0))
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

  frame3.scl.1.2 <- tkwidget(frame3, "ttk::scale", from=0, to=360,
                             orient="horizontal", value=h1, variable=h1.scl.var,
                             command=function(...) ScaleH1(...))
  frame3.scl.2.2 <- tkwidget(frame3, "ttk::scale", from=0, to=360,
                             orient="horizontal", value=h2, variable=h2.scl.var,
                             command=function(...) ScaleH2(...))
  frame3.scl.3.2 <- tkwidget(frame3, "ttk::scale", from=0, to=100,
                             orient="horizontal", value=c1, variable=c1.scl.var,
                             command=function(...) ScaleC1(...))
  frame3.scl.4.2 <- tkwidget(frame3, "ttk::scale", from=0, to=100,
                             orient="horizontal", value=c2, variable=c2.scl.var,
                             command=function(...) ScaleC2(...))
  frame3.scl.5.2 <- tkwidget(frame3, "ttk::scale", from=0, to=100,
                             orient="horizontal", value=l1, variable=l1.scl.var,
                             command=function(...) ScaleL1(...))
  frame3.scl.6.2 <- tkwidget(frame3, "ttk::scale", from=0, to=100,
                             orient="horizontal", value=l2, variable=l2.scl.var,
                             command=function(...) ScaleL2(...))
  frame3.scl.7.2 <- tkwidget(frame3, "ttk::scale", from=0, to=5,
                             orient="horizontal", value=p1, variable=p1.scl.var,
                             command=function(...) ScaleP1(...))
  frame3.scl.8.2 <- tkwidget(frame3, "ttk::scale", from=0, to=5,
                             orient="horizontal", value=p2, variable=p2.scl.var,
                             command=function(...) ScaleP2(...))

  frame3.ent.1.3 <- ttkentry(frame3, textvariable=h1.ent.var, width=4)
  frame3.ent.2.3 <- ttkentry(frame3, textvariable=h2.ent.var, width=4)
  frame3.ent.3.3 <- ttkentry(frame3, textvariable=c1.ent.var, width=4)
  frame3.ent.4.3 <- ttkentry(frame3, textvariable=c2.ent.var, width=4)
  frame3.ent.5.3 <- ttkentry(frame3, textvariable=l1.ent.var, width=4)
  frame3.ent.6.3 <- ttkentry(frame3, textvariable=l2.ent.var, width=4)
  frame3.ent.7.3 <- ttkentry(frame3, textvariable=p1.ent.var, width=4)
  frame3.ent.8.3 <- ttkentry(frame3, textvariable=p2.ent.var, width=4)

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

  frame4 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                          text="Number of colors in palette (does not affect returned palette)")

  frame4.lab.1 <- ttklabel(frame4, text="n", width=2)
  frame4.scl.2 <- tkwidget(frame4, "ttk::scale", from=1, to=50,
                           orient="horizontal", value=n, variable=n.scl.var,
                           command=function(...) ScaleN(...))
  frame4.ent.3 <- ttkentry(frame4, textvariable=n.ent.var, width=4)

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

  # Bind events

  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)













  # GUI control

  tkfocus(tt)
  tkgrab(tt)

  tkwait.variable(tt.done.var)

# tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
# tclServiceMode(TRUE)


}






