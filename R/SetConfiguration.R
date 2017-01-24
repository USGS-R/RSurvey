SetConfiguration <- function(parent=NULL) {


  UpdatePar <- function() {
    val <- as.numeric(tclvalue(width.var))
    Data("width", if (is.na(val)) NULL else val)

    val <- as.numeric(tclvalue(cex.pts.var))
    Data("cex.pts", if (is.na(val)) NULL else val)

    val <- as.integer(tclvalue(nlevels.var))
    Data("nlevels", if (is.na(val)) NULL else val)

    val <- as.numeric(tclvalue(asp.yx.var))
    Data("asp.yx", if (is.na(val)) NULL else val)

    val <- as.numeric(tclvalue(asp.zx.var))
    Data("asp.zx", if (is.na(val)) NULL else val)

    Data("useRaster",     as.integer(tclvalue(useRaster.var)))
    Data("contour.lines", as.integer(tclvalue(contour.lines.var)))
    Data("dms.tick",      as.integer(tclvalue(dms.tick.var)))
    Data("bg.lines",      as.integer(tclvalue(bg.lines.var)))


    tclvalue(tt.done.var) <- 1
  }


  # assign variables linked to tk widgets
  width.var         <- tclVar()
  cex.pts.var       <- tclVar()
  nlevels.var       <- tclVar()
  asp.yx.var        <- tclVar()
  asp.zx.var        <- tclVar()

  useRaster.var     <- tclVar()
  contour.lines.var <- tclVar()
  dms.tick.var      <- tclVar()
  bg.lines.var      <- tclVar()

  if (!is.null(Data("width")))         tclvalue(width.var)         <- Data("width")
  if (!is.null(Data("cex.pts")))       tclvalue(cex.pts.var)       <- Data("cex.pts")
  if (!is.null(Data("nlevels")))       tclvalue(nlevels.var)       <- Data("nlevels")
  if (!is.null(Data("asp.yx")))        tclvalue(asp.yx.var)        <- Data("asp.yx")
  if (!is.null(Data("asp.zx")))        tclvalue(asp.zx.var)        <- Data("asp.zx")

  if (!is.null(Data("useRaster")))     tclvalue(useRaster.var)     <- Data("useRaster")
  if (!is.null(Data("contour.lines"))) tclvalue(contour.lines.var) <- Data("contour.lines")
  if (!is.null(Data("dms.tick")))      tclvalue(dms.tick.var)      <- Data("dms.tick")
  if (!is.null(Data("bg.lines")))      tclvalue(bg.lines.var)      <- Data("bg.lines")

  tt.done.var <- tclVar(0)

  # open gui
  tclServiceMode(FALSE)
  tt <- tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    geo <- as.integer(geo[2:3]) + 25
    tkwm.geometry(tt, sprintf("+%s+%s", geo[1], geo[2]))
  }
  tktitle(tt) <- "Configuration"
  tkwm.resizable(tt, 1, 0)

  # frame 0 contains ok and cancel buttons
  f0 <- ttkframe(tt, relief="flat")
  f0.but.2 <- ttkbutton(f0, width=12, text="OK", command=UpdatePar)
  f0.but.3 <- ttkbutton(f0, width=12, text="Cancel",
                        command=function() tclvalue(tt.done.var) <- 1)
  f0.but.4 <- ttkbutton(f0, width=12, text="Help",
                        command=function() {
                          print(help("SetConfiguration", package="RSurvey"))
                        })
  tkgrid("x", f0.but.2, f0.but.3, f0.but.4, sticky="se", pady=10, padx=c(4, 0))
  tkgrid.columnconfigure(f0, 0, weight=1)
  tkgrid.configure(f0.but.4, padx=c(4, 10))
  tkpack(f0, fill="x", side="bottom", anchor="e")

  # paned window
  pw <- ttkpanedwindow(tt, orient="horizontal")

  # frame 1 contains parameters
  f1 <- ttkframe(pw, relief="flat", borderwidth=0, padding=10)

  f1.lab.1.1 <- ttklabel(f1, text="Width of plotting window canvas, in inches")
  f1.lab.2.1 <- ttklabel(f1, text="Scaling for point symbols")
  f1.lab.3.1 <- ttklabel(f1, text="Approximate number of contour levels")
  f1.lab.4.1 <- ttklabel(f1, text="Horizontal aspect ratio")
  f1.lab.5.1 <- ttklabel(f1, text="Vertical aspect ratio")

  f1.ent.1.2 <- ttkentry(f1, width=8, textvariable=width.var)
  f1.ent.2.2 <- ttkentry(f1, width=8, textvariable=cex.pts.var)
  f1.ent.3.2 <- ttkentry(f1, width=8, textvariable=nlevels.var)
  f1.ent.4.2 <- ttkentry(f1, width=8, textvariable=asp.yx.var)
  f1.ent.5.2 <- ttkentry(f1, width=8, textvariable=asp.zx.var)

  tkgrid(f1.lab.1.1, f1.ent.1.2, pady=c(15, 4))
  tkgrid(f1.lab.2.1, f1.ent.2.2, pady=c(0, 4))
  tkgrid(f1.lab.3.1, f1.ent.3.2, pady=c(0, 4))
  tkgrid(f1.lab.4.1, f1.ent.4.2, pady=c(0, 4))
  tkgrid(f1.lab.5.1, f1.ent.5.2)

  tkgrid.configure(f1.lab.1.1, f1.lab.2.1, f1.lab.3.1, f1.lab.4.1, f1.lab.5.1, sticky="w")
  tkgrid.configure(f1.ent.1.2, f1.ent.2.2, f1.ent.3.2, f1.ent.4.2, f1.ent.5.2,
                   padx=c(2, 15), sticky="we")

  tkgrid.columnconfigure(f1, 1, weight=1, minsize=6)

  # frame 2 contains plot features
  f2 <- ttkframe(pw, relief="flat", borderwidth=0, padding=10)

  f2.chk.01.1 <- ttkcheckbutton(f2, text="Use bitmap raster",     variable=useRaster.var)
  f2.chk.02.1 <- ttkcheckbutton(f2, text="Show contour lines",    variable=contour.lines.var)
  f2.chk.03.1 <- ttkcheckbutton(f2, text="Axes tickmarks in DMS", variable=dms.tick.var)
  f2.chk.04.1 <- ttkcheckbutton(f2, text="Draw graticule",        variable=bg.lines.var)

  tkgrid(f2.chk.01.1, sticky="w", pady=c(15, 4))
  tkgrid(f2.chk.02.1, sticky="w", pady=c(0, 4))
  tkgrid(f2.chk.03.1, sticky="w", pady=c(0, 4))
  tkgrid(f2.chk.04.1, sticky="w")

  # final layout
  tkgrid(f1, f2, sticky="nswe")
  tkgrid.columnconfigure(pw, 0, weight=2)
  tkpack(pw, fill="x", expand=TRUE)

  # bind events
  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tkbind(f1.ent.1.2, "<KeyRelease>",
         function() {
           tclvalue(width.var) <- CheckEntry("numeric", tclvalue(width.var))
         })
  tkbind(f1.ent.2.2, "<KeyRelease>",
         function() {
           tclvalue(nlevels.var) <- CheckEntry("integer", tclvalue(nlevels.var))
         })
  tkbind(f1.ent.3.2, "<KeyRelease>",
         function() {
           tclvalue(cex.pts.var) <- CheckEntry("numeric", tclvalue(cex.pts.var))
         })
  tkbind(f1.ent.4.2, "<KeyRelease>",
         function() {
           tclvalue(asp.yx.var) <- CheckEntry("numeric", tclvalue(asp.yx.var))
         })
  tkbind(f1.ent.5.2, "<KeyRelease>",
         function() {
           tclvalue(asp.zx.var) <- CheckEntry("numeric", tclvalue(asp.zx.var))
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
