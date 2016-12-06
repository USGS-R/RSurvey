SetConfiguration <- function(parent=NULL) {


  UpdatePar <- function() {
    val <- as.numeric(tclvalue(width.var))
    Data("width", if (is.na(val)) NULL else val)

    val <- as.integer(tclvalue(nlevels.var))
    Data("nlevels", if (is.na(val)) NULL else val)

    val <- as.numeric(tclvalue(cex.pts.var))
    Data("cex.pts", if (is.na(val)) NULL else val)

    val <- as.numeric(tclvalue(asp.yx.var))
    Data("asp.yx", if (is.na(val)) NULL else val)

    val <- as.numeric(tclvalue(asp.zx.var))
    Data("asp.zx", if (is.na(val)) NULL else val)

    Data("rkey",         as.integer(tclvalue(rkey.var)))
    Data("img.contour",  as.integer(tclvalue(img.contour.var)))
    Data("show.lines",   as.integer(tclvalue(show.lines.var)))
    Data("show.points",  as.integer(tclvalue(show.points.var)))
    Data("show.poly",    as.integer(tclvalue(show.poly.var)))
    Data("show.2.axes",  as.integer(tclvalue(show.2.axes.var)))
    Data("minor.ticks",  as.integer(tclvalue(minor.ticks.var)))
    Data("ticks.inside", as.integer(tclvalue(ticks.inside.var)))
    Data("rm.pnt.line",  as.integer(tclvalue(rm.pnt.line.var)))

    tclvalue(tt.done.var) <- 1
  }


  # assign variables linked to Tk widgets
  width.var        <- tclVar()
  nlevels.var      <- tclVar()
  cex.pts.var      <- tclVar()
  asp.yx.var       <- tclVar()
  asp.zx.var       <- tclVar()
  rkey.var         <- tclVar()
  show.poly.var    <- tclVar()
  img.contour.var  <- tclVar()
  show.lines.var   <- tclVar()
  show.points.var  <- tclVar()
  show.2.axes.var  <- tclVar()
  minor.ticks.var  <- tclVar()
  ticks.inside.var <- tclVar()
  rm.pnt.line.var  <- tclVar()

  if (!is.null(Data("width")))        tclvalue(width.var)        <- Data("width")
  if (!is.null(Data("nlevels")))      tclvalue(nlevels.var)      <- Data("nlevels")
  if (!is.null(Data("cex.pts")))      tclvalue(cex.pts.var)      <- Data("cex.pts")
  if (!is.null(Data("asp.yx")))       tclvalue(asp.yx.var)       <- Data("asp.yx")
  if (!is.null(Data("asp.zx")))       tclvalue(asp.zx.var)       <- Data("asp.zx")
  if (!is.null(Data("rkey")))         tclvalue(rkey.var)         <- Data("rkey")
  if (!is.null(Data("show.poly")))    tclvalue(show.poly.var)    <- Data("show.poly")
  if (!is.null(Data("img.contour")))  tclvalue(img.contour.var)  <- Data("img.contour")
  if (!is.null(Data("show.lines")))   tclvalue(show.lines.var)   <- Data("show.lines")
  if (!is.null(Data("show.points")))  tclvalue(show.points.var)  <- Data("show.points")
  if (!is.null(Data("show.2.axes")))  tclvalue(show.2.axes.var)  <- Data("show.2.axes")
  if (!is.null(Data("minor.ticks")))  tclvalue(minor.ticks.var)  <- Data("minor.ticks")
  if (!is.null(Data("ticks.inside"))) tclvalue(ticks.inside.var) <- Data("ticks.inside")
  if (!is.null(Data("rm.pnt.line")))  tclvalue(rm.pnt.line.var)  <- Data("rm.pnt.line")

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
  f1.lab.2.1 <- ttklabel(f1, text="Approximate number of contour levels")
  f1.lab.3.1 <- ttklabel(f1, text="Scaling for point symbols")
  f1.lab.4.1 <- ttklabel(f1, text="Horizontal aspect ratio")
  f1.lab.5.1 <- ttklabel(f1, text="Vertical aspect ratio")

  f1.ent.1.2 <- ttkentry(f1, width=8, textvariable=width.var)
  f1.ent.2.2 <- ttkentry(f1, width=8, textvariable=nlevels.var)
  f1.ent.3.2 <- ttkentry(f1, width=8, textvariable=cex.pts.var)
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

  f2.chk.01.1 <- ttkcheckbutton(f2, text="Reverse legend",
                                variable=rkey.var)
  f2.chk.02.1 <- ttkcheckbutton(f2, text="Show polygons",
                                variable=show.poly.var)
  f2.chk.03.1 <- ttkcheckbutton(f2, text="Use image contour",
                                variable=img.contour.var)
  f2.chk.04.1 <- ttkcheckbutton(f2, text="Show contour lines",
                                variable=show.lines.var)
  f2.chk.05.1 <- ttkcheckbutton(f2, text="Show points on maps",
                                variable=show.points.var)
  f2.chk.06.1 <- ttkcheckbutton(f2, text="Show tickmarks on second axis",
                                variable=show.2.axes.var)
  f2.chk.07.1 <- ttkcheckbutton(f2, text="Add minor tickmarks",
                                variable=minor.ticks.var)
  f2.chk.08.1 <- ttkcheckbutton(f2, text="Place tickmarks inside plot region",
                                variable=ticks.inside.var)
  f2.chk.09.1 <- ttkcheckbutton(f2, text="Remove point symbol boundary line",
                                variable=rm.pnt.line.var)

  tkgrid(f2.chk.01.1, sticky="w", pady=c(0, 2))
  tkgrid(f2.chk.02.1, sticky="w", pady=c(0, 2))
  tkgrid(f2.chk.03.1, sticky="w", pady=c(0, 2))
  tkgrid(f2.chk.04.1, sticky="w", pady=c(0, 2))
  tkgrid(f2.chk.05.1, sticky="w", pady=c(0, 2))
  tkgrid(f2.chk.06.1, sticky="w", pady=c(0, 2))
  tkgrid(f2.chk.07.1, sticky="w", pady=c(0, 2))
  tkgrid(f2.chk.08.1, sticky="w", pady=c(0, 2))
  tkgrid(f2.chk.09.1, sticky="w")

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
