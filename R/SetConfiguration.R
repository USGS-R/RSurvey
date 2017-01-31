SetConfiguration <- function(parent=NULL) {


  UpdatePar <- function() {
    val <- as.numeric(tclvalue(cex.pts.var))
    Data("cex.pts", if (is.na(val)) NULL else val)

    val <- as.integer(tclvalue(nlevels.var))
    Data("nlevels", if (is.na(val)) NULL else val)

    val <- as.numeric(tclvalue(asp.yx.var))
    Data("asp.yx", if (is.na(val)) NULL else val)

    val <- as.numeric(tclvalue(asp.zx.var))
    Data("asp.zx", if (is.na(val)) NULL else val)

    val <- as.character(tclvalue(legend.loc.var))
    Data("legend.loc", if (val == "") NULL else val)

    val <- as.character(tclvalue(scale.loc.var))
    Data("scale.loc", if (val == "") NULL else val)

    val <- as.character(tclvalue(arrow.loc.var))
    Data("arrow.loc", if (val == "") NULL else val)

    Data("useRaster",       as.integer(tclvalue(useRaster.var)))
    Data("draw.key",        as.integer(tclvalue(draw.key.var)))
    Data("dms.tick",        as.integer(tclvalue(dms.tick.var)))
    Data("contour.lines",   as.integer(tclvalue(contour.lines.var)))
    Data("make.intervals",  as.integer(tclvalue(make.intervals.var)))
    Data("proportional",    as.integer(tclvalue(proportional.var)))
    Data("quantile.breaks", as.integer(tclvalue(quantile.breaks.var)))
    Data("bg.lines",        as.integer(tclvalue(bg.lines.var)))

    lim <- as.integer(c(tclvalue(width.var), tclvalue(height.var)))
    # TODO(JCF): check for missing values
    Data("max.dev.dim", lim)

    tclvalue(tt.done.var) <- 1
  }


 # toggle widget state
  ToggleState <- function() {
    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE))
    dev.dim <- as.character(tclvalue(dev.dim.var))

    if (dev.dim == "Single column") {
      lim <- c(21L, 56L)
    } else if (dev.dim == "Double column") {
      lim <- c(43L, 56L)
    } else if (dev.dim == "Sidetitle") {
      lim <- c(56L, 43L)
    } else {
      lim <- c(tclvalue(width.var), tclvalue(height.var))
    }

    tclvalue(width.var)  <- lim[1]
    tclvalue(height.var) <- lim[2]

    s <- ifelse(dev.dim == "Custom\u2026", "normal", "readonly")
    tkconfigure(f1.ent.09.2, state=s)
    tkconfigure(f1.ent.10.2, state=s)
  }


  # assign variables linked to tk widgets
  cex.pts.var         <- tclVar()
  nlevels.var         <- tclVar()
  asp.yx.var          <- tclVar()
  asp.zx.var          <- tclVar()
  legend.loc.var      <- tclVar()
  scale.loc.var       <- tclVar()
  arrow.loc.var       <- tclVar()

  useRaster.var       <- tclVar()
  draw.key.var        <- tclVar()
  dms.tick.var        <- tclVar()
  contour.lines.var   <- tclVar()
  make.intervals.var  <- tclVar()
  proportional.var    <- tclVar()
  quantile.breaks.var <- tclVar()
  bg.lines.var        <- tclVar()

  if (!is.null(Data("cex.pts")))         tclvalue(cex.pts.var)         <- Data("cex.pts")
  if (!is.null(Data("nlevels")))         tclvalue(nlevels.var)         <- Data("nlevels")
  if (!is.null(Data("asp.yx")))          tclvalue(asp.yx.var)          <- Data("asp.yx")
  if (!is.null(Data("asp.zx")))          tclvalue(asp.zx.var)          <- Data("asp.zx")
  if (!is.null(Data("legend.loc")))      tclvalue(legend.loc.var)      <- Data("legend.loc")
  if (!is.null(Data("scale.loc")))       tclvalue(scale.loc.var)       <- Data("scale.loc")
  if (!is.null(Data("arrow.loc")))       tclvalue(arrow.loc.var)       <- Data("arrow.loc")

  if (!is.null(Data("useRaster")))       tclvalue(useRaster.var)       <- Data("useRaster")
  if (!is.null(Data("draw.key")))        tclvalue(draw.key.var)        <- Data("draw.key")
  if (!is.null(Data("dms.tick")))        tclvalue(dms.tick.var)        <- Data("dms.tick")
  if (!is.null(Data("contour.lines")))   tclvalue(contour.lines.var)   <- Data("contour.lines")
  if (!is.null(Data("make.intervals")))  tclvalue(make.intervals.var)  <- Data("make.intervals")
  if (!is.null(Data("proportional")))    tclvalue(proportional.var)    <- Data("proportional")
  if (!is.null(Data("quantile.breaks"))) tclvalue(quantile.breaks.var) <- Data("quantile.breaks")
  if (!is.null(Data("bg.lines")))        tclvalue(bg.lines.var)        <- Data("bg.lines")

  lim <- if (is.null(Data("max.dev.dim"))) c(43L, 56L) else Data("max.dev.dim")
  width.var  <- tclVar(as.integer(lim[1]))
  height.var <- tclVar(as.integer(lim[2]))
  if (identical(lim, c(21L, 56L))) {
    dev.dim <- "Single column"
  } else if (identical(lim, c(43L, 56L))) {
    dev.dim <- "Double column"
  } else if (identical(lim, c(56L, 43L))) {
    dev.dim <- "Sidetitle"
  } else {
    dev.dim <- "Custom\u2026"
  }
  dev.dim.var <- tclVar(dev.dim)

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

  # frame 0
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

  # frame 1
  f1 <- ttkframe(tt, relief="flat", borderwidth=0, padding=10)

  f1.lab.01.1 <- ttklabel(f1, text="Scaling for point symbols")
  f1.lab.02.1 <- ttklabel(f1, text="Approx. number of contours")
  f1.lab.03.1 <- ttklabel(f1, text="Horizontal aspect ratio")
  f1.lab.04.1 <- ttklabel(f1, text="Vertical aspect ratio")
  f1.lab.05.1 <- ttklabel(f1, text="Point legend position")
  f1.lab.06.1 <- ttklabel(f1, text="Scale bar position")
  f1.lab.07.1 <- ttklabel(f1, text="North arrow position")
  f1.lab.08.1 <- ttklabel(f1, text="Graphics page dimensions")
  f1.lab.09.1 <- ttklabel(f1, text="Width of graphics device in picas")
  f1.lab.10.1 <- ttklabel(f1, text="Height of graphics device in picas")

  f1.ent.01.2 <- ttkentry(f1, width=15, textvariable=cex.pts.var)
  f1.ent.02.2 <- ttkentry(f1, width=15, textvariable=nlevels.var)
  f1.ent.03.2 <- ttkentry(f1, width=15, textvariable=asp.yx.var)
  f1.ent.04.2 <- ttkentry(f1, width=15, textvariable=asp.zx.var)
  f1.ent.09.2 <- ttkentry(f1, width=15, textvariable=width.var)
  f1.ent.10.2 <- ttkentry(f1, width=15, textvariable=height.var)

  loc.vals <- c("", "bottomleft", "topleft", "topright", "bottomright")
  dim.vals <- c("Double column", "Single column", "Sidetitle", "Custom\u2026")
  f1.box.05.2 <- ttkcombobox(f1, width=15, state="readonly", values=loc.vals,
                             textvariable=legend.loc.var)
  f1.box.06.2 <- ttkcombobox(f1, width=15, state="readonly", values=loc.vals,
                             textvariable=scale.loc.var)
  f1.box.07.2 <- ttkcombobox(f1, width=15, state="readonly", values=loc.vals,
                             textvariable=arrow.loc.var)
  f1.box.08.2 <- ttkcombobox(f1, width=15, state="readonly", values=dim.vals,
                             textvariable=dev.dim.var)








  f1.chk.01.3 <- ttkcheckbutton(f1, text="Use bitmap raster image",
                                variable=useRaster.var)
  f1.chk.02.3 <- ttkcheckbutton(f1, text="Add color key for gridded data",
                                variable=draw.key.var)
  f1.chk.03.3 <- ttkcheckbutton(f1, text="Show axes tickmarks in DMS",
                                variable=dms.tick.var)
  f1.chk.04.3 <- ttkcheckbutton(f1, text="Add contour lines",
                                variable=contour.lines.var)
  f1.chk.05.3 <- ttkcheckbutton(f1, text="Apply data binning",
                                variable=make.intervals.var)
  f1.chk.06.3 <- ttkcheckbutton(f1, text="Show proportional point symbols",
                                variable=proportional.var)
  f1.chk.07.3 <- ttkcheckbutton(f1, text="Use quantile break points",
                                variable=quantile.breaks.var)
  f1.chk.08.3 <- ttkcheckbutton(f1, text="Add grids and graticules",
                                variable=bg.lines.var)

  tkgrid(f1.lab.01.1, f1.ent.01.2, f1.chk.01.3, pady=c(15, 4))
  tkgrid(f1.lab.02.1, f1.ent.02.2, f1.chk.02.3, pady=c(0, 4))
  tkgrid(f1.lab.03.1, f1.ent.03.2, f1.chk.03.3, pady=c(0, 4))
  tkgrid(f1.lab.04.1, f1.ent.04.2, f1.chk.04.3, pady=c(0, 4))
  tkgrid(f1.lab.05.1, f1.box.05.2, f1.chk.05.3, pady=c(0, 4))
  tkgrid(f1.lab.06.1, f1.box.06.2, f1.chk.06.3, pady=c(0, 4))
  tkgrid(f1.lab.07.1, f1.box.07.2, f1.chk.07.3, pady=c(0, 4))
  tkgrid(f1.lab.08.1, f1.box.08.2, f1.chk.08.3, pady=c(0, 4))
  tkgrid(f1.lab.09.1, f1.ent.09.2, "x", pady=c(0, 4))
  tkgrid(f1.lab.10.1, f1.ent.10.2, "x", pady=c(0, 4))

  tkgrid.configure(f1.lab.01.1, f1.lab.02.1, f1.lab.03.1, f1.lab.04.1,
                   f1.lab.05.1, f1.lab.06.1, f1.lab.07.1, f1.lab.08.1,
                   f1.lab.09.1, f1.lab.10.1, sticky="w")
  tkgrid.configure(f1.ent.01.2, f1.ent.02.2, f1.ent.03.2, f1.ent.04.2,
                   f1.box.05.2, f1.box.06.2, f1.box.07.2, f1.box.08.2,
                   f1.ent.09.2, f1.ent.10.2, sticky="we", padx=c(2, 15))
  tkgrid.configure(f1.chk.01.3, f1.chk.02.3, f1.chk.03.3, f1.chk.04.3,
                   f1.chk.05.3, f1.chk.06.3, f1.chk.07.3, f1.chk.08.3,
                   sticky="w")

  tkgrid.columnconfigure(f1, 1, weight=1, minsize=6)
  tkpack(f1, fill="x", expand=TRUE)

  # bind events
  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tkbind(f1.ent.01.2, "<KeyRelease>",
         function() {
           tclvalue(nlevels.var) <- CheckEntry("integer", tclvalue(nlevels.var))
         })
  tkbind(f1.ent.02.2, "<KeyRelease>",
         function() {
           tclvalue(cex.pts.var) <- CheckEntry("numeric", tclvalue(cex.pts.var))
         })
  tkbind(f1.ent.03.2, "<KeyRelease>",
         function() {
           tclvalue(asp.yx.var) <- CheckEntry("numeric", tclvalue(asp.yx.var))
         })
  tkbind(f1.ent.04.2, "<KeyRelease>",
         function() {
           tclvalue(asp.zx.var) <- CheckEntry("numeric", tclvalue(asp.zx.var))
         })
  tkbind(f1.ent.09.2, "<KeyRelease>",
         function() {
           tclvalue(width.var) <- CheckEntry("numeric", tclvalue(width.var))
         })
  tkbind(f1.ent.10.2, "<KeyRelease>",
         function() {
           tclvalue(height.var) <- CheckEntry("numeric", tclvalue(height.var))
         })

  tkbind(f1.box.08.2, "<<ComboboxSelected>>", ToggleState)

  # gui control
  ToggleState()

  tkfocus(tt)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)
}
