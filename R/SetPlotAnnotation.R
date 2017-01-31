SetPlotAnnotation <- function(parent=NULL) {


  # update titles
  UpdateTitles <- function() {
    val <- as.character(tclvalue(credit.var))
    Data("credit", if (val == "") NULL else val)

    val <- as.character(tclvalue(explanation.var))
    Data("explanation", if (val == "") NULL else val)

    val <- as.character(tclvalue(legend.title.var))
    Data("legend.title", if (val == "") NULL else val)

    val <- as.character(tclvalue(legend.subtitle.var))
    Data("legend.subtitle", if (val == "") NULL else val)

    tclvalue(tt.done.var) <- 1
  }


  # assign variables linked to tk widgets
  credit.var          <- tclVar()
  explanation.var     <- tclVar()
  legend.title.var    <- tclVar()
  legend.subtitle.var <- tclVar()
  tt.done.var         <- tclVar(0)

  if (!is.null(Data("credit")))          tclvalue(credit.var)          <- Data("credit")
  if (!is.null(Data("explanation")))     tclvalue(explanation.var)     <- Data("explanation")
  if (!is.null(Data("legend.title")))    tclvalue(legend.title.var)    <- Data("legend.title")
  if (!is.null(Data("legend.subtitle"))) tclvalue(legend.subtitle.var) <- Data("legend.subtitle")

  # open gui
  tclServiceMode(FALSE)
  tt <- tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    geo <- as.integer(geo[2:3]) + 25
    tkwm.geometry(tt, sprintf("+%s+%s", geo[1], geo[2]))
  }
  tktitle(tt) <- "Plot Annotation"
  tkwm.resizable(tt, 1, 0)

  # frame 0
  f0 <- ttkframe(tt, relief="flat")
  f0.but.2 <- ttkbutton(f0, width=12, text="OK", command=UpdateTitles)
  f0.but.3 <- ttkbutton(f0, width=12, text="Cancel",
                        command=function() tclvalue(tt.done.var) <- 1)
  f0.but.4 <- ttkbutton(f0, width=12, text="Help",
                        command=function() {
                          print(help("SetPlotAnnotation", package="RSurvey"))
                        })
  tkgrid("x", f0.but.2, f0.but.3, f0.but.4, sticky="se", pady=10, padx=c(4, 0))
  tkgrid.columnconfigure(f0, 0, weight=1)
  tkgrid.configure(f0.but.4, padx=c(4, 10))
  tkpack(f0, fill="x", side="bottom", anchor="e")

  # frame 1
  f1 <- ttkframe(tt, relief="flat", borderwidth=0, padding=10)

  f1.lab.1.1 <- ttklabel(f1, text="Map credit note")
  f1.lab.2.1 <- ttklabel(f1, text="Color-key explanation")
  f1.lab.3.1 <- ttklabel(f1, text="Point-legend title")
  f1.lab.4.1 <- ttklabel(f1, text="Point-legend subtitle")

  f1.ent.1.2 <- ttkentry(f1, width=50, textvariable=credit.var)
  f1.ent.2.2 <- ttkentry(f1, width=50, textvariable=explanation.var)
  f1.ent.3.2 <- ttkentry(f1, width=50, textvariable=legend.title.var)
  f1.ent.4.2 <- ttkentry(f1, width=50, textvariable=legend.subtitle.var)

  tkgrid(f1.lab.1.1, f1.ent.1.2, pady=c(15, 4))
  tkgrid(f1.lab.2.1, f1.ent.2.2, pady=c(0, 4))
  tkgrid(f1.lab.3.1, f1.ent.3.2, pady=c(0, 4))
  tkgrid(f1.lab.4.1, f1.ent.4.2, pady=c(0, 4))

  tkgrid.configure(f1.lab.1.1, f1.lab.2.1, f1.lab.3.1, f1.lab.4.1, sticky="w")
  tkgrid.configure(f1.ent.1.2, f1.ent.2.2, f1.ent.3.2, f1.ent.4.2,
                   sticky="we", padx=c(2, 0))

  tkgrid.columnconfigure(f1, 1, weight=1, minsize=6)
  tkpack(f1, fill="x", expand=TRUE)

  # bind events
  tclServiceMode(TRUE)
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  # gui control
  tkfocus(tt)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)
}
