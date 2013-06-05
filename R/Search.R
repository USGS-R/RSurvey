# Search

Search <- function(is.replace=FALSE, parent=NULL) {




  # Assigin global variables

  rtn <- NULL


  # Assign variables linked to Tk widgets

  match.whole.word.var <- tclVar(0)
  match.case.var <- tclVar(0)
  reg.exps.var <- tclVar(0)
  search.var <- tclVar(0)
  perl.var <- tclVar(0)

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

  if (is.replace)
    tktitle(tt) <- "Replace"
  else
    tktitle(tt) <- "Find"


  # Frame 0

  frame0 <- ttkframe(tt, relief="flat")

  if (is.replace) {
    frame0.but.1.2 <- ttkbutton(frame0, width=12, text="Replace First",
                                command=function() print("notyet"))
    frame0.but.1.3 <- ttkbutton(frame0, width=12, text="Replace All",
                                command=function() print("notyet"))
    frame0.but.1.4 <- ttkbutton(frame0, width=12, text="Cancel",
                                command=function() tclvalue(tt.done.var) <- 1)
  } else {
    frame0.but.1.2 <- "x"
    frame0.but.1.3 <- ttkbutton(frame0, width=12, text="Find",
                                command=function() print("notyet"))
    frame0.but.1.4 <- ttkbutton(frame0, width=12, text="Cancel",
                                command=function() tclvalue(tt.done.var) <- 1)
  }
  frame0.but.1.5 <- ttkbutton(frame0, width=12, text="Help",
                              command=function() {
                                print(help("Search", package="RSurvey"))
                              })
  frame0.grp.1.6 <- ttksizegrip(frame0)

  tkgrid("x", frame0.but.1.2, frame0.but.1.3, frame0.but.1.4, frame0.but.1.5,
         frame0.grp.1.6)

  tkgrid.columnconfigure(frame0, 0, weight=1)

  tkgrid.configure(frame0.but.1.3, frame0.but.1.4, padx=c(4, 0))

  if (is.replace)
    tkgrid.configure(frame0.but.1.2, padx=c(10, 0))
  else
    tkgrid.configure(frame0.but.1.3, padx=c(10, 0))


  tkgrid.configure(frame0.but.1.5, columnspan=2, padx=c(4, 10), pady=10)
  tkgrid.configure(frame0.grp.1.6, sticky="se")

  tkraise(frame0.but.1.5, frame0.grp.1.6)

  tkpack(frame0, fill="x", side="bottom", anchor="e")

  # Frame 1

  frame1 <- ttkframe(tt, relief="flat")

  frame1.lab.1.1 <- ttklabel(frame1, text="Find what:")

  frame1.txt.2.1 <- tktext(frame1, bg="white", font="TkTextFont", padx=2,
                           pady=2, width=40, height=1, undo=1, autoseparators=1,
                           wrap="none", foreground="black", relief="flat",
                           yscrollcommand=function(...)
                                            tkset(frame1.ysc.2.2, ...))
  frame1.ysc.2.2 <- ttkscrollbar(frame1, orient="vertical")
  tkconfigure(frame1.ysc.2.2, command=paste(.Tk.ID(frame1.txt.2.1), "yview"))

  tkgrid(frame1.lab.1.1, "x", sticky="w")
  tkgrid(frame1.txt.2.1, frame1.ysc.2.2)
  tkgrid.configure(frame1.txt.2.1, padx=0, pady=0, sticky="nswe")
  tkgrid.configure(frame1.ysc.2.2, sticky="ns")

  tkgrid.columnconfigure(frame1, 0, weight=1)
  tkgrid.rowconfigure(frame1, 1, weight=1)

  tkinsert(frame1.txt.2.1, "end", "notyet")
  tcl(frame1.txt.2.1, "edit", "reset")
  tcl(frame1.txt.2.1, "edit", "separator")

  if (is.replace) {
    frame1.lab.3.1 <- ttklabel(frame1, text="Replace with:")

    frame1.txt.4.1 <- tktext(frame1, bg="white", font="TkTextFont", padx=2,
                             pady=2, width=40, height=1, undo=1,
                             autoseparators=1, wrap="none", foreground="black",
                             relief="flat",
                             yscrollcommand=function(...)
                                              tkset(frame1.ysc.4.2, ...))
    frame1.ysc.4.2 <- ttkscrollbar(frame1, orient="vertical")
    tkconfigure(frame1.ysc.4.2, command=paste(.Tk.ID(frame1.txt.4.1), "yview"))

    tkgrid(frame1.lab.3.1, "x", sticky="w", pady=c(10, 0))
    tkgrid(frame1.txt.4.1, frame1.ysc.4.2)
    tkgrid.configure(frame1.txt.4.1, padx=0, pady=0, sticky="nswe")
    tkgrid.configure(frame1.ysc.4.2, sticky="ns")

    tkgrid.rowconfigure(frame1, 3, weight=1)
  }

  tkpack(frame1, fill="both", expand="yes", padx=10, pady=10)


  # Frame 3

  frame3 <- ttkframe(tt, relief="flat")

  frame3.chk.1.1 <- ttkcheckbutton(frame3, text="Match whole word only",
                                   variable=match.whole.word.var)
  frame3.chk.2.1 <- ttkcheckbutton(frame3, text="Match case",
                                   variable=match.case.var)
  frame3.chk.3.1 <- ttkcheckbutton(frame3, text="Regular expressions:",
                                   variable=reg.exps.var)
  frame3.chk.4.1 <- ttkcheckbutton(frame3, text="Search in variable:",
                                   variable=search.var)

  frame3.rad.3.2 <- ttkradiobutton(frame3, variable=perl.var, value=TRUE,
                                   text="Perl")
  frame3.rad.3.3 <- ttkradiobutton(frame3, variable=perl.var, value=FALSE,
                                   text="Unix")

  frame3.box.3.2 <- ttkcombobox(frame3, state="readonly")


  tkgrid(frame3.chk.1.1, "x", "x", sticky="w")
  tkgrid(frame3.chk.2.1, "x", "x", sticky="w", pady=2)
  tkgrid(frame3.chk.3.1, frame3.rad.3.2, frame3.rad.3.3, sticky="w")
  tkgrid(frame3.chk.4.1, frame3.box.3.2, sticky="w", pady=c(2, 5))

  tkgrid.configure(frame3.box.3.2, columnspan=2, sticky="w")


  tkpack(frame3, anchor="w", padx=10)


  # Bind events

  tclServiceMode(TRUE)


  # GUI control

  tkgrab(tt)
  tkfocus(frame1.txt.2.1)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  return(rtn)
}
