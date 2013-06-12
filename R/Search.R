# A GUI for establishing find and replace arguments.

Search <- function(is.replace=FALSE, defaults=NULL, col.names=NULL,
                   parent=NULL) {

  ## Additional functions (subroutines)

  # Return find and replace parameters
  ReturnParameters <- function(is.replace.first=FALSE) {
    find.what <- as.character(tclvalue(tkget(frame1.txt.2.1, "1.0", "end-1c")))
    if (is.replace)
      replace.with <- as.character(tclvalue(tkget(frame1.txt.4.1, "1.0",
                                                  "end-1c")))
    else
      replace.with <- NULL
    is.match.word <- as.logical(as.integer(tclvalue(match.word.var)))
    is.match.case <- as.logical(as.integer(tclvalue(match.case.var)))
    is.reg.exps   <- as.logical(as.integer(tclvalue(reg.exps.var)))
    is.search.col <- as.logical(as.integer(tclvalue(search.col.var)))
    is.perl <- as.logical(as.integer(tclvalue(perl.var)))
    col.name <- as.character(tclvalue(col.name.var))
    if (col.name == "")
      col.name <- NULL
    rtn <<- list(find.what=find.what, replace.with=replace.with,
                 is.match.word=is.match.word, is.match.case=is.match.case,
                 is.reg.exps=is.reg.exps, is.search.col=is.search.col,
                 is.perl=is.perl, col.name=col.name,
                 is.replace.first=is.replace.first)
    tclvalue(tt.done.var) <- 1
  }

  # Toggle match word
  ToggleMatchWord <- function() {
    is.match.word <- as.logical(as.integer(tclvalue(match.word.var)))
    if (is.match.word) {
      tclvalue(reg.exps.var) <- FALSE
      tkconfigure(frame3.chk.3.1, state="disabled")
    } else {
      tkconfigure(frame3.chk.3.1, state="normal")
    }
    ToggleRegExps()
  }

  # Toggle regular expression
  ToggleRegExps <- function() {
    is.reg.exps <- as.logical(as.integer(tclvalue(reg.exps.var)))
    if (is.reg.exps) {
      tkconfigure(frame3.rad.3.2, state="normal")
      tkconfigure(frame3.rad.3.3, state="normal")
    } else {
      tkconfigure(frame3.rad.3.2, state="disabled")
      tkconfigure(frame3.rad.3.3, state="disabled")
    }
  }

  # Toggle search in variable
  ToggleSearchCol <- function() {
    is.search.col <- as.logical(as.integer(tclvalue(search.col.var)))
    if (is.search.col) {
      tkconfigure(frame3.box.3.2, state="readonly")
    } else {
      tkconfigure(frame3.box.3.2, state="disabled")
    }
  }

  ## Main program

  # Assigin global variables
  rtn <- NULL

  # Variable names
  if (is.null(col.names) || !is.character(col.names))
    col.names <- ""

  # Assign variables linked to Tk widgets
  match.word.var <- tclVar(0)
  match.case.var <- tclVar(0)
  reg.exps.var <- tclVar(0)
  search.col.var <- tclVar(0)
  perl.var <- tclVar(0)
  col.name.var <- tclVar()
  tt.done.var <- tclVar(0)

  # Set default values
  find.what <- ""
  replace.with <- ""
  if (!is.null(defaults) && is.list(defaults)) {
    if (!is.null(defaults$find.what) && is.character(defaults$find.what))
      find.what <- defaults$find.what
    if (!is.null(defaults$replace.with) && is.character(defaults$replace.with))
      replace.with <- defaults$replace.with
    if (!is.null(defaults$is.match.word) && is.logical(defaults$is.match.word))
      tclvalue(match.word.var) <- defaults$is.match.word
    if (!is.null(defaults$is.match.case) && is.logical(defaults$is.match.case))
      tclvalue(match.case.var) <- defaults$is.match.case
    if (!is.null(defaults$is.reg.exps) && is.logical(defaults$is.reg.exps))
      tclvalue(reg.exps.var) <- defaults$is.reg.exps
    if (!is.null(defaults$is.search.col) && is.logical(defaults$is.search.col))
      tclvalue(search.col.var) <- defaults$is.search.col
    if (!is.null(defaults$is.perl) && is.logical(defaults$is.perl))
      tclvalue(perl.var) <- defaults$is.perl
    if (!is.null(defaults$col.name) && is.character(defaults$col.name))
      tclvalue(col.name.var) <- defaults$col.name
  }

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
                                command=function() ReturnParameters(TRUE))
    frame0.but.1.3 <- ttkbutton(frame0, width=12, text="Replace All",
                                command=function() ReturnParameters(FALSE))
    frame0.but.1.4 <- ttkbutton(frame0, width=12, text="Cancel",
                                command=function() tclvalue(tt.done.var) <- 1)
  } else {
    frame0.but.1.2 <- "x"
    frame0.but.1.3 <- ttkbutton(frame0, width=12, text="Find",
                                command=function() ReturnParameters())
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

  frame1.lab.1.1 <- ttklabel(frame1, text="Find what:", foreground="#141414")

  frame1.txt.2.1 <- tktext(frame1, bg="white", font="TkFixedFont", padx=2,
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

  tkinsert(frame1.txt.2.1, "end", find.what)
  tcl(frame1.txt.2.1, "edit", "reset")
  tcl(frame1.txt.2.1, "edit", "separator")

  if (is.replace) {
    frame1.lab.3.1 <- ttklabel(frame1, text="Replace with:",
                               foreground="#141414")
    frame1.txt.4.1 <- tktext(frame1, bg="white", font="TkFixedFont", padx=2,
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

    tkinsert(frame1.txt.4.1, "end", replace.with)
    tcl(frame1.txt.4.1, "edit", "reset")
    tcl(frame1.txt.4.1, "edit", "separator")
  }

  tkpack(frame1, fill="both", expand="yes", padx=10, pady=10)

  # Frame 3

  frame3 <- ttkframe(tt, relief="flat")

  frame3.chk.1.1 <- ttkcheckbutton(frame3, text="Match whole word only",
                                   variable=match.word.var,
                                   command=function() ToggleMatchWord())
  frame3.chk.2.1 <- ttkcheckbutton(frame3, text="Match case",
                                   variable=match.case.var)
  frame3.chk.3.1 <- ttkcheckbutton(frame3, text="Regular expressions:",
                                   variable=reg.exps.var,
                                   command=function() ToggleRegExps())
  frame3.chk.4.1 <- ttkcheckbutton(frame3, text="Search in variable:",
                                   variable=search.col.var,
                                   command=function() ToggleSearchCol())

  frame3.rad.3.2 <- ttkradiobutton(frame3, variable=perl.var, value=FALSE,
                                   text="Unix")
  frame3.rad.3.3 <- ttkradiobutton(frame3, variable=perl.var, value=TRUE,
                                   text="Perl")

  frame3.box.3.2 <- ttkcombobox(frame3, state="readonly",
                                textvariable=col.name.var, values=col.names)

  tkgrid(frame3.chk.1.1, "x", "x", sticky="w")
  tkgrid(frame3.chk.2.1, "x", "x", sticky="w", pady=2)
  tkgrid(frame3.chk.3.1, frame3.rad.3.2, frame3.rad.3.3, sticky="w")
  tkgrid(frame3.chk.4.1, frame3.box.3.2, sticky="w", pady=c(2, 10))

  tkgrid.configure(frame3.rad.3.2, padx=c(0, 4))
  tkgrid.configure(frame3.box.3.2, columnspan=2, sticky="we")
  tkgrid.columnconfigure(frame3, 2, weight=1)

  tkpack(frame3, fill="x", padx=10)

  # GUI control

  tclServiceMode(TRUE)

  ToggleRegExps()
  ToggleSearchCol()

  tkgrab(tt)
  tkfocus(frame1.txt.2.1)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  return(rtn)
}
