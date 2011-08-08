Rename <- function(names=NULL, cur.name=NULL, win.title=NULL, parent=NULL) {
  # A GUI for renaming values in a vector of character strings.

  # Additional functions (subroutines)

  # Update entry

  UpdateEntry <- function() {
    if (tclvalue(cur.var) != "" && !(tclvalue(new.var) %in% new.names))
      new.names[names %in% tclvalue(cur.var)] <<- tclvalue(new.var)
    tclvalue(new.var) <- new.names[names %in% tclvalue(old.var)]
    tclvalue(cur.var) <- tclvalue(old.var)
  }

  # Save renamed values

  SaveNames <- function() {
    UpdateEntry()
    rtn.names <<- new.names
    tclvalue(tt.done.var) <- 1
  }


  # Main program

  if (is.null(names))
    return(NULL)

  rtn.names <- new.names <- names

  # Assign the variables linked to Tk widgets

  old.var <- tclVar("")
  new.var <- tclVar("")
  cur.var <- tclVar("")

  tt.done.var <- tclVar(0)

  # Open GUI

  tclServiceMode(FALSE)
  tt <- tktoplevel(padx=25, pady=15)

  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(geo[2]) + 25,
                            "+", as.integer(geo[3]) + 25, sep=""))
  }

  if (!is.null(win.title))
    tktitle(tt) <- win.title

  tkwm.resizable(tt, 1, 0)

  # Frame 0

  frame0 <- ttkframe(tt, relief="flat", borderwidth=2)

  frame0.lab.1 <- ttklabel(frame0, text="Old name")
  frame0.lab.2 <- ttklabel(frame0, text="New name")

  if (length(names) == 1)
    prep.names <- paste("{", names, "}", sep="")
  else
    prep.names <- names

  frame0.box.1 <- ttkcombobox(frame0, state="readonly", values=prep.names,
                              textvariable=old.var)

  frame0.ent.1 <- ttkentry(frame0, textvariable=new.var)

  if (!is.null(cur.name) && cur.name %in% names)
    tcl(frame0.box.1, "current", match(cur.name, names) - 1)

  tkgrid(frame0.lab.1, frame0.box.1, pady=0)
  tkgrid(frame0.lab.2, frame0.ent.1, pady=5)

  tkgrid.configure(frame0.lab.1, frame0.lab.2, sticky="e", padx=c(0, 2))
  tkgrid.configure(frame0.box.1, frame0.ent.1, sticky="we")

  tkgrid.columnconfigure(frame0, 1, weight=1, minsize=25)

  tkpack(frame0, fill="x", expand=TRUE, pady=5)

  tkbind(frame0.box.1, "<<ComboboxSelected>>", UpdateEntry)

  # Frame 1

  frame1 <- ttkframe(tt, relief="flat", borderwidth=2)

  frame1.but.1 <- ttkbutton(frame1, width=12, text="OK",
                            command=SaveNames)
  frame1.but.2 <- ttkbutton(frame1, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)

  tkgrid(frame1.but.1, frame1.but.2, pady=c(2, 0), padx=c(4, 0))

  tkpack(frame1, anchor="e")

  UpdateEntry()

  # GUI control

  tkfocus(tt)
  tkgrab(tt)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tclServiceMode(TRUE)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)

  if (!is.null(parent))
    tkfocus(parent)

  tclServiceMode(TRUE)

  rtn.names
}
