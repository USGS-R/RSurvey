EditTimeFormat <- function(spec=NULL, parent=NULL) {
  # A GUI for constructing a date-time format.

  # Additional functions (subroutines)

  # Save conversion specification format

  SaveFormat <- function() {
    new.spec <<- as.character(tclvalue(con.var))
    tclvalue(tt.done.var) <- 1
  }

  # Selection change in the viewtree

  SelectionChange <- function() {
    cur.sel <- tcl(frame1.tre, "selection")
    cur.val <- as.character(tcl(frame1.tre, "item", cur.sel, "-values"))[1]
    tkfocus(frame2a.ent)
    if (!is.na(cur.val))
      AddString(cur.val)
  }

  # Update formated date-time entry

  UpdateFormat <- function() {
    txt <- sub("%$", "", tclvalue(con.var))
    tclvalue(fmt.var) <- if (txt == "") "" else format(d.t, format=txt)
  }

  # Add string to conversion format entry

  AddString <- function(txt) {
    if (as.logical(tcl(frame2a.ent, "selection", "present")))
      tcl(frame2a.ent, "delete", "sel.first", "sel.last")
    tkinsert(frame2a.ent, "insert", txt)
    UpdateFormat()
    tkfocus(frame2a.ent)
  }

  # Clear conversion specification format

  ClearAll <- function() {
    tclvalue(con.var) <- ""
    UpdateFormat()
    tkfocus(frame2a.ent)
  }


  # Main program

  d.t <- Sys.time()
  cur.val <- NA
  new.spec <- NULL

  # Assign variables linked to Tk widgets

  con.var <- tclVar()
  fmt.var <- tclVar()
  if (!is.null(spec) && is.character(spec)) {
     tclvalue(con.var) <- spec
     UpdateFormat()
  }

  tt.done.var <- tclVar(0)

  # Open GUI

  tclServiceMode(FALSE)
  tt <- tktoplevel()

  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    tmp <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(tmp[2]) + 25,
                            "+", as.integer(tmp[3]) + 25, sep=""))
  }
  tktitle(tt) <- "Date and Time Format"

  # Frame 0, load and cancel buttons, and size grip

  frame0 <- ttkframe(tt, relief="flat")

  frame0.but.1 <- ttkbutton(frame0, width=12, text="OK",
                            command=SaveFormat)
  frame0.but.2 <- ttkbutton(frame0, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)

  frame0.grp.3 <- ttksizegrip(frame0)

  tkgrid(frame0.but.1, frame0.but.2, frame0.grp.3)

  tkgrid.configure(frame0.but.1, sticky="e", padx=2, pady=c(8, 8))
  tkgrid.configure(frame0.but.2, sticky="w", padx=2, pady=c(8, 8), rowspan=2)
  tkgrid.configure(frame0.grp.3, sticky="se")

  tkpack(frame0, side="bottom", anchor="e")

  # Paned window

  pw <- ttkpanedwindow(tt, orient="horizontal")

  # Frame 1, treeview for conversion specifications

  frame1 <- ttkframe(pw, relief="flat", padding=0, borderwidth=0)

  frame1.tre <- ttktreeview(frame1, selectmode="browse",
                            columns=c("spec", "exam"))

  frame1.ysc <- ttkscrollbar(frame1, orient="vertical")
  tkconfigure(frame1.tre, yscrollcommand=paste(.Tk.ID(frame1.ysc), "set"))
  tkconfigure(frame1.ysc, command=paste(.Tk.ID(frame1.tre), "yview"))

  tcl(frame1.tre, "column", "#0", width=200, anchor="center")
  tcl(frame1.tre, "column", "spec", width=80, minwidth=80, anchor="center")
  tcl(frame1.tre, "column", "exam", width=80, minwidth=80, anchor="center")

  tcl(frame1.tre, "heading", "#0", text="Select", anchor="w")
  tcl(frame1.tre, "heading", "spec", text="Specification")
  tcl(frame1.tre, "heading", "exam", text="Example")

  id.yr <- tkinsert(frame1.tre, "", "end", tags="bg", text="year")
  id.mo <- tkinsert(frame1.tre, "", "end", tags="bg", text="month")
  id.dy <- tkinsert(frame1.tre, "", "end", tags="bg", text="day")
  id.hr <- tkinsert(frame1.tre, "", "end", tags="bg", text="hour")
  id.mn <- tkinsert(frame1.tre, "", "end", tags="bg", text="minute")
  id.sc <- tkinsert(frame1.tre, "", "end", tags="bg", text="second")
  id.wk <- tkinsert(frame1.tre, "", "end", tags="bg", text="week")

  tkinsert(frame1.tre, id.yr, "end", text="year without century (00-99)",
           values=c("%y", format(d.t, format="%y")), tags="bg")
  tkinsert(frame1.tre, id.yr, "end", text="year with century",
           values=c("%Y", format(d.t, format="%Y")), tags="bg")

  tkinsert(frame1.tre, id.mo, "end", text="month (01-12)",
           values=c("%m", format(d.t, format="%m")), tags="bg")
  tkinsert(frame1.tre, id.mo, "end", text="abbreviated month name",
           values=c("%b", format(d.t, format="%b")), tags="bg")
  tkinsert(frame1.tre, id.mo, "end", text="full month name",
           values=c("%B", format(d.t, format="%B")), tags="bg")

  tkinsert(frame1.tre, id.dy, "end", text="day of the month (01-31)",
           values=c("%d", format(d.t, format="%d")), tags="bg")
  tkinsert(frame1.tre, id.dy, "end", text="day of the year (001-366)",
           values=c("%j", format(d.t, format="%j")), tags="bg")
  tkinsert(frame1.tre, id.dy, "end", text="weekday (0-6, Sunday is 0)",
           values=c("%w", format(d.t, format="%w")), tags="bg")
  tkinsert(frame1.tre, id.dy, "end", text="abbreviated weekday name",
           values=c("%a", format(d.t, format="%a")), tags="bg")
  tkinsert(frame1.tre, id.dy, "end", text="full weekday name",
           values=c("%A", format(d.t, format="%A")), tags="bg")

  tkinsert(frame1.tre, id.hr, "end", text="hours (00-23)",
           values=c("%H", format(d.t, format="%H")), tags="bg")
  tkinsert(frame1.tre, id.hr, "end", text="hours (01-12)",
           values=c("%I", format(d.t, format="%I")), tags="bg")
  tkinsert(frame1.tre, id.hr, "end", text="AM/PM indicator",
           values=c("%p", format(d.t, format="%p")), tags="bg")

  tkinsert(frame1.tre, id.mn, "end", text="minute (00-59)",
           values=c("%M", format(d.t, format="%M")), tags="bg")

  tkinsert(frame1.tre, id.sc, "end", text="second (00-61)",
           values=c("%S", format(d.t, format="%S")), tags="bg")
  tkinsert(frame1.tre, id.sc, "end", text="second with decimal places",
           values=c("%OS", format(d.t, format="%OS")), tags="bg")

  tkinsert(frame1.tre, id.wk, "end", text="week of the year (00-53), US",
           values=c("%U", format(d.t, format="%U")), tags="bg")
  tkinsert(frame1.tre, id.wk, "end", text="week of the year (00-53), UK",
           values=c("%W", format(d.t, format="%W")), tags="bg")

  tktag.configure(frame1.tre, "bg", background="white")

  tkbind(frame1.tre, "<<TreeviewSelect>>", SelectionChange)

  # Frame 2

  frame2 <- ttkframe(pw, relief="flat")

  frame2a <- ttklabelframe(frame2, relief="flat", borderwidth=3, padding=3,
                           text="Conversion format")

  frame2a.ent <- ttkentry(frame2a, textvariable=con.var, width=30)
  tkicursor(frame2a.ent, "end")
  tkbind(frame2a.ent, "<KeyRelease>", UpdateFormat)

  frame2a.but.1 <- ttkbutton(frame2a, width=2, text="/",
                             command=function() AddString("/"))
  frame2a.but.2 <- ttkbutton(frame2a, width=2, text="-",
                             command=function() AddString("-"))
  frame2a.but.3 <- ttkbutton(frame2a, width=2, text=",",
                             command=function() AddString(","))
  frame2a.but.4 <- ttkbutton(frame2a, width=2, text=":",
                             command=function() AddString(":"))
  frame2a.but.5 <- ttkbutton(frame2a, width=2, text=" ",
                             command=function() AddString(" "))
  frame2a.but.6 <- ttkbutton(frame2a, width=5, text="Clear",
                             command=ClearAll)

  tkgrid(frame2a.ent, padx=5, pady=c(5, 0))
  tkgrid(frame2a.but.1, frame2a.but.2, frame2a.but.3, frame2a.but.4,
         frame2a.but.5, frame2a.but.6, padx=2, pady=c(8, 5), sticky="e")

  tkgrid.configure(frame2a.ent, sticky="we", columnspan=6)
  tkgrid.configure(frame2a.but.1, padx=c(5, 2))
  tkgrid.configure(frame2a.but.6, padx=c(15, 5))

  tcl("grid", "anchor", frame2a, "w")
  tkgrid.columnconfigure(frame2a, 0, weight=1, minsize=13)

  tkpack(frame2a, fill="both", expand=TRUE, padx=c(5, 0), pady=c(0, 2))

  frame2b <- ttklabelframe(frame2, relief="flat", borderwidth=3, padding=3,
                           text="Formatted date and time")

  frame2b.ent <- ttkentry(frame2b, textvariable=fmt.var, width=30,
                          state="readonly", takefocus=FALSE)

  tkgrid(frame2b.ent, padx=5, pady=5)
  tkgrid.configure(frame2b.ent, sticky="we")
  tcl("grid", "anchor", frame2b, "w")
  tkgrid.columnconfigure(frame2b, 0, weight=1, minsize=13)

  tkpack(frame2b, fill="both", expand=TRUE, padx=c(5, 0), pady=c(5, 2))


  frame2c <- ttklabelframe(frame2, relief="flat", borderwidth=3, padding=3,
                           text="Example")

  fg <- "#414042"
  fmt <- "%Y-%m-%d %H:%M:%S"
  frame2c.lab.1 <- ttklabel(frame2c, foreground=fg, text=fmt)
  frame2c.lab.2 <- ttklabel(frame2c, foreground=fg, text=format(d.t, format=fmt))

  tkgrid(frame2c.lab.1, padx=5, pady=c(5, 1))
  tkgrid(frame2c.lab.2, padx=5, pady=c(1, 5))
  tcl("grid", "anchor", frame2c, "w")
  tkgrid.columnconfigure(frame2c, 0, weight=1, minsize=13)

  tkpack(frame2c, fill="both", expand=TRUE, padx=c(5, 0), pady=c(5, 0))

  # Layout paned window

  tkgrid(frame1.tre, frame1.ysc, frame2)

  tkgrid.configure(frame1.ysc, padx=0, pady=c(20, 0), sticky="nws")
  tkgrid.configure(frame1.tre, padx=0, pady=0, sticky="news")

  tkgrid.rowconfigure(frame1, frame1.tre, weight=1)
  tkgrid.columnconfigure(frame1, frame1.tre, weight=1)

  tkadd(pw, frame1, weight=2)
  tkadd(pw, frame2, weight=1)

  tkpack(pw, fill="both", expand=TRUE, padx=10, pady=c(10, 5))

  # GUI control

  tkfocus(frame2a.ent)
  tkgrab(tt)
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tclServiceMode(TRUE)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  new.spec
}
