FormatDateTime <- function(sample=as.POSIXct("1991-08-25 20:57:08"),
                           spec=NULL, parent=NULL) {
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
    tkfocus(frame2b.ent)
    if (!is.na(cur.val))
      AddString(cur.val)
  }

  # Update formated date-time entry

  UpdateFormat <- function() {
    txt <- sub("%$", "", tclvalue(con.var))
    tclvalue(fmt.var) <- if (txt == "") "" else format(sample, format=txt)
  }

  # Add string to conversion format entry

  AddString <- function(txt) {
    if (as.logical(tcl(frame2b.ent, "selection", "present")))
      tcl(frame2b.ent, "delete", "sel.first", "sel.last")
    tkinsert(frame2b.ent, "insert", txt)
    UpdateFormat()
    tkfocus(frame2b.ent)
  }

  # Copy conversion specification to clipboard

  CopySpec <- function() {
    txt <- as.character(tclvalue(con.var))
    writeClipboard(txt)
  }

  # Paste conversion specification from clipboard

  PasteSpec <- function() {
    tclvalue(con.var) <- readClipboard()
    UpdateFormat()
    tkfocus(frame2b.ent)
  }

 # Clear conversion specification

  ClearSpec <- function() {
    tclvalue(con.var) <- ""
    UpdateFormat()
    tkfocus(frame2b.ent)
  }


  # Main program

  if (!inherits(sample, "POSIXct"))
    stop("Sample object must be of class POSIXct or POSIXlt.")

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
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(geo[2]) + 25,
                            "+", as.integer(geo[3]) + 25, sep=""))
  }
  tktitle(tt) <- "Format Date and Time"

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
  tcl(frame1.tre, "column", "exam", width=100, minwidth=80, anchor="center")

  tcl(frame1.tre, "heading", "#0", text="Select", anchor="w")
  tcl(frame1.tre, "heading", "spec", text="Specification")
  tcl(frame1.tre, "heading", "exam", text="Example")

  id.dt <- tkinsert(frame1.tre, "", "end", tags="bg", text="date")
  id.tm <- tkinsert(frame1.tre, "", "end", tags="bg", text="time")
  id.yr <- tkinsert(frame1.tre, "", "end", tags="bg", text="year")
  id.mo <- tkinsert(frame1.tre, "", "end", tags="bg", text="month")
  id.dy <- tkinsert(frame1.tre, "", "end", tags="bg", text="day")
  id.hr <- tkinsert(frame1.tre, "", "end", tags="bg", text="hour")
  id.mn <- tkinsert(frame1.tre, "", "end", tags="bg", text="minute")
  id.sc <- tkinsert(frame1.tre, "", "end", tags="bg", text="second")
  id.wk <- tkinsert(frame1.tre, "", "end", tags="bg", text="week")

  tkinsert(frame1.tre, id.dt, "end", tags="bg", text="month day year",
           values=c("%m/%d/%Y", format(sample, format="%m/%d/%Y")))
  tkinsert(frame1.tre, id.dt, "end", tags="bg", text="month day year",
           values=c("%m/%d/%y", format(sample, format="%m/%d/%y")))
  tkinsert(frame1.tre, id.dt, "end", tags="bg", text="year month day",
           values=c("%Y-%m-%d", format(sample, format="%Y-%m-%d")))
  tkinsert(frame1.tre, id.dt, "end", tags="bg", text="day month year",
           values=c("%d%b%Y", format(sample, format="%d%b%Y")))
  tkinsert(frame1.tre, id.dt, "end", tags="bg", text="weekday month day",
           values=c("%A %B %d", format(sample, format="%A %B %d")))
  tkinsert(frame1.tre, id.dt, "end", tags="bg", text="weekday month day",
           values=c("%a %b %d", format(sample, format="%a %b %d")))

  tkinsert(frame1.tre, id.tm, "end", tags="bg", text="hour minute second",
           values=c("%H:%M:%S", format(sample, format="%H:%M:%S")))
  tkinsert(frame1.tre, id.tm, "end", tags="bg", text="hour minute second",
           values=c("%H:%M:%OS", format(sample, format="%H:%M:%OS")))
  tkinsert(frame1.tre, id.tm, "end", tags="bg", text="hour minute",
           values=c("%I:%M %p", format(sample, format="%I:%M %p")))

  tkinsert(frame1.tre, id.yr, "end", tags="bg",
           text="year without century (00-99)",
           values=c("%y", format(sample, format="%y")))
  tkinsert(frame1.tre, id.yr, "end", tags="bg", text="year with century",
           values=c("%Y", format(sample, format="%Y")))

  tkinsert(frame1.tre, id.mo, "end", tags="bg", text="month (01-12)",
           values=c("%m", format(sample, format="%m")))
  tkinsert(frame1.tre, id.mo, "end", tags="bg", text="abbreviated month name",
           values=c("%b", format(sample, format="%b")))
  tkinsert(frame1.tre, id.mo, "end", tags="bg", text="full month name",
           values=c("%B", format(sample, format="%B")))

  tkinsert(frame1.tre, id.dy, "end", tags="bg", text="day of the month (01-31)",
           values=c("%d", format(sample, format="%d")))
  tkinsert(frame1.tre, id.dy, "end", tags="bg",
           text="day of the year (001-366)",
           values=c("%j", format(sample, format="%j")))
  tkinsert(frame1.tre, id.dy, "end",  tags="bg",
           text="weekday (0-6, Sunday is 0)",
           values=c("%w", format(sample, format="%w")))

  tkinsert(frame1.tre, id.dy, "end", tags="bg", text="abbreviated weekday name",
           values=c("%a", format(sample, format="%a")))
  tkinsert(frame1.tre, id.dy, "end", tags="bg", text="full weekday name",
           values=c("%A", format(sample, format="%A")))
  tkinsert(frame1.tre, id.hr, "end", tags="bg", text="hours (00-23)",
           values=c("%H", format(sample, format="%H")))
  tkinsert(frame1.tre, id.hr, "end", tags="bg", text="hours (01-12)",
           values=c("%I", format(sample, format="%I")))
  tkinsert(frame1.tre, id.hr, "end", tags="bg", text="AM/PM indicator",
           values=c("%p", format(sample, format="%p")))

  tkinsert(frame1.tre, id.mn, "end", tags="bg", text="minute (00-59)",
           values=c("%M", format(sample, format="%M")))

  tkinsert(frame1.tre, id.sc, "end", tags="bg", text="second (00-61)",
           values=c("%S", format(sample, format="%S")))
  tkinsert(frame1.tre, id.sc, "end",  tags="bg",
           text="second with decimal places",
           values=c("%OS", format(sample, format="%OS")))

  tkinsert(frame1.tre, id.wk, "end", tags="bg",
           text="week of the year (00-53), US",
           values=c("%U", format(sample, format="%U")))
  tkinsert(frame1.tre, id.wk, "end", tags="bg",
           text="week of the year (00-53), UK",
           values=c("%W", format(sample, format="%W")))

  tktag.configure(frame1.tre, "bg", background="white")

  tkbind(frame1.tre, "<<TreeviewSelect>>", SelectionChange)

  # Frame 2

  frame2 <- ttkframe(pw, relief="flat")

  frame2a <- ttklabelframe(frame2, relief="flat", borderwidth=5, padding=5,
                           text="Sample")
  frame2a.ent <- ttkentry(frame2a, textvariable=fmt.var, width=30,
                          state="readonly", takefocus=FALSE)
  tkgrid(frame2a.ent, padx=0, pady=5)
  tkgrid.configure(frame2a.ent, sticky="we")
  tcl("grid", "anchor", frame2a, "w")
  tkgrid.columnconfigure(frame2a, 0, weight=1, minsize=13)
  tkpack(frame2a, fill="both", expand=TRUE, padx=c(5, 0), pady=c(0, 2))

  frame2b <- ttklabelframe(frame2, relief="flat", borderwidth=5, padding=5,
                           text="Conversion specification")

  frame2b.ent <- ttkentry(frame2b, textvariable=con.var, width=30)
  tkicursor(frame2b.ent, "end")
  tkbind(frame2b.ent, "<KeyRelease>", UpdateFormat)
  frame2b.but.1 <- ttkbutton(frame2b, width=2, text="/",
                             command=function() AddString("/"))
  frame2b.but.2 <- ttkbutton(frame2b, width=2, text="-",
                             command=function() AddString("-"))
  frame2b.but.3 <- ttkbutton(frame2b, width=2, text=",",
                             command=function() AddString(","))
  frame2b.but.4 <- ttkbutton(frame2b, width=2, text=":",
                             command=function() AddString(":"))
  frame2b.but.5 <- ttkbutton(frame2b, width=2, text=" ",
                             command=function() AddString(" "))

  frame2b.but.6 <- ttkbutton(frame2b, width=2, image=GetBitmapImage("copy"),
                             command=CopySpec)
  frame2b.but.7 <- ttkbutton(frame2b, width=2, image=GetBitmapImage("paste"),
                             command=PasteSpec)
  frame2b.but.8 <- ttkbutton(frame2b, width=2, image=GetBitmapImage("delete"),
                             command=ClearSpec)

  tkgrid(frame2b.ent, pady=c(5, 0))
  tkgrid(frame2b.but.1, frame2b.but.2, frame2b.but.3, frame2b.but.4,
         frame2b.but.5, frame2b.but.6, frame2b.but.7, frame2b.but.8,
         padx=c(0, 2), pady=c(8, 5), sticky="e")
  tkgrid.configure(frame2b.ent, sticky="we", columnspan=8)

  tkgrid.configure(frame2b.but.6, padx=c(5, 2))
  tkgrid.configure(frame2b.but.8, padx=0)

  tcl("grid", "anchor", frame2b, "w")
  tkgrid.columnconfigure(frame2b, 0, weight=1, minsize=13)
  tkpack(frame2b, fill="both", expand=TRUE, padx=c(5, 0), pady=c(5, 2))

  frame2c <- ttklabelframe(frame2, relief="flat", borderwidth=5, padding=5,
                           text="Example")
  fg <- "#414042"
  fmt <- "%Y-%m-%d %H:%M:%S"
  frame2c.lab.1 <- ttklabel(frame2c, foreground=fg, text=fmt)
  frame2c.lab.2 <- ttklabel(frame2c, foreground=fg,
                            text=format(sample, format=fmt))
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

  tkfocus(frame2b.ent)
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
