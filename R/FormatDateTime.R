FormatDateTime <- function(sample=as.POSIXct("1991-08-25 20:57:08"), fmt="", parent=NULL) {


  # save format
  SaveFormat <- function() {
    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE))
    if (as.character(tclvalue(sample.var)) == "") {
      msg <- "Format results in empty character string, please try again."
      tkmessageBox(icon="error", message=msg, title="Error", type="ok", parent=tt)
      return()
    }
    fmt <- as.character(tclvalue(fmt.var))
    new.fmt <<- fmt
    tclvalue(tt.done.var) <- 1
  }


  # selection change in the viewtree
  SelectionChange <- function() {
    cur.sel <- tcl(f1.tre, "selection")
    cur.val <- as.character(tcl(f1.tre, "item", cur.sel, "-values"))[1]
    tkfocus(f2a.ent)
    if (!is.na(cur.val)) AddString(cur.val)
  }


  # update sample date-time entry
  UpdateSample <- function() {
    fmt <- sub("%$", "", tclvalue(fmt.var))
    tclvalue(sample.var) <- format(sample, format=fmt)
  }


  # add string to format entry
  AddString <- function(txt) {
    if (as.logical(tcl(f2a.ent, "selection", "present")))
      tcl(f2a.ent, "delete", "sel.first", "sel.last")
    tkinsert(f2a.ent, "insert", txt)
    UpdateSample()
    tkfocus(f2a.ent)
  }


  # expand or collapse nodes in treeview
  ToggleTreeView <- function(open.nodes) {
    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE))
    img <- if (open.nodes) img.minus else img.plus
    tkconfigure(f0.but.1, image=img, command=function() ToggleTreeView(!open.nodes))
    tcl(f1.tre, "item", id.dt, "-open", open.nodes)
    tcl(f1.tre, "item", id.yr, "-open", open.nodes)
    tcl(f1.tre, "item", id.mo, "-open", open.nodes)
    tcl(f1.tre, "item", id.wk, "-open", open.nodes)
    tcl(f1.tre, "item", id.dy, "-open", open.nodes)
    if (is.posixt) {
      tcl(f1.tre, "item", id.tm, "-open", open.nodes)
      tcl(f1.tre, "item", id.hr, "-open", open.nodes)
      tcl(f1.tre, "item", id.mn, "-open", open.nodes)
      tcl(f1.tre, "item", id.sc, "-open", open.nodes)
    }
  }


  # copy format to clipboard
  CopyFormat <- function() {
    txt <- as.character(tclvalue(fmt.var))
    cat(txt, file="clipboard")
  }


  # paste format from clipboard
  PasteFormat <- function() {
    cb <- try(scan(file="clipboard", what="character", sep="\n", quiet=TRUE), silent=TRUE)
    if (inherits(cb, "try-error")) return()
    tclvalue(fmt.var) <- cb
    UpdateSample()
    tkfocus(f2a.ent)
  }


 # clear format from entry
  ClearFormat <- function() {
    tclvalue(fmt.var) <- ""
    UpdateSample()
    tkfocus(f2a.ent)
  }


  if (!inherits(sample, c("POSIXt", "Date")))
    stop("Sample object must be of class POSIXt or Date.")
  if (!is.character(fmt))
    stop("format argument must be of class character")
  is.posixt <- inherits(sample, "POSIXt")

  cur.val <- NA
  new.fmt <- NULL
  img.plus  <- GetBitmapImage("plus")
  img.minus <- GetBitmapImage("minus")

  # assign variables linked to Tk widgets
  sample.var  <- tclVar()
  fmt.var     <- tclVar(fmt)
  tt.done.var <- tclVar(0)

  UpdateSample()

  # open gui
  tclServiceMode(FALSE)
  tt <- tktoplevel()

  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    geo <- as.integer(geo[2:3]) + 25
    tkwm.geometry(tt, sprintf("+%s+%s", geo[1], geo[2]))
  }
  tktitle(tt) <- ifelse(is.posixt, "Format Date and Time", "Format Date")

  # frame 0, load and cancel buttons, and size grip
  f0 <- ttkframe(tt, relief="flat")

  f0.but.1 <- ttkbutton(f0, width=2, image=GetBitmapImage("plus"),
                        command=function() ToggleTreeView(TRUE))
  f0.but.3 <- ttkbutton(f0, width=12, text="OK", command=SaveFormat)
  f0.but.4 <- ttkbutton(f0, width=12, text="Cancel",
                        command=function() tclvalue(tt.done.var) <- 1)
  f0.but.5 <- ttkbutton(f0, width=12, text="Help",
                        command=function() {
                          print(help("FormatDateTime", package="RSurvey"))
                        })
  f0.grp.6 <- ttksizegrip(f0)

  tkgrid(f0.but.1, "x", f0.but.3, f0.but.4, f0.but.5, f0.grp.6)

  tkgrid.columnconfigure(f0, 1, weight=1)

  tkgrid.configure(f0.but.1, padx=c(10, 0), pady=4, sticky="n")
  tkgrid.configure(f0.but.3, f0.but.4, f0.but.5, padx=c(0, 4), pady=c(15, 10))
  tkgrid.configure(f0.but.5, columnspan=2, padx=c(0, 10))
  tkgrid.configure(f0.grp.6, sticky="se")

  tkraise(f0.but.5, f0.grp.6)

  tkpack(f0, fill="x", side="bottom", anchor="e")

  # paned window
  pw <- ttkpanedwindow(tt, orient="horizontal")

  # frame 1, treeview for conversion specifications
  f1 <- ttkframe(pw, relief="flat", padding=0, borderwidth=0)

  f1.tre <- ttktreeview(f1, selectmode="browse", columns=c("spec", "exam"))

  f1.ysc <- ttkscrollbar(f1, orient="vertical")
  tkconfigure(f1.tre, yscrollcommand=paste(.Tk.ID(f1.ysc), "set"))
  tkconfigure(f1.ysc, command=paste(.Tk.ID(f1.tre), "yview"))

  tcl(f1.tre, "column", "#0",   width=230, anchor="center")
  tcl(f1.tre, "column", "spec", width=80,  minwidth=80, anchor="center")
  tcl(f1.tre, "column", "exam", width=120, minwidth=80, anchor="center")

  tcl(f1.tre, "heading", "#0", text="Select", anchor="w")
  tcl(f1.tre, "heading", "spec", text="Specification")
  tcl(f1.tre, "heading", "exam", text="Example")

  id.dt <- tkinsert(f1.tre, "", "end", tags="bg", text="date")
  id.yr <- tkinsert(f1.tre, "", "end", tags="bg", text="year")
  id.mo <- tkinsert(f1.tre, "", "end", tags="bg", text="month")
  id.wk <- tkinsert(f1.tre, "", "end", tags="bg", text="week")
  id.dy <- tkinsert(f1.tre, "", "end", tags="bg", text="day")
  tkinsert(f1.tre, id.dt, "end", tags="bg", text="month day year",
           values=c("%m/%d/%Y", format(sample, format="%m/%d/%Y")))
  tkinsert(f1.tre, id.dt, "end", tags="bg", text="month day year",
           values=c("%m/%d/%y", format(sample, format="%m/%d/%y")))
  tkinsert(f1.tre, id.dt, "end", tags="bg", text="year month day",
           values=c("%Y-%m-%d", format(sample, format="%Y-%m-%d")))
  tkinsert(f1.tre, id.dt, "end", tags="bg", text="day month year",
           values=c("%d%b%Y", format(sample, format="%d%b%Y")))
  tkinsert(f1.tre, id.dt, "end", tags="bg", text="weekday month day",
           values=c("%A %B %d", format(sample, format="%A %B %d")))
  tkinsert(f1.tre, id.dt, "end", tags="bg", text="weekday month day",
           values=c("%a %b %d", format(sample, format="%a %b %d")))
  tkinsert(f1.tre, id.yr, "end", tags="bg", text="year without century (00-99)",
           values=c("%y", format(sample, format="%y")))
  tkinsert(f1.tre, id.yr, "end", tags="bg", text="year with century",
           values=c("%Y", format(sample, format="%Y")))
  tkinsert(f1.tre, id.mo, "end", tags="bg", text="month (01-12)",
           values=c("%m", format(sample, format="%m")))
  tkinsert(f1.tre, id.mo, "end", tags="bg", text="abbreviated month name",
           values=c("%b", format(sample, format="%b")))
  tkinsert(f1.tre, id.mo, "end", tags="bg", text="full month name",
           values=c("%B", format(sample, format="%B")))
  tkinsert(f1.tre, id.wk, "end", tags="bg", text="week of the year (00-53), US",
           values=c("%U", format(sample, format="%U")))
  tkinsert(f1.tre, id.wk, "end", tags="bg", text="week of the year (00-53), UK",
           values=c("%W", format(sample, format="%W")))
  tkinsert(f1.tre, id.dy, "end", tags="bg", text="day of the month (01-31)",
           values=c("%d", format(sample, format="%d")))
  tkinsert(f1.tre, id.dy, "end", tags="bg", text="day of the year (001-366)",
           values=c("%j", format(sample, format="%j")))
  tkinsert(f1.tre, id.dy, "end",  tags="bg", text="weekday (0-6, Sunday is 0)",
           values=c("%w", format(sample, format="%w")))
  tkinsert(f1.tre, id.dy, "end", tags="bg", text="abbreviated weekday name",
           values=c("%a", format(sample, format="%a")))
  tkinsert(f1.tre, id.dy, "end", tags="bg", text="full weekday name",
           values=c("%A", format(sample, format="%A")))

  if (is.posixt) {
    id.tm <- tkinsert(f1.tre, "", "end", tags="bg", text="time")
    id.hr <- tkinsert(f1.tre, "", "end", tags="bg", text="hour")
    id.mn <- tkinsert(f1.tre, "", "end", tags="bg", text="minute")
    id.sc <- tkinsert(f1.tre, "", "end", tags="bg", text="second")
    id.tz <- tkinsert(f1.tre, "", "end", tags="bg", text="timezone")
    tkinsert(f1.tre, id.tm, "end", tags="bg", text="hour minute second",
             values=c("%H:%M:%S", format(sample, format="%H:%M:%S")))
    tkinsert(f1.tre, id.tm, "end", tags="bg", text="hour minute fractional-second",
             values=c("%H:%M:%OS3", format(sample, format="%H:%M:%OS3")))
    tkinsert(f1.tre, id.tm, "end", tags="bg", text="hour minute",
             values=c("%I:%M %p", format(sample, format="%I:%M %p")))
    tkinsert(f1.tre, id.hr, "end", tags="bg", text="hours (00-23)",
             values=c("%H", format(sample, format="%H")))
    tkinsert(f1.tre, id.hr, "end", tags="bg", text="hours (01-12)",
             values=c("%I", format(sample, format="%I")))
    tkinsert(f1.tre, id.hr, "end", tags="bg", text="AM/PM indicator",
             values=c("%p", format(sample, format="%p")))
    tkinsert(f1.tre, id.mn, "end", tags="bg", text="minute (00-59)",
             values=c("%M", format(sample, format="%M")))
    tkinsert(f1.tre, id.sc, "end", tags="bg", text="second (00-61)",
             values=c("%S", format(sample, format="%S")))
    tkinsert(f1.tre, id.sc, "end",  tags="bg", text="decisecond precision",
             values=c("%OS1", format(sample, format="%OS1")))
    tkinsert(f1.tre, id.sc, "end",  tags="bg", text="centisecond precision",
             values=c("%OS2", format(sample, format="%OS2")))
    tkinsert(f1.tre, id.sc, "end",  tags="bg", text="millisecond precision",
             values=c("%OS3", format(sample, format="%OS3")))
     tkinsert(f1.tre, id.tz, "end",  tags="bg", text="time zone (output only)",
             values=c("%Z", format(sample, format="%Z")))
  }

  tktag.configure(f1.tre, "bg", background="white")

  # frame 2
  f2 <- ttkframe(pw, relief="flat")

  f2a <- ttklabelframe(f2, relief="flat", borderwidth=5, padding=5,
                       text="Conversion specification format")

  f2a.ent <- ttkentry(f2a, textvariable=fmt.var, width=30)
  tkicursor(f2a.ent, "end")

  f2a.but.1 <- ttkbutton(f2a, width=2, text="/",
                         command=function() AddString("/"))
  f2a.but.2 <- ttkbutton(f2a, width=2, text="-",
                         command=function() AddString("-"))
  f2a.but.3 <- ttkbutton(f2a, width=2, text=",",
                         command=function() AddString(","))
  f2a.but.4 <- ttkbutton(f2a, width=2, text=":",
                         command=function() AddString(":"))
  f2a.but.5 <- ttkbutton(f2a, width=2, text=" ",
                         command=function() AddString(" "))

  f2a.but.6 <- ttkbutton(f2a, width=2, image=GetBitmapImage("copy"),
                         command=CopyFormat)
  f2a.but.7 <- ttkbutton(f2a, width=2, image=GetBitmapImage("paste"),
                         command=PasteFormat)
  f2a.but.8 <- ttkbutton(f2a, width=2, image=GetBitmapImage("delete"),
                         command=ClearFormat)

  tkgrid(f2a.ent, pady=c(5, 0))
  tkgrid(f2a.but.1, f2a.but.2, f2a.but.3, f2a.but.4, f2a.but.5, f2a.but.6, f2a.but.7,
         f2a.but.8, "x", padx=c(0, 2), pady=c(8, 5), sticky="w")
  tkgrid.configure(f2a.ent, sticky="we", columnspan=9)

  tkgrid.configure(f2a.but.6, padx=c(5, 2))
  tkgrid.configure(f2a.but.8)

  tcl("grid", "anchor", f2a, "w")
  tkgrid.columnconfigure(f2a, 8, weight=1, minsize=0)
  tkpack(f2a, fill="x", padx=c(5, 0), pady=c(5, 2))

  f2b <- ttklabelframe(f2, relief="flat", borderwidth=5, padding=5, text="Sample")
  f2b.ent <- ttkentry(f2b, textvariable=sample.var, width=30, state="readonly",
                      takefocus=FALSE)
  tkgrid(f2b.ent, pady=5)
  tkgrid.configure(f2b.ent, sticky="we")
  tcl("grid", "anchor", f2b, "w")
  tkgrid.columnconfigure(f2b, 0, weight=1, minsize=10)
  tkpack(f2b, fill="x", padx=c(5, 0), pady=c(0, 2))

  f2c <- ttklabelframe(f2, relief="flat", borderwidth=5, padding=5, text="Example")
  fg <- "#414042"
  fmt <- ifelse(is.posixt, "%Y-%m-%d %H:%M:%S", "%Y-%m-%d")
  f2c.lab.1 <- ttklabel(f2c, foreground=fg, text=fmt)
  f2c.lab.2 <- ttklabel(f2c, foreground=fg, text=format(sample, format=fmt))
  tkgrid(f2c.lab.1, padx=0, pady=c(5, 1))
  tkgrid(f2c.lab.2, padx=0, pady=c(1, 5))
  tcl("grid", "anchor", f2c, "w")
  tkpack(f2c, fill="x", padx=c(5, 0), pady=c(5, 0))

  # layout paned window
  tkgrid(f1.tre, f1.ysc, f2)

  tkgrid.configure(f1.ysc, pady=c(20, 0), sticky="nws")
  tkgrid.configure(f1.tre, sticky="news")

  tkgrid.rowconfigure(f1, f1.tre, weight=1)
  tkgrid.columnconfigure(f1, f1.tre, weight=1)

  tkadd(pw, f1, weight=2)
  tkadd(pw, f2, weight=0)

  tkpack(pw, fill="both", expand=TRUE, padx=10, pady=c(10, 0))

  # bind events
  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)
  tkbind(f1.tre, "<<TreeviewSelect>>", SelectionChange)
  tkbind(f2a.ent, "<KeyRelease>", UpdateSample)

  # gui control
  tkfocus(f2a.ent)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  return(new.fmt)
}
