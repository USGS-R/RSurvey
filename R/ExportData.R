ExportData <- function(col.ids, file.type="text", parent=NULL) {
  # Export data to file

  # Additional functions (subroutines)

  # Final export of data to file

  ExportToFile <- function() {
    file.name <- as.character(tclvalue(file.var))

    idxs <- as.integer(tkcurselection(frame1.lst.1.1)) + 1L
    col.ids <- col.ids[idxs]

    tkconfigure(tt, cursor="watch")
    if (file.type == "text") {
      is.processed <- as.logical(as.integer(tclvalue(records.var)))
      headers <- c(as.logical(as.integer(tclvalue(head.fmts.var))),
                   as.logical(as.integer(tclvalue(head.names.var))))
      sep <- as.character(tclvalue(sep.var))
      if (sep == "custom")
        sep <- as.character(tclvalue(sep.other.var))
      is.compressed <- as.logical(as.integer(tclvalue(compress.var)))
      WriteFile(file.type, file.name, col.ids, is.processed, headers, sep,
                is.compressed)
    } else {
      WriteFile(file.type, file.name, col.ids)
    }
    tkconfigure(tt, cursor="arrow")
    tclvalue(tt.done.var) <- 1
  }

  # Select all or none from variable list

  SelectVariables <- function(sel) {
    n <- length(col.ids) - 1L
    if (sel == "all") {
      tkselection.set(frame1.lst.1.1, 0, n)
    } else if (sel == "none") {
      tkselection.clear(frame1.lst.1.1, 0, n)
    } else {
      idxs <- 0:n
      sel <- as.integer(tkcurselection(frame1.lst.1.1))
      tkselection.clear(frame1.lst.1.1, 0, n)
      for (i in idxs[!(idxs %in% sel)])
        tkselection.set(frame1.lst.1.1, i)
    }
    ToggleExport()
  }

  # Get file

  GetDataFile <- function() {
    if (file.type == "text") {
      default.ext <- "txt"
      exts <- c("txt", "csv", "dat")
      is.compressed <- as.logical(as.integer(tclvalue(compress.var)))
      if (is.compressed) {
        default.ext <- "gz"
        exts <- "gz"
      }
    } else {
      default.ext <- "shp"
      exts <- "shp"
    }
    f <- GetFile(cmd="Save As", exts=exts, file=NULL, win.title="Save Data As",
                 defaultextension=default.ext)
    if (is.null(f))
      return()
    
    if (attr(f, "extension") == "csv")
      tclvalue(sep.var) <- ","
    tclvalue(file.var) <- f
    ToggleExport()
  }

  # Toggle gz extension on file entry

  ToggleExtension <- function() {
    f <- as.character(tclvalue(file.var))
    n <- nchar(f)
    if (nchar(f) < 3L)
      return()
    is.gz <- substr(f, n - 2L, n) == ".gz"
    is.compressed <- as.logical(as.integer(tclvalue(compress.var)))

    f.new <- f
    if (is.compressed & !is.gz)
      f.new <- paste(f, ".gz", sep="")
    if (!is.compressed & is.gz)
      f.new <- substr(f, 1L, n - 3L)
    if (!identical(f, f.new))
      tclvalue(file.var) <- f.new
  }

  # Toggle state of other separator entry

  ToggleSeperator <- function() {
    is.other <- as.character(tclvalue(sep.var)) == "custom"
    if (is.other) {
      tkconfigure(frame3.ent.2.4, state="normal")
      tkfocus(frame3.ent.2.4)
    } else {
      tkconfigure(frame3.ent.2.4, state="readonly")
    }
  }

  # Toggle state of export button

  ToggleExport <- function() {
    idxs <- as.integer(tkcurselection(frame1.lst.1.1))
    f <- as.character(tclvalue(file.var))
    s <- if (length(idxs) == 0L || nchar(f) == 0L) "disabled" else "normal"
    tkconfigure(frame0.but.2, state=s)
  }


  # Main program

  # Check arguments

  if (missing(col.ids) || length(col.ids) < 1L || !is.character(col.ids))
    stop()
  if (!file.type %in% c("text", "shape"))
    stop()

  # Assign variables linked to Tk widgets

  variables.var    <- tclVar()
  records.var      <- tclVar(0)
  head.fmts.var    <- tclVar(0)
  head.names.var   <- tclVar(0)
  sep.var          <- tclVar("\t")
  sep.other.var    <- tclVar()
  sort.on.var      <- tclVar()
  na.last.var      <- tclVar(1)
  decreasing.var   <- tclVar(0)
  file.var         <- tclVar()
  compress.var     <- tclVar(0)
  tt.done.var      <- tclVar(0)

  for (i in seq(along=col.ids))
    tcl("lappend", variables.var, col.ids[i])

  # Open GUI

  tclServiceMode(FALSE)
  tt <- tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(geo[2]) + 25,
                            "+", as.integer(geo[3]) + 25, sep=""))
  }
  tktitle(tt) <- "Export Data"

  # Frame 0, export and cancel buttons

  frame0 <- ttkframe(tt, relief="flat")
  frame0.but.2 <- ttkbutton(frame0, width=12, text="Export",
                            command=ExportToFile)
  frame0.but.3 <- ttkbutton(frame0, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)
  
  frame0.but.4 <- ttkbutton(frame0, width=12, text="Help",
                            command=function() {
                              print(help("ExportData", package="RSurvey"))
                            })
  frame0.grp.5 <- ttksizegrip(frame0)

  tkgrid("x", frame0.but.2, frame0.but.3, frame0.but.4, frame0.grp.5)
  tkgrid.columnconfigure(frame0, 0, weight=1)
  tkgrid.configure(frame0.but.2, frame0.but.3, frame0.but.4, 
                   padx=c(0, 4), pady=c(4, 10))
  tkgrid.configure(frame0.but.4, columnspan=2, padx=c(0, 10))
  tkgrid.configure(frame0.grp.5, sticky="se")

  tkraise(frame0.but.4, frame0.grp.5)

  tkpack(frame0, fill="x", side="bottom", anchor="e")

  # Frame 1, sample entry

  if (file.type == "text")
    txt <- "Select variables and data records"
  else
    txt <- "Select variables"
  frame1 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5, text=txt)

  frame1.lst.1.1 <- tklistbox(frame1, selectmode="extended", activestyle="none",
                              relief="flat", borderwidth=5, width=10, height=4,
                              exportselection=FALSE, listvariable=variables.var,
                              highlightthickness=0)
  frame1.ysc.1.6 <- ttkscrollbar(frame1, orient="vertical")
  tkconfigure(frame1.lst.1.1, background="white",
              yscrollcommand=paste(.Tk.ID(frame1.ysc.1.6), "set"))
  tkconfigure(frame1.ysc.1.6, command=paste(.Tk.ID(frame1.lst.1.1), "yview"))

  frame1.but.2.1 <- ttkbutton(frame1, width=8, text="All",
                              command=function() SelectVariables("all"))
  frame1.but.2.2 <- ttkbutton(frame1, width=8, text="None",
                              command=function() SelectVariables("none"))
  frame1.but.2.3 <- ttkbutton(frame1, width=8, text="Inverse",
                              command=function() SelectVariables("inverse"))
  frame1.chk.2.4 <- ttkcheckbutton(frame1, variable=records.var,
                                   text="Include only processed records")

  tkgrid(frame1.lst.1.1, "x", "x", "x", "x", frame1.ysc.1.6)
  tkgrid(frame1.but.2.1, frame1.but.2.2, frame1.but.2.3, frame1.chk.2.4, 
         "x", "x", pady=c(4, 0))

  tkgrid.configure(frame1.lst.1.1, sticky="nsew", columnspan=5)
  tkgrid.configure(frame1.ysc.1.6, sticky="ns")
  tkgrid.configure(frame1.but.2.1, frame1.but.2.2, padx=c(0, 4))
  tkgrid.configure(frame1.chk.2.4, padx=c(25, 0))

  tkgrid.columnconfigure(frame1, 4, weight=1, minsize=0)
  tkgrid.rowconfigure(frame1, 0, weight=1)

  tkpack(frame1, fill="both", expand=TRUE, side="top", padx=10, pady=10)

  if (file.type == "text") {

    # Frame 2, header lines

    frame2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                            text="Add metadata")
    frame2.chk.1.1 <- ttkcheckbutton(frame2, variable=head.fmts.var,
                                     text="Conversion specification formats")
    frame2.chk.1.2 <- ttkcheckbutton(frame2, variable=head.names.var,
                                     text="Column names")

    tkgrid(frame2.chk.1.1, frame2.chk.1.2)
    tkgrid.configure(frame2.chk.1.1, padx=c(15, 15))

    tkpack(frame2, fill="x", padx=10, pady=c(0, 10))

    # Frame 3, export parmaters

    frame3 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                            text="Export parameters")

    frame3.ent.2.4 <- ttkentry(frame3, width=7, textvariable=sep.other.var,
                               state="readonly")
    frame3.rad.1.1 <- ttkradiobutton(frame3, variable=sep.var, value="\t",
                                     text="Tab", width=13,
                                     command=ToggleSeperator)
    frame3.rad.1.2 <- ttkradiobutton(frame3, variable=sep.var, value="",
                                     text="Space", width=13,
                                     command=ToggleSeperator)
    frame3.rad.1.3 <- ttkradiobutton(frame3, variable=sep.var, value="|",
                                     text="Pipe",
                                     command=ToggleSeperator)
    frame3.rad.2.1 <- ttkradiobutton(frame3, variable=sep.var, value=";",
                                     text="Semicolon", width=13,
                                     command=ToggleSeperator)
    frame3.rad.2.2 <- ttkradiobutton(frame3, variable=sep.var, value=",",
                                     text="Comma", width=13,
                                     command=ToggleSeperator)
    frame3.rad.2.3 <- ttkradiobutton(frame3, variable=sep.var,
                                     value="custom", text="Custom\u2026",
                                     command=ToggleSeperator)

    tkgrid(frame3.rad.1.1, frame3.rad.1.2, frame3.rad.1.3, "x", sticky="w")
    tkgrid(frame3.rad.2.1, frame3.rad.2.2, frame3.rad.2.3, frame3.ent.2.4,
           sticky="w")
    
    tkgrid.configure(frame3.rad.1.1, frame3.rad.2.1, padx=c(15, 0))
    
    tkgrid.configure(frame3.rad.1.3, columnspan=2)
    tkgrid.configure(frame3.ent.2.4, padx=c(2, 0))

    tkpack(frame3, fill="x", padx=10, pady=c(0, 10))
  }

  # Frame 4, output file and compression

  frame4 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                          text="Output file")
  frame4.ent.1.1 <- ttkentry(frame4, width=12, textvariable=file.var)
  frame4.but.1.3 <- ttkbutton(frame4, width=8, text="Browse",
                              command=GetDataFile)

  txt <- "Compress file using gzip"
  frame4.chk.2.2 <- ttkcheckbutton(frame4, variable=compress.var, text=txt,
                                   command=ToggleExtension)

  tkgrid(frame4.ent.1.1, frame4.but.1.3)
  if (file.type == "text")
    tkgrid(frame4.chk.2.2, "x", pady=c(4, 0), sticky="w")

  tkgrid.configure(frame4.ent.1.1, sticky="we", padx=c(0, 2))
  tkgrid.columnconfigure(frame4, 0, weight=1)
  tkpack(frame4, fill="x", padx=10, pady=c(0, 15))

  # Bind events

  tclServiceMode(TRUE)
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)
  tkbind(frame1.lst.1.1, "<<ListboxSelect>>", ToggleExport)
  tkbind(frame4.ent.1.1, "<KeyRelease>", ToggleExport)

  # GUI control

  ToggleExport()

  tkfocus(frame1.lst.1.1)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)
}
