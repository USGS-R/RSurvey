ImportData <- function(parent=NULL) {
  # A GUI for reading table formatted data.

  # Additional functions (subroutines)

  # Raise error message for bad connection

  RaiseError <- function(type, detail) {
    msg <- NULL
    msg[1L] <- "Connection to data source failed."
    msg[2L] <- "Problems occured while reading data from text file."
    tkmessageBox(icon="error", message=msg[type], detail=detail, title="Error",
                 type="ok", parent=tt)
  }

  # Raise warning message if data already exists

  RaiseWarning <- function(parent) {
    if (!is.null(Data("cols"))) {
      msg <- "This action will delete existing data?"
      ans <- as.character(tkmessageBox(icon="question", message=msg,
                                       title="Warning", type="okcancel",
                                       parent=parent))
      if (ans == "ok")
        Data(clear.data=TRUE)
    }
  }

  # Establish data connection

  GetConnection <- function(src) {
    if (src == "") {
      con <- try(textConnection(cb), silent=TRUE)
    } else if (substr(src, 1, 6) %in% c("http:/", "ftp://", "file:/")) {
      con <- try(url(description=src, open="r", encoding=enc), silent=TRUE)
    } else if (attr(GetFile(file=src), "extension") == "gz") {
      con <- try(gzfile(description=src, open="r", encoding=enc,
                        compression=6), silent=TRUE)
    } else {
      con <- try(file(description=src, open="r", encoding=enc), silent=TRUE)
    }
    con
  }

  # Read file and populate example table

  ReadFile <- function(summary.only=TRUE) {
    src <- as.character(tclvalue(source.var))
    con <- GetConnection(src)

    if (inherits(con, "try-error") || !isOpen(con, "r")) {
      RaiseError(1L, con)
      return()
    }

    skp <- as.integer(tclvalue(skip.var))
    if (is.na(skp) || skp < 0)
      skp <- 0

    nrw <- as.integer(tclvalue(nrow.var))
    if (is.na(nrw))
      nrw <- -1
    if (nrw > 0 && nrw < nrows)
      nrows <- nrw

    sep <- sep0[as.integer(tcl(frame3.box.1.2, "current")) + 1]
    dec <- dec0[as.integer(tcl(frame3.box.1.5, "current")) + 1]
    nas <- nas0[as.integer(tcl(frame3.box.2.2, "current")) + 1]
    quo <- quo0[as.integer(tcl(frame3.box.2.5, "current")) + 1]
    com <- com0[as.integer(tcl(frame3.box.3.2, "current")) + 1]
    
    if (is.na(sep)) {
      sep <- as.character(tclvalue(sep.var))
    }
    if (is.na(nas)) {
      nas <- as.character(tclvalue(nas.var))
      if (nas == "")
        nas <- "NA"
    }
    if (is.na(com))
      com <- as.character(tclvalue(com.var))
    
    if (summary.only) {
      d <- try(read.table(con, header=FALSE, sep=sep, quote=quo, dec=dec, 
                          row.names=NULL, na.strings=c("", nas), 
                          colClasses="character", nrows=nrows, skip=skp, 
                          check.names=TRUE, fill=TRUE, strip.white=TRUE, 
                          blank.lines.skip=TRUE, comment.char=com, 
                          allowEscapes=TRUE, flush=TRUE), silent=TRUE)
      close(con)
      if (inherits(d, "try-error")) {
        RaiseError(2L, d)
        return()
      }

      # Remove columns containing all NA values
      is.all.na <- sapply(seq(along=d), function(i) all(is.na(d[, i])))
      d <- d[, !is.all.na, drop=FALSE]
      return(d)

    } else {
      hds <- as.logical(c(as.integer(tclvalue(decis.var)),
                          as.integer(tclvalue(names.var))))

      RaiseWarning(tt)
      if (!is.null(Data("cols"))) {
        close(con)
        return()
      }

      tkconfigure(tt, cursor="watch")
      ans <- ReadData(con, headers=hds, sep=sep, dec=dec, quote=quo, nrows=nrw,
                      na.strings=c("", nas), skip=skp, comment.char=com)
      tkconfigure(tt, cursor="arrow")
      close(con)

      if (inherits(ans, "try-error")) {
        RaiseError(2L, ans)
        return()
      }

      if (!is.null(ans)) {
        Data("table.headers", hds)
        Data("table.skip", skp)
        Data("table.sep", sep)
        Data("table.dec", dec)
        Data("table.na", nas)
        Data("table.quote", quo)
        Data("comment.char", com)
        tclvalue(tt.done.var) <- 1
      }
    }
  }

  # Rebuild table

  RebuildTable <- function() {
    sep <- sep0[as.integer(tcl(frame3.box.1.2, "current")) + 1]
    ent.state <- if (is.na(sep)) "normal" else "disabled"
    tkconfigure(frame3.ent.1.3, state=ent.state)
    if (ent.state == "normal")
      tkfocus(frame3.ent.1.3)
    nas <- nas0[as.integer(tcl(frame3.box.2.2, "current")) + 1]
    ent.state <- if (is.na(nas)) "normal" else "disabled"
    tkconfigure(frame3.ent.2.3, state=ent.state)
    if (ent.state == "normal")
      tkfocus(frame3.ent.2.3)
    com <- com0[as.integer(tcl(frame3.box.3.2, "current")) + 1]
    ent.state <- if (is.na(com)) "normal" else "disabled"
    tkconfigure(frame3.ent.3.3, state=ent.state)
    if (ent.state == "normal")
      tkfocus(frame3.ent.3.3)
    
    if (tclvalue(source.var) == "" && is.null(cb))
      return()

    d <- ReadFile()
    if (is.null(d))
      return()

    tclServiceMode(FALSE)
    ResetGUI()

    insert.rows <- nrow(d) - 1 - GetEndRow()
    insert.cols <- ncol(d) - 1 - GetEndCol()

    tkconfigure(frame4.tbl, state="normal")

    if (insert.rows > 0)
      tkinsert(frame4.tbl, "rows", "end", insert.rows)
    if (insert.cols > 0)
      tkinsert(frame4.tbl, "cols", "end", insert.cols)

    for(j in 1:ncol(d))
      sapply(1:nrow(d), function(i)
        table.var[[i - 1, j - 1]] <- as.tclObj(d[i, j], drop=TRUE))
    
    for (i in 1:ncol(d)) {
      len <- max(nchar(gsub("\t", "    ", d[1:nrows, i])), na.omit=TRUE) + 1
      if (len < 7)
        len <- 7
      tcl(frame4.tbl, "width", i - 1, len)
    }

    SetTags()
    tkconfigure(frame4.tbl, state="disabled")
    tclServiceMode(TRUE)
  }

  # Determine the number of lines in a file

  NumLinesInFile <- function() {
    src <- as.character(tclvalue(source.var))
    con <- GetConnection(src)
    if (inherits(con, "try-error"))
      return()

    tkconfigure(tt, cursor="watch")
    total.rows <- 0
    while ((read.rows <- length(readLines(con, n=50000))) > 0)
      total.rows <- total.rows + read.rows
    tkconfigure(tt, cursor="arrow")

    close(con)
    tclvalue(nrow.var) <- total.rows
  }

  # Data file

  GetDataFile <- function() {
    exts <- c("txt", "csv", "dat", "gz")
    f <- GetFile(cmd="Open", exts=exts, win.title="Open Data File", parent=tt)
    tkfocus(tt)
    if (is.null(f))
      return()
    tclvalue(source.var) <- f
    tclvalue(nrow.var) <- ""
    cb <<- NULL
    if (attr(f, "extension") == "csv")
      tcl(frame3.box.1.2, "current", match(",", sep0) - 1)

    RebuildTable()
  }

  # Paste clipboard

  PasteData <- function() {
    tkselection.set(frame4.tbl, "origin")

    cb <- try(scan(file="clipboard", what="character", sep="\n", quiet=TRUE),
              silent=TRUE)
    cb <<- if (inherits(cb, "try-error")) NULL else cb

    if (is.null(cb))
      return()
    tclvalue(source.var) <- ""
    RebuildTable()
  }

  # Clear all

  ClearData <- function() {
    cb <<- NULL
    tclvalue(source.var) <- ""
    tclvalue(nrow.var) <- ""
    tclServiceMode(FALSE)
    ResetGUI()
    tclServiceMode(TRUE)
  }

  # Reset GUI

  ResetGUI <- function() {
    tkconfigure(frame4.tbl, state="normal")

    tcl("unset", table.var)
    if (GetEndRow() > 0)
      tkdelete(frame4.tbl, "rows", 1, GetEndRow())
    if (GetEndCol() > 0)
      tkdelete(frame4.tbl, "cols", 1, GetEndCol())
    tcl(frame4.tbl, "clear", "all")

    tkselection.set(frame4.tbl, "origin")
    tkconfigure(frame4.tbl, state="disabled")

    s <- "normal"
    if (tclvalue(source.var) == "" && is.null(cb))
      s <- "disabled"
    tkconfigure(frame0.but.4, state=s)
  }

  # Set tags in table

  SetTags <- function() {
    if (GetEndRow() == 0 & GetEndCol() == 0)
      return()

    tcl(frame4.tbl, "clear", "tags")
    tcl(frame4.tbl, "tag", "row", "h1", 0)
    tcl(frame4.tbl, "tag", "row", "h2", 1)

    logic <- as.logical(as.integer(c(tclvalue(decis.var), tclvalue(names.var))))
    headCol <- c("#EBFFC6", "#FFD0D4")[logic]
    if (length(headCol) < 2)
      headCol[(length(headCol) + 1):2] <- "white"

    tktag.configure(frame4.tbl, "h1", background=headCol[1])
    tktag.configure(frame4.tbl, "h2", background=headCol[2])
  }

  # Determine the tables maximum row and column

  GetEndRow <- function() as.numeric(tkindex(frame4.tbl, "end", "row"))
  GetEndCol <- function() as.numeric(tkindex(frame4.tbl, "end", "col"))


  # Main program

  # GUI requires Tktable

  is.tktable <- !inherits(try(tcl("package", "present", "Tktable"),
                          silent=TRUE), "try-error")

  if (!is.tktable) {
    f <- GetFile(cmd="Open", exts="txt", win.title="Open Data File",
                 parent=parent)
    if (!is.null(f)) {
      RaiseWarning(parent)
      if (is.null(Data("cols")))
        ReadData(f)
    }
    return()
  }

  # Initialize values

  cb <- NULL

  nrows <- 50

  sep0 <- c("\t", "", ",", ";", "|", NA)
  sep1 <- c("Tab ( \\t )", "White space (  )", "Comma ( , )",
            "Semicolon ( ; )", "Pipe ( | )", "Custom\u2026")

  nas0 <- c("NA", "na", "N/A", "n/a", NA)
  nas1 <- c("NA", "na", "N/A", "n/a", "Custom\u2026")

  quo0 <- c("", "\"", "'")
  quo1 <- c("", "Double ( \" )", "Single ( \' )")
  
  dec0 <- c(".", ",")
  dec1 <- c("Period ( . )", "Comma ( , )")

  com0 <- c("", "#", "!", "\\", "~", NA)
  com1 <- c("", "Number sign ( # )", "Exclamation ( ! )",
            "Backslash ( \\\\ )", "Tilde ( ~ )", "Custom\u2026")

  enc <- Data("encoding")

  # Assign variables linked to Tk widgets

  table.var <- tclArray()
  
  decis.var   <- tclVar(0)
  names.var   <- tclVar(0)
  skip.var    <- tclVar(0)
  nrow.var    <- tclVar()
  source.var  <- tclVar()
  sep.var     <- tclVar()
  nas.var     <- tclVar()
  comment.var <- tclVar()

  tt.done.var <- tclVar(0)

  # Set header variables

  if (!is.null(Data("table.headers"))) {
    tclvalue(decis.var) <- Data("table.headers")[1]
    tclvalue(names.var) <- Data("table.headers")[2]
  }

  # Open GUI

  tclServiceMode(FALSE)

  tt <- tktoplevel(padx=0, pady=0)

  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(geo[2]) + 25,
                            "+", as.integer(geo[3]) + 25, sep=""))
  }

  tktitle(tt) <- "Import Data"

  # Frame 0 contains load and cancel buttons, and size grip

  frame0 <- ttkframe(tt, relief="flat")

  frame0.but.1 <- ttkbutton(frame0, width=8, text="Paste",
                            command=PasteData)
  frame0.but.2 <- ttkbutton(frame0, width=8, text="Clear",
                            command=ClearData)
  frame0.but.4 <- ttkbutton(frame0, width=12, text="Load",
                            command=function() ReadFile(FALSE))
  frame0.but.5 <- ttkbutton(frame0, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)
  frame0.but.6 <- ttkbutton(frame0, width=12, text="Help",
                            command=function() {
                              print(help("ImportData", package="RSurvey"))
                            })
  frame0.grp.7 <- ttksizegrip(frame0)

  tkgrid(frame0.but.1, frame0.but.2, "x", frame0.but.4, frame0.but.5, 
         frame0.but.6, frame0.grp.7)

  tkgrid.columnconfigure(frame0, 2, weight=1)

  tkgrid.configure(frame0.but.1, frame0.but.2,
                   sticky="n", padx=c(0, 4), pady=c(4, 0))
  tkgrid.configure(frame0.but.1, padx=c(10, 4))
  
  tkgrid.configure(frame0.but.4, frame0.but.5, frame0.but.6, 
                   padx=c(0, 4), pady=c(15, 10))
  tkgrid.configure(frame0.but.6, columnspan=2, padx=c(0, 10))
  
  tkgrid.configure(frame0.grp.7, sticky="se")

  tkraise(frame0.but.6, frame0.grp.7)

  tkpack(frame0, fill="x", side="bottom", anchor="e")

  tkconfigure(frame0.but.4, state="disabled")

  # Frame 1, file locator

  frame1 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0)

  frame1.lab.1 <- ttklabel(frame1, text="Import data from")
  txt <- paste("    or transfer data from clipboard via a copy and paste",
               "operation.")
  frame1.lab.2 <- ttklabel(frame1, text=txt)

  frame1.ent.1 <- ttkentry(frame1, textvariable=source.var)
  frame1.but.1 <- ttkbutton(frame1, width=8, text="Browse",
                            command=GetDataFile)

  tkgrid(frame1.lab.1, frame1.ent.1, frame1.but.1, pady=c(10, 0))
  tkgrid(frame1.lab.2, "x", "x", pady=c(5, 0))

  tkgrid.configure(frame1.lab.1, sticky="w")
  tkgrid.configure(frame1.ent.1, sticky="we", padx=2)

  tkgrid.configure(frame1.lab.2, columnspan=3, sticky="w")

  tkgrid.columnconfigure(frame1, 1, weight=1)

  tkpack(frame1, fill="x", anchor="w", padx=10)

  # Frame 2, header line information

  frame2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                          text="Header lines")

  txt <- paste("Conversion specification formats of the variables,",
               "for example, '%10.6f' or '%Y-%m-%d %H:%M'")
  frame2.chk.1.1 <- ttkcheckbutton(frame2, variable=decis.var,
                                   command=SetTags, text=txt)
  txt <- "Names of the variables, that is, column names in the data table"
  frame2.chk.2.1 <- ttkcheckbutton(frame2, variable=names.var,
                                   command=SetTags, text=txt)

  tkgrid(frame2.chk.1.1, pady=1, sticky="w")
  tkgrid(frame2.chk.2.1, pady=1, sticky="w")

  tkpack(frame2, anchor="w", fill="x", padx=10, pady=10)

  # Frame 3, import parameters

  frame3 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                          text="Select import parameters")

  frame3.lab.1.1 <- ttklabel(frame3, text="Seperator")
  frame3.lab.1.4 <- ttklabel(frame3, text="Decimal")
  frame3.lab.1.6 <- ttklabel(frame3, text="Skip lines")
  frame3.lab.2.1 <- ttklabel(frame3, text="NA strings")
  frame3.lab.2.4 <- ttklabel(frame3, text="Quote")
  frame3.lab.2.6 <- ttklabel(frame3, text="Max lines")
  frame3.lab.3.1 <- ttklabel(frame3, text="Comment")

  frame3.box.1.2 <- ttkcombobox(frame3, width=17, state="readonly", value=sep1)
  frame3.box.1.5 <- ttkcombobox(frame3, width=12, state="readonly", value=dec1)
  frame3.box.2.2 <- ttkcombobox(frame3, width=17, state="readonly", value=nas1)
  frame3.box.2.5 <- ttkcombobox(frame3, width=12, state="readonly", value=quo1)
  frame3.box.3.2 <- ttkcombobox(frame3, width=17, state="readonly", value=com1)
  
  frame3.ent.1.3 <- ttkentry(frame3, width=12, textvariable=sep.var)
  frame3.ent.1.7 <- ttkentry(frame3, width=12, textvariable=skip.var)
  frame3.ent.2.3 <- ttkentry(frame3, width=12, textvariable=nas.var)
  frame3.ent.2.7 <- ttkentry(frame3, width=12, textvariable=nrow.var)
  frame3.ent.3.3 <- ttkentry(frame3, width=12, textvariable=comment.var)

  frame3.but.2.8 <- ttkbutton(frame3, width=2, image=GetBitmapImage("find"),
                              command=NumLinesInFile)

  tkgrid(frame3.lab.1.1, frame3.box.1.2, frame3.ent.1.3, frame3.lab.1.4, 
         frame3.box.1.5, frame3.lab.1.6, frame3.ent.1.7, "x")
  tkgrid(frame3.lab.2.1, frame3.box.2.2, frame3.ent.2.3, frame3.lab.2.4, 
         frame3.box.2.5, frame3.lab.2.6, frame3.ent.2.7, frame3.but.2.8, 
         pady=c(4, 0))
  tkgrid(frame3.lab.3.1, frame3.box.3.2, frame3.ent.3.3, 
         "x", "x", "x", "x", "x", pady=c(4, 0))

  tkgrid.configure(frame3.lab.1.1, frame3.lab.1.4, frame3.lab.1.6,
                   frame3.lab.2.1, frame3.lab.2.4, frame3.lab.2.6,
                   frame3.lab.3.1, padx=c(10, 2), sticky="w")

  tkgrid.configure(frame3.lab.1.1, frame3.lab.2.1, frame3.lab.3.1, padx=c(0, 2))
  tkgrid.configure(frame3.ent.1.3, frame3.ent.2.3, frame3.ent.3.3, padx=c(2, 0))
  tkgrid.configure(frame3.but.2.8, padx=c(2, 0))

  tkpack(frame3, anchor="w", fill="x", padx=10, pady=c(0, 15))

  tcl(frame3.box.1.2, "current", 0)
  tcl(frame3.box.1.5, "current", 0)
  tcl(frame3.box.2.2, "current", 0)
  tcl(frame3.box.2.5, "current", 0)
  tcl(frame3.box.3.2, "current", 0)

  if (!is.null(Data("table.skip")))
    tclvalue(skip.var) <- Data("table.skip")
  
  if (!is.null(Data("table.sep"))) {
    if (Data("table.sep") %in% sep0) {
      tcl(frame3.box.1.2, "current", match(Data("table.sep"), sep0) - 1)
      tkconfigure(frame3.ent.1.3, state="disabled")
    } else {
      tcl(frame3.box.1.2, "current", match(NA, sep0) - 1)
      tkconfigure(frame3.ent.1.3, state="normal")
      tclvalue(sep.var) <- Data("table.sep")
    }
  }
  if (!is.null(Data("table.na"))) {
    if (Data("table.na") %in% nas0) {
      tcl(frame3.box.2.2, "current", match(Data("table.na"), nas0) - 1)
      tkconfigure(frame3.ent.2.3, state="disabled")
    } else {
      tcl(frame3.box.2.2, "current", match(NA, nas0) - 1)
      tkconfigure(frame3.ent.2.3, state="normal")
      tclvalue(nas.var) <- Data("table.na")
    }
  }
  if (!is.null(Data("comment.char"))) {
    if (Data("comment.char") %in% com0) {
      tcl(frame3.box.3.2, "current", match(Data("comment.char"), com0) - 1)
      tkconfigure(frame3.ent.3.3, state="disabled")
    } else {
      tcl(frame3.box.3.2, "current", match(NA, com0) - 1)
      tkconfigure(frame3.ent.3.3, state="normal")
      tclvalue(com.var) <- Data("comment.char")
    }
  }
  
  if (!is.null(Data("table.dec")))
    tcl(frame3.box.1.5, "current", match(Data("table.dec"), dec0) - 1)
  if (!is.null(Data("table.quote")))
    tcl(frame3.box.2.5, "current", match(Data("table.quote"), quo0) - 1)

  # Frame 4, example data table

  frame4 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0)

  .Tcl("option add *Table.font {CourierNew 9}")
  frame4.tbl <- tkwidget(frame4, "table", rows=1, cols=1, variable=table.var,
                         state="disabled", colwidth=13, rowheight=1, width=1,
                         height=1, ipadx=3, ipady=1, wrap=0,
                         highlightcolor="gray75", background="white",
                         foreground="black", titlerows=0, titlecols=0,
                         multiline=0, resizeborders="col",
                         bordercursor="sb_h_double_arrow", cursor="plus",
                         colstretchmode="none", rowstretchmode="none",
                         anchor="nw", drawmode="single", rowseparator="\n",
                         colseparator="\t", selectmode="extended",
                         insertofftime=0, highlightthickness=0,
                         xscrollcommand=function(...) tkset(frame4.xsc,...),
                         yscrollcommand=function(...) tkset(frame4.ysc,...))

  frame4.xsc <- tkscrollbar(frame4, orient="horizontal",
                            command=function(...) tkxview(frame4.tbl,...))
  frame4.ysc <- tkscrollbar(frame4, orient="vertical",
                            command=function(...) tkyview(frame4.tbl,...))

  tkgrid(frame4.tbl, frame4.ysc)
  tkgrid(frame4.xsc, "x")

  tkgrid.configure(frame4.tbl, sticky="news", padx=c(10, 0), pady=0)
  tkgrid.configure(frame4.ysc, sticky="ns", padx=c(0, 10), pady=0)
  tkgrid.configure(frame4.xsc, sticky="we", padx=c(10, 0), pady=0)

  tktag.configure(frame4.tbl, "active", background="#EAEEFE", relief="")
  tktag.configure(frame4.tbl, "sel", background="#EAEEFE", foreground="black")

  tkgrid.columnconfigure(frame4, 0, weight=1)
  tkgrid.rowconfigure(frame4, 0, weight=1)

  tkpack(frame4, fill="both", expand=TRUE)

  tkselection.set(frame4.tbl, "origin")

  # Bind events

  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tkbind(frame1.ent.1, "<Return>", RebuildTable)

  tkbind(frame3.box.1.2, "<<ComboboxSelected>>", RebuildTable)
  tkbind(frame3.box.1.5, "<<ComboboxSelected>>", RebuildTable)
  tkbind(frame3.box.2.2, "<<ComboboxSelected>>", RebuildTable)
  tkbind(frame3.box.2.5, "<<ComboboxSelected>>", RebuildTable)
  tkbind(frame3.box.3.2, "<<ComboboxSelected>>", RebuildTable)
  
  tkbind(frame3.ent.1.3, "<KeyRelease>", RebuildTable)
  tkbind(frame3.ent.2.3, "<KeyRelease>", RebuildTable)
  tkbind(frame3.ent.3.3, "<KeyRelease>", RebuildTable)
  
  tkbind(frame3.ent.1.7, "<KeyRelease>",
         function() {
           tclvalue(skip.var) <- CheckEntry("integer", tclvalue(skip.var))
           RebuildTable()
         }
  )
  tkbind(frame3.ent.2.7, "<KeyRelease>",
         function() {
           tclvalue(nrow.var) <- CheckEntry("integer", tclvalue(nrow.var))
           RebuildTable()
         }
  )

  tkbind(frame4.tbl, "<<Paste>>", PasteData)

  # GUI control

  RebuildTable()

  tkgrab(tt)
  tkfocus(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  invisible()
}
