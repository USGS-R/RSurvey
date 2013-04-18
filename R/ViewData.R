ViewData <- function(d, col.names=NULL, col.formats=NULL, is.editable=FALSE, 
                     parent=NULL) {
# A GUI for viewing table formatted data.

  # Additional functions (subroutines)

  # Copy values to clipboard
  CopyValues <- function() {
    tcl("tk_tableCopy", frame2.tbl)
  }

  # Select all cells
  SelectAll <- function() {
    tkselection.set(frame2.tbl, "origin", "end")
  }

  # Find value in table

  Find <- function(direction="next") {
    pattern <- as.character(tclvalue(pattern.var))
    if (pattern == "")
      return()

    if (is.null(matched.cells)) {
      n <- ncol(d) - 1L

      case  <- as.logical(as.integer(tclvalue(case.var)))
      perl  <- as.logical(as.integer(tclvalue(perl.var)))
      fixed <- as.logical(as.integer(tclvalue(fixed.var)))

      matched.idxs <- suppressWarnings(grep(pattern, t(d[-1L, -1L]),
                                       fixed=fixed, perl=perl, ignore.case=case,
                                       useBytes=FALSE, invert=FALSE))

      if (length(matched.idxs) == 0L) {
        msg <- paste("Search string \'", pattern, "\' not found.", sep="")
        tkmessageBox(icon="info", message=msg, title="Find", type="ok",
                     parent=tt)
        return()
      }
      col.div <- matched.idxs / n
      i <- as.integer(ceiling(col.div))
      j <- as.integer(round(n * (col.div - trunc(col.div))))
      j[j == 0L] <- n
      matched.cells <<- cbind(i, j)
    }

    active.i <- as.integer(tcl(frame2.tbl, "tag", "row", "active"))
    active.j <- as.integer(tcl(frame2.tbl, "tag", "col", "active"))
    if (length(active.i) == 0L) {
      active.i <- 1L
      active.j <- 1L
    }

    if (direction == "next") {
      cell.below <- matched.cells[, 1] > active.i |
                   (matched.cells[, 1] == active.i &
                    matched.cells[, 2] > active.j)
      cell.above <- !cell.below
      if (any(cell.below)) {
        cell <- head(matched.cells[cell.below, , drop=FALSE], n=1)
      } else if (any(cell.above)) {
        cell <- head(matched.cells[cell.above, , drop=FALSE], n=1)
      } else {
        return()
      }
    } else {
      cell.above <- matched.cells[, 1] < active.i |
                   (matched.cells[, 1] == active.i &
                    matched.cells[, 2] < active.j)
      cell.below <- !cell.above
      if (any(cell.above)) {
        cell <- tail(matched.cells[cell.above, , drop=FALSE], n=1)
      } else if (any(cell.below)) {
        cell <- tail(matched.cells[cell.below, , drop=FALSE], n=1)
      } else {
        return()
      }
    }

    cell.str <- paste(cell[1, 1], cell[1, 2], sep=",")
    tkselection.clear(frame2.tbl, "all")

    tkactivate(frame2.tbl, cell.str)
    tkselection.set(frame2.tbl, cell.str)

    tkyview(frame2.tbl, cell[1, 1] - 1L)
    tkxview(frame2.tbl, cell[1, 2] - 1L)
  }

  # Goto data record
  GotoRecord <- function() {
    rec <- as.character(tclvalue(record.var))
    if (is.na(rec))
      return()
    idx <- which(row.names %in% rec)
    if (length(idx) > 0) {
      tkyview(frame2.tbl, idx[1] - 1L)
    } else {
      msg <- "Row name (or record number) not found."
      tkmessageBox(icon="info", message=msg, title="Goto", type="ok",
                   parent=tt)
    }
  }

  # Get single cell value for table
  GetCellValue <- function(r, c) {
    as.tclObj(d[as.integer(r) + 1L, as.integer(c) + 1L], drop=TRUE)
  }
  
  # Show help message
  ShowHelpMessage <- function() {
    if (is.editable) {
      msg <- paste("Navigation",
                   "    Button:",
                   "        Clicking Button-1 in a cell activates that cell.",
                   "        Clicking Button-1 in an already active cell moves the",
                   "            insertion cursor to the character nearest the mouse.",
                   "    Keyboard:",
                   "        Home moves the table to have the first cell in view.",
                   "        End moves the table to have the last cell in view.",
                   "        Control-Home moves the table to the first cell and",
                   "            activates that cell.",
                   "        Control-End moves the table to the last cell and activates",
                   "            that cell.",
                   "        The left, right, up and down arrows move the active cell.",
                   "        Control-leftarrow and Control-rightarrow move the",
                   "            insertion cursor within the cell.",
                   "        Control-a moves the insertion cursor to the beginning.",
                   "        Control-e moves the insertion cursor to the end.",
                   "",
                   "Selection",
                   "    Button:",
                   "        Moving the mouse while Button-1 is pressed will stroke",
                   "            out a selection area.",
                   "        Clicking Button-1 in a header cell selects all cells in that",
                   "            row or column.",
                   "    Keyboard:",
                   "        Shift-Control-Home extends the selection to the first cell.",
                   "        Shift-Control-End extends the selection to the last cell.",
                   "        Shift-<arrow> extends the selection in that direction.",
                   "        Control-slash selects all the cells.",
                   "        Control-backslash clears selection from all the cells.",
                   "",
                   "Editing",
                   "    Keyboard:",
                   "        Backspace deletes the character before the insertion cursor.",
                   "        Delete removes the character after the insertion cursor.",
                   "        Escape rereads the value from the data source, discarding",
                   "            any edits that have may been performed on the cell.",
                   "",
                   "Miscellaneous",
                   "    Keyboard:",
                   "        Control-c copies the selected cell.",
                   "        Control-v pastes to the selected cell.",
                   "        Control-Minus and Control-Equals decrease and increase",
                   "            the width of the column with the active cell.",
                   "        Moving the mouse while Button-1 is pressed while you are",
                   "            over a column border will cause interactive resizing of",
                   "            that column to occur.",
                   sep="\n")
    } else {
      msg <- paste("Navigation",
                   "    Button:",
                   "        Clicking Button-1 in a cell activates that cell.",
                   "    Keyboard:",
                   "        Home moves the table to have the first cell in view.",
                   "        End moves the table to have the last cell in view.",
                   "        Control-Home moves the table to the first cell and",
                   "            activates that cell.",
                   "        Control-End moves the table to the last cell and activates",
                   "            that cell.",
                   "        The left, right, up and down arrows move the active cell.",
                   "",
                   "Selection",
                   "    Button:",
                   "        Moving the mouse while Button-1 is pressed will stroke",
                   "            out a selection area.",
                   "        Clicking Button-1 in a header cell selects all cells in that",
                   "            row or column.",
                   "    Keyboard:",
                   "        Shift-Control-Home extends the selection to the first cell.",
                   "        Shift-Control-End extends the selection to the last cell.",
                   "        Shift-<arrow> extends the selection in that direction.",
                   "        Control-slash selects all the cells.",
                   "        Control-backslash clears selection from all the cells.",
                   "",
                   "Miscellaneous",
                   "    Keyboard:",
                   "        Control-c copies the selected cell.",
                   "        Control-v pastes to the selected cell.",
                   "        Control-Minus and Control-Equals decrease and increase",
                   "            the width of the column with the active cell.",
                   "        Moving the mouse while Button-1 is pressed while you are",
                   "            over a column border will cause interactive resizing of",
                   "            that column to occur.",
                   sep="\n")
    }
    tkmessageBox(icon="info", message=msg, title="Information", type="ok", 
                 parent=tt)
  }


  # Main program

  # Check if Tktable is loaded
  is.tktable <- !inherits(try(tcl("package", "present", "Tktable"),
                              silent=TRUE), "try-error")
  if (!is.tktable)
    return()
  
  # Set parameters based on whether the table is editable
  if (is.editable) {
    ent.state <- "normal"
    anchor.text <- "nw"
    col.formats <- NULL
  } else {
    ent.state <- "disabled"
    anchor.text <- "ne"
  }

  # Initialize search results
  matched.cells <- NULL

  # Table dimensions
  m <- nrow(d)
  n <- ncol(d)
  if (m == 0)
    return()

  # Number of rows and columns in the viewable table
  nrows <- if (m > 15) 15 else m
  ncols <- if (n >  6)  6 else n

  # Account for missing arguments
  if (is.null(col.names)) {
    col.names <- colnames(d)
    if (is.null(col.names) | length(col.names) != n) {
      col.names <- LETTERS[1:n]
      if (any(is.na(col.names))) {
        from <- seq(27, n, by=26)
        for (i in seq(along=from)) {
          to <- from[i] + 25
          if (to > n)
            to <- n
          l <- paste(LETTERS[i], LETTERS[1:(to - from[i] + 1L)], sep="")
          col.names[from[i]:to] <- l
        }
      }
    }
  } else {
    col.names <- col.names[1:n]
    col.names[is.na(col.names)] <- ""
    col.names <- gsub("(^ +)|( +$)", "", col.names)
  }
  if (is.null(col.formats)) {
    col.formats <- rep("", n)
  } else {
    col.formats <- as.character(col.formats[1:n])
    col.formats[is.na(col.formats)] <- ""
  }
  
  if (length(rownames(d)) == m)
    row.names <- rownames(d)
  else
    row.names <- 1:m
  row.names <- gsub("(^ +)|( +$)", "", as.character(row.names))

  # Determine width and height of column 0 and row 0, respectively
  col.0.width  <- max(nchar(row.names)) + 1L
  row.0.height <- max(vapply(strsplit(col.names, "\n"), length, 0L))

  # Format data table and determine column widths
  col.width <- NULL
  for (j in 1:n) {
    if (col.formats[j] == "") {
      d[, j] <- format(d[, j], digits=15, scientific=FALSE, drop0trailing=TRUE)
    } else if (inherits(d[, j], c("POSIXct", "POSIXlt"))) {
      d[, j] <- format(d[, j], format=col.formats[j])
    } else {
      d[, j] <- try(sprintf(col.formats[j], d[, j]), silent=TRUE)
      if (inherits(d[, j], "try-error"))
        d[, j] <- format(d[, j])
    }
    d[, j] <- gsub("(^ +)|( +$)", "", d[, j])
    if (col.names[j] == "")
      nchar.title <- 0
    else
      nchar.title <- max(vapply(strsplit(col.names[j], "\n"),
                                function(i) nchar(i), 0L))
    nchar.data <- max(nchar(d[,j]))
    len <- max(c(nchar.title, nchar.data)) + 1
    if (len < 5)
      len <- if (n == 1) 10 else 5
    col.width[j] <- len
  }

  # Add titles and row names to character data frame
  d <- rbind(c("", col.names), cbind(row.names, as.matrix(d)))

  # Assign variables linked to Tk widgets
  table.var   <- tclArray()
  record.var  <- tclVar()
  pattern.var <- tclVar()
  fixed.var   <- tclVar(1)
  perl.var    <- tclVar(0)
  case.var    <- tclVar(0)
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
  
  tktitle(tt) <- "View Data"
  
  # Create menus

  top.menu <- tkmenu(tt, tearoff=0)

  menu.edit <- tkmenu(tt, tearoff=0, relief="flat")
  tkadd(top.menu, "cascade", label="Edit", menu=menu.edit, underline=0)
  tkadd(menu.edit, "command", label="Copy", accelerator="Ctrl+C",
        command=CopyValues)
  tkadd(menu.edit, "command", label="Select all", command=SelectAll)
  
  help.edit <- tkmenu(tt, tearoff=0, relief="flat")
  tkadd(top.menu, "cascade", label="Help", menu=help.edit, underline=0)
  tkadd(help.edit, "command", label="Behavior", command=ShowHelpMessage)
  
  tkconfigure(tt, menu=top.menu)

  # Frame 0, ok button and size grip

  frame0 <- ttkframe(tt, relief="flat")

  frame0.but.2 <- ttkbutton(frame0, width=12, text="Close",
                            command=function() tclvalue(tt.done.var) <- 1)
  frame0.but.3 <- ttkbutton(frame0, width=12, text="Help",
                            command=function() {
                              print(help("ViewData", package="RSurvey"))
                            }) 
  frame0.grp.4 <- ttksizegrip(frame0)

  tkgrid("x", frame0.but.2, frame0.but.3, frame0.grp.4)

  tkgrid.columnconfigure(frame0, 0, weight=1)
  
  tkgrid.configure(frame0.but.2, frame0.but.3, pady=10)
  tkgrid.configure(frame0.but.3, columnspan=2, padx=c(4, 10))
  tkgrid.configure(frame0.grp.4, sticky="se")

  tkraise(frame0.but.3, frame0.grp.4)

  tkpack(frame0, fill="x", side="bottom", anchor="e")

  # Frame 1, line search

  frame1 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0, height=200)

  frame1.lab.1.1 <- ttklabel(frame1, text="Find")
  frame1.lab.2.1 <- ttklabel(frame1, text="Row")

  frame1.ent.1.2 <- ttkentry(frame1, width=15, textvariable=pattern.var)
  frame1.ent.2.2 <- ttkentry(frame1, width=15, textvariable=record.var)

  frame1.but.1.3 <- ttkbutton(frame1, width=2, image=GetBitmapImage("previous"),
                              command=function() Find("prev"))
  frame1.but.1.4 <- ttkbutton(frame1, width=2, image=GetBitmapImage("next"),
                              command=function() Find("next"))
  frame1.but.2.3 <- ttkbutton(frame1, width=4, text="Goto", command=GotoRecord)

  frame1.chk.1.5 <- ttkcheckbutton(frame1, variable=case.var,
                                   text="Ignore case",
                                   command=function() matched.cells <<- NULL)
  frame1.chk.1.6 <- ttkcheckbutton(frame1, variable=perl.var, text="Perl",
                                   command=function() matched.cells <<- NULL)
  frame1.chk.1.7 <- ttkcheckbutton(frame1, variable=fixed.var, text="Fixed",
                                   command=function() matched.cells <<- NULL)

  tkgrid(frame1.lab.1.1, frame1.ent.1.2, frame1.but.1.3, frame1.but.1.4,
         frame1.chk.1.5, frame1.chk.1.6, frame1.chk.1.7, pady=c(0, 4))
  tkgrid(frame1.lab.2.1, frame1.ent.2.2, frame1.but.2.3)

  tkgrid.configure(frame1.ent.1.2, frame1.ent.2.2, padx=c(0, 2))

  tkgrid.configure(frame1.lab.1.1, frame1.lab.2.1, padx=c(0, 2), sticky="w")
  tkgrid.configure(frame1.but.1.3, padx=c(0, 2))

  tkgrid.configure(frame1.but.2.3, columnspan=2, sticky="we")
  tkgrid.configure(frame1.chk.1.5, padx=c(12, 0))
  tkgrid.configure(frame1.chk.1.6, padx=c(4, 0))
  tkgrid.configure(frame1.chk.1.7, padx=c(4, 25))

  tkpack(frame1, side="bottom", anchor="nw", padx=c(10, 0))

  # Frame 2, the data table

  frame2 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0)

  .Tcl("option add *Table.font {CourierNew 9}")
  frame2.tbl <- tkwidget(frame2, "table", rows=m + 1, cols=n + 1,
                         colwidth=-2, rowheight=1, state=ent.state, 
                         height=nrows + 1, width=ncols + 1,
                         ipadx=1, ipady=1, wrap=1, justify="right",
                         highlightcolor="gray75", background="white",
                         foreground="black", titlerows=1, titlecols=1,
                         multiline=0, resizeborders="col", colorigin=0,
                         bordercursor="sb_h_double_arrow", cursor="plus",
                         colstretchmode="none", rowstretchmode="none",
                         drawmode="single", flashmode=1, rowseparator="\n",
                         colseparator="\t", selectmode="extended", 
                         selecttitle=1, insertofftime=0, anchor=anchor.text, 
                         highlightthickness=0, cache=1, 
                         command=function(r, c) GetCellValue(r, c),
                         xscrollcommand=function(...) tkset(frame2.xsc,...),
                         yscrollcommand=function(...) tkset(frame2.ysc,...))

  frame2.xsc <- tkscrollbar(frame2, orient="horizontal",
                            command=function(...) tkxview(frame2.tbl,...))
  frame2.ysc <- tkscrollbar(frame2, orient="vertical",
                            command=function(...) tkyview(frame2.tbl,...))

  tcl(frame2.tbl,  "width", 0, col.0.width)
  tcl(frame2.tbl, "height", 0, row.0.height)
  for (j in 1:n)
    tcl(frame2.tbl, "width", j, col.width[j])

  tkgrid(frame2.tbl, frame2.ysc)
  tkgrid(frame2.xsc, "x")

  tkgrid.configure(frame2.tbl, sticky="news", padx=c(10, 0), pady=c(10, 0))
  tkgrid.configure(frame2.ysc, sticky="ns", padx=c(0, 10), pady=c(10, 0))
  tkgrid.configure(frame2.xsc, sticky="we", padx=c(10, 0), pady=c(0, 5))

  tktag.configure(frame2.tbl, "active", background="#EAEEFE", relief="")
  tktag.configure(frame2.tbl, "sel",    background="#EAEEFE", foreground="#000000")
  tktag.configure(frame2.tbl, "title",  background="#D9D9D9", foreground="#000000")
  tktag.configure(frame2.tbl, "flash",  background="#FFFFFF", foreground="#FF0033")

  tcl(frame2.tbl, "tag", "row", "coltitles", 0)
  tktag.configure(frame2.tbl, "coltitles", anchor="center", justify="center")
  tcl(frame2.tbl, "tag", "col", "rowtitles", 0)

  tkgrid.columnconfigure(frame2, 0, weight=1)
  tkgrid.rowconfigure(frame2, 0, weight=1)

  tkpack(frame2, fill="both", expand=TRUE)

  tkselection.set(frame2.tbl, "origin")

  # Bind events

  tclServiceMode(TRUE)
  
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)
  tkbind(tt, "<Control-c>", CopyValues)
  
  tkbind(frame1.ent.1.2, "<KeyRelease>",
         function() {
           matched.cells <<- NULL
         })
  tkbind(frame1.ent.1.2, "<Return>", function() Find("next"))
  tkbind(frame1.ent.1.2, "<Up>", function() Find("prev"))
  tkbind(frame1.ent.1.2, "<Down>", function() Find("next"))
  tkbind(frame1.ent.2.2, "<Return>", function() GotoRecord())

  # GUI control

  tkgrab(tt)
  tkfocus(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  invisible()
}
