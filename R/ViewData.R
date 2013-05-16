# A GUI for viewing and editing table formatted data.

ViewData <- function(d, col.names=NULL, col.formats=NULL, read.only=FALSE,
                     changelog=NULL, win.title="View Data", parent=NULL) {

  ## Additional functions (subroutines)

  # Save table and close
  SaveTable <- function() {
    s <- GetEdits()
    tclServiceMode(FALSE)

    changelog <- s
    if (!is.null(s)) {
      s <- s[, c("Class", "New", "Row", "Col"), drop=FALSE]
      for (i in unique(s$Class)) {
        ss <- s[s$Class == i, ]
        if (i %in% c("POSIXct", "POSIXlt")) {
          new <- suppressWarnings(strptime(ss$New, "%Y-%m-%d %H:%M:%S"))
        } else if (i == "integer") {
          new <- suppressWarnings(as.integer(ss$New))
        } else if (i == "numeric") {
          new <- suppressWarnings(as.numeric(ss$New))
        } else if (i == "logical") {
          new <- suppressWarnings(as.logical(ss$New))
        } else {
          new <- ss$New
        }
        d[cbind(ss$Row, ss$Col)] <<- new
      }
    }
    rtn <<- list(d=d, changelog=changelog)

    tclServiceMode(TRUE)
    tclvalue(tt.done.var) <- 1
  }

  # Select all cells
  SelectAll <- function() {
    tkselection.set(frame2.tbl, "0,0", "end")
  }

  # Find value in table

  Find <- function(direction="next") {
    pattern <- as.character(tclvalue(pattern.var))
    if (pattern == "")
      return()

    if (is.null(matched.cells)) {
      n <- ncol(dd) - 1L

      matched.idxs <- suppressWarnings(grep(pattern, t(dd[-1L, -1L]),
                                       fixed=fixed, perl=perl,
                                       ignore.case=!match.case,
                                       useBytes=FALSE, invert=FALSE))

      if (length(matched.idxs) == 0L) {
        msg <- paste0("Search string \'", pattern, "\' not found.")
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
      first.cell.str <- paste0(idx, ",0")
      last.cell.str <- paste(idx, n, sep=",")
      tkselection.clear(frame2.tbl, "all")
      tkselection.set(frame2.tbl, first.cell.str, last.cell.str)
    } else {
      msg <- "Row name (or record number) not found."
      tkmessageBox(icon="info", message=msg, title="Goto", type="ok",
                   parent=tt)
    }
  }

  # Get single cell value for table
  GetCellValue <- function(r, c) {
    as.tclObj(dd[as.integer(r) + 1L, as.integer(c) + 1L], drop=TRUE)
  }

  # Tag column
  TagColumn <- function(...) {
    if (as.integer(...) %in% read.only)
      return(as.tclObj("disabledcol"))
  }

  # Validate cell value

  ValidateCellValue <- function(s, S) {
    tclServiceMode(FALSE)

    e <- data.frame(time=Sys.time(),
                    cell=as.character(tkindex(frame2.tbl, "active")),
                    old=as.character(s), new=as.character(S),
                    stringsAsFactors=FALSE)
    cell <- as.integer(strsplit(e$cell, ",")[[1]])
    obj <- d[cell[1], cell[2]]
    if (inherits(obj, c("numeric", "integer", "logical")) &&
        !identical(e$new, CheckEntry(class(obj)[1], e$new))) {
      is.valid <- FALSE
    } else {
      undo.stack <<- rbind(undo.stack, e)
      redo.stack <<- NULL
      is.valid <- TRUE
    }

    tclServiceMode(TRUE)
    return(as.tclObj(is.valid))
  }

  # Undo edit

  UndoEdit <- function() {
    if (is.null(undo.stack) || nrow(undo.stack) == 0)
      return()
    tclServiceMode(FALSE)

    m <- nrow(undo.stack)
    e <- undo.stack[m, , drop=FALSE]
    cell <- as.integer(strsplit(e$cell, ",")[[1]])
    dd[cell[1] + 1L, cell[2] + 1L] <<- e$old

    undo.stack <<- undo.stack[-m, , drop=FALSE]
    redo.stack <<- rbind(redo.stack, e)

    tkactivate(frame2.tbl, "0,0")
    tkselection.clear(frame2.tbl, "all")
    tksee(frame2.tbl, e$cell)

    tclServiceMode(TRUE)
    tcl(frame2.tbl, "clear", "cache", e$cell)
    tkactivate(frame2.tbl, e$cell)
  }

  # Redo edit
  RedoEdit <- function() {
    if (is.null(redo.stack) || nrow(redo.stack) == 0)
      return()
    tclServiceMode(FALSE)

    m <- nrow(redo.stack)
    e <- redo.stack[m, , drop=FALSE]
    cell <- as.integer(strsplit(e$cell, ",")[[1]])
    dd[cell[1] + 1L, cell[2] + 1L] <<- e$new

    redo.stack <<- redo.stack[-m, , drop=FALSE]
    undo.stack <<- rbind(undo.stack, e)

    tkactivate(frame2.tbl, "0,0")
    tkselection.clear(frame2.tbl, "all")
    tksee(frame2.tbl, e$cell)

    tclServiceMode(TRUE)
    tcl(frame2.tbl, "clear", "cache", e$cell)
    tkactivate(frame2.tbl, e$cell)
  }

  # View changelog
  ViewChangeLog <- function() {
    s <- GetEdits()
    if (is.null(s)) {
      txt <- ""
    } else {
      s <- s[order(s$Record, s$Variable),
             c("Record", "Variable", "Old", "New", "Class", "Time"), drop=FALSE]
      header <- names(s)
      breaks <- vapply(header, function(i) paste(rep("-", nchar(i)),
                                                 collapse=""), "")
      s <- rbind(header, breaks, s)
      width <- apply(s, 2, function(i) max(nchar(i)) + 1L)
      justify <- c("right", "left", "right", "right", "left", "left")
      for (j in 1:ncol(s)) {
        s[, j] <- format(s[, j], width=width[j], justify=justify[j])
      }
      txt <- apply(s, 1, function(i) paste(i, collapse=" "))
    }
    ViewText(txt, read.only=TRUE, win.title="Changelog", parent=tt)
    tkfocus(frame2.tbl)
  }

  # Get edits

  GetEdits <- function() {
    s <- NULL
    if (is.null(changelog) & (is.null(undo.stack) || nrow(undo.stack) == 0))
      return(s)

    if (!is.null(changelog)) {
      changelog$Cell <- paste(changelog[, "Row"], changelog[, "Col"], sep=",")
      changelog <- changelog[, c("Time", "Cell", "Old", "New")]
      names(changelog) <- c("time", "cell", "old", "new")
      undo.stack <- rbind(undo.stack, changelog)
    }

    for (i in unique(undo.stack$cell)) {
      undo.stack.cell <- undo.stack[undo.stack$cell == i, , drop=FALSE]
      undo.stack.cell <- undo.stack.cell[order(undo.stack.cell$time), ,
                                         drop=FALSE]
      m <- nrow(undo.stack.cell)
      cell <- as.integer(strsplit(undo.stack.cell$cell, ",")[[1]])
      obj <- d[cell[1], cell[2]]
      old <- undo.stack.cell$old[1]
      new <- undo.stack.cell$new[m]
      if (inherits(obj, "POSIXt") &&
          is.na(strptime(new, "%Y-%m-%d %H:%M:%S"))) {
        new <- "NA"
      } else if (inherits(obj, c("numeric", "integer")) &&
                 is.na(suppressWarnings(as.numeric(new)))) {
        new <- "NA"
      } else if (inherits(obj, "logical") && is.na(as.logical(new))) {
        new <- "NA"
      }

      if (identical(old, new))
        next
      e <- data.frame(Record=as.integer(dd[cell[1] + 1L, 1]),
                      Variable=dd[1, cell[2] + 1L],
                      Old=old, New=new,
                      Time=format(undo.stack.cell$time[m]),
                      Row=cell[1], Col=cell[2], Class=class(obj)[1],
                      stringsAsFactors=FALSE)
      s <- rbind(s, e)
    }

    return(s)
  }

  ## Main program

  # Check if Tktable is loaded
  is.tktable <- !inherits(try(tcl("package", "present", "Tktable"),
                              silent=TRUE), "try-error")
  if (!is.tktable)
    return()

  # Table dimensions
  m <- nrow(d)
  n <- ncol(d)
  if (m == 0)
    return()

  # Check validity of changelog
  if (is.null(changelog) || !is.data.frame(changelog))
    changelog <- NULL

  # Set parameters based on whether the table is editable

  if (inherits(read.only, "logical")) {
    if (read.only)
      read.only <- 1:n
    else
      read.only <- NULL
  }
  if (!inherits(read.only, c("NULL", "integer")))
    stop("problem with read.only argument")
  is.editable <- is.null(read.only) || read.only != 1:n

  # Initialize search results
  matched.cells <- NULL

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
          l <- paste0(LETTERS[i], LETTERS[1:(to - from[i] + 1L)])
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
    col.formats[!1:n %in% read.only] <- ""
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
  dd <- d
  col.width <- NULL
  for (j in 1:n) {
    if (col.formats[j] == "") {
      dd[, j] <- format(dd[, j], digits=15, scientific=FALSE,
                        drop0trailing=TRUE)
    } else if (inherits(dd[, j], c("POSIXct", "POSIXlt"))) {
      dd[, j] <- format(dd[, j], format=col.formats[j])
    } else {
      dd[, j] <- try(sprintf(col.formats[j], dd[, j]), silent=TRUE)
      if (inherits(dd[, j], "try-error"))
        dd[, j] <- format(dd[, j])
    }
    dd[, j] <- gsub("(^ +)|( +$)", "", dd[, j])
    if (col.names[j] == "")
      nchar.title <- 0
    else
      nchar.title <- max(vapply(strsplit(col.names[j], "\n"),
                                function(i) nchar(i), 0L))
    nchar.data <- max(nchar(dd[,j]))
    len <- max(c(nchar.title, nchar.data)) + 1
    if (len < 5)
      len <- if (n == 1) 10 else 5
    col.width[j] <- len
  }

  # Add titles and row names to character data frame
  dd <- rbind(c("", col.names), cbind(row.names, as.matrix(dd)))

  # Assigin global variables
  match.case <- TRUE
  perl <- FALSE
  fixed <- TRUE
  undo.stack <- NULL
  redo.stack <- NULL
  rtn <- NULL

  # Assign variables linked to Tk widgets
  table.var   <- tclArray()
  record.var  <- tclVar()
  pattern.var <- tclVar()
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

  tktitle(tt) <- win.title

  # Start top menu
  top.menu <- tkmenu(tt, tearoff=0)

  # File menu
  if (is.editable) {
    menu.file <- tkmenu(tt, tearoff=0, relief="flat")
    tkadd(top.menu, "cascade", label="File", menu=menu.file, underline=0)
    tkadd(menu.file, "command", label="View changelog",
          command=ViewChangeLog)
  }

  # Edit menu
  menu.edit <- tkmenu(tt, tearoff=0, relief="flat")
  tkadd(top.menu, "cascade", label="Edit", menu=menu.edit, underline=0)
  if (is.editable) {
    tkadd(menu.edit, "command", label="Undo", accelerator="Ctrl+z",
          command=UndoEdit)
    tkadd(menu.edit, "command", label="Redo", accelerator="Ctrl+y",
          command=RedoEdit)
    tkadd(menu.edit, "separator")
  }
  tkadd(menu.edit, "command", label="Copy", accelerator="Ctrl+c",
        command=function() tcl("tk_tableCopy", frame2.tbl))
  if (is.editable) {
    tkadd(menu.edit, "command", label="Cut", accelerator="Ctrl+x",
          command=function() tcl("tk_tableCut", frame2.tbl))
    tkadd(menu.edit, "command", label="Paste", accelerator="Ctrl+v",
          command=function() tcl("tk_tablePaste", frame2.tbl))
    tkadd(menu.edit, "separator")
    menu.edit.del <- tkmenu(tt, tearoff=0)
    tkadd(menu.edit.del, "command", label="Character after cursor",
          accelerator="Delete",
          command=function() tkevent.generate(frame2.tbl, "<Delete>"))
    tkadd(menu.edit.del, "command", label="Character before cursor",
          accelerator="Backspace",
          command=function() tkevent.generate(frame2.tbl, "<BackSpace>"))
    tkadd(menu.edit.del, "command", label="All characters after cursor",
          accelerator="Ctrl+k",
          command=function() tkevent.generate(frame2.tbl, "<Control-k>"))
    tkadd(menu.edit, "cascade", label="Inside cell delete", menu=menu.edit.del)
  }
  tkadd(menu.edit, "separator")
  menu.edit.width <- tkmenu(tt, tearoff=0)
  tkadd(menu.edit.width, "command", label="Increase", accelerator="Ctrl+\u003d",
        command=function() tkevent.generate(frame2.tbl, "<Control-equal>"))
  tkadd(menu.edit.width, "command", label="Decrease", accelerator="Ctrl+\u2212",
        command=function() tkevent.generate(frame2.tbl, "<Control-minus>"))
  tkadd(menu.edit, "cascade", label="Column width", menu=menu.edit.width)

  # Selection menu
  menu.sel <- tkmenu(tt, tearoff=0, relief="flat")
  tkadd(top.menu, "cascade", label="Select", menu=menu.sel, underline=0)
  tkadd(menu.sel, "command", label="Select all cells",
        accelerator="Ctrl+\u2044",
        command=function() tkevent.generate(frame2.tbl, "<Control-slash>"))
  tkadd(menu.sel, "separator")
  menu.sel.extend <- tkmenu(tt, tearoff=0)
  tkadd(menu.sel.extend, "command", label="First cell",
        accelerator="Shift+Ctrl+Home",
        command=function() tkevent.generate(frame2.tbl, "<Shift-Control-Home>"))
  tkadd(menu.sel.extend, "command", label="Last cell",
        accelerator="Shift+Ctrl+End",
        command=function() tkevent.generate(frame2.tbl, "<Shift-Control-End>"))
  tkadd(menu.sel.extend, "separator")
  tkadd(menu.sel.extend, "command", label="Row above",
        accelerator="Shift+\u2191",
        command=function() tkevent.generate(frame2.tbl, "<Shift-Up>"))
  tkadd(menu.sel.extend, "command", label="Row below",
        accelerator="Shift+\u2193",
        command=function() tkevent.generate(frame2.tbl, "<Shift-Down>"))
  tkadd(menu.sel.extend, "command", label="Column left",
        accelerator="Shift+\u2190",
        command=function() tkevent.generate(frame2.tbl, "<Shift-Left>"))
  tkadd(menu.sel.extend, "command", label="Column right",
        accelerator="Shift+\u2192",
        command=function() tkevent.generate(frame2.tbl, "<Shift-Right>"))
  tkadd(menu.sel, "cascade", label="Extend selection to", menu=menu.sel.extend)

  # Navigation menu
  menu.nav <- tkmenu(tt, tearoff=0, relief="flat")
  tkadd(top.menu, "cascade", label="Navigate", menu=menu.nav, underline=0)
  tkadd(menu.nav, "command", label="Move up", accelerator="\u2191",
        command=function() tkevent.generate(frame2.tbl, "<Up>"))
  tkadd(menu.nav, "command", label="Move down", accelerator="\u2193",
        command=function() tkevent.generate(frame2.tbl, "<Down>"))
  tkadd(menu.nav, "command", label="Move left", accelerator="\u2190",
        command=function() tkevent.generate(frame2.tbl, "<Left>"))
  tkadd(menu.nav, "command", label="Move right", accelerator="\u2192",
        command=function() tkevent.generate(frame2.tbl, "<Right>"))
  tkadd(menu.nav, "separator")
  if (is.editable) {
    menu.nav.view <- tkmenu(tt, tearoff=0)
    tkadd(menu.nav.view, "command", label="First cell in view",
          accelerator="Home",
          command=function() tkevent.generate(frame2.tbl, "<Home>"))
    tkadd(menu.nav.view, "command", label="Last cell in view",
          accelerator="End",
          command=function() tkevent.generate(frame2.tbl, "<End>"))
    tkadd(menu.nav.view, "separator")
    tkadd(menu.nav.view, "command", label="Prior page in view",
          accelerator="Ctrl+PageUp",
          command=function() tkevent.generate(frame2.tbl, "<Control-Prior>"))
    tkadd(menu.nav.view, "command", label="Next page in view",
          accelerator="Ctrl+PageDown",
          command=function() tkevent.generate(frame2.tbl, "<Control-Next>"))
    tkadd(menu.nav, "cascade", label="Move table to have", menu=menu.nav.view)
    tkadd(menu.nav, "separator")
    menu.nav.active <- tkmenu(tt, tearoff=0)
    tkadd(menu.nav.active, "command", label="First cell",
          accelerator="Ctrl+Home",
          command=function() tkevent.generate(frame2.tbl, "<Control-Home>"))
    tkadd(menu.nav.active, "command", label="Last cell", accelerator="Ctrl+End",
          command=function() tkevent.generate(frame2.tbl, "<Control-End>"))
    tkadd(menu.nav.active, "separator")
    tkadd(menu.nav.active, "command", label="Prior page", accelerator="PageUp",
          command=function() tkevent.generate(frame2.tbl, "<Prior>"))
    tkadd(menu.nav.active, "command", label="Next page", accelerator="PageDown",
          command=function() tkevent.generate(frame2.tbl, "<Next>"))
    tkadd(menu.nav, "cascade", label="Move activate cell to",
          menu=menu.nav.active)
    tkadd(menu.nav, "separator")
    menu.nav.in <- tkmenu(tt, tearoff=0)
    tkadd(menu.nav.in, "command", label="Left", accelerator="Ctrl+\u2190",
          command=function() tkevent.generate(frame2.tbl, "<Control-Left>"))
    tkadd(menu.nav.in, "command", label="Right", accelerator="Ctrl+\u2192",
          command=function() tkevent.generate(frame2.tbl, "<Control-Right>"))
    tkadd(menu.nav.in, "separator")
    tkadd(menu.nav.in, "command", label="Beggining", accelerator="Ctrl+a",
          command=function() tkevent.generate(frame2.tbl, "<Control-a>"))
    tkadd(menu.nav.in, "command", label="End", accelerator="Ctrl+e",
          command=function() tkevent.generate(frame2.tbl, "<Control-e>"))
    tkadd(menu.nav, "cascade", label="Move inside cell to the",
          menu=menu.nav.in)
  } else {
    menu.nav.view <- tkmenu(tt, tearoff=0)
    tkadd(menu.nav.view, "command", label="Prior page in view",
          accelerator="PageUp",
          command=function() tkevent.generate(frame2.tbl, "<Prior>"))
    tkadd(menu.nav.view, "command", label="Next page in view",
          accelerator="PageDown",
          command=function() tkevent.generate(frame2.tbl, "<Next>"))
    tkadd(menu.nav, "cascade", label="Move table to have", menu=menu.nav.view)
    tkadd(menu.nav, "separator")
    menu.nav.active <- tkmenu(tt, tearoff=0)
    tkadd(menu.nav.active, "command", label="First cell",
          accelerator="Ctrl+Home",
          command=function() tkevent.generate(frame2.tbl, "<Control-Home>"))
    tkadd(menu.nav.active, "command", label="Last cell", accelerator="Ctrl+End",
          command=function() tkevent.generate(frame2.tbl, "<Control-End>"))
    tkadd(menu.nav, "cascade", label="Move activate cell to",
          menu=menu.nav.active)
  }

  # Finish top menu
  tkconfigure(tt, menu=top.menu)

  # Frame 0, ok button and size grip

  frame0 <- ttkframe(tt, relief="flat")

  if (is.editable) {
    frame0.but.1.2 <- ttkbutton(frame0, width=12, text="Save",
                                command=SaveTable)
    frame0.but.1.3 <- ttkbutton(frame0, width=12, text="Cancel",
                                command=function() tclvalue(tt.done.var) <- 1)
  } else {
    frame0.but.1.2 <- "x"
    frame0.but.1.3 <- ttkbutton(frame0, width=12, text="Close",
                                command=function() tclvalue(tt.done.var) <- 1)
  }
  frame0.but.1.4 <- ttkbutton(frame0, width=12, text="Help",
                            command=function() {
                              print(help("ViewData", package="RSurvey"))
                            })
  frame0.grp.1.5 <- ttksizegrip(frame0)

  tkgrid("x", frame0.but.1.2, frame0.but.1.3, frame0.but.1.4, frame0.grp.1.5)

  tkgrid.columnconfigure(frame0, 0, weight=1)

  tkgrid.configure(frame0.but.1.3, padx=c(4, 0))
  tkgrid.configure(frame0.but.1.4, pady=10, padx=c(4, 10), columnspan=2)
  tkgrid.configure(frame0.grp.1.5, sticky="se")

  tkraise(frame0.but.1.4, frame0.grp.1.5)

  tkpack(frame0, fill="x", side="bottom", anchor="e")

  # Frame 1, line search

  frame1 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0, height=200)

  frame1.lab.1.1 <- ttklabel(frame1, text="Find")
  frame1.lab.2.1 <- ttklabel(frame1, text="Record")

  frame1.ent.1.2 <- ttkentry(frame1, width=15, textvariable=pattern.var)
  frame1.ent.2.2 <- ttkentry(frame1, width=15, textvariable=record.var)

  frame1.but.1.3 <- ttkbutton(frame1, width=2, image=GetBitmapImage("previous"),
                              command=function() Find("prev"))
  frame1.but.1.4 <- ttkbutton(frame1, width=2, image=GetBitmapImage("next"),
                              command=function() Find("next"))
  frame1.but.2.3 <- ttkbutton(frame1, width=4, text="Goto", command=GotoRecord)

  tkgrid(frame1.lab.1.1, frame1.ent.1.2, frame1.but.1.3, frame1.but.1.4,
         pady=c(0, 4))
  tkgrid(frame1.lab.2.1, frame1.ent.2.2, frame1.but.2.3)

  tkgrid.configure(frame1.ent.1.2, frame1.ent.2.2, padx=c(0, 2))
  tkgrid.configure(frame1.lab.1.1, frame1.lab.2.1, padx=c(0, 2), sticky="w")
  tkgrid.configure(frame1.but.1.4, padx=c(2, 10))
  tkgrid.configure(frame1.but.2.3, columnspan=2, padx=c(0, 10), sticky="we")

  tkpack(frame1, side="bottom", anchor="nw", padx=c(10, 0))

  # Frame 2, the data table

  frame2 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0)

  .Tcl("option add *Table.font {CourierNew 9}")
  frame2.tbl <- tkwidget(frame2, "table", rows=m + 1, cols=n + 1,
                         colwidth=-2, rowheight=1, state="normal",
                         height=nrows + 1, width=ncols + 1,
                         ipadx=1, ipady=1, wrap=1, justify="right",
                         highlightcolor="gray75", background="white",
                         foreground="black", titlerows=1, titlecols=1,
                         multiline=0, resizeborders="col", colorigin=0,
                         bordercursor="sb_h_double_arrow", cursor="plus",
                         colstretchmode="none", rowstretchmode="none",
                         drawmode="single", flashmode=1, rowseparator="\n",
                         colseparator="\t", selectmode="extended",
                         selecttitle=1, insertofftime=0, anchor="nw",
                         highlightthickness=0, cache=1, validate=1,
                         validatecommand=function(s, S) ValidateCellValue(s, S),
                         command=function(r, c) GetCellValue(r, c),
                         coltagcommand=function(...) TagColumn(...),
                         xscrollcommand=function(...) tkset(frame2.xsc, ...),
                         yscrollcommand=function(...) tkset(frame2.ysc, ...))

  frame2.xsc <- tkscrollbar(frame2, orient="horizontal",
                            command=function(...) tkxview(frame2.tbl, ...))
  frame2.ysc <- tkscrollbar(frame2, orient="vertical",
                            command=function(...) tkyview(frame2.tbl, ...))

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
  tktag.configure(frame2.tbl, "sel",    background="#EAEEFE",
                  foreground="#000000")
  tktag.configure(frame2.tbl, "title",  background="#D9D9D9",
                  foreground="#000000")
  tktag.configure(frame2.tbl, "flash",  background="#FFFFFF",
                  foreground="#FF0033")

  tcl(frame2.tbl, "tag", "row", "coltitles", 0)
  tcl(frame2.tbl, "tag", "col", "rowtitles", 0)

  tktag.configure(frame2.tbl, "coltitles", justify="center", anchor="n")
  tktag.configure(frame2.tbl, "rowtitles", justify="right", anchor="ne")
  tktag.configure(frame2.tbl, "disabledcol", state="disabled", anchor="ne")

  tkgrid.columnconfigure(frame2, 0, weight=1)
  tkgrid.rowconfigure(frame2, 0, weight=1)

  tkpack(frame2, fill="both", expand=TRUE)

  # Bind events

  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)
  tkbind(frame2.tbl, "<Return>", "break")

  tkbind(frame2.tbl, "<Control-z>", UndoEdit)
  tkbind(frame2.tbl, "<Control-y>", RedoEdit)

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
  tkfocus(frame2.tbl)
  tkactivate(frame2.tbl, "origin")
  tkselection.set(frame2.tbl, "active")
  tksee(frame2.tbl, "active")
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  invisible(rtn)
}
