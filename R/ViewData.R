ViewData <- function(d, col.names=NULL, col.units=NULL, col.digs=NULL,
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

      matched.idxs <- grep(pattern, t(d[-1L, -1L]), fixed=TRUE)

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

  # Goto line number

  GotoLine <- function() {
    line.no <- as.integer(tclvalue(line.no.var))
    if (is.na(line.no))
      return()
    idx <- which(rows %in% line.no)
    if (length(idx) == 0) {
      if (line.no > rows[m]) {
        idx <- m
        tclvalue(line.no.var) <- rows[m]
      } else if (line.no < rows[1]) {
        idx <- 1
        tclvalue(line.no.var) <- rows[1]
      } else {
        return()
      }
    } else {
      idx <- idx[1]
    }
    tkyview(frame2.tbl, idx - 1L)
  }

  # Get single cell value for table, a simplified version of
  # as.tclObj(as.character(x), drop=TRUE) is used.

  GetCellValue <- function(r, c) {
    val <- .External("RTcl_ObjFromCharVector",
                     d[as.integer(r) + 1L, as.integer(c) + 1L],
                     drop=TRUE, PACKAGE="tcltk")
    class(val) <- "tclObj"
    val
  }


  # Main program

  # Check if Tktable is loaded

  is.tktable <- !inherits(try(tcl("package", "present", "Tktable"),
                              silent=TRUE), "try-error")
  if (!is.tktable)
    return()

  # Initialize search results

  matched.cells <- NULL

  # Number of rows and columns

  m <- nrow(d)
  n <- ncol(d)
  if (m == 0)
    return()

  # Row titles

  rows <- row.names(d)

  # Height and width of viewable table

  height <- if (m > 15) 15 else m
  width <- if (n > 6) 6 else n

  # Table titles

  col.width.title <- nchar(max(as.integer(row.names(d)))) + 1

  if (is.null(col.names)) {
    cols <- rep("", n)
  } else {
    cols <- col.names[1:n]
    cols[is.na(cols)] <- ""
  }

  if (is.null(col.units))
    col.units <- rep(NA, n)
  else
    col.units <- as.character(col.units[1:n])
  not.na <- !is.na(col.units)
  cols[not.na] <- paste(cols[not.na], col.units[not.na], sep="\n")

  col.digs <- if (is.null(col.units)) rep(NA, n) else as.integer(col.digs[1:n])

  col.height.title <- max(sapply(strsplit(cols, "\n"), length))

  # Identify column classses

  col.class <- sapply(d, function(i) class(i)[1])
  dt.cols <- which(col.class %in% c("POSIXt", "POSIXct", "POSIXlt"))
  no.cols <- which(col.class %in% c("numeric", "integer"))

  # Format data table and determine column widths

  col.width <- NULL
  for (j in 1:n) {
    if (j %in% dt.cols && !is.na(col.units[j])) {
      d[, j] <- format(d[, j], format=col.units[j])
    } else if (j %in% no.cols && !is.na(col.digs[j])) {
      d[, j] <- format(round(d[, j], col.digs[j]), nsmall=col.digs[j], trim=TRUE)
    } else {
      d[, j] <- format(d[, j])
    }

    len <- if (cols[j] == "") 0 else nchar(strsplit(cols[j], "\n")[[1]])
    len <- max(c(nchar(sample(d[,j], height)), len)) + 3
    if (len < 10)
      len <- if (n == 1) 20 else 10
    col.width[j] <- len
  }

  # Construct character matrix from data frame

  d <- rbind(c("", cols), cbind(row.names(d), as.matrix(d)))

  # Assign variables linked to Tk widgets

  table.var <- tclArray()
  line.no.var <- tclVar()
  pattern.var <- tclVar()
  tt.done.var <- tclVar(0)

  # Open GUI

  tclServiceMode(FALSE)

  tt <- tktoplevel(padx=0, pady=0)

  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(geo[2]) + 25,
                            "+", as.integer(geo[3]) + 25, sep=""))
  }

  tktitle(tt) <- "View Data"

  # Frame 0, ok button and size grip

  frame0 <- ttkframe(tt, relief="flat")

  frame0.but.1 <- ttkbutton(frame0, width=12, text="Copy",
                            command=CopyValues)
  frame0.but.2 <- ttkbutton(frame0, width=12, text="Close",
                            command=function() tclvalue(tt.done.var) <- 1)
  frame0.grp.3 <- ttksizegrip(frame0)

  tkgrid(frame0.but.1, frame0.but.2, frame0.grp.3)

  tkgrid.configure(frame0.but.1, sticky="e", padx=2, pady=c(5, 10))
  tkgrid.configure(frame0.but.2, sticky="w", padx=2, pady=c(5, 10), rowspan=2)
  tkgrid.configure(frame0.grp.3, sticky="se")

  tkpack(frame0, side="bottom", anchor="e")

  # Frame 1, line search

  frame1 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0, height=200)

  frame1.lab.1.1 <- ttklabel(frame1, text="Find")
  frame1.lab.2.1 <- ttklabel(frame1, text="Record")

  frame1.ent.1.2 <- ttkentry(frame1, width=15, textvariable=pattern.var)
  frame1.ent.2.2 <- ttkentry(frame1, width=15, textvariable=line.no.var)

  tkbind(frame1.ent.1.2, "<KeyRelease>",
         function() {
           matched.cells <<- NULL
         })
  tkbind(frame1.ent.2.2, "<KeyRelease>",
         function() {
           tclvalue(line.no.var) <- CheckEntry("integer", tclvalue(line.no.var))
         })

  frame1.but.1.3 <- ttkbutton(frame1, width=8, text="Previous",
                              command=function() Find("prev"))
  frame1.but.1.4 <- ttkbutton(frame1, width=8, text="Next",
                              command=function() Find("next"))
  frame1.but.2.3 <- ttkbutton(frame1, width=8, text="Goto",
                              command=GotoLine)

  tkgrid(frame1.lab.1.1, frame1.ent.1.2, frame1.but.1.3, frame1.but.1.4)
  tkgrid(frame1.lab.2.1, frame1.ent.2.2, frame1.but.2.3, pady=c(1, 0))

  tkgrid.configure(frame1.lab.1.1, frame1.lab.2.1, sticky="e")
  tkgrid.configure(frame1.but.1.3, frame1.but.2.3, padx=2)
  tkgrid.configure(frame1.but.1.4, padx=c(0, 10))

  tkpack(frame1, side="bottom", anchor="nw", padx=c(10, 0))

  # Frame 2, the data table

  frame2 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0)

  .Tcl("option add *Table.font {CourierNew 9}")
  frame2.tbl <- tkwidget(frame2, "table", rows=m + 1, cols=n + 1,
                         colwidth=13, rowheight=1, state="disabled",
                         height=height + 1, width=width + 1,
                         ipadx=5, ipady=1, wrap=0,
                         highlightcolor="gray75", background="white",
                         foreground="black", titlerows=1, titlecols=1,
                         multiline=0, resizeborders="col",
                         colstretchmode="all", rowstretchmode="none",
                         drawmode="single", rowseparator="\n",
                         colseparator="\t", selectmode="extended",
                         insertofftime=0, anchor="nw", justify="left",
                         borderwidth=0, highlightthickness=0, cache=1,
                         command=function(r, c) GetCellValue(r, c),
                         xscrollcommand=function(...) tkset(frame2.xsc,...),
                         yscrollcommand=function(...) tkset(frame2.ysc,...))

  frame2.xsc <- tkscrollbar(frame2, orient="horizontal",
                            command=function(...) tkxview(frame2.tbl,...))
  frame2.ysc <- tkscrollbar(frame2, orient="vertical",
                            command=function(...) tkyview(frame2.tbl,...))

  tcl(frame2.tbl, "width", 0, col.width.title)
  tcl(frame2.tbl, "height", 0, col.height.title)
  for (j in 1:n)
    tcl(frame2.tbl, "width", j, col.width[j])

  tkgrid(frame2.tbl, frame2.ysc)
  tkgrid(frame2.xsc, "x")

  tkgrid.configure(frame2.tbl, sticky="news", padx=c(10, 0), pady=c(10, 0))
  tkgrid.configure(frame2.ysc, sticky="ns", padx=c(0, 10), pady=c(10, 0))
  tkgrid.configure(frame2.xsc, sticky="we", padx=c(10, 0), pady=c(0, 5))

  tktag.configure(frame2.tbl, "active", background="#EAEEFE", relief="")
  tktag.configure(frame2.tbl, "sel", background="#EAEEFE", foreground="black")

  tktag.configure(frame2.tbl, "title", background="white",
                  foreground="#0000FF", multiline=1, ellipsis="...", wrap=1)

  tcl(frame2.tbl, "tag", "row", "coltitles", 0)
  tktag.configure(frame2.tbl, "coltitles", anchor="center", anchor="n",
                  justify="center")
  tcl(frame2.tbl, "tag", "col", "rowtitles", 0)
  tktag.configure(frame2.tbl, "rowtitles", anchor="ne", justify="right")

  tag.cols <- c(no.cols, dt.cols)
  if (length(tag.cols) > 0) {
    for (j in tag.cols)
      tcl(frame2.tbl, "tag", "col", "numeric", j)
    tktag.configure(frame2.tbl, "numeric", anchor="ne", justify="right")
  }

  tkgrid.columnconfigure(frame2, 0, weight=1)
  tkgrid.rowconfigure(frame2, 0, weight=1)

  tkpack(frame2, fill="both", expand=TRUE)

  tkselection.set(frame2.tbl, "origin")

  # Text bindings

  tkbind(tt, "<Control-a>", SelectAll)
  tkbind(frame1.ent.1.2, "<Return>", function() Find("next"))
  tkbind(frame1.ent.1.2, "<Up>", function() Find("prev"))
  tkbind(frame1.ent.1.2, "<Down>", function() Find("next"))
  tkbind(frame1.ent.2.2, "<Return>", function() GotoLine())

  # GUI control

  tkgrab(tt)
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tclServiceMode(TRUE)
  tkfocus(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  invisible()
}
