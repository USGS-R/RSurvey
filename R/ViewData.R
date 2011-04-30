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
    tkyview(frame2.tbl, idx - 1)
  }

  # Get single cell value for table, a simplified version of
  #   as.tclObj(as.character(x), drop=TRUE) is used.

  GetCellValue <- function(r, c) {
    val <- .External("RTcl_ObjFromCharVector",
                     d[as.integer(r) + 1, as.integer(c) + 1],
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

  # Number of rows and columns

  m <- nrow(d)
  n <- ncol(d)
  if (m == 0)
    return()

  # Row titles

  rows <- row.names(d)

  # Height and width of viewable table

  height <- if (m > 10) 10 else m
  width <- if (n > 6) 6 else n

  # Table titles

  col.width.title <- nchar(max(as.integer(row.names(d)))) + 1

  if (is.null(col.names)) {
    cols <- rep("", n)
  } else {
    cols <- col.names[1:n]
    cols[is.na(cols)] <- ""
  }

  if (is.null(col.units)) {
    col.units <- rep(NA, n)
  } else {
    col.units <- as.character(col.units[1:n])
  }
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
  tt.done.var <- tclVar(0)

  # Open GUI

  tclServiceMode(FALSE)

  tt <- tktoplevel(padx=0, pady=0)

  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    tmp <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(tmp[2]) + 25,
                            "+", as.integer(tmp[3]) + 25, sep=""))
  }

  tktitle(tt) <- "View Data"

  # Frame 0 contains ok button and size grip

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

  # Frame 1 contains line search

  frame1 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0, height=200)
  frame1.lab.1 <- ttklabel(frame1, text="Record no.")
  frame1.ent.2 <- ttkentry(frame1, width=10, textvariable=line.no.var)
  frame1.but.3 <- ttkbutton(frame1, width=6, text="Goto", command=GotoLine)

  tkbind(frame1.ent.2, "<Return>", function() GotoLine())

  tkgrid(frame1.lab.1, frame1.ent.2, frame1.but.3)

  tkgrid.configure(frame1.but.3, padx=2)

  tkpack(frame1, side="bottom", anchor="nw", padx=c(10, 0))

  # Frame 2 contains the data table

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
