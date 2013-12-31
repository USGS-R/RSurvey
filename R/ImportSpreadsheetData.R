
.UnzipWorkbook <- function(f) {
  path <- tempfile("workbook")
  suppressWarnings(dir.create(path))
  suppressWarnings(file.copy(f, path))
  f.tmp <- list.files(path, pattern=basename(f), full.names=TRUE)
  unzip(f.tmp, exdir=path)
  return(path)
}

.RbindFill <- function (lst) {  # substitute for plyr::rbind.fill
  available.args <- unique(unlist(lapply(lst, names)))
  fun <- function(i) {
    missing.args <- available.args[which(!available.args %in% names(i))]
    if (length(missing.args) > 0)
      i[, missing.args] <- NA
    return(i)
  }
  return(do.call("rbind", lapply(lst, fun)))
}

.GetStyles <- function(path) {
  path.xl <- file.path(path, "xl")
  f.styles <- list.files(path.xl, "styles.xml$", full.names=TRUE)
  styles <- xmlParse(f.styles)
  custom.styles <- xpathApply(styles, "//x:numFmt[@numFmtId and @formatCode]",
                              xmlAttrs, namespaces="x")
  custom.styles <- as.data.frame(.RbindFill(custom.styles),
                                 stringsAsFactors=FALSE)
  custom.styles$numFmtId <- as.integer(custom.styles$numFmtId)
  zeros <- sapply(1:11, function(i) paste(rep("0", i), collapse=""))
  dt.fmt.codes <- c("yyyy\\-mm\\-dd\\ hh:mm:ss",
                    paste0("yyyy\\-mm\\-dd\\ hh:mm:ss.", zeros),
                    "yyyy\\-mm\\-dd",
                    "mm/dd/yyyy\\ hh:mm:ss",
                    paste0("mm/dd/yyyy\\ hh:mm:ss.", zeros),
                    "m/d/yy\\ h:mm;@",
                    "[$-409]dddd\\,\\ mmmm\ dd\\,\\ yyyy",
                    "[$-409]m/d/yy\\ h:mm\\ AM/PM;@")
  dt.ids <- custom.styles$numFmtId[custom.styles$formatCode %in% dt.fmt.codes]
  styles <- xpathApply(styles, "//x:xf[@xfId and @numFmtId]", xmlAttrs,
                       namespaces="x")
  styles <- lapply(styles, function(i) i[grepl("numFmtId", names(i))])
  styles <- as.integer(sapply(styles, function(i) i["numFmtId"]))
  names(styles) <- seq_along(styles) - 1

  styles[styles %in% dt.ids] <- 22

  return(styles)
}

.GetSheetNames <- function(path) {
  path.xl <- file.path(path, "xl")
  f.workbook <- list.files(path.xl, "workbook.xml$", full.names=TRUE)
  sheets <- xmlToList(xmlParse(f.workbook))
  fun <- function(i) as.data.frame(as.list(i), stringsAsFactors=FALSE)
  sheets <- .RbindFill(lapply(sheets$sheets, fun))
  rownames(sheets) <- NULL
  sheets$id <- gsub("\\D", "", sheets$id)
  return(sheets)
}

.GetWorksheet <- function(path, sheet.id) {
  path.ws <- file.path(path, "xl", "worksheets")
  f.ws <- list.files(path.ws, paste0("sheet(", sheet.id, ")\\.xml$"),
                     full.names=TRUE)
  ws <- xmlRoot(xmlParse(f.ws))[["sheetData"]]
  fun <- function(i) c("v"=xmlValue(i[["v"]]), xmlAttrs(i))
  ws <- xpathApply(ws, "//x:c", fun, namespaces="x")
  fun <- function(i) rep(i, length(ws[[i]]))
  rows <- unlist(lapply(seq_along(ws), fun))
  ws <- unlist(ws)
  ws <- data.frame("row"=rows, "ind"=names(ws), "value"=ws,
                   stringsAsFactors=FALSE)
  ws <- reshape(ws, idvar="row", timevar="ind", direction="wide")
  colnames(ws) <- gsub("^value\\.", "", colnames(ws))
  return(ws)
}

.GetSharedStrings <- function(path) {
  path.xl <- file.path(path, "xl")
  f.strings <- list.files(path.xl, "sharedStrings.xml$", full.names=TRUE)
  strings <- xpathSApply(xmlParse(f.strings), "//x:si", xmlValue,
                         namespaces="x")
  names(strings) <- seq_along(strings) - 1
  return(strings)
}

.ParseCellRange <- function(x) {
  if (!is.character(x))
    return()
  if (x == "")
    return(NA)
  cells <- strsplit(x, ":")[[1]]
  if (length(cells) != 2)
    return()
  rows <- as.integer(gsub("\\D", "", cells))
  if (any(is.na(rows)))
    return()
  cols <- .Letters2Columns(gsub("\\d", "", cells))
  if (any(is.na(cols)))
    return()
  if (rows[1] > rows[2] || cols[1] > cols[2])
    return()
  return(list(rows=as.character(seq(rows[1], rows[2])),
              cols=as.character(seq(cols[1], cols[2]))))
}

.Letters2Columns <- function(x) {
  fun <- function(i) paste0(LETTERS, i)
  potential.cols <- c(LETTERS, as.vector(t(vapply(LETTERS, fun, rep("", 26)))))
  return(match(toupper(x), potential.cols))
}

.ReadWorksheet <- function(path, sheet.id, cell.range, header, str.as.fact) {

  styles <- .GetStyles(path)
  strings <- .GetSharedStrings(path)
  ws <- .GetWorksheet(path, sheet.id)

  matched.strings <- strings[match(ws$v[ws$t == "s" & !is.na(ws$t)],
                                   names(strings))]
  ws$v[!is.na(ws$t) & ws$t == "s"] <- matched.strings
  ws$cols <- .Letters2Columns(gsub("\\d", "", ws$r))
  ws$rows <- as.numeric(gsub("\\D", "", ws$r))
  if (!any(grepl("^s$", colnames(ws))))
    ws$s <- NA

  d <- tapply(ws$v, list(ws$rows, ws$cols), identity)
  d.style <- tapply(ws$s, list(ws$rows, ws$cols), identity)

  if (is.list(cell.range)) {
    rows <- match(cell.range$rows, rownames(d))
    cols <- match(cell.range$cols, colnames(d))
    if (length(rows) == 0 || length(cols) == 0)
      return()
    d <- d[rows, cols]
    d.style <- d.style[rows, cols]
  }

  if (header) {
    colnames(d) <- d[1, ]
    d <- d[-1, ]
    d.style <- d.style[-1, ]
  }
  d <- as.data.frame(d, stringsAsFactors=FALSE)
  d.style <- as.data.frame(d.style, stringsAsFactors=FALSE)

  fun <- function(i) as.numeric(names(which.max(table(i))))
  col.style <- sapply(d.style, fun)
  if (length(styles) > 0)
    col.style[] <- styles[col.style + 1]

  origin.date <- "1899-12-30"
  for (i in seq_along(col.style)) {
    if (col.style[i] %in% 14:17) {  # date
      d[, i] <- as.Date(as.numeric(d[, i]), origin=origin.date)
    } else if (col.style[i] %in% c(18:21, 45:47)) {  # time
      d[, i] <- as.POSIXct(as.numeric(d[, i]) * 86400, origin=origin.date,
                           tz="GMT")
    } else if (col.style[i] %in% 22) {  # date-time
      d[, i] <- as.POSIXct(as.numeric(d[, i]), origin=origin.date)
    } else {
      d[, i] <- type.convert(d[, i], as.is=!str.as.fact)
    }
  }
  return(d)
}



ImportSpreadsheetData <- function(parent=NULL) {


  GetDataFile <- function(f) {

    if (missing(f)) {
      txt <- "Open XML Spreadsheet File"
      f <- GetFile(cmd="Open", exts="xlsx", win.title=txt, parent=tt)
      if (is.null(f) || attr(f, "extension") != "xlsx")
        return()
    }

    path <<- NULL
    sheets <<- NULL
    tkconfigure(frame0.but.1.2, state="disabled")
    tkconfigure(frame2.lab.1.1, state="disabled")
    tkconfigure(frame2.box.1.2, state="disabled", value="{}")
    tcl(frame2.box.1.2, "current", 0)

    if (f == "")
      return()

    path <- try(.UnzipWorkbook(f), silent=TRUE)
    if (inherits(path, "try-error") || !is.character(path)) {
      tkmessageBox(icon="error", message="Unable to access workbook.",
                   detail=path, title="Error", type="ok", parent=tt)
      return()
    }

    sheets <- try(.GetSheetNames(path), silent=TRUE)
    if (inherits(sheets, "try-error")) {
      tkmessageBox(icon="error", message="Unable to access worksheets.",
                   detail=sheets, title="Error", type="ok", parent=tt)
      return()
    }

    sheet.names <- as.character(sheets$name)
    if (length(sheet.names) == 0)
      sheet.names <- paste0("{", sheet.names, "}")
    tkconfigure(frame0.but.1.2, state="normal")
    tkconfigure(frame2.lab.1.1, state="normal")
    tkconfigure(frame2.box.1.2, state="normal", value=sheet.names)
    tcl(frame2.box.1.2, "current", 0)

    path <<- path
    sheets <<- sheets
    tclvalue(source.var) <- f
  }


  LoadDataset <- function() {

    if (is.null(path) || is.null(sheets))
      return()

    idx <- as.integer(tcl(frame2.box.1.2, "current")) + 1L
    sheet.id <- sheets$id[idx]

    cell.range <- .ParseCellRange(as.character(tclvalue(cell.range.var)))
    if (is.null(cell.range)) {
      tkmessageBox(icon="error", message="Unable to parse cell range.",
                   title="Error", type="ok", parent=tt)
      tkfocus(frame2.ent.2.2)
      return()
    }

    header <- as.logical(as.integer(tclvalue(header.var)))
    str.as.fact <- as.logical(as.integer(tclvalue(str.as.fact.var)))
    src <- as.character(tclvalue(source.var))

    tkconfigure(tt, cursor="watch")
    tclServiceMode(FALSE)
    on.exit(tkconfigure(tt, cursor="arrow"))
    on.exit(tclServiceMode(TRUE), add=TRUE)

    rtn <<- list(d=.ReadWorksheet(path, sheet.id, cell.range, header,
                                  str.as.fact), src=src)
    tclvalue(tt.done.var) <- 1
  }




  if (!require("XML"))
    stop()

  rtn <- NULL
  path <- NULL
  sheets <- NULL

  # Assign variables linked to Tk widgets
  source.var <- tclVar()
  cell.range.var <- tclVar("")
  header.var <- tclVar(0)
  str.as.fact.var <- tclVar(0)
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
  tktitle(tt) <- "Import Data From XML Spreadsheet File"
  tkwm.resizable(tt, 1, 0)

  # Frame 0, buttons
  frame0 <- ttkframe(tt, relief="flat")
  frame0.but.1.2 <- ttkbutton(frame0, width=12, text="Load",
                              command=LoadDataset)
  frame0.but.1.3 <- ttkbutton(frame0, width=12, text="Cancel",
                              command=function() tclvalue(tt.done.var) <- 1)
  frame0.but.1.4 <- ttkbutton(frame0, width=12, text="Help",
                              command=function() {
                                print(help("ImportSpreadsheetData",
                                           package="RSurvey"))
                              })
  tkgrid("x", frame0.but.1.2, frame0.but.1.3, frame0.but.1.4, pady=10)
  tkgrid.configure(frame0.but.1.2, padx=c(10, 0))
  tkgrid.configure(frame0.but.1.3, padx=4)
  tkgrid.configure(frame0.but.1.4, padx=c(0, 10))
  tkgrid.columnconfigure(frame0, 0, weight=1)
  tkpack(frame0, fill="x", side="bottom", anchor="e")
  tkconfigure(frame0.but.1.2, state="disabled")


  # Frame 1, file locator
  frame1 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0)
  frame1.lab.1.1 <- ttklabel(frame1, text="Import from")
  frame1.ent.1.2 <- ttkentry(frame1, textvariable=source.var)
  frame1.but.1.3 <- ttkbutton(frame1, width=8, text="Browse",
                              command=function() GetDataFile())
  tkgrid(frame1.lab.1.1, frame1.ent.1.2, frame1.but.1.3, pady=c(10, 0))
  tkgrid.configure(frame1.lab.1.1, sticky="w")
  tkgrid.configure(frame1.ent.1.2, sticky="we", padx=2)
  tkgrid.columnconfigure(frame1, 1, weight=1)
  tkpack(frame1, fill="x", padx=10)


  # Frame 2, worksheets
  frame2 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0)
  txt <- "Select worksheet in workbook"
  frame2.lab.1.1 <- ttklabel(frame2, text=txt, state="disabled")
  txt <- "Cell range in worksheet (optional)"
  frame2.lab.2.1 <- ttklabel(frame2, text=txt)
  frame2.lab.2.3 <- ttklabel(frame2, width=9, text="e.g. B2:F18")
  frame2.box.1.2 <- ttkcombobox(frame2, width=16, state="disabled", value="{}")
  frame2.ent.2.2 <- ttkentry(frame2, width=16, textvariable=cell.range.var)
  tkgrid(frame2.lab.1.1, frame2.box.1.2, "x", pady=c(10, 0))
  tkgrid(frame2.lab.2.1, frame2.ent.2.2, frame2.lab.2.3, pady=c(4, 0))
  tkgrid.configure(frame2.lab.1.1, frame2.lab.2.1, sticky="w", padx=c(0, 2))
  tkgrid.configure(frame2.box.1.2, frame2.ent.2.2, sticky="we")
  tkgrid.configure(frame2.lab.2.3, padx=c(2, 0))
  tkgrid.columnconfigure(frame2, 1, weight=1)
  tkpack(frame2, fill="x", padx=10)


  # Frame 3, header and factors
  frame3 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0)
  txt <- "Names of variables are in first row of table"
  frame3.chk.1.1 <- ttkcheckbutton(frame3, variable=header.var, text=txt)
  frame3.chk.2.1 <- ttkcheckbutton(frame3, variable=str.as.fact.var,
                                   text="Convert strings to factors")
  tkgrid(frame3.chk.1.1, sticky="w", pady=c(10, 0))
  tkgrid(frame3.chk.2.1, sticky="w")
  tkpack(frame3, fill="x", padx=10)


  # Bind events
  tclServiceMode(TRUE)

  tkbind(frame1.ent.1.2, "<Return>",
         function() GetDataFile(as.character(tclvalue(source.var))))


  # GUI control

  tkfocus(tt)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  invisible(rtn)
}
