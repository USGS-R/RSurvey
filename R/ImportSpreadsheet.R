# Derived with permission from xlsxToR function, accessed on 2014-01-01:
#   Schaun Wheeler <schaun.wheeler at gmail.com>
#   https://gist.github.com/schaunwheeler/5825002
ImportSpreadsheet <- function(parent=NULL) {


  GetDataFile <- function(f) {
    if (missing(f)) {
      txt <- "Open XML Spreadsheet File"
      f <- GetFile(cmd="Open", exts="xlsx", win.title=txt, parent=tt)
      if (is.null(f)) return()
    }
    path <<- NULL
    sheets <<- NULL
    tkconfigure(f0.but.1.2, state="disabled")
    tkconfigure(f2.lab.1.1, state="disabled")
    tkconfigure(f2.box.1.2, state="disabled", value="{}")
    tcl(f2.box.1.2, "current", 0)
    if (f == "") return()
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
    if (length(sheet.names) == 0) sheet.names <- paste0("{", sheet.names, "}")
    tkconfigure(f0.but.1.2, state="normal")
    tkconfigure(f2.lab.1.1, state="normal")
    tkconfigure(f2.box.1.2, state="readonly", value=sheet.names)
    tcl(f2.box.1.2, "current", 0)
    path <<- path
    sheets <<- sheets
    tclvalue(source.var) <- f
  }


  LoadDataset <- function() {
    if (is.null(path) || is.null(sheets)) return()
    idx <- as.integer(tcl(f2.box.1.2, "current")) + 1L
    sheet.id <- sheets$id[idx]
    worksheet <- sheets$name[idx]
    cell.range <- .ParseCellRange(as.character(tclvalue(cell.range.var)))
    if (is.null(cell.range)) {
      tkmessageBox(icon="error", message="Unable to parse cell range.",
                   title="Error", type="ok", parent=tt)
      tkfocus(f2.ent.2.2)
      return()
    }
    header      <- as.logical(as.integer(tclvalue(header.var)))
    rm.col.na   <- as.logical(as.integer(tclvalue(rm.col.na.var)))
    rm.row.na   <- as.logical(as.integer(tclvalue(rm.row.na.var)))
    save.row.no <- as.logical(as.integer(tclvalue(save.row.no.var)))
    str.as.fact <- as.logical(as.integer(tclvalue(str.as.fact.var)))
    pathname <- as.character(tclvalue(source.var))
    tkconfigure(tt, cursor="watch")
    tclServiceMode(FALSE)
    on.exit(tkconfigure(tt, cursor="arrow"))
    on.exit(tclServiceMode(TRUE), add=TRUE)
    d <- try(.ReadWorksheet(path, sheet.id, cell.range, header, rm.col.na,
                            rm.row.na, save.row.no, str.as.fact), silent=TRUE)
    if (inherits(d, "try-error")) {
      tkmessageBox(icon="error", message="Unable to read worksheet.",
                   title="Error", type="ok", detail=d, parent=tt)
      return()
    }
    rtn <<- list(d=d, src=c(pathname=pathname, worksheet=worksheet,
                            accessed=format(Sys.time())))
    tclvalue(tt.done.var) <- 1
  }


  # load dependent package
  if (!requireNamespace("XML", quietly=TRUE)) stop()

  # initialize variables
  rtn    <- NULL
  path   <- NULL
  sheets <- NULL

  # assign variables linked to Tk widgets
  source.var      <- tclVar()
  cell.range.var  <- tclVar()
  header.var      <- tclVar(0)
  rm.col.na.var   <- tclVar(0)
  rm.row.na.var   <- tclVar(0)
  save.row.no.var <- tclVar(0)
  str.as.fact.var <- tclVar(0)
  tt.done.var     <- tclVar(0)

  # open gui
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

  # frame 0, buttons
  f0 <- ttkframe(tt, relief="flat")
  f0.but.1.2 <- ttkbutton(f0, width=12, text="Load", command=LoadDataset)
  f0.but.1.3 <- ttkbutton(f0, width=12, text="Cancel",
                          command=function() tclvalue(tt.done.var) <- 1)
  f0.but.1.4 <- ttkbutton(f0, width=12, text="Help",
                          command=function() {
                            print(help("ImportSpreadsheet", package="RSurvey"))
                          })
  tkgrid("x", f0.but.1.2, f0.but.1.3, f0.but.1.4, pady=c(15, 10))
  tkgrid.configure(f0.but.1.2, padx=c(10, 0))
  tkgrid.configure(f0.but.1.3, padx=4)
  tkgrid.configure(f0.but.1.4, padx=c(0, 10))
  tkgrid.columnconfigure(f0, 0, weight=1)
  tkpack(f0, fill="x", side="bottom", anchor="e")
  tkconfigure(f0.but.1.2, state="disabled")

  # frame 1, file
  f1 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0)
  f1.lab.1.1 <- ttklabel(f1, text="Import from")
  f1.ent.1.2 <- ttkentry(f1, textvariable=source.var)
  f1.but.1.3 <- ttkbutton(f1, width=8, text="Browse", command=function() GetDataFile())
  tkgrid(f1.lab.1.1, f1.ent.1.2, f1.but.1.3, pady=c(10, 0))
  tkgrid.configure(f1.lab.1.1, sticky="w")
  tkgrid.configure(f1.ent.1.2, sticky="we", padx=2)
  tkgrid.configure(f1.but.1.3, padx=c(2, 3))
  tkgrid.columnconfigure(f1, 1, weight=1)
  tkpack(f1, fill="x", padx=10)

  # frame 2, worksheet and cell range
  f2 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0)
  f2.lab.1.1 <- ttklabel(f2, text="Select worksheet in workbook", state="disabled")
  f2.lab.2.1 <- ttklabel(f2, text="Cell range in worksheet (optional)")
  f2.lab.2.3 <- ttklabel(f2, width=10, text="e.g. B3:F18")
  f2.box.1.2 <- ttkcombobox(f2, width=20, state="disabled", value="{}")
  f2.ent.2.2 <- ttkentry(f2, width=20, textvariable=cell.range.var)
  tkgrid(f2.lab.1.1, f2.box.1.2, "x", pady=c(10, 0))
  tkgrid(f2.lab.2.1, f2.ent.2.2, f2.lab.2.3, pady=c(4, 0))
  tkgrid.configure(f2.lab.1.1, f2.lab.2.1, sticky="w", padx=c(0, 2))
  tkgrid.configure(f2.box.1.2, f2.ent.2.2, sticky="we")
  tkgrid.configure(f2.lab.2.3, padx=c(2, 0))
  tkgrid.columnconfigure(f2, 1, weight=1)
  tkpack(f2, fill="x", padx=10)

  # frame 3, options
  f3 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0)
  txt <- "Column names are located in the first row of the data table"
  f3.chk.1.1 <- ttkcheckbutton(f3, variable=header.var, text=txt)
  txt <- "Remove columns which contain only missing values"
  f3.chk.2.1 <- ttkcheckbutton(f3, variable=rm.col.na.var, text=txt)
  txt <- "Remove rows which contain only missing values"
  f3.chk.3.1 <- ttkcheckbutton(f3, variable=rm.row.na.var, text=txt)
  txt <- "Preserve row numbering in worksheet"
  f3.chk.4.1 <- ttkcheckbutton(f3, variable=save.row.no.var, text=txt)
  txt <- "Convert strings to factors"
  f3.chk.5.1 <- ttkcheckbutton(f3, variable=str.as.fact.var, text=txt)
  txt <- "An attempt is made to preserve column classes; cell formats are removed."
  f3.lab.6.1 <- ttklabel(f3, text=txt, foreground="#A40802")
  tkgrid(f3.chk.1.1, sticky="w", pady=c(10, 0))
  tkgrid(f3.chk.2.1, sticky="w")
  tkgrid(f3.chk.3.1, sticky="w")
  tkgrid(f3.chk.4.1, sticky="w")
  tkgrid(f3.chk.5.1, sticky="w")
  tkgrid(f3.lab.6.1, sticky="w", pady=c(10, 0))
  tkpack(f3, fill="x", padx=10)

  # bind events
  tclServiceMode(TRUE)
  tkbind(f1.ent.1.2, "<Return>", function() GetDataFile(as.character(tclvalue(source.var))))

  # gui control
  tkfocus(tt)
  tkgrab(tt)
  tkwait.variable(tt.done.var)
  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)
  invisible(rtn)
}


.UnzipWorkbook <- function(f) {
  path <- tempfile("workbook")
  suppressWarnings(dir.create(path))
  suppressWarnings(file.copy(f, path))
  f.tmp <- list.files(path, pattern=basename(f), full.names=TRUE)
  unzip(f.tmp, exdir=path)
  return(path)
}


.GetStyles <- function(path) {
  path.xl <- file.path(path, "xl")
  f.styles <- list.files(path.xl, "styles.xml$", full.names=TRUE)
  styles <- XML::xmlParse(f.styles)
  custom.styles <- XML::xpathApply(styles, "//x:numFmt[@numFmtId and @formatCode]",
                                   XML::xmlAttrs, namespaces="x")
  custom.styles <- as.data.frame(.RbindFill(custom.styles),
                                 stringsAsFactors=FALSE)
  custom.styles$formatCode <- sub(";@$", "", custom.styles$formatCode)
  custom.styles$numFmtId <- as.integer(custom.styles$numFmtId)
  zeros <- vapply(seq_len(15), function(i) paste(rep("0", i), collapse=""), "")
  fmt.codes.d <- c("yyyy\\-mm\\-dd",
                   "yyyy/mm/dd",
                   "m\\-d\\-yy",
                   "m\\-d\\-yyyy",
                   "m/d/yy",
                   "m/d/yyyy",
                   "mm/dd/yy",
                   "mm/dd/yyyy",
                   "mm\\-dd\\-yy",
                   "mm\\-dd\\-yyyy",
                   "mmm\\ d\\ yyyy",
                   "mmm\\-yy",
                   "d\\-mmm",
                   "d\\-mmm\\-yy",
                   "d\\-mmm\\-yyyy",
                   "dd/mm/yy",
                   "dd/mm/yyyy",
                   "dd\\-mm\\-yy",
                   "dd\\-mm\\-yyyy",
                   "[$-409]d\\-mmm\\-yy",
                   "[$-409]dddd\\,\\ mmmm\\ dd\\,\\ yyyy")
  fmt.codes.t <- c("h:mm",
                   "hh:mm",
                   "h:mm\\ AM/PM",
                   "hh:mm\\ AM/PM",
                   "h:mm:ss\\ AM/PM",
                   "hh:mm:ss\\ AM/PM",
                   "hh:mm:ss", paste("hh:mm:ss", zeros, sep="."),
                   "h:mm:ss", paste("h:mm:ss", zeros, sep="."),
                   "mm:ss", paste("mm:ss", zeros, sep="."))
  fmt.codes.dt <- NULL
  for (i in fmt.codes.d) {
    FUN <- function(j) paste(i, j, sep="\\ ")
    fmt.codes.dt <- c(fmt.codes.dt, vapply(fmt.codes.t, FUN, ""))
  }
  names(fmt.codes.dt) <- NULL
  ids.d  <- custom.styles$numFmtId[custom.styles$formatCode %in% fmt.codes.d]
  ids.t  <- custom.styles$numFmtId[custom.styles$formatCode %in% fmt.codes.t]
  ids.dt <- custom.styles$numFmtId[custom.styles$formatCode %in% fmt.codes.dt]
  styles <- XML::xpathApply(styles, "//x:xf[@numFmtId and @xfId]", XML::xmlAttrs,
                       namespaces="x")
  styles <- lapply(styles, function(i) i[grepl("numFmtId", names(i))])
  styles <- vapply(styles, function(i) as.integer(i["numFmtId"]), 0L)
  names(styles) <- seq_along(styles) - 1L
  styles[styles %in% ids.d]  <- 14  # date
  styles[styles %in% ids.t]  <- 18  # time
  styles[styles %in% ids.dt] <- 22  # date-time
  return(styles)
}


.GetSheetNames <- function(path) {
  path.xl <- file.path(path, "xl")
  f.workbook <- list.files(path.xl, "workbook.xml$", full.names=TRUE)
  sheets <- XML::xmlToList(XML::xmlParse(f.workbook))
  FUN <- function(i) as.data.frame(as.list(i), stringsAsFactors=FALSE)
  sheets <- .RbindFill(lapply(sheets$sheets, FUN))
  rownames(sheets) <- NULL
  sheets$id <- gsub("\\D", "", sheets$id)
  return(sheets)
}


.GetWorksheet <- function(path, sheet.id) {
  path.ws <- file.path(path, "xl", "worksheets")
  f.ws <- list.files(path.ws, paste0("sheet(", sheet.id, ")\\.xml$"),
                     full.names=TRUE)
  ws <- XML::xmlRoot(XML::xmlParse(f.ws))[["sheetData"]]
  if (XML::xmlSize(ws) == 0)
    stop("selected worksheet is empty.", call.=FALSE)
  FUN <- function(i) c(v=XML::xmlValue(i[["v"]]), XML::xmlAttrs(i))
  ws <- XML::xpathApply(ws, "//x:c", FUN, namespaces="x")
  rows <- unlist(lapply(seq_along(ws), function(i) rep(i, length(ws[[i]]))))
  ws <- unlist(ws)
  ws <- data.frame(row=rows, ind=names(ws), value=ws, stringsAsFactors=FALSE)
  ws <- reshape(ws, idvar="row", timevar="ind", direction="wide")
  colnames(ws) <- gsub("^value\\.", "", colnames(ws))
  return(ws)
}


.GetSharedStrings <- function(path) {
  path.xl <- file.path(path, "xl")
  f.strings <- list.files(path.xl, "sharedStrings.xml$", full.names=TRUE)
  strings <- XML::xpathSApply(XML::xmlParse(f.strings), "//x:si", XML::xmlValue,
                              namespaces="x")
  names(strings) <- seq_along(strings) - 1
  return(strings)
}


.ReadWorksheet <- function(path, sheet.id, cell.range, header, rm.col.na,
                           rm.row.na, save.row.no, str.as.fact) {
  styles <- .GetStyles(path)
  strings <- .GetSharedStrings(path)
  ws <- .GetWorksheet(path, sheet.id)
  matched.strings <- strings[match(ws$v[ws$t == "s" & !is.na(ws$t)],
                                   names(strings))]
  ws$v[!is.na(ws$t) & ws$t == "s"] <- matched.strings
  ws$cols <- .Letters2Indexes(gsub("\\d", "", ws$r))
  ws$rows <- as.numeric(gsub("\\D", "", ws$r))
  if (!any(grepl("^s$", colnames(ws))))
    ws$s <- NA
  d <- tapply(ws$v, list(ws$rows, ws$cols), identity)
  d.style <- tapply(ws$s, list(ws$rows, ws$cols), identity)
  d.style[is.na(d.style)] <- "0"  # set missing styles to general format string
  if (is.list(cell.range)) {
    rows <- which(rownames(d) %in% cell.range$rows)
    cols <- which(colnames(d) %in% cell.range$cols)
    if (length(rows) == 0 || length(cols) == 0)
      stop("cell range results in empty data table.", call.=FALSE)
    d <- d[rows, cols, drop=FALSE]
    d.style <- d.style[rows, cols, drop=FALSE]
  }
  if (header) {
    col.names <- d[1, ]
    col.names[is.na(col.names)] <- "Unknown"
    colnames(d) <- .TrimSpace(col.names)
    d <- d[-1, , drop=FALSE]
    d.style <- d.style[-1, , drop=FALSE]
  } else {
    FUN <- function(i) paste0(LETTERS, i)
    all.cols <- c(LETTERS, as.vector(t(vapply(LETTERS, FUN, rep("", 26)))))
    colnames(d) <- all.cols[as.integer(colnames(d))]
  }
  if (rm.col.na) {
    cols <- apply(d, 2, function(i) !all(is.na(i)))
    if (length(cols) == 0)
      stop("removing columns with all missing values results in empty table.", call.=FALSE)
    d <- d[, cols, drop=FALSE]
    d.style <- d.style[, cols, drop=FALSE]
  }
  if (rm.row.na) {
    rows <- apply(d, 1, function(i) !all(is.na(i)))
    if (length(rows) == 0)
      stop("removing rows with all missing values results in empty table.",
           call.=FALSE)
    d <- d[rows, , drop=FALSE]
    d.style <- d.style[rows, , drop=FALSE]
  }
  d <- as.data.frame(d, stringsAsFactors=FALSE)
  d.style <- as.data.frame(d.style, stringsAsFactors=FALSE)
  FUN <- function(i) as.numeric(names(which.max(table(i))))
  col.style <- vapply(d.style, FUN, 0)
  col.style[] <- styles[col.style + 1L]
  origin <- "1899-12-30"  # TODO(jfisher): check mac os, might be "1904-01-01"
  for (i in seq_along(col.style)) {
    if (col.style[i] %in% 14:17) {  # date-time
      d[, i] <- as.Date(as.numeric(d[, i]), origin=origin)
    } else if (col.style[i] %in% c(18:21, 22, 45:47)) {
      d[, i] <- as.POSIXct(as.numeric(d[, i]) * 86400, origin=origin, tz="GMT")
    } else {
      d[, i] <- type.convert(.TrimSpace(d[, i]), as.is=!str.as.fact)
    }
  }
  if (!save.row.no)
    row.names(d) <- seq_len(nrow(d))
  return(d)
}

.TrimSpace <- function(x) {
  if (is.character(x))
    x <- gsub("(^ +)|( +$)", "", x)
  return(x)
}


.RbindFill <- function (lst) {  # substitute for plyr::rbind.fill
  available.args <- unique(unlist(lapply(lst, names)))
  FUN <- function(i) {
    missing.args <- available.args[which(!available.args %in% names(i))]
    if (length(missing.args) > 0)
      i[, missing.args] <- NA
    return(i)
  }
  return(do.call("rbind", lapply(lst, FUN)))
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
  if (anyNA(rows))
    return()
  cols <- .Letters2Indexes(gsub("\\d", "", cells))
  if (anyNA(cols))
    return()
  if (rows[1] > rows[2] || cols[1] > cols[2])
    return()
  return(list(rows=as.character(seq(rows[1], rows[2])),
              cols=as.character(seq(cols[1], cols[2]))))
}


.Letters2Indexes <- function(x) {
  FUN <- function(i) paste0(LETTERS, i)
  all.cols <- c(LETTERS, as.vector(t(vapply(LETTERS, FUN, rep("", 26)))))
  return(match(toupper(x), all.cols))
}
