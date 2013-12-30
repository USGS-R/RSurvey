
.UnzipWorkbook <- function(f) {
  path <- tempfile("workbook")
  suppressWarnings(dir.create(path))
  file.copy(f, path)
  f.tmp <- list.files(path, pattern=basename(f), full.names=TRUE)
  unzip(f.tmp, exdir=path)
  return(path)
}

.GetStyles <- function(path) {
  path.xl <- file.path(path, "xl")
  f.styles <- list.files(path.xl, "styles.xml$", full.names=TRUE)
  styles <- xmlParse(f.styles)
  styles <- xpathApply(styles, "//x:xf[@xfId and @numFmtId]", xmlAttrs,
                       namespaces="x")
  fun <- function(i) i[grepl("numFmtId", names(i))]
  styles <- lapply(styles, fun)
  fun <- function(i) i["numFmtId"]
  styles <- as.integer(sapply(styles, fun))
  names(styles) <- seq_along(styles) - 1
  return(styles)
}

.GetSheetNames <- function(path) {
  path.xl <- file.path(path, "xl")
  f.workbook <- list.files(path.xl, "workbook.xml$", full.names=TRUE)
  sheets <- xmlToList(xmlParse(f.workbook))
  fun <- function(i) as.data.frame(as.list(i), stringsAsFactors=FALSE)
  sheets.lst <- lapply(sheets$sheets, fun)
  available.args <- unique(unlist(sapply(sheets.lst, names)))
  fun <- function(i) {
    missing.args <- available.args[which(!available.args %in% names(i))]
    i[, missing.args] <- NA
    return(i)
  }
  sheets <- do.call("rbind", lapply(sheets.lst, fun))  # or use plyr::rbind.fill
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
  return(match(x, potential.cols))
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

  cell.range <- .ParseCellRange(cell.range)
  if (is.list(cell.range)) {
    rows <- match(cell.range$rows, row.names(d))
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
  if (!require("XML"))
    stop()


  f <- "C:/Users/jfisher/Desktop/xlsxToR/ex.data.xlsx"
  path <- .UnzipWorkbook(f)
  sheets <- .GetSheetNames(path)
  sheets.name <- sheets$name


  sheet.idx <- 2L
  sheet.id <- sheets$id[sheet.idx]
  cell.range <- "X2:AB20"
  header <- TRUE
  str.as.fact <- FALSE


  d <- .ReadWorksheet(path, sheet.id, cell.range, header, str.as.fact)


}
