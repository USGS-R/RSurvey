ExportData <- function(file.type="txt", parent=NULL) {


  # final export of data to file
  ExportToFile <- function() {
    tkconfigure(tt, cursor="watch")
    on.exit(tkconfigure(tt, cursor="arrow"))

    idxs <- as.integer(tkcurselection(f1.lst.1.1)) + 1L
    col.ids <- col.ids[idxs]
    is.proc <- as.logical(as.integer(tclvalue(processed.var)))
    file.name <- as.character(tclvalue(file.var))

    # organize data
    vars <- Data("vars")
    cols <- Data("cols")
    rows <- Data("data.raw", which.attr="row.names")

    all.col.ids <- vapply(seq_along(cols), function(i) cols[[i]]$id, "")
    if (file.type == "shp") {
      id.x <- all.col.ids[vars$x]
      id.y <- all.col.ids[vars$y]
      if (!id.x %in% col.ids) col.ids <- c(col.ids, id.x)
      if (!id.y %in% col.ids) col.ids <- c(col.ids, id.y)
    }
    col.idxs <- which(all.col.ids %in% col.ids)
    col.ids  <- vapply(col.idxs, function(i) cols[[i]]$id, "")
    col.funs <- vapply(col.idxs, function(i) cols[[i]]$fun, "")
    col.nams <- vapply(col.idxs, function(i) cols[[i]]$name, "")
    col.fmts <- vapply(col.idxs, function(i) cols[[i]]$format, "")

    # identify data set and records
    if (is.proc) {
      row.names <- row.names(Data("data.pts"))
      row.idxs <- which(row.names %in% rows)
      if (length(row.names) != length(row.idxs))
        stop("length of row names different from length of row indexes")
    } else {
      row.names <- rows
      row.idxs <- seq_along(row.names)
    }
    n <- length(col.idxs)
    m <- length(row.idxs)

    d <- list()
    for (i in seq_len(n)) {
      obj <- EvalFunction(col.funs[i], cols)[row.idxs]
      if (file.type == "txt") {
        fmt <- col.fmts[i]
        if (fmt == "") {
          obj <- .DefaultFormat(obj)
        } else {
          if (inherits(obj, "POSIXt")) {
            obj <- format(obj, format=fmt, na.encode=FALSE)
          } else {
            ans <- try(sprintf(fmt, obj), silent=TRUE)
            if (inherits(ans, "try-error")) {
              obj <- .DefaultFormat(obj)
            } else {
              obj <- ans
              obj[obj %in% c("NA", "NaN")] <- NA
            }
          }
        }
      }
      d[[i]] <- obj
    }

    if (file.type == "txt") {
      d <- do.call(cbind, d)  # matrix
    } else {
      class(d) <- "data.frame"  # see warning in utils::read.table (R v3.0.2)
    }
    rownames(d) <- row.names

    if (is.null(Data("export"))) Data("export", list())
    Data(c("export", "processed"), is.proc)

    # write text file
    if (file.type == "txt") {
      is.fmts <- as.logical(as.integer(tclvalue(conv.fmts.var)))
      is.cols <- as.logical(as.integer(tclvalue(col.names.var)))
      is.rows <- as.logical(as.integer(tclvalue(row.names.var)))
      is.quot <- as.logical(as.integer(tclvalue(quote.var)))
      is.comm <- as.logical(as.integer(tclvalue(comment.var)))
      is.log  <- as.logical(as.integer(tclvalue(changelog.var)))

      sep <- sep0[as.integer(tcl(f3.box.1.2, "current")) + 1L]
      dec <- dec0[as.integer(tcl(f3.box.1.5, "current")) + 1L]
      nas <- nas0[as.integer(tcl(f3.box.2.2, "current")) + 1L]
      qme <- qme0[as.integer(tcl(f3.box.2.5, "current")) + 1L]
      com <- com0[as.integer(tcl(f3.box.3.2, "current")) + 1L]
      enc <- enc0[as.integer(tcl(f4.box.2.2, "current")) + 1L]
      eol <- eol0[as.integer(tcl(f4.box.3.2, "current")) + 1L]
      zip <- zip0[as.integer(tcl(f4.box.4.2, "current")) + 1L]

      if (is.na(sep)) sep <- as.character(tclvalue(sep.var))
      if (is.na(nas)) {
        nas <- as.character(tclvalue(nas.var))
        if (nas == "") nas <- "NA"
      }
      if (is.na(com)) com <- as.character(tclvalue(com.var))

      # write changelog
      if (is.log) {
        base.name <- sub(paste0("\\.", zip, "$"), "", basename(file.name))
        base.name <- paste0(base.name, ".log")
        f.log <- file.path(dirname(file.name), base.name)
        if (file.access(f.log, mode=0) == 0) {
          msg <- paste0("\'", base.name, "\' already exists.\n\nDo you want to replace it?")
          ans <- tkmessageBox(icon="info", message=msg, title="Confirm Save As",
                              type="yesno", parent=tt)
          if (as.character(ans) == "no") return()
        }
        write.table(Data("changelog"), file=f.log, quote=is.quot, sep=sep,
                    eol=eol, na=nas, dec=dec, row.names=FALSE, col.names=TRUE,
                    qmethod=qme, fileEncoding=enc)
      }

      # set connection
      args <- list(description=file.name, open="w", encoding=enc)
      what <- paste0(ifelse(zip == "bz2", "bz", zip), "file")
      con <- do.call(what, args)
      if (!inherits(con, "connection")) stop("Connection error")
      on.exit(close(con), add=TRUE)

      # write comment
      if (is.comm) {
        txt <- Data("comment")
        if (!is.null(txt) && length(txt) > 0 && is.character(txt))
          writeLines(paste(com, txt), con=con, sep=eol)
      }

      # write headers
      headers <- c(is.fmts, is.cols)
      if (any(headers)) {

        if (is.rows) {
          h <- matrix(NA, nrow=sum(headers), ncol=ncol(d) + 1L)
          col.fmts <- c("", col.fmts)
          col.nams <- c("", col.nams)
        } else {
          h <- matrix(NA, nrow=sum(headers), ncol=ncol(d))
        }

        i <- 1L
        if (headers[1]) {
          h[i, ] <- col.fmts
          i <- i + 1L
        }
        if (headers[2]) h[i, ] <- col.nams
        write.table(h, file=con, append=is.comm, quote=is.quot, sep=sep,
                    eol=eol, na=nas, dec=dec, row.names=FALSE, col.names=FALSE,
                    qmethod=qme, fileEncoding=enc)
      }

      # write records
      write.table(d, file=con, append=(is.comm | any(headers)), quote=is.quot,
                  sep=sep, eol=eol, na=nas, dec=dec, row.names=is.rows,
                  col.names=FALSE, qmethod=qme, fileEncoding=enc)

      Data(c("export", "fmts"), is.fmts)
      Data(c("export", "cols"), is.cols)
      Data(c("export", "rows"), is.rows)
      Data(c("export", "comment"), is.comm)
      Data(c("export", "sep"), sep)
      Data(c("export", "dec"), dec)
      Data(c("export", "na"), nas)
      Data(c("export", "com"), com)
      Data(c("export", "qmethod"), qme)
      Data(c("export", "quote"), is.quot)
      Data(c("export", "encoding"), enc)
      Data(c("export", "eol"), eol)
      Data(c("export", "zip"), zip)
      Data(c("export", "changelog"), is.log)

    # write shapefile
    } else if (file.type == "shp") {
      # names are finicky for shapefiles, rules are convoluted,
      # that is, 8-bit names and no periods
      col.ids.8bit <- gsub("\\.", "", make.names(substr(col.ids, 1, 7), unique=TRUE))
      colnames(d) <- col.ids.8bit
      idx.x <- which(col.ids %in% id.x)
      idx.y <- which(col.ids %in% id.y)
      is.coord.na <- is.na(d[, idx.x]) | is.na(d[, idx.y])
      d <- d[!is.coord.na, ]  # remove coordinates containing missing values
      coordinates(d) <- col.ids.8bit[c(idx.x, idx.y)]
      dsn <- dirname(file.name)
      layer <- basename(file.name)
      ext <- tolower(tail(unlist(strsplit(layer, "\\."))[-1], 1))
      if (length(ext) != 0) layer <- sub(paste0(".", ext, "$"), "", layer)
      rgdal::writeOGR(obj=d, dsn=dsn, layer=layer, driver="ESRI Shapefile",
                      verbose=TRUE, overwrite_layer=TRUE)

    # write r data file
    } else if (file.type == "rda") {
      ascii <- as.logical(as.integer(tclvalue(ascii.var)))
      names(d) <- make.names(names=col.ids, unique=TRUE)
      comment(d) <- Data("comment")
      obj.name <- sub("[.][^.]*$", "", basename(file.name))
      assign(obj.name, d, envir=environment(ExportToFile))
      save(list=obj.name, file=file.name, ascii=ascii)
      Data(c("export", "ascii"), ascii)
    }
    memory.usage <- gc()
    tclvalue(tt.done.var) <- 1
  }


  # select all or none from variable list
  SelectVariables <- function(sel) {
    n <- length(col.ids) - 1L
    if (sel == "all") {
      tkselection.set(f1.lst.1.1, 0, n)
    } else if (sel == "none") {
      tkselection.clear(f1.lst.1.1, 0, n)
    } else {
      idxs <- 0:n
      sel <- as.integer(tkcurselection(f1.lst.1.1))
      tkselection.clear(f1.lst.1.1, 0, n)
      for (i in idxs[!(idxs %in% sel)]) tkselection.set(f1.lst.1.1, i)
    }
    ToggleExport()
  }


  # get file
  GetDataFile <- function() {
    if (file.type == "txt") {
      zip <- zip0[as.integer(tcl(f4.box.4.2, "current")) + 1L]
      if (zip == "") {
        sep <- sep0[as.integer(tcl(f3.box.1.2, "current")) + 1L]
        if ("," %in% sep) {
          default.ext <- "csv"
          exts <- "csv"
        } else if ("\t" %in% sep) {
          default.ext <- "tsv"
          exts <- c("tsv", "tab")
        } else {
          default.ext <- "txt"
          exts <- c("txt", "tsv", "csv")
        }
      } else {
        default.ext <- zip
        exts <- zip
      }
    } else {
      default.ext <- file.type
      exts <- file.type
    }
    f <- GetFile(cmd="Save As", exts=exts, file=NULL, win.title="Save Data As",
                 defaultextension=default.ext)
    if (is.null(f)) return()
    tclvalue(file.var) <- f
    ToggleExport()
  }


  # toggle file extension
  ToggleExtension <- function() {
    f <- as.character(tclvalue(file.var))
    if (f == "") return()
    ext <- attr(GetFile(file=f), "extension")
    zip <- zip0[as.integer(tcl(f4.box.4.2, "current")) + 1L]
    for (i in zip0) f <- sub(paste0("\\.", i, "$"), "", f)
    if (zip != ext) tclvalue(file.var) <- ifelse(zip == "", f, paste(f, zip, sep="."))
    return()
  }


  # toggle state of export button
  ToggleExport <- function() {
    idxs <- as.integer(tkcurselection(f1.lst.1.1))
    f <- as.character(tclvalue(file.var))
    s <- if (length(idxs) == 0L || nchar(f) == 0L) "disabled" else "normal"
    tkconfigure(f0.but.2, state=s)
  }


  # check for required information
  col.ids <- vapply(Data("cols"), function(i) i$id, "")
  if (length(col.ids) == 0) return()
  if (!file.type %in% c("txt", "shp", "rda")) stop()
  if (file.type == "shp") {
    if (!requireNamespace("rgdal", quietly=TRUE)) stop()
    if (is.null(Data(c("vars", "x"))) | is.null(Data(c("vars", "y"))))
      stop("shapefiles require x,y coordinate values")
  }

  # initialize values
  sep0 <- c("\t", ",", "", ";", "|", NA)
  sep1 <- c("Tab ( \\t )", "Comma ( , )", "White space (  )",
            "Semicolon ( ; )", "Pipe ( | )", "Custom\u2026")

  nas0 <- c("NA", "na", "N/A", "n/a", NA)
  nas1 <- c("NA", "na", "N/A", "n/a", "Custom\u2026")

  dec0 <- c(".", ",")
  dec1 <- c("Period ( . )", "Comma ( , )")

  qme0 <- c("escape", "double")
  qme1 <- c("Escape quote", "Double quote")

  com0 <- c("#", "!", "\\", "~", NA)
  com1 <- c("Number sign ( # )", "Exclamation ( ! )", "Backslash ( \\\\ )",
            "Tilde ( ~ )", "Custom\u2026")

  enc0 <- c("native.enc", iconvlist())
  enc1 <- c("Default", iconvlist())

  eol0 <- c("\n", "\r", "\r\n")
  eol1 <- c("LF ( \\n )", "CR ( \\r )", "CR+LF ( \\r\\n )")

  zip0 <- c("", "gz", "bz2", "xz")
  zip1 <- c("None", "gzip", "bzip2", "xz")

  # assign variables linked to tk widgets
  variables.var <- tclVar()
  for (i in seq_along(col.ids)) tcl("lappend", variables.var, col.ids[i])

  sep.var       <- tclVar()
  nas.var       <- tclVar()
  com.var       <- tclVar()
  file.var      <- tclVar()
  tt.done.var   <- tclVar(0)

  if (is.null(Data(c("export", "processed"))))
    processed.var <- tclVar(FALSE)
  else
    processed.var <- tclVar(Data(c("export", "processed")))
  if (is.null(Data(c("export", "fmts"))))
    conv.fmts.var <- tclVar(FALSE)
  else
    conv.fmts.var <- tclVar(Data(c("export", "fmts")))
  if (is.null(Data(c("export", "cols"))))
    col.names.var <- tclVar(FALSE)
  else
    col.names.var <- tclVar(Data(c("export", "cols")))
  if (is.null(Data(c("export", "rows"))))
    row.names.var <- tclVar(FALSE)
  else
    row.names.var <- tclVar(Data(c("export", "rows")))
  if (is.null(Data(c("export", "comment"))))
    comment.var <- tclVar(FALSE)
  else
    comment.var <- tclVar(Data(c("export", "comment")))
  if (is.null(Data(c("export", "quote"))))
    quote.var <- tclVar(FALSE)
  else
    quote.var <- tclVar(Data(c("export", "quote")))
  if (is.null(Data(c("export", "changelog"))))
    changelog.var <- tclVar(FALSE)
  else
    changelog.var <- tclVar(Data(c("export", "changelog")))
  if (is.null(Data(c("export", "ascii"))))
    ascii.var <- tclVar(FALSE)
  else
    ascii.var <- tclVar(Data(c("export", "ascii")))

  # open gui
  tclServiceMode(FALSE)
  tt <- tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste0("+", as.integer(geo[2]) + 25,
                             "+", as.integer(geo[3]) + 25))
  }
  tktitle(tt) <- "Export Data"

  # frame 0, export and cancel buttons
  f0 <- ttkframe(tt, relief="flat")
  f0.but.2 <- ttkbutton(f0, width=12, text="Export", command=ExportToFile)
  f0.but.4 <- ttkbutton(f0, width=12, text="Help",
                        command=function() {
                          print(help("ExportData", package="RSurvey"))
                        })
  f0.but.3 <- ttkbutton(f0, width=12, text="Cancel",
                        command=function() tclvalue(tt.done.var) <- 1)
  f0.grp.5 <- ttksizegrip(f0)

  tkgrid("x", f0.but.2, f0.but.3, f0.but.4, f0.grp.5)
  tkgrid.columnconfigure(f0, 0, weight=1)
  tkgrid.configure(f0.but.2, f0.but.3, f0.but.4, padx=c(0, 4), pady=c(0, 10))
  tkgrid.configure(f0.but.4, columnspan=2, padx=c(0, 10))
  tkgrid.configure(f0.grp.5, sticky="se")

  tkraise(f0.but.4, f0.grp.5)

  tkpack(f0, fill="x", side="bottom", anchor="e")

  # frame 1, sample entry
  txt <- "Select variables and data records"
  f1 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5, text=txt)

  f1.lst.1.1 <- tklistbox(f1, selectmode="extended", activestyle="none",
                          relief="flat", borderwidth=5, width=10, height=4,
                          exportselection=FALSE, listvariable=variables.var,
                          highlightthickness=0)
  f1.ysc.1.6 <- ttkscrollbar(f1, orient="vertical")
  tkconfigure(f1.lst.1.1, background="white", yscrollcommand=paste(.Tk.ID(f1.ysc.1.6), "set"))
  tkconfigure(f1.ysc.1.6, command=paste(.Tk.ID(f1.lst.1.1), "yview"))

  f1.but.2.1 <- ttkbutton(f1, width=8, text="All",
                          command=function() SelectVariables("all"))
  f1.but.2.2 <- ttkbutton(f1, width=8, text="None",
                          command=function() SelectVariables("none"))
  f1.but.2.3 <- ttkbutton(f1, width=8, text="Inverse",
                          command=function() SelectVariables("inverse"))
  f1.chk.2.4 <- ttkcheckbutton(f1, variable=processed.var, text="Only export processed records")

  tkgrid(f1.lst.1.1, "x", "x", "x", "x", f1.ysc.1.6)
  tkgrid(f1.but.2.1, f1.but.2.2, f1.but.2.3, f1.chk.2.4, "x", "x", pady=c(4, 0))

  tkgrid.configure(f1.lst.1.1, sticky="nsew", columnspan=5)
  tkgrid.configure(f1.ysc.1.6, sticky="ns")
  tkgrid.configure(f1.but.2.1, f1.but.2.2, padx=c(0, 4))
  tkgrid.configure(f1.chk.2.4, padx=c(40, 0))

  tkgrid.columnconfigure(f1, 4, weight=1, minsize=0)
  tkgrid.rowconfigure(f1, 0, weight=1)

  tkpack(f1, fill="both", expand=TRUE, side="top", padx=10, pady=10)

  if (is.null(Data("data.pts"))) {
    tclvalue(processed.var) <- 0
    tkconfigure(f1.chk.2.4, state="disabled")
  }

  if (file.type == "txt") {

    # frame 2, meta data
    f2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5, text="Metadata")

    f2.chk.1.1 <- ttkcheckbutton(f2, variable=comment.var, text="Comment")
    txt <- "Format conversion specification strings"
    f2.chk.1.2 <- ttkcheckbutton(f2, variable=conv.fmts.var, text=txt)
    f2.chk.2.1 <- ttkcheckbutton(f2, variable=row.names.var, text="Record (row) names")
    f2.chk.2.2 <- ttkcheckbutton(f2, variable=col.names.var, text="Variable (column) names")

    tkgrid(f2.chk.1.1, f2.chk.1.2, sticky="w")
    tkgrid(f2.chk.2.1, f2.chk.2.2, sticky="w")
    tkgrid.configure(f2.chk.1.2, f2.chk.2.2, padx=c(40, 0))

    tkpack(f2, fill="x", padx=10, pady=c(0, 10))

    # frame 3, export parmaters
    f3 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5, text="Options")

    f3.lab.1.1 <- ttklabel(f3, text="Separator")
    f3.lab.1.4 <- ttklabel(f3, text="Decimal")
    f3.lab.2.1 <- ttklabel(f3, text="NA string")
    f3.lab.2.4 <- ttklabel(f3, text="Method")
    f3.lab.3.1 <- ttklabel(f3, text="Comment")

    f3.box.1.2 <- ttkcombobox(f3, width=17, state="readonly", value=sep1)
    f3.box.1.5 <- ttkcombobox(f3, width=17, state="readonly", value=dec1)
    f3.box.2.2 <- ttkcombobox(f3, width=17, state="readonly", value=nas1)
    f3.box.2.5 <- ttkcombobox(f3, width=17, state="readonly", value=qme1)
    f3.box.3.2 <- ttkcombobox(f3, width=17, state="readonly", value=com1)

    f3.ent.1.3 <- ttkentry(f3, width=12, textvariable=sep.var, state="disabled")
    f3.ent.2.3 <- ttkentry(f3, width=12, textvariable=nas.var, state="disabled")
    f3.ent.3.3 <- ttkentry(f3, width=12, textvariable=com.var, state="disabled")

    txt <- "Surround character variables by double quotes"
    f3.chk.4.1 <- ttkcheckbutton(f3, variable=quote.var, text=txt)

    tkgrid(f3.lab.1.1, f3.box.1.2, f3.ent.1.3, f3.lab.1.4, f3.box.1.5)
    tkgrid(f3.lab.2.1, f3.box.2.2, f3.ent.2.3, f3.lab.2.4, f3.box.2.5, pady=4)
    tkgrid(f3.lab.3.1, f3.box.3.2, f3.ent.3.3)
    tkgrid(f3.chk.4.1, pady=c(4, 0))

    tkgrid.configure(f3.lab.1.1, f3.lab.1.4, f3.lab.2.1, f3.lab.2.4, f3.lab.3.1,
                     padx=c(10, 2), sticky="w")

    tkgrid.configure(f3.lab.1.1, f3.lab.2.1, f3.lab.3.1, padx=c(0, 2))
    tkgrid.configure(f3.ent.1.3, f3.ent.2.3, f3.ent.3.3, padx=c(2, 0))
    tkgrid.configure(f3.chk.4.1, columnspan=5, sticky="w", pady=c(5, 0))

    tkpack(f3, fill="x", padx=10, pady=c(0, 10))

    tcl(f3.box.1.2, "current", 0)
    tcl(f3.box.1.5, "current", 0)
    tcl(f3.box.2.2, "current", 0)
    tcl(f3.box.2.5, "current", 0)
    tcl(f3.box.3.2, "current", 0)

    if (!is.null(Data(c("export", "sep")))) {
      if (Data(c("export", "sep")) %in% sep0) {
        tcl(f3.box.1.2, "current", match(Data(c("export", "sep")), sep0) - 1)
        tkconfigure(f3.ent.1.3, state="disabled")
      } else {
        tcl(f3.box.1.2, "current", match(NA, sep0) - 1)
        tkconfigure(f3.ent.1.3, state="normal")
        tclvalue(sep.var) <- Data(c("export", "sep"))
      }
    }
    if (!is.null(Data(c("export", "na")))) {
      if (Data(c("export", "na")) %in% nas0) {
        tcl(f3.box.2.2, "current", match(Data(c("export", "na")), nas0) - 1)
        tkconfigure(f3.ent.2.3, state="disabled")
      } else {
        tcl(f3.box.2.2, "current", match(NA, nas0) - 1)
        tkconfigure(f3.ent.2.3, state="normal")
        tclvalue(nas.var) <- Data(c("export", "na"))
      }
    }
    if (!is.null(Data(c("export", "com")))) {
      if (Data(c("export", "com")) %in% com0) {
        tcl(f3.box.3.2, "current", match(Data(c("export", "com")), com0) - 1)
        tkconfigure(f3.ent.3.3, state="disabled")
      } else {
        tcl(f3.box.3.2, "current", match(NA, com0) - 1)
        tkconfigure(f3.ent.3.3, state="normal")
        tclvalue(com.var) <- Data(c("export", "com"))
      }
    }

    if (!is.null(Data(c("export", "dec"))))
      tcl(f3.box.1.5, "current", match(Data(c("export", "dec")), dec0) - 1)
    if (!is.null(Data(c("export", "qmethod"))))
      tcl(f3.box.2.5, "current",
          match(Data(c("export", "qmethod")), qme0) - 1)
  }

  # frame 4, output file and compression
  f4 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5, text="Output file")
  f4.ent.1.1 <- ttkentry(f4, width=12, textvariable=file.var)
  f4.but.1.5 <- ttkbutton(f4, width=8, text="Browse", command=GetDataFile)

  f4.lab.2.1 <- ttklabel(f4, text="Encoding")
  f4.lab.3.1 <- ttklabel(f4, text="End-of-line")
  f4.lab.4.1 <- ttklabel(f4, text="Compression")
  f4.box.2.2 <- ttkcombobox(f4, width=17, state="readonly", value=enc1)
  f4.box.3.2 <- ttkcombobox(f4, width=17, state="readonly", value=eol1)
  f4.box.4.2 <- ttkcombobox(f4, width=17, state="readonly", value=zip1)
  txt <- "Export change log ( *.log )"
  f4.chk.2.3 <- ttkcheckbutton(f4, variable=changelog.var, text=txt)

  tkgrid(f4.ent.1.1, "x", "x", "x", f4.but.1.5)
  tkgrid.configure(f4.ent.1.1, sticky="we", columnspan=4, padx=c(0, 2))

  if (file.type == "txt") {
    tkgrid(f4.lab.2.1, f4.box.2.2, f4.chk.2.3, pady=c(4, 0), sticky="w")
    tkgrid(f4.lab.3.1, f4.box.3.2, "x", pady=c(4, 0), sticky="w")
    tkgrid(f4.lab.4.1, f4.box.4.2, "x", pady=c(4, 0), sticky="w")
    tkgrid.configure(f4.lab.2.1, f4.lab.3.1, f4.lab.4.1, padx=c(0, 2))
    tkgrid.configure(f4.chk.2.3, padx=c(20, 0), columnspan=2)

    tcl(f4.box.2.2, "current", 0)
    tcl(f4.box.3.2, "current", 0)
    tcl(f4.box.4.2, "current", 0)

    if (is.character(Data(c("export", "encoding"))))
      tcl(f4.box.2.2, "current", match(Data(c("export", "encoding")), enc0) - 1L)
    if (is.character(Data(c("export", "eol"))))
      tcl(f4.box.3.2, "current", match(Data(c("export", "eol")), eol0) - 1L)
    if (is.character(Data(c("export", "zip"))))
      tcl(f4.box.4.2, "current", match(Data(c("export", "zip")), zip0) - 1L)

    if (is.null(Data("changelog"))) {
      tclvalue(changelog.var) <- 0
      tkconfigure(f4.chk.2.3, state="disabled")
    }
  } else if (file.type == "rda") {
    f4.rbt.2.1 <- ttkradiobutton(f4, variable=ascii.var, value=0, text="Binary")
    f4.rbt.2.2 <- ttkradiobutton(f4, variable=ascii.var, value=1, text="ASCII")
    tkgrid(f4.rbt.2.1, f4.rbt.2.2, "x", "x")
    tkgrid.configure(f4.rbt.2.1, padx=c(0, 4))
  }

  tkgrid.columnconfigure(f4, 3, weight=1)
  tkpack(f4, fill="x", padx=10, pady=c(0, 15))

  # bind events
  tclServiceMode(TRUE)
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)
  tkbind(f1.lst.1.1, "<<ListboxSelect>>", ToggleExport)
  tkbind(f4.ent.1.1, "<KeyRelease>", ToggleExport)

  if (file.type == "txt") {
    tkbind(f3.box.1.2, "<<ComboboxSelected>>",
           function() {
             sep <- sep0[as.integer(tcl(f3.box.1.2, "current")) + 1]
             if (is.na(sep)) {
               tkconfigure(f3.ent.1.3, state="normal")
               tkfocus(f3.ent.1.3)
             } else {
               tkconfigure(f3.ent.1.3, state="disabled")
             }
           })
    tkbind(f3.box.2.2, "<<ComboboxSelected>>",
           function() {
             nas <- nas0[as.integer(tcl(f3.box.2.2, "current")) + 1]
             if (is.na(nas)) {
               tkconfigure(f3.ent.2.3, state="normal")
               tkfocus(f3.ent.2.3)
             } else {
               tkconfigure(f3.ent.2.3, state="disabled")
             }
           })
    tkbind(f3.box.3.2, "<<ComboboxSelected>>",
           function() {
             com <- com0[as.integer(tcl(f3.box.3.2, "current")) + 1]
             if (is.na(com)) {
               tkconfigure(f3.ent.3.3, state="normal")
               tkfocus(f3.ent.3.3)
             } else {
               tkconfigure(f3.ent.3.3, state="disabled")
             }
           })
    tkbind(f4.box.4.2, "<<ComboboxSelected>>", ToggleExtension)
  }

  # gui control
  ToggleExport()

  tkfocus(f1.lst.1.1)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)
}


.DefaultFormat <- function(x) {
  x <- format(x, trim=TRUE, na.encode=FALSE, scientific=FALSE, drop0trailing=TRUE)
  return(gsub("(^ +)|( +$)", "", x))
}
