ImportText <- function(parent=NULL) {


  # read table
  ReadTable <- function(con, headers=c(FALSE, FALSE), sep="\t", dec=".",
                        quote="\"'", nrows=-1, na.strings=c("", "NA"), skip=0L,
                        comment.char="#", str.as.fact=TRUE, encoding=getOption("encoding")) {

    # clear previous data
    Data(clear.data=TRUE)

    # track computational time
    elapsed.time <- system.time({

      # load comment
      comments <- NULL
      if (!is.na(comment.char) && comment.char != "" && isSeekable(con)) {
        pattern <- paste0("^", comment.char)
        while (TRUE) {
          read.line <- readLines(con, n=1)
          if (length(grep(pattern, read.line)) > 0) {
            line <- sub("^\\s+", "", sub(pattern, "", read.line))
            comments <- c(comments, line)
          } else if (length(read.line) == 0 || nchar(read.line) > 0) {
            break
          }
        }
        invisible(seek(con, where=0, origin="start", rw="read"))
      }

      # establish arguments to pass to read.table
      args <- list(file=con, header=FALSE, sep=sep, dec=dec, quote=quote,
                   row.names=NULL, na.strings=na.strings, check.names=TRUE,
                   fill=TRUE, strip.white=TRUE, blank.lines.skip=TRUE,
                   comment.char=comment.char, allowEscapes=TRUE, flush=TRUE,
                   fileEncoding="", encoding=encoding)

      # load headers
      col.classes <- "character"
      nheaders <- sum(headers)
      if (nheaders > 0L) {
        h.args <- c(args, skip=skip, nrows=nheaders, colClasses=col.classes)
        h <- try(do.call(read.table, h.args), silent=TRUE)
        if (inherits(h, "try-error")) return(h)

        i <- 1L
        if (headers[1]) {
          col.formats <- as.character(h[i, ])

          # use formats to determine column classes
          n <- ncol(h)
          col.classes <- rep("character", n)
          for (j in seq_len(n)) {
            fmt <- col.formats[j]
            test <- try(sprintf(fmt, 1), silent=TRUE)
            is.error <- inherits(test, "try-error")
            if (!is.error) {
              is.num <- !is.na(suppressWarnings(as.numeric(test)))
              if (is.num) {
                s <- paste0(substr(fmt, 1, 1), substr(fmt, nchar(fmt), nchar(fmt)))
                if (s %in% c("%d", "%i")) {
                  col.classes[j] <- "integer"
                } else if (s %in% c("%f", "%e", "%E")) {
                  col.classes[j] <- "numeric"
                }
              }
            }
          }
          col.classes[col.formats %in% "%Y-%m-%d %H:%M:%S"] <- "POSIXct"
          i <- i + 1L
        }
        if (headers[2]) {
          col.names <- as.character(h[i, ])
          col.names[is.na(col.names)] <- "Unknown"
        }

        skip <- 0L
        nrows <- nrows - nheaders
      }

      # load data
      d.args <- c(args, skip=skip, nrows=nrows, list(colClasses=col.classes))
      d <- try(do.call(read.table, d.args), silent=TRUE)
      if (inherits(d, "try-error")) return(d)

      # table dimensions
      m <- nrow(d)
      n <- ncol(d)

      # initialize missing headers
      if (!headers[1]) col.formats <- rep(NA, n)
      if (!headers[2]) col.names <- rep("Unknown", n)

      # determine unique column names
      ids <- col.names
      matched <- lapply(unique(ids), function(i) which(ids %in% i)[-1])
      names(matched) <- unique(ids)
      for (i in seq_along(matched))
        ids[matched[[i]]] <- paste0(names(matched[i]), " (", seq_along(matched[[i]]), ")")

      # initialize columns list
      cols <- list()

      # Establish column types
      for (j in seq_len(n)) {
        val <- d[, j]
        fmt <- if (is.na(col.formats[j])) NULL else col.formats[j]

        # determine if character variables are POSIXct class
        # TODO(jfisher): ensure variable is date-time
        if (inherits(val, "character")) {
          is.time <- FALSE
          if (!is.null(fmt) && fmt != "" && !all(is.na(val))) {
            sys.time.str <- format(Sys.time(), format=fmt)
            if (!sys.time.str %in% c("", gsub("%%", "%", fmt))) {
              posix.fmt <- gsub("%OS[[:digit:]]+", "%OS", fmt)
              date.time <- try(as.POSIXlt(val, format=posix.fmt), silent=TRUE)
              if (!inherits(date.time, "try-error") && !all(is.na(date.time))) {
                date.time.str <- inlmisc::POSIXct2Character(date.time, fmt)
                is.time <- TRUE
              }
            }
          }
          val <- if (is.time) as.POSIXct(date.time) else type.convert(val, as.is=!str.as.fact)
        }

        # organize metadata
        cols[[j]] <- list()
        cols[[j]]$id      <- ids[j]
        cols[[j]]$name    <- col.names[j]
        cols[[j]]$format  <- ifelse(is.null(fmt), "", fmt)
        cols[[j]]$class   <- class(val)
        cols[[j]]$index   <- j
        cols[[j]]$fun     <- paste0("\"", ids[j], "\"")
        cols[[j]]$sample  <- na.omit(val)[1]
        cols[[j]]$summary <- summary(val)
        d[, j] <- val
      }

      Data("comment", comments)
      Data("data.raw", as.list(d))
      Data("rows", as.character(seq_len(m)))
      Data("cols", cols)

      memory.usage <- gc()
    })

    ans <- paste("\nTime required to import data:",
                 format(elapsed.time["elapsed"]), "secs\n", "\n")
    return(ans)
  }


  # raise error message for bad connection
  RaiseError <- function(type, detail) {
    msg <- NULL
    msg[1] <- "Connection to data source failed."
    msg[2] <- "Problems occured while reading data from text file."
    tkmessageBox(icon="error", message=msg[type], detail=detail, title="Error", type="ok", parent=tt)
  }


  # establish data connection
  GetConnection <- function(src, enc, opn="r") {
    if (src == "") {
      con <- try(textConnection(cb, local=TRUE), silent=TRUE)
    } else if (substr(src, 1, 6) %in% c("http:/", "https:/", "ftp://", "file:/")) {
      con <- try(url(description=src, open=opn, encoding=enc), silent=TRUE)
    } else {
      ext <- attr(GetFile(file=src), "extension")
      if (ext %in% c("gz", "bz2", "xz"))
        con <- try(gzfile(description=src, open=opn, encoding=enc), silent=TRUE)
      else
        con <- try(file(description=src, open=opn, encoding=enc), silent=TRUE)
    }
    return(con)
  }

  # read data from file and populate example table
  ReadFile <- function(summary.only=TRUE) {
    tkconfigure(tt, cursor="watch")
    on.exit(tkconfigure(tt, cursor="arrow"))

    sep <- sep0[as.integer(tcl(f3.box.1.2, "current")) + 1]
    dec <- dec0[as.integer(tcl(f3.box.1.5, "current")) + 1]
    nas <- nas0[as.integer(tcl(f3.box.2.2, "current")) + 1]
    quo <- quo0[as.integer(tcl(f3.box.2.5, "current")) + 1]
    com <- com0[as.integer(tcl(f3.box.3.2, "current")) + 1]
    enc <- enc0[as.integer(tcl(f3.box.3.5, "current")) + 1]

    src <- as.character(tclvalue(source.var))
    con <- GetConnection(src, enc)
    on.exit(close(con), add=TRUE)

    if (inherits(con, "try-error") || !isOpen(con, "r")) {
      RaiseError(1L, con)
      return()
    }

    skp <- as.integer(tclvalue(skip.var))
    if (is.na(skp) || skp < 0) skp <- 0

    nrw <- as.integer(tclvalue(nrow.var))
    if (is.na(nrw)) nrw <- -1
    if (nrw > 0 && nrw < nrows) nrows <- nrw

    if (is.na(sep)) sep <- as.character(tclvalue(sep.var))
    if (is.na(nas)) {
      nas <- as.character(tclvalue(nas.var))
      if (nas == "") nas <- "NA"
    }
    if (is.na(com)) com <- as.character(tclvalue(com.var))

    if (summary.only) {
      d <- try(read.table(con, header=FALSE, sep=sep, quote=quo, dec=dec,
                          row.names=NULL, na.strings=c("", nas),
                          colClasses="character", nrows=nrows, skip=skp,
                          check.names=TRUE, fill=TRUE, strip.white=TRUE,
                          blank.lines.skip=TRUE, comment.char=com,
                          allowEscapes=TRUE, flush=TRUE), silent=TRUE)
      if (inherits(d, "try-error")) {
        RaiseError(2L, d)
        return()
      }

      # remove columns containing all missing values
      is.all.na <- vapply(seq_along(d), function(i) all(is.na(d[, i])), TRUE)
      d <- d[, !is.all.na, drop=FALSE]
      return(d)

    } else {

      # raise warning message if data already exists
      if (!is.null(Data("cols"))) {
        msg <- "This action will delete existing data?"
        ans <- tkmessageBox(icon="question", message=msg, title="Warning", type="okcancel", parent=parent)
        if (as.character(ans) == "ok")
          Data(clear.data=TRUE)
        else
          return()
      }

      is.fmts <- as.logical(as.integer(tclvalue(conv.fmts.var)))
      is.cols <- as.logical(as.integer(tclvalue(col.names.var)))
      is.fact <- as.logical(as.integer(tclvalue(str.as.fact.var)))
      headers <- c(is.fmts, is.cols)

      ans <- ReadTable(con, headers=headers, sep=sep, dec=dec, quote=quo,
                       nrows=nrw, na.strings=c("", nas), skip=skp,
                       comment.char=com, str.as.fact=is.fact)
      if (inherits(ans, "try-error")) {
        RaiseError(2L, ans)
        return()
      }

      if (!is.null(ans)) {
        Data("import", list())
        Data(c("import", "source"), c(pathname=src, accessed=format(Sys.time())))
        Data(c("import", "fmts"), is.fmts)
        Data(c("import", "cols"), is.cols)
        Data(c("import", "str.as.fact"), is.fact)
        Data(c("import", "skip"), skp)
        Data(c("import", "sep"), sep)
        Data(c("import", "dec"), dec)
        Data(c("import", "na"), nas)
        Data(c("import", "quote"), quo)
        Data(c("import", "comment"), com)
        Data(c("import", "encoding"), enc)
        tclvalue(tt.done.var) <- 1
      }
    }
  }

  # rebuild table
  RebuildTable <- function() {
    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE))

    sep <- sep0[as.integer(tcl(f3.box.1.2, "current")) + 1]
    sep.state <- if (is.na(sep)) "normal" else "disabled"
    tkconfigure(f3.ent.1.3, state=sep.state)

    nas <- nas0[as.integer(tcl(f3.box.2.2, "current")) + 1]
    nas.state <- if (is.na(nas)) "normal" else "disabled"
    tkconfigure(f3.ent.2.3, state=nas.state)

    com <- com0[as.integer(tcl(f3.box.3.2, "current")) + 1]
    com.state <- if (is.na(com)) "normal" else "disabled"
    tkconfigure(f3.ent.3.3, state=com.state)

    if (tclvalue(source.var) == "" && is.null(cb)) return()

    d <- ReadFile()
    if (is.null(d)) return()

    ResetGUI()

    insert.rows <- nrow(d) - 1 - GetEndRow()
    insert.cols <- ncol(d) - 1 - GetEndCol()

    tkconfigure(f4.tbl, state="normal")

    if (insert.rows > 0) tkinsert(f4.tbl, "rows", "end", insert.rows)
    if (insert.cols > 0) tkinsert(f4.tbl, "cols", "end", insert.cols)

    for (j in seq_len(ncol(d)))
      sapply(seq_len(nrow(d)), function(i) table.var[[i - 1, j - 1]] <- as.tclObj(d[i, j], drop=TRUE))

    for (i in seq_len(ncol(d))) {
      len <- max(nchar(gsub("\t", "    ", d[seq_len(nrows), i])), na.rm=TRUE)
      len <- len + 1L
      if (len < 10L) {
        len <- 10L
      } else if (len > 50L) {
        len <- 50L
      }
      tcl(f4.tbl, "width", i - 1L, len)
    }

    SetTags()
    tkconfigure(f4.tbl, state="disabled")
  }


  # count the number of lines in a file; derived with permission from R.utils::countLines (v1.26.2)
  CountLines <- function() {
    tkconfigure(tt, cursor="watch")
    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE))
    on.exit(tkconfigure(tt, cursor="arrow"), add=TRUE)
    src <- as.character(tclvalue(source.var))
    enc <- enc0[as.integer(tcl(f3.box.3.5, "current")) + 1]
    con <- GetConnection(src, enc, opn="rb")
    on.exit(close(con), add=TRUE)
    if (inherits(con, "try-error")) return()
    lf <- as.raw(0x0a)
    cr <- as.raw(0x0d)
    is.last.cr <- FALSE
    nbreaks <- 0L
    while(TRUE) {
      bfr <- readBin(con=con, what="raw", n=5e+07L)
      if (is.last.cr && bfr[1] == lf) bfr[1] <- as.raw(32)
      n <- length(bfr)
      if (n == 0) break
      idxs.cr <- which(bfr == cr)
      ncr <- length(idxs.cr)
      if (ncr > 0) {
        idxs.crlf <- idxs.cr[bfr[idxs.cr + 1L] == lf]
        bfr <- bfr[-idxs.crlf]
        n <- length(bfr)
        idxs.crlf <- NULL
        ncr <- length(which(bfr == cr))
      }
      nlf <- length(which(bfr == lf))
      nbreaks <- nbreaks + ncr + nlf
      is.last.cr <- bfr[n] == cr
    }
    tclvalue(nrow.var) <- nbreaks
  }


  # data file
  GetDataFile <- function() {
    exts <- c("csv", "tsv", "tab", "txt", "gz", "bz2", "xz")
    f <- GetFile(cmd="Open", exts=exts, win.title="Open Data File", parent=tt)
    tkfocus(tt)
    if (is.null(f)) return()
    tclvalue(source.var) <- f
    tclvalue(nrow.var) <- ""
    cb <<- NULL
    ext <- attr(f, "extension")
    if (ext %in% c("gz", "bz2", "xz")) {
      nam <- sub("[.][^.]*$", "", basename(f))
      ext <- tail(unlist(strsplit(nam, "\\."))[-1], 1)
      if (length(ext) == 0L) ext <- ""
    }
    if (ext == "csv") {
      tcl(f3.box.1.2, "current", match(",", sep0) - 1)
    } else if (ext %in% c("tsv", "tab")) {
      tcl(f3.box.1.2, "current", match("\t", sep0) - 1)
    }
    RebuildTable()
  }


  # paste clipboard
  PasteData <- function() {
    tkselection.set(f4.tbl, "origin")
    cb <- try(scan(file="clipboard", what="character", sep="\n", quiet=TRUE), silent=TRUE)
    cb <<- if (inherits(cb, "try-error")) NULL else cb
    if (is.null(cb)) return()
    tclvalue(source.var) <- ""
    RebuildTable()
  }


  # clear all
  ClearData <- function() {
    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE))
    cb <<- NULL
    tclvalue(source.var) <- ""
    tclvalue(nrow.var) <- ""
    ResetGUI()
  }


  # reset gui
  ResetGUI <- function() {
    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE))
    tkconfigure(f4.tbl, state="normal")
    tcl("unset", table.var)
    if (GetEndRow() > 0) tkdelete(f4.tbl, "rows", 1, GetEndRow())
    if (GetEndCol() > 0) tkdelete(f4.tbl, "cols", 1, GetEndCol())
    tcl(f4.tbl, "clear", "all")
    tkselection.set(f4.tbl, "origin")
    tkconfigure(f4.tbl, state="disabled")
    s <- if (tclvalue(source.var) == "" && is.null(cb)) "disabled" else "normal"
    tkconfigure(f0.but.4, state=s)
  }


  # set tags in table
  SetTags <- function() {
    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE))
    if (GetEndRow() == 0 & GetEndCol() == 0) return()
    tcl(f4.tbl, "clear", "tags")
    tcl(f4.tbl, "tag", "row", "h1", 0)
    tcl(f4.tbl, "tag", "row", "h2", 1)
    is.fmts <- as.logical(as.integer(tclvalue(conv.fmts.var)))
    is.cols <- as.logical(as.integer(tclvalue(col.names.var)))
    headCol <- c("#EBFFC6", "#FFD0D4")[c(is.fmts, is.cols)]
    if (length(headCol) < 2) headCol[(length(headCol) + 1):2] <- "white"
    tktag.configure(f4.tbl, "h1", background=headCol[1])
    tktag.configure(f4.tbl, "h2", background=headCol[2])
  }

  # determine the tables maximum row and column
  GetEndRow <- function() as.numeric(tkindex(f4.tbl, "end", "row"))
  GetEndCol <- function() as.numeric(tkindex(f4.tbl, "end", "col"))


  # gui requires TkTable
  if (inherits(try(tcl("package", "present", "Tktable"), silent=TRUE), "try-error"))
    return("TkTable is not available")

  # initialize values
  cb <- NULL

  nrows <- 50

  sep0 <- c("", "\t", ",", ";", "|", NA)
  sep1 <- c("White space (  )", "Tab ( \\t )", "Comma ( , )",
            "Semicolon ( ; )", "Pipe ( | )", "Custom\u2026")

  nas0 <- c("NA", "na", "N/A", "n/a", NA)
  nas1 <- c("NA", "na", "N/A", "n/a", "Custom\u2026")

  quo0 <- c("\"'", "\"", "'", "")
  quo1 <- c("Double Single ( \" \' )", "Double ( \" )", "Single ( \' )", "None")

  dec0 <- c(".", ",")
  dec1 <- c("Period ( . )", "Comma ( , )")

  com0 <- c("#", "!", "\\", "~", "", NA)
  com1 <- c("Number sign ( # )", "Exclamation ( ! )", "Backslash ( \\\\ )",
            "Tilde ( ~ )", "None", "Custom\u2026")

  enc0 <- c("native.enc", iconvlist())
  enc1 <- c("Default", iconvlist())

  # assign variables linked to Tk widgets
  table.var   <- tclArray()
  nrow.var    <- tclVar()
  source.var  <- tclVar()
  sep.var     <- tclVar()
  nas.var     <- tclVar()
  com.var     <- tclVar()
  tt.done.var <- tclVar(0)

  if (is.null(Data(c("import", "fmts"))))
    conv.fmts.var <- tclVar(FALSE)
  else
    conv.fmts.var <- tclVar(Data(c("import", "fmts")))
  if (is.null(Data(c("import", "cols"))))
    col.names.var <- tclVar(FALSE)
  else
    col.names.var <- tclVar(Data(c("import", "cols")))
  if (is.null(Data(c("import", "skip"))))
    skip.var <- tclVar(FALSE)
  else
    skip.var <- tclVar(Data(c("import", "skip")))
  if (is.null(Data(c("import", "str.as.fact"))))
    str.as.fact.var <- tclVar(FALSE)
  else
    str.as.fact.var <- tclVar(Data(c("import", "str.as.fact")))

  # open gui
  tclServiceMode(FALSE)
  tt <- tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    geo <- as.integer(geo[2:3]) + 25
    tkwm.geometry(tt, sprintf("+%s+%s", geo[1], geo[2]))
  }
  tktitle(tt) <- "Import Data from Text File or Clipboard"

  # frame 0 contains load and cancel buttons, and size grip
  f0 <- ttkframe(tt, relief="flat")

  f0.but.1 <- ttkbutton(f0, width=8, text="Paste", command=PasteData)
  f0.but.2 <- ttkbutton(f0, width=8, text="Clear", command=ClearData)
  f0.but.4 <- ttkbutton(f0, width=12, text="Import",
                        command=function() ReadFile(FALSE))
  f0.but.5 <- ttkbutton(f0, width=12, text="Cancel",
                        command=function() tclvalue(tt.done.var) <- 1)
  f0.but.6 <- ttkbutton(f0, width=12, text="Help",
                        command=function() {
                          print(help("ImportText", package="RSurvey"))
                        })
  f0.grp.7 <- ttksizegrip(f0)

  tkgrid(f0.but.1, f0.but.2, "x", f0.but.4, f0.but.5, f0.but.6, f0.grp.7)

  tkgrid.columnconfigure(f0, 2, weight=1)

  tkgrid.configure(f0.but.1, f0.but.2, sticky="n", padx=c(0, 4), pady=c(4, 0))
  tkgrid.configure(f0.but.1, padx=c(10, 4))
  tkgrid.configure(f0.but.4, f0.but.5, f0.but.6, padx=c(0, 4), pady=c(15, 10))
  tkgrid.configure(f0.but.6, columnspan=2, padx=c(0, 10))
  tkgrid.configure(f0.grp.7, sticky="se")

  tkraise(f0.but.6, f0.grp.7)

  tkpack(f0, fill="x", side="bottom", anchor="e")

  tkconfigure(f0.but.4, state="disabled")

  # frame 1, file locator
  f1 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0)

  f1.lab.1.1 <- ttklabel(f1, text="Import from")
  txt <- paste("or transfer data from clipboard via a copy and paste operation. ",
               "The first part of the data table will be shown below.")
  f1.lab.2.1 <- ttklabel(f1, text=txt)

  f1.ent.1.2 <- ttkentry(f1, textvariable=source.var)
  f1.but.1.3 <- ttkbutton(f1, width=8, text="Browse", command=GetDataFile)

  tkgrid(f1.lab.1.1, f1.ent.1.2, f1.but.1.3, pady=c(10, 0))
  tkgrid(f1.lab.2.1, "x", "x", "x", pady=c(5, 0), padx=c(15, 0))

  tkgrid.configure(f1.lab.1.1, sticky="w")
  tkgrid.configure(f1.ent.1.2, sticky="we", padx=2)

  tkgrid.configure(f1.lab.2.1, columnspan=3, sticky="w")

  tkgrid.columnconfigure(f1, 1, weight=1)

  tkpack(f1, fill="x", anchor="w", padx=10)

  # frame 2, header line information
  f2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5, text="Header lines")

  txt <- paste("Format conversion specification strings of the variables,",
               "for example, '%10.6f' and '%Y-%m-%d %H:%M'.")
  f2.chk.1.1 <- ttkcheckbutton(f2, variable=conv.fmts.var, command=SetTags, text=txt)
  txt <- "Field names of the variables, that is, names given to the columns in the data table."
  f2.chk.2.1 <- ttkcheckbutton(f2, variable=col.names.var, command=SetTags, text=txt)

  tkgrid(f2.chk.1.1, pady=1, sticky="w")
  tkgrid(f2.chk.2.1, pady=1, sticky="w")

  tkpack(f2, anchor="w", fill="x", padx=10, pady=10)

  # frame 3, import parameters
  f3 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5, text="Options")

  f3.lab.1.1 <- ttklabel(f3, text="Separator")
  f3.lab.1.4 <- ttklabel(f3, text="Decimal")
  f3.lab.1.6 <- ttklabel(f3, text="Max lines")
  f3.lab.2.1 <- ttklabel(f3, text="NA string")
  f3.lab.2.4 <- ttklabel(f3, text="Quote")
  f3.lab.2.6 <- ttklabel(f3, text="Skip lines")
  f3.lab.3.1 <- ttklabel(f3, text="Comment")
  f3.lab.3.4 <- ttklabel(f3, text="Encoding")
  txt <- paste("Comments located above data records and header lines",
               "will be preserved (files only); all other comments are ignored.")
  f3.lab.4.1 <- ttklabel(f3, text=txt, foreground="#A40802")

  f3.box.1.2 <- ttkcombobox(f3, width=17, state="readonly", value=sep1)
  f3.box.1.5 <- ttkcombobox(f3, width=17, state="readonly", value=dec1)
  f3.box.2.2 <- ttkcombobox(f3, width=17, state="readonly", value=nas1)
  f3.box.2.5 <- ttkcombobox(f3, width=17, state="readonly", value=quo1)
  f3.box.3.2 <- ttkcombobox(f3, width=17, state="readonly", value=com1)
  f3.box.3.5 <- ttkcombobox(f3, width=17, state="readonly", value=enc1)

  f3.ent.1.3 <- ttkentry(f3, width=12, textvariable=sep.var)
  f3.ent.2.7 <- ttkentry(f3, width=12, textvariable=skip.var)
  f3.ent.2.3 <- ttkentry(f3, width=12, textvariable=nas.var)
  f3.ent.1.7 <- ttkentry(f3, width=12, textvariable=nrow.var)
  f3.ent.3.3 <- ttkentry(f3, width=12, textvariable=com.var)

  f3.but.1.8 <- ttkbutton(f3, width=2, image=GetBitmapImage("find"), command=CountLines)

  f3.chk.3.6 <- ttkcheckbutton(f3, variable=str.as.fact.var,
                               text="Convert strings to factors")

  tkgrid(f3.lab.1.1, f3.box.1.2, f3.ent.1.3, f3.lab.1.4,
         f3.box.1.5, f3.lab.1.6, f3.ent.1.7, f3.but.1.8)
  tkgrid(f3.lab.2.1, f3.box.2.2, f3.ent.2.3, f3.lab.2.4,
         f3.box.2.5, f3.lab.2.6, f3.ent.2.7, "x", pady=c(4, 0))
  tkgrid(f3.lab.3.1, f3.box.3.2, f3.ent.3.3, f3.lab.3.4,
         f3.box.3.5, f3.chk.3.6, "x", "x", pady=c(4, 0))
  tkgrid(f3.lab.4.1, "x", "x", "x", "x", "x", "x", "x", pady=c(5, 0))

  tkgrid.configure(f3.lab.1.1, f3.lab.1.4, f3.lab.1.6,
                   f3.lab.2.1, f3.lab.2.4, f3.lab.2.6,
                   f3.lab.3.1, f3.lab.3.4, padx=c(10, 2), sticky="w")

  tkgrid.configure(f3.lab.1.1, f3.lab.2.1, f3.lab.3.1, padx=c(0, 2))
  tkgrid.configure(f3.ent.1.3, f3.ent.2.3, f3.ent.3.3, padx=c(2, 0))
  tkgrid.configure(f3.but.1.8, padx=c(2, 0))
  tkgrid.configure(f3.chk.3.6, padx=c(10, 0), columnspan=3, sticky="w")
  tkgrid.configure(f3.lab.4.1, columnspan=8, sticky="w")

  tkpack(f3, anchor="w", fill="x", padx=10, pady=c(0, 15))

  tcl(f3.box.1.2, "current", 0)
  tcl(f3.box.1.5, "current", 0)
  tcl(f3.box.2.2, "current", 0)
  tcl(f3.box.2.5, "current", 0)
  tcl(f3.box.3.2, "current", 0)
  tcl(f3.box.3.5, "current", 0)

  if (!is.null(Data(c("import", "sep")))) {
    if (Data(c("import", "sep")) %in% sep0) {
      tcl(f3.box.1.2, "current", match(Data(c("import", "sep")), sep0) - 1)
      tkconfigure(f3.ent.1.3, state="disabled")
    } else {
      tcl(f3.box.1.2, "current", match(NA, sep0) - 1)
      tkconfigure(f3.ent.1.3, state="normal")
      tclvalue(sep.var) <- Data(c("import", "sep"))
    }
  }
  if (!is.null(Data(c("import", "na")))) {
    if (Data(c("import", "na")) %in% nas0) {
      tcl(f3.box.2.2, "current", match(Data(c("import", "na")), nas0) - 1)
      tkconfigure(f3.ent.2.3, state="disabled")
    } else {
      tcl(f3.box.2.2, "current", match(NA, nas0) - 1)
      tkconfigure(f3.ent.2.3, state="normal")
      tclvalue(nas.var) <- Data(c("import", "na"))
    }
  }
  if (!is.null(Data(c("import", "comment")))) {
    if (Data(c("import", "comment")) %in% com0) {
      tcl(f3.box.3.2, "current", match(Data(c("import", "comment")), com0) - 1)
      tkconfigure(f3.ent.3.3, state="disabled")
    } else {
      tcl(f3.box.3.2, "current", match(NA, com0) - 1)
      tkconfigure(f3.ent.3.3, state="normal")
      tclvalue(com.var) <- Data(c("import", "comment"))
    }
  }
  if (!is.null(Data(c("import", "dec"))))
    tcl(f3.box.1.5, "current", match(Data(c("import", "dec")), dec0) - 1)
  if (!is.null(Data(c("import", "quote"))))
    tcl(f3.box.2.5, "current", match(Data(c("import", "quote")), quo0) - 1)
  if (!is.null(Data(c("import", "encoding"))))
    tcl(f3.box.3.5, "current", match(Data(c("import", "encoding")), enc0) - 1)

  # frame 4, example data table
  f4 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0)

  f4.tbl <- tkwidget(f4, "table", rows=1, cols=1, variable=table.var,
                     state="disabled", colwidth=13, rowheight=1, width=1,
                     height=5, ipadx=3, ipady=1, wrap=0,
                     highlightcolor="gray75", background="white",
                     foreground="black", titlerows=0, titlecols=0,
                     multiline=0, resizeborders="col",
                     bordercursor="sb_h_double_arrow", cursor="plus",
                     colstretchmode="none", rowstretchmode="none",
                     anchor="nw", drawmode="single", rowseparator="\n",
                     colseparator="\t", selectmode="extended",
                     insertofftime=0, highlightthickness=0,
                     font="TkFixedFont",
                     xscrollcommand=function(...) tkset(f4.xsc, ...),
                     yscrollcommand=function(...) tkset(f4.ysc, ...))

  f4.xsc <- ttkscrollbar(f4, orient="horizontal",
                         command=function(...) tkxview(f4.tbl, ...))
  f4.ysc <- ttkscrollbar(f4, orient="vertical",
                         command=function(...) tkyview(f4.tbl, ...))

  tkgrid(f4.tbl, f4.ysc)
  tkgrid(f4.xsc, "x")

  tkgrid.configure(f4.tbl, sticky="news", padx=c(10, 0))
  tkgrid.configure(f4.ysc, sticky="ns", padx=c(0, 10))
  tkgrid.configure(f4.xsc, sticky="we", padx=c(10, 0))

  tktag.configure(f4.tbl, "active", background="#EAEEFE", relief="")
  tktag.configure(f4.tbl, "sel", background="#EAEEFE", foreground="black")

  tkgrid.columnconfigure(f4, 0, weight=1)
  tkgrid.rowconfigure(f4, 0, weight=1)

  tkpack(f4, fill="both", expand=TRUE)

  tkselection.set(f4.tbl, "origin")

  # bind events
  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tkbind(f1.ent.1.2, "<Return>", RebuildTable)

  tkbind(f3.box.1.2, "<<ComboboxSelected>>",
         function() {
           RebuildTable()
           if (is.na(sep0[as.integer(tcl(f3.box.1.2, "current")) + 1])) tkfocus(f3.ent.1.3)
         })
  tkbind(f3.box.2.2, "<<ComboboxSelected>>",
         function() {
           RebuildTable()
           if (is.na(nas0[as.integer(tcl(f3.box.2.2, "current")) + 1])) tkfocus(f3.ent.2.3)
         })
  tkbind(f3.box.3.2, "<<ComboboxSelected>>",
         function() {
           RebuildTable()
           if (is.na(com0[as.integer(tcl(f3.box.3.2, "current")) + 1])) tkfocus(f3.ent.3.3)
         })
  tkbind(f3.box.1.5, "<<ComboboxSelected>>", RebuildTable)
  tkbind(f3.box.2.5, "<<ComboboxSelected>>", RebuildTable)
  tkbind(f3.box.3.5, "<<ComboboxSelected>>", RebuildTable)

  tkbind(f3.ent.1.3, "<KeyRelease>", RebuildTable)
  tkbind(f3.ent.2.3, "<KeyRelease>", RebuildTable)
  tkbind(f3.ent.3.3, "<KeyRelease>", RebuildTable)
  tkbind(f3.ent.2.7, "<KeyRelease>",
         function() {
           tclvalue(skip.var) <- CheckEntry("integer", tclvalue(skip.var))
           RebuildTable()
         }
  )
  tkbind(f3.ent.2.7, "<KeyRelease>",
         function() {
           tclvalue(nrow.var) <- CheckEntry("integer", tclvalue(nrow.var))
           RebuildTable()
         }
  )

  tkbind(f4.tbl, "<<Paste>>", PasteData)

  D <- ""  # force 'D' to be something other than a function
  tkbind(f4.tbl, "<MouseWheel>",
         function(D) {
           number <- as.integer((-as.integer(D) / 120)^3)
           tkyview(f4.tbl, "scroll", number, "units")
         })

  # gui control
  RebuildTable()

  tkfocus(tt)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  invisible()
}
