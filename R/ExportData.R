# Export data to file.

ExportData <- function(file.type="txt", parent=NULL) {

  ## Additional functions (subroutines)

  # Final export of data to file

  ExportToFile <- function() {
    idxs <- as.integer(tkcurselection(frame1.lst.1.1)) + 1L
    col.ids <- col.ids[idxs]
    is.proc <- as.logical(as.integer(tclvalue(processed.var)))
    file.name <- as.character(tclvalue(file.var))

    # Organize data
    vars <- Data("vars")
    cols <- Data("cols")

    all.col.ids <- vapply(1:length(cols), function(i) cols[[i]]$id, "")
    if (file.type == "shp") {
      id.x <- all.col.ids[vars$x]
      id.y <- all.col.ids[vars$y]
      if (!id.x %in% col.ids)
        col.ids <- c(col.ids, id.x)
      if (!id.y %in% col.ids)
        col.ids <- c(col.ids, id.y)
    }
    col.idxs <- which(all.col.ids %in% col.ids)
    col.ids  <- vapply(col.idxs, function(i) cols[[i]]$id, "")
    col.funs <- vapply(col.idxs, function(i) cols[[i]]$fun, "")
    col.nams <- vapply(col.idxs, function(i) cols[[i]]$name, "")
    col.fmts <- vapply(col.idxs, function(i) cols[[i]]$format, "")

    # Identify data set and records

    if (is.proc) {
      row.nams <- row.names(Data("data.pts"))
      row.idxs <- which(row.nams %in% row.names(Data("data.raw")))
      if (length(row.nams) != length(row.idxs))
        stop("length of row names different from length of row indexes")
    } else {
      row.nams <- row.names(Data("data.raw"))
      row.idxs <- 1:length(row.nams)
    }
    n <- length(col.idxs)
    m <- length(row.idxs)
    d <- as.data.frame(matrix(NA, nrow=m, ncol=n))

    for (i in 1:n) {
      obj <- EvalFunction(col.funs[i], cols)[row.idxs]

      # Format variables
      if (file.type == "txt") {
        fmt <- col.fmts[i]
        if (fmt == "") {
          obj <- format(obj, na.encode=FALSE)
        } else {
          if (inherits(obj, "POSIXt")) {
            obj <- format(obj, format=fmt, na.encode=FALSE)
          } else {
            ans <- try(sprintf(fmt, obj), silent=TRUE)
            if (inherits(ans, "try-error")) {
              obj <- format(obj, na.encode=FALSE)
            } else {
              obj <- ans
              obj[obj %in% c("NA", "NaN")] <- NA
            }
          }
        }
      }
      d[, i] <- obj
    }

    row.names(d) <- row.nams

    # Write text file

    if (file.type == "txt") {
      is.fmts <- as.logical(as.integer(tclvalue(conv.fmts.var)))
      is.cols <- as.logical(as.integer(tclvalue(col.names.var)))
      is.rows <- as.logical(as.integer(tclvalue(row.names.var)))
      is.quot <- as.logical(as.integer(tclvalue(quote.var)))
      is.comm <- as.logical(as.integer(tclvalue(comment.var)))
      is.gzip <- as.logical(as.integer(tclvalue(compress.var)))
      is.clog <- as.logical(as.integer(tclvalue(changelog.var)))

      sep <- sep0[as.integer(tcl(frame3.box.1.2, "current")) + 1]
      dec <- dec0[as.integer(tcl(frame3.box.1.5, "current")) + 1]
      nas <- nas0[as.integer(tcl(frame3.box.2.2, "current")) + 1]
      qme <- qme0[as.integer(tcl(frame3.box.2.5, "current")) + 1]
      com <- com0[as.integer(tcl(frame3.box.3.2, "current")) + 1]

      enc <- enc0[as.integer(tcl(frame4.box.2.2, "current")) + 1]
      eol <- eol0[as.integer(tcl(frame4.box.3.2, "current")) + 1]

      if (is.na(sep))
        sep <- as.character(tclvalue(sep.var))
      if (is.na(nas)) {
        nas <- as.character(tclvalue(nas.var))
        if (nas == "")
          nas <- "NA"
      }
      if (is.na(com))
        com <- as.character(tclvalue(com.var))

      # Write changelog
      if (is.clog) {
        dir.name <- dirname(file.name)
        base.name <- sub(".gz$", "", basename(file.name))
        base.name <- unlist(strsplit(base.name, "\\."))
        if (length(base.name) > 1)
          base.name <- base.name[-length(base.name)]
        base.name <- paste0(base.name, ".log")
        f.log <- file.path(dir.name, base.name)
        if (file.access(f.log, mode=0) == 0) {
          msg <- paste0("\'", base.name, "\' already exists.\n\n",
                        "Do you want to replace it?")
          ans <- tkmessageBox(icon="info", message=msg, title="Confirm Save As",
                              type="yesno", parent=tt)
          if (as.character(ans) == "no")
            return()
        }
        write.table(Data("changelog"), file=f.log, quote=is.quot, sep=sep,
                    eol=eol, na=nas, dec=dec, row.names=FALSE, col.names=TRUE,
                    qmethod=qme, fileEncoding=enc)
      }

      # Set data file connection
      if (is.gzip)
        con <- gzfile(description=file.name, open="w", encoding=enc)
      else
        con <- file(description=file.name, open="w", encoding=enc)
      if (!inherits(con, "connection"))
        stop("Connection error")
      on.exit(close(con))

      # Write comment
      if (is.comm) {
        txt <- Data("comment")
        if (!is.null(txt) && length(txt) > 0 && is.character(txt))
          writeLines(paste(com, txt), con=con, sep=eol)
      }

      # Write headers
      headers <- c(is.fmts, is.cols)
      if (any(headers)) {

        if (is.rows) {
          h <- as.data.frame(matrix(NA, nrow=sum(headers), ncol=ncol(d) + 1L))
          col.fmts <- c("", col.fmts)
          col.nams <- c("", col.nams)
        } else {
          h <- as.data.frame(matrix(NA, nrow=sum(headers), ncol=ncol(d)))
        }

        i <- 1L
        if (headers[1]) {
          h[i, ] <- col.fmts
          i <- i + 1L
        }
        if (headers[2])
          h[i, ] <- col.nams
        write.table(h, file=con, append=is.comm, quote=is.quot, sep=sep,
                    eol=eol, na=nas, dec=dec, row.names=FALSE, col.names=FALSE,
                    qmethod=qme, fileEncoding=enc)
      }

      # Write table
      write.table(d, file=con, append=(is.comm | any(headers)), quote=is.quot,
                  sep=sep, eol=eol, na=nas, dec=dec, row.names=is.rows,
                  col.names=FALSE, qmethod=qme, fileEncoding=enc)

      # Update default values for GUI
      if (file.access(file.name, mode=0) == 0) {
        Data("export.fmts", is.fmts)
        Data("export.cols", is.cols)
        Data("export.rows", is.rows)
        Data("export.comment", is.comm)
        Data("export.sep", sep)
        Data("export.dec", dec)
        Data("export.na", nas)
        Data("export.com", com)
        Data("export.qmethod", qme)
        Data("export.quote", is.quot)
        Data("export.encoding", enc)
        Data("export.eol", eol)
        Data("export.compressed", is.gzip)
        Data("export.changelog", is.clog)
      }

    # Write shapefile
    } else if (file.type == "shp") {
      # Names are finicky for shapefiles, rules are convoluted,
      # that is, 8-bit names and no periods
      new.col.ids <- gsub("\\.", "", make.names(substr(col.ids, 1, 7),
                          unique=TRUE))
      colnames(d) <- new.col.ids
      idx.x <- which(col.ids %in% id.x)
      idx.y <- which(col.ids %in% id.y)
      coordinates(d) <- new.col.ids[c(idx.x, idx.y)]
      dsn <- dirname(file.name)
      layer <- basename(file.name)
      ext <- tolower(tail(unlist(strsplit(layer, "\\."))[-1], 1))
      if (length(ext) != 0)
        layer <- sub(paste0(".", ext, "$"), "", layer)
      rgdal::writeOGR(obj=d, dsn=dsn, layer=layer, driver="ESRI Shapefile",
                      verbose=TRUE, overwrite_layer=TRUE)
      Data("export.processed", is.proc)

    # Write R data file
    } else if (file.type == "rda") {
      ascii <- as.logical(as.integer(tclvalue(ascii.var)))
      names(d) <- make.names(names=col.ids, unique=TRUE)
      comment(d) <- Data("comment")
      save(d, file=file.name, ascii=ascii)
    }
    Data("export.processed", is.proc)
    tclvalue(tt.done.var) <- 1
  }

  # Select all or none from variable list

  SelectVariables <- function(sel) {
    n <- length(col.ids) - 1L
    if (sel == "all") {
      tkselection.set(frame1.lst.1.1, 0, n)
    } else if (sel == "none") {
      tkselection.clear(frame1.lst.1.1, 0, n)
    } else {
      idxs <- 0:n
      sel <- as.integer(tkcurselection(frame1.lst.1.1))
      tkselection.clear(frame1.lst.1.1, 0, n)
      for (i in idxs[!(idxs %in% sel)])
        tkselection.set(frame1.lst.1.1, i)
    }
    ToggleExport()
  }

  # Get file
  GetDataFile <- function() {
    if (file.type == "txt") {
      default.ext <- "txt"
      exts <- c("txt", "csv", "tab")
      is.gzip <- as.logical(as.integer(tclvalue(compress.var)))
      if (is.gzip) {
        default.ext <- "gz"
        exts <- "gz"
      }
    } else if (file.type == "shp") {
      default.ext <- "shp"
      exts <- "shp"
    } else if (file.type == "rda") {
      default.ext <- "rda"
      exts <- "rda"
    }
    f <- GetFile(cmd="Save As", exts=exts, file=NULL, win.title="Save Data As",
                 defaultextension=default.ext)
    if (is.null(f))
      return()
    if (attr(f, "extension") == "csv")
      tclvalue(sep.var) <- ","
    tclvalue(file.var) <- f
    ToggleExport()
  }

  # Toggle gz extension on file entry
  ToggleExtension <- function() {
    f <- as.character(tclvalue(file.var))
    n <- nchar(f)
    if (nchar(f) < 3L)
      return()
    is.gz <- substr(f, n - 2L, n) == ".gz"
    is.gzip <- as.logical(as.integer(tclvalue(compress.var)))
    f.new <- f
    if (is.gzip & !is.gz)
      f.new <- paste0(f, ".gz")
    if (!is.gzip & is.gz)
      f.new <- substr(f, 1L, n - 3L)
    if (!identical(f, f.new))
      tclvalue(file.var) <- f.new
  }

  # Toggle state of export button
  ToggleExport <- function() {
    idxs <- as.integer(tkcurselection(frame1.lst.1.1))
    f <- as.character(tclvalue(file.var))
    s <- if (length(idxs) == 0L || nchar(f) == 0L) "disabled" else "normal"
    tkconfigure(frame0.but.2, state=s)
  }

  ## Main program

  # Check for required information
  col.ids <- vapply(Data("cols"), function(i) i$id, "")
  if (length(col.ids) == 0)
    return()
  if (!file.type %in% c("txt", "shp", "rda"))
    stop()
  if (file.type == "shp") {
    is.pkg <- "rgdal" %in% .packages(all.available=TRUE) && require(rgdal)
    if (!is.pkg)
      stop("package rgdal required for shapefile support")
    if (is.null(Data(c("vars", "x"))) | is.null(Data(c("vars", "y"))))
      stop("shapefiles require x,y coordinate values")
  }

  # Initialize values

  sep0 <- c("\t", "", ",", ";", "|", NA)
  sep1 <- c("Tab ( \\t )", "White space (  )", "Comma ( , )",
            "Semicolon ( ; )", "Pipe ( | )", "Custom\u2026")

  nas0 <- c("NA", "na", "N/A", "n/a", NA)
  nas1 <- c("NA", "na", "N/A", "n/a", "Custom\u2026")

  dec0 <- c(".", ",")
  dec1 <- c("Period ( . )", "Comma ( , )")

  qme0 <- c("escape", "double")
  qme1 <- c("Escape quote", "Double quote")

  com0 <- c("#", "!", "\\", "~", NA)
  com1 <- c("Number sign ( # )", "Exclamation ( ! )",
            "Backslash ( \\\\ )", "Tilde ( ~ )", "Custom\u2026")

  enc0 <- c("native.enc", iconvlist())
  enc1 <- c("Default", iconvlist())

  eol0 <- c("\n", "\r", "\r\n")
  eol1 <- c("LF ( \\n )", "CR ( \\r )", "CR+LF ( \\r\\n )")

  # Assign variables linked to Tk widgets

  variables.var <- tclVar()
  processed.var <- tclVar(0)
  conv.fmts.var <- tclVar(0)
  col.names.var <- tclVar(0)
  row.names.var <- tclVar(0)
  comment.var   <- tclVar(0)
  sep.var       <- tclVar()
  nas.var       <- tclVar()
  com.var       <- tclVar()
  quote.var     <- tclVar(0)
  file.var      <- tclVar()
  compress.var  <- tclVar(0)
  ascii.var     <- tclVar(0)
  changelog.var <- tclVar(0)
  tt.done.var   <- tclVar(0)

  for (i in seq(along=col.ids))
    tcl("lappend", variables.var, col.ids[i])

  # Open GUI

  tclServiceMode(FALSE)
  tt <- tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste0("+", as.integer(geo[2]) + 25,
                             "+", as.integer(geo[3]) + 25))
  }

  tktitle(tt) <- "Export Data"

  # Frame 0, export and cancel buttons

  frame0 <- ttkframe(tt, relief="flat")
  frame0.but.2 <- ttkbutton(frame0, width=12, text="Export",
                            command=ExportToFile)
  frame0.but.4 <- ttkbutton(frame0, width=12, text="Help",
                            command=function() {
                              print(help("ExportData", package="RSurvey"))
                            })
  frame0.but.3 <- ttkbutton(frame0, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)
  frame0.grp.5 <- ttksizegrip(frame0)

  tkgrid("x", frame0.but.2, frame0.but.3, frame0.but.4, frame0.grp.5)
  tkgrid.columnconfigure(frame0, 0, weight=1)
  tkgrid.configure(frame0.but.2, frame0.but.3, frame0.but.4,
                   padx=c(0, 4), pady=c(4, 10))
  tkgrid.configure(frame0.but.4, columnspan=2, padx=c(0, 10))
  tkgrid.configure(frame0.grp.5, sticky="se")

  tkraise(frame0.but.4, frame0.grp.5)

  tkpack(frame0, fill="x", side="bottom", anchor="e")

  # Frame 1, sample entry

  txt <- "Select variables and data records"
  frame1 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5, text=txt)

  frame1.lst.1.1 <- tklistbox(frame1, selectmode="extended", activestyle="none",
                              relief="flat", borderwidth=5, width=10, height=4,
                              exportselection=FALSE, listvariable=variables.var,
                              highlightthickness=0)
  frame1.ysc.1.6 <- ttkscrollbar(frame1, orient="vertical")
  tkconfigure(frame1.lst.1.1, background="white",
              yscrollcommand=paste(.Tk.ID(frame1.ysc.1.6), "set"))
  tkconfigure(frame1.ysc.1.6, command=paste(.Tk.ID(frame1.lst.1.1), "yview"))

  frame1.but.2.1 <- ttkbutton(frame1, width=8, text="All",
                              command=function() SelectVariables("all"))
  frame1.but.2.2 <- ttkbutton(frame1, width=8, text="None",
                              command=function() SelectVariables("none"))
  frame1.but.2.3 <- ttkbutton(frame1, width=8, text="Inverse",
                              command=function() SelectVariables("inverse"))
  frame1.chk.2.4 <- ttkcheckbutton(frame1, variable=processed.var,
                                   text="Only export processed records")

  tkgrid(frame1.lst.1.1, "x", "x", "x", "x", frame1.ysc.1.6)
  tkgrid(frame1.but.2.1, frame1.but.2.2, frame1.but.2.3, frame1.chk.2.4,
         "x", "x", pady=c(4, 0))

  tkgrid.configure(frame1.lst.1.1, sticky="nsew", columnspan=5)
  tkgrid.configure(frame1.ysc.1.6, sticky="ns")
  tkgrid.configure(frame1.but.2.1, frame1.but.2.2, padx=c(0, 4))
  tkgrid.configure(frame1.chk.2.4, padx=c(10, 0))

  tkgrid.columnconfigure(frame1, 4, weight=1, minsize=0)
  tkgrid.rowconfigure(frame1, 0, weight=1)

  tkpack(frame1, fill="both", expand=TRUE, side="top", padx=10, pady=10)

  if (!is.null(Data("export.processed")))
    tclvalue(processed.var) <- Data("export.processed")
  if (is.null(Data("data.pts"))) {
    tclvalue(processed.var) <- 0
    tkconfigure(frame1.chk.2.4, state="disabled")
  }

  if (file.type == "txt") {

    # Frame 2, meta data

    frame2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                            text="Add metadata")

    frame2.chk.1.1 <- ttkcheckbutton(frame2, variable=comment.var,
                                     text="Comment")
    frame2.chk.1.2 <- ttkcheckbutton(frame2, variable=conv.fmts.var,
                                     text="Conversion specification formats")
    frame2.chk.2.1 <- ttkcheckbutton(frame2, variable=row.names.var,
                                     text="Record numbers")
    frame2.chk.2.2 <- ttkcheckbutton(frame2, variable=col.names.var,
                                     text="Variable (column) names")

    tkgrid(frame2.chk.1.1, frame2.chk.1.2, sticky="w")
    tkgrid(frame2.chk.2.1, frame2.chk.2.2, sticky="w")
    tkgrid.configure(frame2.chk.1.2, frame2.chk.2.2, padx=c(40, 0))

    tkpack(frame2, fill="x", padx=10, pady=c(0, 10))

    if (!is.null(Data("export.fmts")))
      tclvalue(conv.fmts.var) <- Data("export.fmts")
    if (!is.null(Data("export.cols")))
      tclvalue(col.names.var) <- Data("export.cols")
    if (!is.null(Data("export.rows")))
      tclvalue(row.names.var) <- Data("export.rows")
    if (!is.null(Data("export.comment")))
      tclvalue(comment.var) <- Data("export.comment")

    # Frame 3, export parmaters

    frame3 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                            text="Export parameters")

    frame3.lab.1.1 <- ttklabel(frame3, text="Separator")
    frame3.lab.1.4 <- ttklabel(frame3, text="Decimal")
    frame3.lab.2.1 <- ttklabel(frame3, text="NA string")
    frame3.lab.2.4 <- ttklabel(frame3, text="Method")
    frame3.lab.3.1 <- ttklabel(frame3, text="Comment")

    frame3.box.1.2 <- ttkcombobox(frame3, width=17, state="readonly",
                                  value=sep1)
    frame3.box.1.5 <- ttkcombobox(frame3, width=17, state="readonly",
                                  value=dec1)
    frame3.box.2.2 <- ttkcombobox(frame3, width=17, state="readonly",
                                  value=nas1)
    frame3.box.2.5 <- ttkcombobox(frame3, width=17, state="readonly",
                                  value=qme1)
    frame3.box.3.2 <- ttkcombobox(frame3, width=17, state="readonly",
                                  value=com1)

    frame3.ent.1.3 <- ttkentry(frame3, width=12, textvariable=sep.var,
                               state="disabled")
    frame3.ent.2.3 <- ttkentry(frame3, width=12, textvariable=nas.var,
                               state="disabled")
    frame3.ent.3.3 <- ttkentry(frame3, width=12, textvariable=com.var,
                               state="disabled")

    txt <- "Surround character variables by double quotes"
    frame3.chk.4.1 <- ttkcheckbutton(frame3, variable=quote.var, text=txt)

    tkgrid(frame3.lab.1.1, frame3.box.1.2, frame3.ent.1.3, frame3.lab.1.4,
           frame3.box.1.5)
    tkgrid(frame3.lab.2.1, frame3.box.2.2, frame3.ent.2.3, frame3.lab.2.4,
           frame3.box.2.5, pady=4)
    tkgrid(frame3.lab.3.1, frame3.box.3.2, frame3.ent.3.3)
    tkgrid(frame3.chk.4.1, pady=c(4, 0))

    tkgrid.configure(frame3.lab.1.1, frame3.lab.1.4, frame3.lab.2.1,
                     frame3.lab.2.4, frame3.lab.3.1, padx=c(10, 2), sticky="w")

    tkgrid.configure(frame3.lab.1.1, frame3.lab.2.1, frame3.lab.3.1,
                     padx=c(0, 2))
    tkgrid.configure(frame3.ent.1.3, frame3.ent.2.3, frame3.ent.3.3,
                     padx=c(2, 0))
    tkgrid.configure(frame3.chk.4.1, columnspan=5, sticky="w", padx=0,
                     pady=c(5, 0))

    tkpack(frame3, fill="x", padx=10, pady=c(0, 10))

    tcl(frame3.box.1.2, "current", 0)
    tcl(frame3.box.1.5, "current", 0)
    tcl(frame3.box.2.2, "current", 0)
    tcl(frame3.box.2.5, "current", 0)
    tcl(frame3.box.3.2, "current", 0)

    if (!is.null(Data("export.sep"))) {
      if (Data("export.sep") %in% sep0) {
        tcl(frame3.box.1.2, "current", match(Data("export.sep"), sep0) - 1)
        tkconfigure(frame3.ent.1.3, state="disabled")
      } else {
        tcl(frame3.box.1.2, "current", match(NA, sep0) - 1)
        tkconfigure(frame3.ent.1.3, state="normal")
        tclvalue(sep.var) <- Data("export.sep")
      }
    }
    if (!is.null(Data("export.na"))) {
      if (Data("export.na") %in% nas0) {
        tcl(frame3.box.2.2, "current", match(Data("export.na"), nas0) - 1)
        tkconfigure(frame3.ent.2.3, state="disabled")
      } else {
        tcl(frame3.box.2.2, "current", match(NA, nas0) - 1)
        tkconfigure(frame3.ent.2.3, state="normal")
        tclvalue(nas.var) <- Data("export.na")
      }
    }
    if (!is.null(Data("export.com"))) {
      if (Data("export.com") %in% com0) {
        tcl(frame3.box.3.2, "current", match(Data("export.com"), com0) - 1)
        tkconfigure(frame3.ent.3.3, state="disabled")
      } else {
        tcl(frame3.box.3.2, "current", match(NA, com0) - 1)
        tkconfigure(frame3.ent.3.3, state="normal")
        tclvalue(com.var) <- Data("export.com")
      }
    }

    if (!is.null(Data("export.dec")))
      tcl(frame3.box.1.5, "current", match(Data("export.dec"), dec0) - 1)
    if (!is.null(Data("export.qmethod")))
      tcl(frame3.box.2.5, "current", match(Data("export.qmethod"), qme0) - 1)
    if (!is.null(Data("export.quote")))
      tclvalue(quote.var) <- Data("export.quote")
  }

  # Frame 4, output file and compression

  frame4 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                          text="Output file")
  frame4.ent.1.1 <- ttkentry(frame4, width=12, textvariable=file.var)
  frame4.but.1.5 <- ttkbutton(frame4, width=8, text="Browse",
                              command=GetDataFile)

  frame4.lab.2.1 <- ttklabel(frame4, text="Encoding")
  frame4.lab.3.1 <- ttklabel(frame4, text="End-of-line")
  frame4.box.2.2 <- ttkcombobox(frame4, width=17, state="readonly", value=enc1)
  frame4.box.3.2 <- ttkcombobox(frame4, width=17, state="readonly", value=eol1)
  txt <- "Compress using gzip"
  frame4.chk.2.3 <- ttkcheckbutton(frame4, variable=compress.var, text=txt,
                                   command=ToggleExtension)
  txt <- "Include changelog ( *.log )"
  frame4.chk.3.3 <- ttkcheckbutton(frame4, variable=changelog.var, text=txt)

  tkgrid(frame4.ent.1.1, "x", "x", "x", frame4.but.1.5)
  tkgrid.configure(frame4.ent.1.1, sticky="we", columnspan=4, padx=c(0, 2))

  if (file.type == "txt") {
    tkgrid(frame4.lab.2.1, frame4.box.2.2, frame4.chk.2.3, pady=c(4, 0),
           sticky="w")
    tkgrid(frame4.lab.3.1, frame4.box.3.2, frame4.chk.3.3, pady=c(4, 4),
           sticky="w")
    tkgrid.configure(frame4.lab.2.1, frame4.lab.3.1, padx=c(0, 2))
    tkgrid.configure(frame4.chk.2.3, frame4.chk.3.3, padx=c(40, 0),
                     columnspan=2)

    tcl(frame4.box.2.2, "current", 0)
    tcl(frame4.box.3.2, "current", 0)

    if (!is.null(Data("export.encoding")))
      tcl(frame4.box.2.2, "current", match(Data("export.encoding"), enc0) - 1)
    if (!is.null(Data("export.eol")))
      tcl(frame4.box.3.2, "current", match(Data("export.eol"), eol0) - 1)
    if (!is.null(Data("export.compress")))
      tclvalue(compress.var) <- Data("export.compress")

    if (is.null(Data("changelog"))) {
      tkconfigure(frame4.chk.3.3, state="disabled")
    } else {
      if (!is.null(Data("export.changelog")))
        tclvalue(changelog.var) <- Data("export.changelog")
    }

  } else if (file.type == "rda") {
    frame4.rbt.2.1 <- ttkradiobutton(frame4, variable=ascii.var,
                                     value=0, text="Binary")
    frame4.rbt.2.2 <- ttkradiobutton(frame4, variable=ascii.var,
                                     value=1, text="ASCII")
    tkgrid(frame4.rbt.2.1, frame4.rbt.2.2, "x", "x")
    tkgrid.configure(frame4.rbt.2.1, padx=c(0, 4))
  }

  tkgrid.columnconfigure(frame4, 3, weight=1)
  tkpack(frame4, fill="x", padx=10, pady=c(0, 15))

  # Bind events

  tclServiceMode(TRUE)
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)
  tkbind(frame1.lst.1.1, "<<ListboxSelect>>", ToggleExport)
  tkbind(frame4.ent.1.1, "<KeyRelease>", ToggleExport)

  if (file.type == "txt") {
    tkbind(frame3.box.1.2, "<<ComboboxSelected>>",
           function() {
             sep <- sep0[as.integer(tcl(frame3.box.1.2, "current")) + 1]
             if (is.na(sep)) {
               tkconfigure(frame3.ent.1.3, state="normal")
               tkfocus(frame3.ent.1.3)
             } else {
               tkconfigure(frame3.ent.1.3, state="disabled")
             }
           })
    tkbind(frame3.box.2.2, "<<ComboboxSelected>>",
           function() {
             nas <- nas0[as.integer(tcl(frame3.box.2.2, "current")) + 1]
             if (is.na(nas)) {
               tkconfigure(frame3.ent.2.3, state="normal")
               tkfocus(frame3.ent.2.3)
             } else {
               tkconfigure(frame3.ent.2.3, state="disabled")
             }
           })
    tkbind(frame3.box.3.2, "<<ComboboxSelected>>",
           function() {
             com <- com0[as.integer(tcl(frame3.box.3.2, "current")) + 1]
             if (is.na(com)) {
               tkconfigure(frame3.ent.3.3, state="normal")
               tkfocus(frame3.ent.3.3)
             } else {
               tkconfigure(frame3.ent.3.3, state="disabled")
             }
           })
  }

  # GUI control

  ToggleExport()

  tkfocus(frame1.lst.1.1)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)
}
