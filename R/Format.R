Format <- function(sample=pi, fmt="", parent=NULL) {


  # save conversion specification format
  SaveFormat <- function() {
    opt <- as.integer(tclvalue(opt.var))
    fmt <- as.character(tclvalue(fmt.var))
    sam <- as.character(tclvalue(sample.var))
    if (opt == 3L && sam == "") {
      msg <- paste0("Invalid format '", fmt, "'; please try again.")
      tkmessageBox(icon="error", message=msg, title="Error", type="ok", parent=tt)
      tkfocus(f3.ent.1)
    } else {
      new.fmt <<- ifelse(opt == 1L, "", fmt)
      tclvalue(tt.done.var) <- 1
    }
  }


  # translate format string
  TranslateFormat <- function() {
    tclvalue(opt.var) <- 1L
    if (nchar(fmt) == 0L) return()
    tclvalue(opt.var) <- 3L

    if (nchar(fmt) < 2L || substr(fmt, 1L, 1L) != "%") return()
    code <- substr(fmt, nchar(fmt), nchar(fmt))
    if (inherits(sample, "integer")) {
      if (code != "d") return()
    } else if (inherits(sample, "numeric")) {
      if (!code %in% c("f", "e")) return()
    } else {
      if(code != "s") return()
    }

    width <- ""
    precision <- ""
    is.scientific <- ifelse(code == "e", TRUE, FALSE)

    len <- attr(regexpr("\\-", fmt), "match.length")
    is.left <- ifelse(len > 0L, TRUE, FALSE)
    if (len > 1L) return()
    len <- attr(regexpr("\\.", fmt), "match.length")
    is.period <- ifelse(len > 0L, TRUE, FALSE)
    if (len > 1L | (is.period & code %in% c("d", "s")))  return()
    len <- attr(regexpr("[[:space:]]", fmt), "match.length")
    is.space <- ifelse(len > 0L, TRUE, FALSE)
    if (len > 1L | (is.space & code == "s")) return()
    len <- attr(regexpr("\\+", fmt), "match.length")
    is.sign <- ifelse(len > 0L, TRUE, FALSE)
    if (len > 1L | (is.sign & code == "s")) return()

    is.pad <- FALSE
    if (nchar(fmt) > 2L) {
      fmt <- substr(fmt, 2L, nchar(fmt) - 1L)
      if (is.period) {
        fmt.split <- strsplit(fmt, "\\.")[[1L]]
        if (length(fmt.split) > 1L) {
          precision <- suppressWarnings(as.integer(fmt.split[2L]))
          if (is.na(precision)) return()
        }
        fmt <- fmt.split[1L]
      }
      fmt <- sub("[[:space:]]", "", fmt)
      fmt <- sub("\\+", "", fmt)
      fmt <- sub("\\-", "", fmt)
      if (nchar(fmt) > 1L) {
        if (substr(fmt, 1L, 1L) == "0") {
          is.pad <- TRUE
          fmt <- substr(fmt, 2L, nchar(fmt))
        }
      }
      if (nchar(fmt) > 0L) {
        width <- suppressWarnings(as.integer(fmt))
        if (is.na(width)) return()
      }
    }

    tclvalue(opt.var)        <- 2L
    tclvalue(width.var)      <- width
    tclvalue(precision.var)  <- precision
    tclvalue(scientific.var) <- is.scientific
    tclvalue(left.var)       <- is.left
    tclvalue(sign.var)       <- is.sign
    tclvalue(space.var)      <- is.space
    tclvalue(pad.var)        <- is.pad

    BuildFormat()
    return()
  }


  # add string to conversion format entry
  AddString <- function(txt) {
    if (as.logical(tcl(f3.ent.1, "selection", "present")))
      tcl(f3.ent.1, "delete", "sel.first", "sel.last")
    tkinsert(f3.ent.1, "insert", txt)
    UpdateSample()
    tkfocus(f3.ent.1)
  }


  # toggle gui state based on custom check box
  ToggleState <- function() {
    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE))

    opt <- as.integer(tclvalue(opt.var))

    s <- ifelse(opt == 2L, "normal", "readonly")
    tkconfigure(f2.ent.1.2, state=s)
    tkconfigure(f2.ent.1.4, state=s)

    s <- ifelse(opt == 2L, "normal",  "disabled")
    tkconfigure(f2.lab.1.1, state=s)
    tkconfigure(f2.lab.1.3, state=s)
    tkconfigure(f2.chk.2.1, state=s)
    tkconfigure(f2.chk.3.1, state=s)
    tkconfigure(f2.chk.4.1, state=s)
    tkconfigure(f2.chk.5.1, state=s)
    tkconfigure(f2.chk.6.1, state=s)

    s <- ifelse(opt == 3L, "normal", "readonly")
    tkconfigure(f3.ent.1, state=s)

    s <- ifelse(opt == 3L, "normal", "disabled")
    tkconfigure(f3a.but.01, state=s)
    tkconfigure(f3a.but.02, state=s)
    tkconfigure(f3a.but.03, state=s)
    tkconfigure(f3a.but.04, state=s)
    tkconfigure(f3a.but.05, state=s)
    tkconfigure(f3a.but.06, state=s)
    tkconfigure(f3a.but.07, state=s)
    tkconfigure(f3a.but.08, state=s)
    tkconfigure(f3a.but.09, state=s)
    tkconfigure(f3a.but.10, state=s)

    if (opt == 1L) {
      UpdateSample()
    } else if (opt == 2L) {
      BuildFormat()
      if (inherits(sample, "numeric"))
        tkfocus(f2.ent.1.4)
      else
        tkfocus(f2.ent.1.2)
    } else if (opt == 3L) {
      UpdateSample()
      tkfocus(f3.ent.1)
    }
    return()
  }


  # update sample value
  UpdateSample <- function() {
    opt <- as.integer(tclvalue(opt.var))
    fmt <- as.character(tclvalue(fmt.var))
    if (fmt == "" || opt == 1L) {
      tclvalue(sample.var) <- format(sample)
    } else {
      ans <- try(sprintf(fmt, sample), silent=TRUE)
      tclvalue(sample.var) <- ifelse(inherits(ans, "try-error"), "", ans)
    }
  }


  # build format
  BuildFormat <- function() {
    tclvalue(width.var) <- CheckEntry("integer", tclvalue(width.var))
    width <- as.character(tclvalue(width.var))

    is.left <- as.logical(as.integer(tclvalue(left.var)))
    left <- ifelse(is.left, "-", "")

    if (is.numeric(sample)) {
      is.space <- as.logical(as.integer(tclvalue(space.var)))
      is.pad   <- as.logical(as.integer(tclvalue(pad.var)))
      is.sign  <- as.logical(as.integer(tclvalue(sign.var)))

      space <- ifelse(is.space, " ", "")
      pad <- ifelse((is.pad && width != ""), "0", "")
      sign <- ifelse(is.sign, "+", "")
      period <- ""
      precision <- ""

      if (is.integer(sample)) {
        letter <- "d"
      } else {
        tclvalue(precision.var) <- CheckEntry("integer", tclvalue(precision.var))
        precision <- as.character(tclvalue(precision.var))
        if (!(width == "" & precision == "")) period <- "."

        is.scientific <- as.logical(as.integer(tclvalue(scientific.var)))
        letter <- ifelse(is.scientific, "e", "f")
      }
      fmt <- paste0("%", space, pad, left, sign, width, period, precision, letter)
    } else {
      fmt <- paste0("%", left, width, "s")
    }
    tclvalue(fmt.var) <- fmt
    UpdateSample()
  }


  # copy format to clipboard
  CopyFormat <- function() {
    txt <- as.character(tclvalue(fmt.var))
    cat(txt, file="clipboard")
  }


  # paste format from clipboard
  PasteFormat <- function() {
    opt <- as.integer(tclvalue(opt.var))
    if (opt != 3L) return()
    cb <- try(scan(file="clipboard", what="character", sep="\n", quiet=TRUE), silent=TRUE)
    if (inherits(cb, "try-error")) return()
    tclvalue(fmt.var) <- cb
    UpdateSample()
    tkfocus(f3.ent.1)
  }


  ans <- try(as.character(sample), silent=TRUE)
  if (inherits(ans, "try-error") || length(ans) != 1) stop("class of sample object is not valid")
  if (!is.character(fmt)) stop("format argument must be of class character")

  new.fmt <- NULL

  # assign variables linked to Tk widgets
  opt.var        <- tclVar()
  sample.var     <- tclVar()
  fmt.var        <- tclVar(fmt)
  width.var      <- tclVar()
  precision.var  <- tclVar()
  scientific.var <- tclVar(0)
  left.var       <- tclVar(0)
  sign.var       <- tclVar(0)
  space.var      <- tclVar(0)
  pad.var        <- tclVar(0)
  tt.done.var    <- tclVar(0)

  TranslateFormat()
  UpdateSample()

  # open gui
  tclServiceMode(FALSE)
  tt <- tktoplevel()
  tkwm.resizable(tt, 1, 0)

  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    geo <- as.integer(geo[2:3]) + 25
    tkwm.geometry(tt, sprintf("+%s+%s", geo[1], geo[2]))
  }
  tktitle(tt) <- "Format"

  # frame 0, ok and cancel buttons
  f0 <- ttkframe(tt, relief="flat")
  f0.but.2 <- ttkbutton(f0, width=12, text="OK", command=SaveFormat)
  f0.but.3 <- ttkbutton(f0, width=12, text="Cancel",
                        command=function() tclvalue(tt.done.var) <- 1)
  f0.but.4 <- ttkbutton(f0, width=12, text="Help",
                        command=function() {
                          print(help("Format", package="RSurvey"))
                        })
  tkgrid("x", f0.but.2, f0.but.3, f0.but.4, sticky="se", pady=10, padx=c(4, 0))
  tkgrid.columnconfigure(f0, 0, weight=1)
  tkgrid.configure(f0.but.4, padx=c(4, 10))
  tkpack(f0, fill="x", side="bottom", anchor="e")

  # frame 1, format using
  f1 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5, text="Format using")

  f1.rbt.1.1 <- ttkradiobutton(f1, variable=opt.var, value=1L, text="Common format",
                               command=ToggleState)
  f1.rbt.1.2 <- ttkradiobutton(f1, variable=opt.var, value=2L, text="C-style options",
                               command=ToggleState)
  f1.rbt.1.3 <- ttkradiobutton(f1, variable=opt.var, value=3L, text="C-style string",
                               command=ToggleState)

  tkgrid(f1.rbt.1.1, f1.rbt.1.2, f1.rbt.1.3)

  tkgrid.configure(f1.rbt.1.2, padx=10)

  tkpack(f1, fill="x", padx=10, pady=10)

  # frame 2
  f2 <- ttkframe(tt, relief="flat", borderwidth=0, padding=0)

  f2.lab.1.1 <- ttklabel(f2, text="Field width")
  f2.lab.1.3 <- ttklabel(f2, text="Precision")

  f2.ent.1.2 <- ttkentry(f2, textvariable=width.var, width=15)
  f2.ent.1.4 <- ttkentry(f2, textvariable=precision.var, width=15)

  txt <- "Use scientific notation"
  f2.chk.2.1 <- ttkcheckbutton(f2, text=txt, variable=scientific.var,
                               command=BuildFormat)
  txt <- "Left adjustment of converted argument in its field"
  f2.chk.3.1 <- ttkcheckbutton(f2, text=txt, variable=left.var,
                               command=BuildFormat)
  txt <- "Always print number with sign (\u002b/\u2212)"
  f2.chk.4.1 <- ttkcheckbutton(f2, text=txt, variable=sign.var,
                               command=BuildFormat)
  txt <- "Prefix a space if the first character is not a sign"
  f2.chk.5.1 <- ttkcheckbutton(f2, text=txt, variable=space.var,
                               command=BuildFormat)
  txt <- "Pad to the field width with leading zeros"
  f2.chk.6.1 <- ttkcheckbutton(f2, text=txt, variable=pad.var,
                               command=BuildFormat)

  if (is.numeric(sample) && !is.integer(sample)) {
    tkgrid(f2.lab.1.1, f2.ent.1.2, f2.lab.1.3, f2.ent.1.4)
    tkgrid(f2.chk.2.1, columnspan=4, sticky="w", padx=c(10, 0))
    tkgrid.configure(f2.lab.1.3, padx=c(10, 2))
  } else {
    tkgrid(f2.lab.1.1, f2.ent.1.2, "x", "x", sticky="w")
    tkgrid.columnconfigure(f2, 2, weight=1)
    tkgrid(f2.chk.3.1, columnspan=3, sticky="w", padx=c(10, 0))
  }
  tkgrid.configure(f2.lab.1.1, padx=c(10, 2))

  if (is.numeric(sample)) {
    tkgrid(f2.chk.3.1, columnspan=4, sticky="w", padx=c(10, 0))
    tkgrid(f2.chk.4.1, columnspan=4, sticky="w", padx=c(10, 0))
    tkgrid(f2.chk.5.1, columnspan=4, sticky="w", padx=c(10, 0))
    tkgrid(f2.chk.6.1, columnspan=4, sticky="w", padx=c(10, 0))
  }

  tkpack(f2, padx=10, pady=0, anchor="w")

  # frame 3, conversion specification format
  f3 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                      text="Conversion specification string")

  f3.ent.1 <- ttkentry(f3, textvariable=fmt.var, width=30)

  f3a <- ttkframe(f3, relief="flat", borderwidth=0, padding=0)

  f3a.but.01 <- ttkbutton(f3a, width=2, text="%",
                          command=function() AddString("%"))
  f3a.but.02 <- ttkbutton(f3a, width=2, text="\u002b",
                          command=function() AddString("+"))
  f3a.but.03 <- ttkbutton(f3a, width=2, text="\u2212",
                          command=function() AddString("-"))
  f3a.but.04 <- ttkbutton(f3a, width=2, text=" ",
                          command=function() AddString(" "))
  f3a.but.05 <- ttkbutton(f3a, width=2, text="0",
                          command=function() AddString("0"))
  f3a.but.06 <- ttkbutton(f3a, width=2, text=".",
                          command=function() AddString("."))
  f3a.but.07 <- ttkbutton(f3a, width=2, text="f",
                          command=function() AddString("f"))
  f3a.but.08 <- ttkbutton(f3a, width=2, text="e",
                          command=function() AddString("e"))
  f3a.but.09 <- ttkbutton(f3a, width=2, text="d",
                          command=function() AddString("d"))
  f3a.but.10 <- ttkbutton(f3a, width=2, text="s",
                          command=function() AddString("s"))

  f3a.but.11 <- ttkbutton(f3a, width=2, image=GetBitmapImage("copy"),
                          command=CopyFormat)
  f3a.but.12 <- ttkbutton(f3a, width=2, image=GetBitmapImage("paste"),
                          command=PasteFormat)

  if (is.numeric(sample)) {
    if (is.integer(sample))
      tkgrid(f3a.but.01, f3a.but.02, f3a.but.03, f3a.but.04, f3a.but.05, f3a.but.06,
             f3a.but.07, f3a.but.08, f3a.but.09, f3a.but.10, f3a.but.11, f3a.but.12,
             pady=c(2, 0), padx=c(0, 2))
    else
      tkgrid(f3a.but.01, f3a.but.02, f3a.but.03, f3a.but.04, f3a.but.05, f3a.but.06,
             f3a.but.07, f3a.but.08, f3a.but.10, f3a.but.11, f3a.but.12,
             pady=c(2, 0), padx=c(0, 2))
  } else {
    if (is.logical(sample))
      tkgrid(f3a.but.01, f3a.but.02, f3a.but.03, f3a.but.04, f3a.but.05, f3a.but.06,
             f3a.but.09, f3a.but.10, f3a.but.11, f3a.but.12, pady=c(2, 0), padx=c(0, 2))
    else
      tkgrid(f3a.but.01, f3a.but.02, f3a.but.03, f3a.but.04, f3a.but.05, f3a.but.06,
             f3a.but.10, f3a.but.11, f3a.but.12, pady=c(2, 0), padx=c(0, 2))
  }

  tkgrid(f3.ent.1)
  tkgrid(f3a, "x", pady=c(2, 0), sticky="w")
  tkgrid.configure(f3a.but.10, padx=c(0, 10))

  tkgrid.configure(f3.ent.1, sticky="we", columnspan=2, padx=c(0, 2))

  tkgrid.columnconfigure(f3, 1, weight=1)

  tkpack(f3, fill="x", padx=10, pady=10)

  # frame 3, sample
  f3 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5, text="Sample")
  f3.ent <- ttkentry(f3, textvariable=sample.var, width=30, state="readonly",
                     takefocus=FALSE)
  tkgrid(f3.ent)
  tkgrid.configure(f3.ent, sticky="we")
  tcl("grid", "anchor", f3, "w")
  tkgrid.columnconfigure(f3, 0, weight=1, minsize=13)
  tkpack(f3, fill="x", expand=TRUE, padx=10)

  # bind events
  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tkbind(f3.ent.1, "<KeyRelease>", UpdateSample)

  tkbind(f2.ent.1.2, "<KeyRelease>", BuildFormat)
  tkbind(f2.ent.1.4, "<KeyRelease>", BuildFormat)

  # gui control
  ToggleState()

  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  return(new.fmt)
}
