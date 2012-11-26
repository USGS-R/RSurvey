Format <- function(sample=pi, fmt=NULL, parent=NULL) {
  # Build C-style string formats.

  # Additional functions (subroutines)

  # Translate format string, return TRUE if custom format

  TranslateFormat <- function() {
    if (nchar(fmt) < 2L)
      return(TRUE)
    if (substr(fmt, 1L, 1L) != "%")
      return(TRUE)

    code <- substr(fmt, nchar(fmt), nchar(fmt))
    if (inherits(sample, "integer")) {
      if (code != "d")
        return(TRUE)
    } else if (inherits(sample, "numeric")) {
      if (!code %in% c("f", "e"))
        return(TRUE)
    } else {
      if(code != "s")
        return(TRUE)
    }

    width <- ""
    precision <- ""

    is.scientific <- if (code == "e") TRUE else FALSE

    len <- attr(regexpr("\\-", fmt), "match.length")
    is.left <- if (len > 0L) TRUE else FALSE
    if (len > 1L)
      return(TRUE)

    len <- attr(regexpr("\\.", fmt), "match.length")
    is.period <- if (len > 0L) TRUE else FALSE
    if (len > 1L | (is.period & code %in% c("d", "s")))
      return(TRUE)

    len <- attr(regexpr("[[:space:]]", fmt), "match.length")
    is.space <- if (len > 0L) TRUE else FALSE
    if (len > 1L | (is.space & code == "s"))
      return(TRUE)

    len <- attr(regexpr("\\+", fmt), "match.length")
    is.sign <- if (len > 0L) TRUE else FALSE
    if (len > 1L | (is.sign & code == "s"))
      return(TRUE)

    is.pad <- FALSE

    if (nchar(fmt) > 2L) {
      fmt <- substr(fmt, 2L, nchar(fmt) - 1L)

      if (is.period) {
        fmt.split <- strsplit(fmt, "\\.")[[1L]]
        if (length(fmt.split) > 1L) {
          precision <- suppressWarnings(as.integer(fmt.split[2L]))
          if (is.na(precision))
            return(TRUE)
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
        if (is.na(width))
          return(TRUE)
      }
    }

    tclvalue(custom.var) <- FALSE
    tclvalue(width.var) <- width
    tclvalue(precision.var) <- precision
    tclvalue(scientific.var) <- is.scientific
    tclvalue(left.var) <- is.left
    tclvalue(sign.var) <- is.sign
    tclvalue(space.var) <- is.space
    tclvalue(pad.var) <- is.pad

    BuildFormat()
    return(FALSE)
  }

  # Add string to conversion format entry

  AddString <- function(txt) {
    if (as.logical(tcl(frame2.ent.2, "selection", "present")))
      tcl(frame2.ent.2, "delete", "sel.first", "sel.last")
    tkinsert(frame2.ent.2, "insert", txt)
    UpdateSample()
    tkfocus(frame2.ent.2)
  }

  # Toggle GUI state based on custom check box

  ToggleState <- function() {
    is.custom <- as.logical(as.integer(tclvalue(custom.var)))

    tclServiceMode(FALSE)
    s <- if (is.custom) "normal" else "readonly"
    tkconfigure(frame2.ent.2, state=s)

    s <- if (is.custom) "normal" else "disabled"
    tkconfigure(frame2a.but.01, state=s)
    tkconfigure(frame2a.but.02, state=s)
    tkconfigure(frame2a.but.03, state=s)
    tkconfigure(frame2a.but.04, state=s)
    tkconfigure(frame2a.but.05, state=s)
    tkconfigure(frame2a.but.06, state=s)
    tkconfigure(frame2a.but.07, state=s)
    tkconfigure(frame2a.but.08, state=s)
    tkconfigure(frame2a.but.09, state=s)
    tkconfigure(frame2a.but.10, state=s)

    s <- if (is.custom) "readonly" else "normal"
    tkconfigure(frame1.ent.1.2, state=s)
    tkconfigure(frame1.ent.1.4, state=s)

    s <- if (is.custom) "disabled" else "normal"
    tkconfigure(frame1.lab.1.1, state=s)
    tkconfigure(frame1.lab.1.3, state=s)
    tkconfigure(frame1.chk.1.5, state=s)
    tkconfigure(frame1.chk.2.1, state=s)
    tkconfigure(frame1.chk.3.1, state=s)
    tkconfigure(frame1.chk.4.1, state=s)
    tkconfigure(frame1.chk.5.1, state=s)
    tclServiceMode(TRUE)

    if (is.custom ) {
      tkfocus(frame2.ent.2)
    } else {
      BuildFormat()
      tkfocus(frame1.ent.1.2)
    }
  }

  # Update sample value

  UpdateSample <- function() {
    fmt <- as.character(tclvalue(fmt.var))
    if (fmt == "") {
      tclvalue(sample.var) <- format(sample)
    } else {
      ans <- try(sprintf(fmt, sample), silent=TRUE)
      if (inherits(ans, "try-error"))
        tclvalue(sample.var) <- ""
      else
        tclvalue(sample.var) <- ans
    }
  }

  # Build format

  BuildFormat <- function() {
    tclvalue(width.var) <- CheckEntry("integer", tclvalue(width.var))
    width <- as.character(tclvalue(width.var))

    is.left <- as.logical(as.integer(tclvalue(left.var)))
    left <- if (is.left) "-" else ""

    if (is.numeric(sample)) {
      is.space <- as.logical(as.integer(tclvalue(space.var)))
      is.pad   <- as.logical(as.integer(tclvalue(pad.var)))
      is.sign  <- as.logical(as.integer(tclvalue(sign.var)))

      space <- if (is.space) " " else ""
      pad <- if (is.pad & width != "") "0" else ""
      sign <- if (is.sign) "+" else ""
      period <- ""
      precision <- ""

      if (is.integer(sample)) {
        letter <- "d"
      } else {
        tclvalue(precision.var) <- CheckEntry("integer",
                                              tclvalue(precision.var))
        precision <- as.character(tclvalue(precision.var))
        if (!(width == "" & precision == ""))
          period <- "."

        is.scientific <- as.logical(as.integer(tclvalue(scientific.var)))
        letter <- if (is.scientific) "e" else "f"
      }
      fmt <- paste("%", space, pad, left, sign, width, period, precision,
                   letter, sep="")
    } else {
      fmt <- paste("%", left, width, "s", sep="")
    }
    tclvalue(fmt.var) <- fmt
    UpdateSample()
  }

  # Copy format to clipboard

  CopyFormat <- function() {
    txt <- as.character(tclvalue(fmt.var))
    cat(txt, file="clipboard")
  }

  # Paste format from clipboard

  PasteFormat <- function() {
    cb <- try(scan(file="clipboard", what="character", sep="\n", quiet=TRUE),
              silent=TRUE)
    if (inherits(cb, "try-error"))
      return()
    tclvalue(custom.var) <- TRUE
    ToggleState()
    tclvalue(fmt.var) <- cb
    UpdateSample()
    tkfocus(frame2.ent.2)
  }

  # Save conversion specification format

  SaveFormat <- function() {
    fmt <- as.character(tclvalue(fmt.var))
    if (as.character(tclvalue(sample.var)) == "") {
      msg <- paste("Invalid format '", fmt, "'; please try again.", sep="")
      tkmessageBox(icon="error", message=msg, title="Error", type="ok",
                   parent=tt)
    } else {
      new.fmt <<- fmt
      tclvalue(tt.done.var) <- 1
    }
  }


    # Main program

  if (!inherits(sample, c("numeric", "integer", "character",
                          "factor", "logical")))
    stop(paste("Class of sample object is not acceptable:",
               class(sample), ".", sep=""))

  new.fmt <- NULL

  # Assign variables linked to Tk widgets

  sample.var     <- tclVar()
  fmt.var        <- tclVar()
  custom.var     <- tclVar(0)
  width.var      <- tclVar()
  precision.var  <- tclVar()
  scientific.var <- tclVar(0)
  left.var       <- tclVar(0)
  sign.var       <- tclVar(0)
  space.var      <- tclVar(0)
  pad.var        <- tclVar(0)
  tt.done.var    <- tclVar(0)

  if (is.character(fmt)) {
    tclvalue(custom.var) <- TRUE
    tclvalue(fmt.var) <- fmt
  }

  is.custom <- TranslateFormat()
  if (is.custom)
    UpdateSample()

  # Open GUI

  tclServiceMode(FALSE)
  tt <- tktoplevel()
  tkwm.resizable(tt, 1, 0)

  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(geo[2]) + 25,
                            "+", as.integer(geo[3]) + 25, sep=""))
  }
  tktitle(tt) <- "Format"

  # Frame 0, ok and cancel buttons

  frame0 <- ttkframe(tt, relief="flat")
  frame0.but.2 <- ttkbutton(frame0, width=12, text="OK", command=SaveFormat)
  frame0.but.3 <- ttkbutton(frame0, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)
  tkgrid(frame0.but.2, frame0.but.3, sticky="se", pady=c(10, 10))
  tkgrid.columnconfigure(frame0, 0, weight=1)
  tkgrid.configure(frame0.but.3, padx=c(4, 10))
  tkpack(frame0, fill="x", side="bottom", anchor="e")

# Frame 1

  frame1 <- ttkframe(tt, relief="flat", borderwidth=0, padding=0)

  frame1.lab.1.1 <- ttklabel(frame1, text="Field width")
  frame1.lab.1.3 <- ttklabel(frame1, text="Precision")

  frame1.ent.1.2 <- ttkentry(frame1, textvariable=width.var, width=12)
  frame1.ent.1.4 <- ttkentry(frame1, textvariable=precision.var, width=12)

  txt <- "Scientific"
  frame1.chk.1.5 <- ttkcheckbutton(frame1, text=txt, variable=scientific.var,
                                   command=BuildFormat)
  txt <- "Left adjustment of converted argument in its field."
  frame1.chk.2.1 <- ttkcheckbutton(frame1, text=txt, variable=left.var,
                                   command=BuildFormat)
  txt <- "Always print number with sign (\u002b/\u2212)."
  frame1.chk.3.1 <- ttkcheckbutton(frame1, text=txt, variable=sign.var,
                                   command=BuildFormat)
  txt <- "Prefix a space if the first character is not a sign."
  frame1.chk.4.1 <- ttkcheckbutton(frame1, text=txt, variable=space.var,
                                   command=BuildFormat)
  txt <- "Pad to the field width with leading zeros."
  frame1.chk.5.1 <- ttkcheckbutton(frame1, text=txt, variable=pad.var,
                                   command=BuildFormat)

  if (is.numeric(sample) && !is.integer(sample)) {
    tkgrid(frame1.lab.1.1, frame1.ent.1.2, frame1.lab.1.3, frame1.ent.1.4,
           frame1.chk.1.5, pady=c(15, 10))

    tkgrid.configure(frame1.lab.1.3, padx=c(10, 2))
    tkgrid.configure(frame1.chk.1.5, padx=c(2, 2))
  } else {
    tkgrid(frame1.lab.1.1, frame1.ent.1.2, "x", pady=c(15, 10), sticky="w")
    tkgrid.columnconfigure(frame1, 2, weight=1)
    tkgrid(frame1.chk.2.1, columnspan=3, sticky="w", padx=c(10, 0))
  }
  tkgrid.configure(frame1.lab.1.1, padx=c(10, 2))

  if (is.numeric(sample)) {
    tkgrid(frame1.chk.2.1, columnspan=5, sticky="w", padx=c(10, 0))
    tkgrid(frame1.chk.3.1, columnspan=5, sticky="w", padx=c(10, 0))
    tkgrid(frame1.chk.4.1, columnspan=5, sticky="w", padx=c(10, 0))
    tkgrid(frame1.chk.5.1, columnspan=5, sticky="w", padx=c(10, 0))
  }

  tkpack(frame1, padx=10, pady=0, anchor="w")

  # Frame 2, conversion specification format

  frame2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                          text="Conversion specification format")

  
  frame2.chk.1 <- ttkcheckbutton(frame2, text="Custom", variable=custom.var,
                                 command=ToggleState)
  frame2.ent.2 <- ttkentry(frame2, textvariable=fmt.var, width=30)
  
  frame2a <- ttkframe(frame2, relief="flat", borderwidth=0, padding=0)

  frame2a.but.01 <- ttkbutton(frame2a, width=2, text="%",
                              command=function() AddString("%"))
  frame2a.but.02 <- ttkbutton(frame2a, width=2, text="\u002b",
                              command=function() AddString("+"))
  frame2a.but.03 <- ttkbutton(frame2a, width=2, text="\u2212",
                              command=function() AddString("-"))
  frame2a.but.04 <- ttkbutton(frame2a, width=2, text=" ",
                              command=function() AddString(" "))
  frame2a.but.05 <- ttkbutton(frame2a, width=2, text="0",
                              command=function() AddString("0"))
  frame2a.but.06 <- ttkbutton(frame2a, width=2, text=".",
                              command=function() AddString("."))
  frame2a.but.07 <- ttkbutton(frame2a, width=2, text="f",
                              command=function() AddString("f"))
  frame2a.but.08 <- ttkbutton(frame2a, width=2, text="e",
                              command=function() AddString("e"))
  frame2a.but.09 <- ttkbutton(frame2a, width=2, text="d",
                              command=function() AddString("d"))
  frame2a.but.10 <- ttkbutton(frame2a, width=2, text="s",
                              command=function() AddString("s"))

  frame2a.but.11 <- ttkbutton(frame2a, width=2, image=GetBitmapImage("copy"),
                             command=CopyFormat)
  frame2a.but.12 <- ttkbutton(frame2a, width=2, image=GetBitmapImage("paste"),
                             command=PasteFormat)

  if (is.numeric(sample)) {
    if (is.integer(sample))
      tkgrid(frame2a.but.01, frame2a.but.02, frame2a.but.03, frame2a.but.04,
             frame2a.but.05, frame2a.but.06, frame2a.but.07, frame2a.but.08,
             frame2a.but.09, frame2a.but.10, frame2a.but.11, frame2a.but.12,
             pady=c(2, 0), padx=c(0, 2))
    else
      tkgrid(frame2a.but.01, frame2a.but.02, frame2a.but.03, frame2a.but.04,
             frame2a.but.05, frame2a.but.06, frame2a.but.07, frame2a.but.08,
             frame2a.but.10, frame2a.but.11, frame2a.but.12,
             pady=c(2, 0), padx=c(0, 2))


  } else {
    if (is.logical(sample))
      tkgrid(frame2a.but.01, frame2a.but.02, frame2a.but.03, frame2a.but.04,
             frame2a.but.05, frame2a.but.06, frame2a.but.09, frame2a.but.10,
             frame2a.but.11, frame2a.but.12, pady=c(2, 0), padx=c(0, 2))
    else
      tkgrid(frame2a.but.01, frame2a.but.02, frame2a.but.03, frame2a.but.04,
             frame2a.but.05, frame2a.but.06, frame2a.but.10, frame2a.but.11,
             frame2a.but.12, pady=c(2, 0), padx=c(0, 2))
  }

  tkgrid(frame2.chk.1)
  tkgrid(frame2.ent.2)
  tkgrid(frame2a, "x", pady=c(2, 0), sticky="w")
  
  tkgrid.configure(frame2.chk.1, sticky="w", pady=c(0, 2))
  tkgrid.configure(frame2.ent.2, sticky="we", columnspan=2, padx=c(0, 2))
  tkgrid.configure(frame2a.but.10, padx=c(0, 10))

  tkgrid.columnconfigure(frame2, 1, weight=1)

  tkpack(frame2, fill="x", padx=10, pady=10)

  # Frame 3, sample entry

  frame3 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                          text="Sample")
  frame3.ent <- ttkentry(frame3, textvariable=sample.var, width=30,
                         state="readonly", takefocus=FALSE)
  tkgrid(frame3.ent, padx=0, pady=0)
  tkgrid.configure(frame3.ent, sticky="we")
  tcl("grid", "anchor", frame3, "w")
  tkgrid.columnconfigure(frame3, 0, weight=1, minsize=13)
  tkpack(frame3, fill="x", expand=TRUE, padx=10, pady=0)

  # Bind events

  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tkbind(frame2.ent.2, "<KeyRelease>", UpdateSample)

  tkbind(frame1.ent.1.2, "<KeyRelease>", BuildFormat)
  tkbind(frame1.ent.1.4, "<KeyRelease>", BuildFormat)

  # GUI control

  ToggleState()

  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  new.fmt
}
