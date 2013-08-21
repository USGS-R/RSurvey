# Build C-style string formats.

Format <- function(sample=pi, fmt="", parent=NULL) {

  ## Additional functions (subroutines)

  # Save conversion specification format
  SaveFormat <- function() {
    fmt <- as.character(tclvalue(fmt.var))
    if (as.character(tclvalue(sample.var)) == "") {
      msg <- paste0("Invalid format '", fmt, "'; please try again.")
      tkmessageBox(icon="error", message=msg, title="Error", type="ok",
                   parent=tt)
    } else {
      new.fmt <<- fmt
      tclvalue(tt.done.var) <- 1
    }
  }

  # Translate format string, return TRUE if format is recognized

  TranslateFormat <- function() {
    if (nchar(fmt) < 2L)
      return(FALSE)
    if (substr(fmt, 1L, 1L) != "%")
      return(FALSE)
    code <- substr(fmt, nchar(fmt), nchar(fmt))
    if (inherits(sample, "integer")) {
      if (code != "d")
        return(FALSE)
    } else if (inherits(sample, "numeric")) {
      if (!code %in% c("f", "e"))
        return(FALSE)
    } else {
      if(code != "s")
        return(FALSE)
    }

    width <- ""
    precision <- ""

    is.scientific <- if (code == "e") TRUE else FALSE

    len <- attr(regexpr("\\-", fmt), "match.length")
    is.left <- if (len > 0L) TRUE else FALSE
    if (len > 1L)
      return(FALSE)

    len <- attr(regexpr("\\.", fmt), "match.length")
    is.period <- if (len > 0L) TRUE else FALSE
    if (len > 1L | (is.period & code %in% c("d", "s")))
      return(FALSE)

    len <- attr(regexpr("[[:space:]]", fmt), "match.length")
    is.space <- if (len > 0L) TRUE else FALSE
    if (len > 1L | (is.space & code == "s"))
      return(FALSE)

    len <- attr(regexpr("\\+", fmt), "match.length")
    is.sign <- if (len > 0L) TRUE else FALSE
    if (len > 1L | (is.sign & code == "s"))
      return(FALSE)

    is.pad <- FALSE

    if (nchar(fmt) > 2L) {
      fmt <- substr(fmt, 2L, nchar(fmt) - 1L)

      if (is.period) {
        fmt.split <- strsplit(fmt, "\\.")[[1L]]
        if (length(fmt.split) > 1L) {
          precision <- suppressWarnings(as.integer(fmt.split[2L]))
          if (is.na(precision))
            return(FALSE)
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
          return(FALSE)
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
    return(TRUE)
  }

  # Add string to conversion format entry
  AddString <- function(txt) {
    if (as.logical(tcl(frame1.ent.1, "selection", "present")))
      tcl(frame1.ent.1, "delete", "sel.first", "sel.last")
    tkinsert(frame1.ent.1, "insert", txt)
    UpdateSample()
    tkfocus(frame1.ent.1)
  }

  # Toggle GUI state based on custom check box

  ToggleState <- function() {
    is.custom <- as.logical(as.integer(tclvalue(custom.var)))

    tclServiceMode(FALSE)
    s <- if (is.custom) "normal" else "readonly"
    tkconfigure(frame1.ent.1, state=s)

    s <- if (is.custom) "normal" else "disabled"
    tkconfigure(frame1a.but.01, state=s)
    tkconfigure(frame1a.but.02, state=s)
    tkconfigure(frame1a.but.03, state=s)
    tkconfigure(frame1a.but.04, state=s)
    tkconfigure(frame1a.but.05, state=s)
    tkconfigure(frame1a.but.06, state=s)
    tkconfigure(frame1a.but.07, state=s)
    tkconfigure(frame1a.but.08, state=s)
    tkconfigure(frame1a.but.09, state=s)
    tkconfigure(frame1a.but.10, state=s)

    s <- if (is.custom) "readonly" else "normal"
    tkconfigure(frame2.ent.1.2, state=s)
    tkconfigure(frame2.ent.1.4, state=s)

    s <- if (is.custom) "disabled" else "normal"
    tkconfigure(frame2.lab.1.1, state=s)
    tkconfigure(frame2.lab.1.3, state=s)
    tkconfigure(frame2.chk.2.1, state=s)
    tkconfigure(frame2.chk.3.1, state=s)
    tkconfigure(frame2.chk.4.1, state=s)
    tkconfigure(frame2.chk.5.1, state=s)
    tkconfigure(frame2.chk.6.1, state=s)
    tclServiceMode(TRUE)

    if (is.custom) {
      tkfocus(frame1.ent.1)
    } else {
      BuildFormat()
      if (is.numeric(sample) && !is.integer(sample))
        tkfocus(frame2.ent.1.4)
      else
        tkfocus(frame2.ent.1.2)
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
      fmt <- paste0("%", space, pad, left, sign, width, period, precision,
                    letter)
    } else {
      fmt <- paste0("%", left, width, "s")
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
    tkfocus(frame1.ent.1)
  }

  ## Main program

  ans <- try(as.character(sample), silent=TRUE)
  if (inherits(ans, "try-error") || length(ans) != 1)
    stop("class of sample object is not valid")
  if (!is.character(fmt))
    stop("format argument must be of class character")

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

  tclvalue(custom.var) <- TRUE
  tclvalue(fmt.var) <- fmt

  if (!TranslateFormat())
    UpdateSample()

  # Open GUI

  tclServiceMode(FALSE)
  tt <- tktoplevel()
  tkwm.resizable(tt, 1, 0)

  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste0("+", as.integer(geo[2]) + 25,
                             "+", as.integer(geo[3]) + 25))
  }
  tktitle(tt) <- "Format"

  # Frame 0, ok and cancel buttons

  frame0 <- ttkframe(tt, relief="flat")
  frame0.but.2 <- ttkbutton(frame0, width=12, text="OK", command=SaveFormat)
  frame0.but.3 <- ttkbutton(frame0, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)
  frame0.but.4 <- ttkbutton(frame0, width=12, text="Help",
                            command=function() {
                              print(help("Format", package="RSurvey"))
                            })
  tkgrid("x", frame0.but.2, frame0.but.3, frame0.but.4,
         sticky="se", pady=10, padx=c(4, 0))
  tkgrid.columnconfigure(frame0, 0, weight=1)
  tkgrid.configure(frame0.but.4, padx=c(4, 10))
  tkpack(frame0, fill="x", side="bottom", anchor="e")

  # Frame 1, conversion specification format

  frame1 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                          text="Conversion specification format")

  frame1.ent.1 <- ttkentry(frame1, textvariable=fmt.var, width=30)

  frame1a <- ttkframe(frame1, relief="flat", borderwidth=0, padding=0)

  frame1a.but.01 <- ttkbutton(frame1a, width=2, text="%",
                              command=function() AddString("%"))
  frame1a.but.02 <- ttkbutton(frame1a, width=2, text="\u002b",
                              command=function() AddString("+"))
  frame1a.but.03 <- ttkbutton(frame1a, width=2, text="\u2212",
                              command=function() AddString("-"))
  frame1a.but.04 <- ttkbutton(frame1a, width=2, text=" ",
                              command=function() AddString(" "))
  frame1a.but.05 <- ttkbutton(frame1a, width=2, text="0",
                              command=function() AddString("0"))
  frame1a.but.06 <- ttkbutton(frame1a, width=2, text=".",
                              command=function() AddString("."))
  frame1a.but.07 <- ttkbutton(frame1a, width=2, text="f",
                              command=function() AddString("f"))
  frame1a.but.08 <- ttkbutton(frame1a, width=2, text="e",
                              command=function() AddString("e"))
  frame1a.but.09 <- ttkbutton(frame1a, width=2, text="d",
                              command=function() AddString("d"))
  frame1a.but.10 <- ttkbutton(frame1a, width=2, text="s",
                              command=function() AddString("s"))

  frame1a.but.11 <- ttkbutton(frame1a, width=2, image=GetBitmapImage("copy"),
                             command=CopyFormat)
  frame1a.but.12 <- ttkbutton(frame1a, width=2, image=GetBitmapImage("paste"),
                             command=PasteFormat)

  frame1a.chk.13 <- ttkcheckbutton(frame1a, text="Custom\u2026",
                                   variable=custom.var, command=ToggleState)

  if (is.numeric(sample)) {
    if (is.integer(sample))
      tkgrid(frame1a.but.01, frame1a.but.02, frame1a.but.03, frame1a.but.04,
             frame1a.but.05, frame1a.but.06, frame1a.but.07, frame1a.but.08,
             frame1a.but.09, frame1a.but.10, frame1a.but.11, frame1a.but.12,
             frame1a.chk.13, pady=c(2, 0), padx=c(0, 2))
    else
      tkgrid(frame1a.but.01, frame1a.but.02, frame1a.but.03, frame1a.but.04,
             frame1a.but.05, frame1a.but.06, frame1a.but.07, frame1a.but.08,
             frame1a.but.10, frame1a.but.11, frame1a.but.12, frame1a.chk.13,
             pady=c(2, 0), padx=c(0, 2))


  } else {
    if (is.logical(sample))
      tkgrid(frame1a.but.01, frame1a.but.02, frame1a.but.03, frame1a.but.04,
             frame1a.but.05, frame1a.but.06, frame1a.but.09, frame1a.but.10,
             frame1a.but.11, frame1a.but.12, frame1a.chk.13,
             pady=c(2, 0), padx=c(0, 2))
    else
      tkgrid(frame1a.but.01, frame1a.but.02, frame1a.but.03, frame1a.but.04,
             frame1a.but.05, frame1a.but.06, frame1a.but.10, frame1a.but.11,
             frame1a.but.12, frame1a.chk.13, pady=c(2, 0), padx=c(0, 2))
  }

  tkgrid(frame1.ent.1)
  tkgrid(frame1a, "x", pady=c(2, 0), sticky="w")
  tkgrid.configure(frame1a.but.10, padx=c(0, 10))
  tkgrid.configure(frame1a.chk.13, padx=c(10, 0))

  tkgrid.configure(frame1.ent.1, sticky="we", columnspan=2, padx=c(0, 2))

  tkgrid.columnconfigure(frame1, 1, weight=1)

  tkpack(frame1, fill="x", padx=10, pady=c(10, 0))

# Frame 2

  frame2 <- ttkframe(tt, relief="flat", borderwidth=0, padding=0)

  frame2.lab.1.1 <- ttklabel(frame2, text="Field width")
  frame2.lab.1.3 <- ttklabel(frame2, text="Precision")

  frame2.ent.1.2 <- ttkentry(frame2, textvariable=width.var, width=15)
  frame2.ent.1.4 <- ttkentry(frame2, textvariable=precision.var, width=15)

  txt <- "Use scientific notation"
  frame2.chk.2.1 <- ttkcheckbutton(frame2, text=txt, variable=scientific.var,
                                   command=BuildFormat)
  txt <- "Left adjustment of converted argument in its field"
  frame2.chk.3.1 <- ttkcheckbutton(frame2, text=txt, variable=left.var,
                                   command=BuildFormat)
  txt <- "Always print number with sign (\u002b/\u2212)"
  frame2.chk.4.1 <- ttkcheckbutton(frame2, text=txt, variable=sign.var,
                                   command=BuildFormat)
  txt <- "Prefix a space if the first character is not a sign"
  frame2.chk.5.1 <- ttkcheckbutton(frame2, text=txt, variable=space.var,
                                   command=BuildFormat)
  txt <- "Pad to the field width with leading zeros"
  frame2.chk.6.1 <- ttkcheckbutton(frame2, text=txt, variable=pad.var,
                                   command=BuildFormat)

  if (is.numeric(sample) && !is.integer(sample)) {
    tkgrid(frame2.lab.1.1, frame2.ent.1.2, frame2.lab.1.3, frame2.ent.1.4)
    tkgrid(frame2.chk.2.1, columnspan=4, sticky="w", padx=c(10, 0))
    tkgrid.configure(frame2.lab.1.3, padx=c(10, 2))
  } else {
    tkgrid(frame2.lab.1.1, frame2.ent.1.2, "x", "x", sticky="w")
    tkgrid.columnconfigure(frame2, 2, weight=1)
    tkgrid(frame2.chk.3.1, columnspan=3, sticky="w", padx=c(10, 0))
  }
  tkgrid.configure(frame2.lab.1.1, padx=c(10, 2))

  if (is.numeric(sample)) {
    tkgrid(frame2.chk.3.1, columnspan=4, sticky="w", padx=c(10, 0))
    tkgrid(frame2.chk.4.1, columnspan=4, sticky="w", padx=c(10, 0))
    tkgrid(frame2.chk.5.1, columnspan=4, sticky="w", padx=c(10, 0))
    tkgrid(frame2.chk.6.1, columnspan=4, sticky="w", padx=c(10, 0))
  }

  tkpack(frame2, padx=10, pady=10, anchor="w")

  # Frame 3, sample

  frame3 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                          text="Sample")
  frame3.ent <- ttkentry(frame3, textvariable=sample.var, width=30,
                         state="readonly", takefocus=FALSE)
  tkgrid(frame3.ent)
  tkgrid.configure(frame3.ent, sticky="we")
  tcl("grid", "anchor", frame3, "w")
  tkgrid.columnconfigure(frame3, 0, weight=1, minsize=13)
  tkpack(frame3, fill="x", expand=TRUE, padx=10)

  # Bind events

  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tkbind(frame1.ent.1, "<KeyRelease>", UpdateSample)

  tkbind(frame2.ent.1.2, "<KeyRelease>", BuildFormat)
  tkbind(frame2.ent.1.4, "<KeyRelease>", BuildFormat)

  # GUI control

  ToggleState()

  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  return(new.fmt)
}
