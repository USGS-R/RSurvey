Format <- function(sample=pi, fmt=NULL, parent=NULL) {
  # A GUI for constructing C-style string formats.

  # Additional functions (subroutines)

  # Save conversion specification format

  SaveFormat <- function() {
    new.fmt <<- as.character(tclvalue(fmt.var))
    tclvalue(tt.done.var) <- 1
  }

  # Add string to conversion format entry

  AddString <- function(txt) {
    if (as.logical(tcl(frame2.ent.1, "selection", "present")))
      tcl(frame2.ent.1, "delete", "sel.first", "sel.last")
    tkinsert(frame2.ent.1, "insert", txt)



    tkfocus(frame2.ent.1)
  }


    # Main program

  if (!inherits(sample, c("numeric", "integer", "character", "factor", "logical")))
    stop("Class of sample object is not acceptable.")

  new.fmt <- NULL

  # Assign variables linked to Tk widgets

  sample.var     <- tclVar()
  fmt.var        <- tclVar()
  custom.var     <- tclVar()
  width.var      <- tclVar()
  precision.var  <- tclVar()
  scientific.var <- tclVar()
  left.var       <- tclVar()
  sign.var       <- tclVar()
  space.var      <- tclVar()
  pad.var        <- tclVar()
  tt.done.var    <- tclVar()

  if (is.null(fmt))
    tclvalue(sample.var) <- format(sample)
  else
    sprintf(fmt, sample)



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

  # Frame 1, sample entry

  frame1 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                          text="Sample")
  frame1.ent <- ttkentry(frame1, textvariable=sample.var, width=30,
                         state="readonly", takefocus=FALSE)
  tkgrid(frame1.ent, padx=0, pady=0)
  tkgrid.configure(frame1.ent, sticky="we")
  tcl("grid", "anchor", frame1, "w")
  tkgrid.columnconfigure(frame1, 0, weight=1, minsize=13)
  tkpack(frame1, fill="x", expand=TRUE, padx=10, pady=10)

  # Frame 2, conversion specification format

  frame2 <- ttklabelframe(tt, relief="flat", borderwidth=5, padding=5,
                          text="Conversion specification format")

  frame2.ent.1 <- ttkentry(frame2, textvariable=fmt.var, width=30)
  frame2.chk.2 <- ttkcheckbutton(frame2, text="Custom", variable=custom.var)

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
  frame2a.but.09 <- ttkbutton(frame2a, width=2, text="i",
                              command=function() AddString("i"))
  frame2a.but.10 <- ttkbutton(frame2a, width=2, text="s",
                              command=function() AddString("s"))

  frame2a.but.11 <- ttkbutton(frame2a, width=2, image=GetBitmapImage("copy"),
                             command=function() print("copy"))
  frame2a.but.12 <- ttkbutton(frame2a, width=2, image=GetBitmapImage("paste"),
                             command=function() print("paste"))
  frame2a.but.13 <- ttkbutton(frame2a, width=2, image=GetBitmapImage("delete"),
                             command=function() print("delete"))

  tkgrid(frame2a.but.01, frame2a.but.02, frame2a.but.03, frame2a.but.04,
         frame2a.but.05, frame2a.but.06, frame2a.but.07, frame2a.but.08,
         frame2a.but.09, frame2a.but.10, frame2a.but.11, frame2a.but.12,
         frame2a.but.13, pady=c(2, 0), padx=c(0, 2))
  tkgrid(frame2.ent.1, frame2.chk.2)
  tkgrid(frame2a, "x", pady=c(2, 0), sticky="w")

  tkgrid.configure(frame2.ent.1, sticky="we", padx=c(0, 2))
  tkgrid.configure(frame2a.but.10, padx=c(0, 10))

  tkgrid.columnconfigure(frame2, 0, weight=1)

  tkpack(frame2, fill="x", padx=10, pady=0)

# Frame 3

  frame3<- ttkframe(tt, relief="flat", borderwidth=0, padding=0)

  frame3.lab.1.1 <- ttklabel(frame3, text="Field width")
  frame3.lab.1.3 <- ttklabel(frame3, text="Precision")

  frame3.ent.1.2 <- ttkentry(frame3, textvariable=width.var, width=12)
  frame3.ent.1.4 <- ttkentry(frame3, textvariable=precision.var, width=12)

  txt <- "Scientific"
  frame3.chk.1.5 <- ttkcheckbutton(frame3, text=txt, variable=scientific.var)
  txt <- "Left adjustment of converted argument in its field."
  frame3.chk.2.1 <- ttkcheckbutton(frame3, text=txt, variable=left.var)
  txt <- "Always print number with sign (\u002b/\u2212)."
  frame3.chk.3.1 <- ttkcheckbutton(frame3, text=txt, variable=sign.var)
  txt <- "Prefix a space if the first character is not a sign."
  frame3.chk.4.1 <- ttkcheckbutton(frame3, text=txt, variable=space.var)
  txt <- "Pad to the field width with leading zeros."
  frame3.chk.5.1 <- ttkcheckbutton(frame3, text=txt, variable=pad.var)

  if (is.numeric(sample)) {
    tkgrid(frame3.lab.1.1, frame3.ent.1.2, frame3.lab.1.3, frame3.ent.1.4,
           frame3.chk.1.5, pady=c(15, 10))
    tkgrid(frame3.chk.2.1, columnspan=5, sticky="w")
    tkgrid(frame3.chk.3.1, columnspan=5, sticky="w")
    tkgrid(frame3.chk.4.1, columnspan=5, sticky="w")
    tkgrid(frame3.chk.5.1, columnspan=5, sticky="w")

    tkgrid.configure(frame3.lab.1.3, padx=c(10, 2))
    tkgrid.configure(frame3.chk.1.5, padx=c(10, 0))
  } else {
    tkgrid(frame3.lab.1.1, frame3.ent.1.2, "x", pady=c(15, 10), sticky="w")
    tkgrid.columnconfigure(frame3, 2, weight=1)
    tkgrid(frame3.chk.2.1, columnspan=3, sticky="w")
  }
  tkgrid.configure(frame3.lab.1.1, padx=c(0, 2))

  tkpack(frame3, padx=10, pady=0, anchor="w")

  # GUI control

  tkgrab(tt)
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tclServiceMode(TRUE)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  new.fmt
}

