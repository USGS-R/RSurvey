ChoosePch <- function(pch=21, parent=NULL) {
# ChoosePch(pch=21)

  # Additional functions (subroutines)

  # Save pch and quit

   SavePch <- function() {
     rtn.pch <<- tclvalue(pch.var)
     tclvalue(tt.done.var) <- 1
   }

  # Frame cell based on mouse location

  MouseMotion <- function(x, y) {
    i <- ceiling(as.numeric(y) / dy)
    j <- ceiling(as.numeric(x) / dx)
    tcl(frame1.cvs, "delete", "brw")
    DrawPolygon(i, j, fill=brw.fill, outline=brw.outline, tag="brw")
    pch <- pch.lst[[(m * (j - 1)) + i]]
    UpdatePch(pch)
  }

  # Draw polygon

  DrawPolygon <- function(i, j, fill, outline, tag) {
    x1 <- j * dx - dx - 0.5
    y1 <- i * dy - dy - 0.5
    x2 <- j * dx - 1.5
    y2 <- i * dy - 1.5
    pts <- .Tcl.args(c(x1, y1, x2, y1, x2, y2, x1, y2))
    tkcreate(frame1.cvs, "polygon", pts, fill=fill, outline=outline, tag=tag)
  }

  # Draw image

  DrawImage <- function() {
    tkcreate(frame1.cvs, "image", center, anchor="center", image=img.var)
  }

  # Update pch

  UpdatePch <- function(pch) {
    if (is.null(pch))
      pch.str <- ""
    else
      pch.str <- if(is.integer(pch)) pch else paste("\"", pch, "\"", sep="")
    tclvalue(pch.var) <- pch.str
  }


  # Main program

  brw.fill <- ""
  sel.fill <- "#FDFEC4"
  brw.outline <- "#CA0020"
  sel.outline <- ""

  pch.lst <- as.list(0:35)
  pch.lst[27:36] <- as.list(c("*", ".", "o", "O", "0", "+", "-", "|", "%", "#"))

  w <- 390
  h <- 330
  m <- 6
  n <- 6

  dx <- w / n
  dy <- h / m
  center <- .Tcl.args(c(w / 2, h / 2))

  rtn.pch <- NULL
  if (!pch %in% pch.lst) {
    pch <- NULL
  } else if (is.numeric(pch)) {
    pch <- as.integer(pch)
  } else {
    pch <- paste("\"", pch, "\"", sep="")
  }

  if ("package:RSurvey" %in% search())
    image.path <- file.path(system.file("images", package="RSurvey"), "pch.gif")
  else
    image.path <- file.path(getwd(), "inst", "images", "pch.gif")

  # Assign variables linked to Tk widgets

  pch.var <- tclVar()
  img.var <- tclVar()
  tt.done.var <- tclVar(0)

  # Open GUI

  tclServiceMode(FALSE)

  tt <- tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(geo[2]) + 25,
                            "+", as.integer(geo[3]) + 25, sep=""))
  }
  tkwm.resizable(tt, 0, 0)
  tktitle(tt) <- "Choose A Graphic Symbol"

  # Create image

  tkimage.create("photo", img.var, format="GIF", file=image.path)

  # Frame 0 contains ok and cancel buttons

  frame0 <- ttkframe(tt, relief="flat")

  frame0.lab.1 <- ttklabel(frame0, text="Graphic symbol, pch =")
  frame0.ent.2 <- ttkentry(frame0, textvariable=pch.var, width=6)
  frame0.but.3 <- ttkbutton(frame0, width=12, text="OK", command=SavePch)
  frame0.but.4 <- ttkbutton(frame0, width=12, text="Cancel",
                            command=function() {
                              pch <<- NULL
                              tclvalue(tt.done.var) <- 1
                            })

  tkgrid(frame0.lab.1, frame0.ent.2, "x", frame0.but.3, frame0.but.4,
         pady=c(0, 10))
  tkgrid.columnconfigure(frame0, 2, weight=1)

  tkgrid.configure(frame0.lab.1, sticky="w", padx=c(10, 0))
  tkgrid.configure(frame0.ent.2, padx=c(2, 0))


  tkgrid.configure(frame0.but.3, sticky="e", padx=c(0, 4))
  tkgrid.configure(frame0.but.4, sticky="w", padx=c(0, 10))

  tkpack(frame0, fill="x", side="bottom", anchor="e")

  # Canvas

  frame1 <- ttkframe(tt, relief="flat")
  frame1.cvs <- tkcanvas(frame1, relief="flat", width=w, height=h,
                         background="white", confine=TRUE, closeenough=0,
                         borderwidth=0, highlightthickness=0)
  tkgrid(frame1.cvs, padx=10, pady=10)
  tkpack(frame1)

  # Draw intial polyon and image

  if (!is.null(pch)) {
    idx <- which(pch.lst %in% pch)
    j <- ceiling(idx / m)
    i <- idx - m * (j - 1L)
    DrawPolygon(i, j, fill=sel.fill, outline=sel.outline, tag="sel")
  }
  DrawImage()

  # Binds on canvas

  tkbind(frame1.cvs, "<ButtonPress>", SavePch)
  tkbind(frame1.cvs, "<Motion>", function(x, y) MouseMotion(x, y))
  tkbind(frame1.cvs, "<Leave>", function() tcl(frame1.cvs, "delete", "brw"))

  # GUI control

  tkfocus(tt)
  tkgrab(tt)
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tclServiceMode(TRUE)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  rtn.pch
}
