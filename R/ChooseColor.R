ChooseColor <- function(col=NA, parent=NULL) {
# ChooseColor("#669933")

  # Additional functions (subroutines)

  # Save color and quit

  SaveColor <- function() {
    hex <- TxtToHex(tclvalue(col.var))
    if (hex == "")
      hex <- NA
    rtn.col <<- hex
    tclvalue(tt.done.var) <- 1
  }

  # Draw polygon on small canvas

  DrawPolygonSmall <- function(color) {
    tcl(frame0.cvs.1, "delete", "col")
    pts <- .Tcl.args(c(0, 0, dx, 0, dx, dy, 0, dy))
    tkcreate(frame0.cvs.1, "polygon", pts, fill=color, outline="", tag="col")
  }

  # Draw polygon on large canvas

  DrawPolygonLarge <- function(i, j, fill, outline, tag) {
    x1 <- j * dx - dx - 0.5
    y1 <- i * dy - dy - 0.5
    x2 <- j * dx - 0.5
    y2 <- i * dy - 0.5
    pts <- .Tcl.args(c(x1, y1, x2, y1, x2, y2, x1, y2))
    tkcreate(frame1.cvs, "polygon", pts, fill=fill, outline=outline, tag=tag)
  }

  # Draw color chart

  DrawColorChart <- function() {
    for (i in 1:m) {
      for (j in 1:n) {
        DrawPolygonLarge(i, j, fill=d[i, j], outline="#FFFFFF", tag="")
      }
    }
  }

  # Frame cell based on mouse location

  MouseMotion <- function(x, y) {
    i <- ceiling((as.numeric(y)) / dy)
    j <- ceiling((as.numeric(x)) / dx)
    tcl(frame1.cvs, "delete", "browse")
    if (i == 0 || j == 0) {
      color <- ""
    } else {
      color <- d[i, j]
      DrawPolygonSmall(color)
      DrawPolygonLarge(i, j, fill="", outline="#000000", tag="browse")
    }
    tclvalue(col.var) <- color
  }

  # Mouse leaves canvas

  MouseLeavesCanvas <- function() {
    tclvalue(col.var) <- ""
    UpdateColor()
  }

  # Update color based on text string in entry-box

  UpdateColor <- function() {
    color <- TxtToHex(tclvalue(col.var))
    tcl(frame1.cvs, "delete", "browse")
    if (color %in% d) {
      ij <- which(d == color, arr.ind=TRUE)
      i <- ij[1]
      j <- ij[2]

      DrawPolygonLarge(i, j, fill="", outline="#000000", tag="browse")
    }
    if (is.na(color) || color == "")
      color <- "#FFFFFF"
    DrawPolygonSmall(color)
  }

  # Coerces text string to hexadecimal color string

  TxtToHex <- function(txt) {
    txt <- as.character(txt)
    if (txt %in% d | txt == "") {
      hex <- txt
    } else {
      sep.txt <- strsplit(txt, "")[[1]]
      idxs <- which(sapply(sep.txt, function(i) i %in% hex.digits))
      txt <- paste(sep.txt[idxs], collapse="")
      tclvalue(col.var) <- txt

      if (substr(txt, 1, 1) != "#")
        txt <- paste("#", txt, sep="")

      nc <- nchar(txt)
      if (nc > 7) {
        txt <- substr(txt, 1, 7)
      } else if (nc < 7) {
        code <- substr(txt, 2, nc)
        txt <- paste("#", gsub(" ", "0", sprintf("%06s", code)), sep="")
      }

      if (inherits(try(col2rgb(txt), silent=TRUE), "try-error"))
        hex <- ""
      else
        hex <- txt
    }
    hex
  }


  # Main program

  w <- 400
  h <- 240
  m <- 12
  n <- 20
  d <- matrix(c("#000000", "#000000", "#000000", "#003300", "#006600",
                "#009900", "#00CC00", "#00FF00", "#330000", "#333300",
                "#336600", "#339900", "#33CC00", "#33FF00", "#660000",
                "#663300", "#666600", "#669900", "#66CC00", "#66FF00",
                "#333333", "#000000", "#000033", "#003333", "#006633",
                "#009933", "#00CC33", "#00FF33", "#330033", "#333333",
                "#336633", "#339933", "#33CC33", "#33FF33", "#660033",
                "#663333", "#666633", "#669933", "#66CC33", "#66FF33",
                "#666666", "#000000", "#000066", "#003366", "#006666",
                "#009966", "#00CC66", "#00FF66", "#330066", "#333366",
                "#336666", "#339966", "#33CC66", "#33FF66", "#660066",
                "#663366", "#666666", "#669966", "#66CC66", "#66FF66",
                "#999999", "#000000", "#000099", "#003399", "#006699",
                "#009999", "#00CC99", "#00FF99", "#330099", "#333399",
                "#336699", "#339999", "#33CC99", "#33FF99", "#660099",
                "#663399", "#666699", "#669999", "#66CC99", "#66FF99",
                "#CCCCCC", "#000000", "#0000CC", "#0033CC", "#0066CC",
                "#0099CC", "#00CCCC", "#00FFCC", "#3300CC", "#3333CC",
                "#3366CC", "#3399CC", "#33CCCC", "#33FFCC", "#6600CC",
                "#6633CC", "#6666CC", "#6699CC", "#66CCCC", "#66FFCC",
                "#FFFFFF", "#000000", "#0000FF", "#0033FF", "#0066FF",
                "#0099FF", "#00CCFF", "#00FFFF", "#3300FF", "#3333FF",
                "#3366FF", "#3399FF", "#33CCFF", "#33FFFF", "#6600FF",
                "#6633FF", "#6666FF", "#6699FF", "#66CCFF", "#66FFFF",
                "#FF0000", "#000000", "#990000", "#993300", "#996600",
                "#999900", "#99CC00", "#99FF00", "#CC0000", "#CC3300",
                "#CC6600", "#CC9900", "#CCCC00", "#CCFF00", "#FF0000",
                "#FF3300", "#FF6600", "#FF9900", "#FFCC00", "#FFFF00",
                "#00FF00", "#000000", "#990033", "#993333", "#996633",
                "#999933", "#99CC33", "#99FF33", "#CC0033", "#CC3333",
                "#CC6633", "#CC9933", "#CCCC33", "#CCFF33", "#FF0033",
                "#FF3333", "#FF6633", "#FF9933", "#FFCC33", "#FFFF33",
                "#0000FF", "#000000", "#990066", "#993366", "#996666",
                "#999966", "#99CC66", "#99FF66", "#CC0066", "#CC3366",
                "#CC6666", "#CC9966", "#CCCC66", "#CCFF66", "#FF0066",
                "#FF3366", "#FF6666", "#FF9966", "#FFCC66", "#FFFF66",
                "#FFFF00", "#000000", "#990099", "#993399", "#996699",
                "#999999", "#99CC99", "#99FF99", "#CC0099", "#CC3399",
                "#CC6699", "#CC9999", "#CCCC99", "#CCFF99", "#FF0099",
                "#FF3399", "#FF6699", "#FF9999", "#FFCC99", "#FFFF99",
                "#00FFFF", "#000000", "#9900CC", "#9933CC", "#9966CC",
                "#9999CC", "#99CCCC", "#99FFCC", "#CC00CC", "#CC33CC",
                "#CC66CC", "#CC99CC", "#CCCCCC", "#CCFFCC", "#FF00CC",
                "#FF33CC", "#FF66CC", "#FF99CC", "#FFCCCC", "#FFFFCC",
                "#FF00FF", "#000000", "#9900FF", "#9933FF", "#9966FF",
                "#9999FF", "#99CCFF", "#99FFFF", "#CC00FF", "#CC33FF",
                "#CC66FF", "#CC99FF", "#CCCCFF", "#CCFFFF", "#FF00FF",
                "#FF33FF", "#FF66FF", "#FF99FF", "#FFCCFF", "#FFFFFF"),
            nrow=m, ncol=n, byrow=TRUE)

  hex.digits <- list("#", 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                     "a", "b", "c", "d", "e", "f",
                     "A", "B", "C", "D", "E", "F")

  dx <- w / n
  dy <- h / m

  if (is.na(col) | !inherits(col, "character"))
    col <- ""
  rtn.col <- NULL

  # Assign variables linked to Tk widgets

  col.var <- tclVar(TxtToHex(col))
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
  tktitle(tt) <- "Choose Color"

  # Frame 0 contains ok and cancel buttons

  frame0 <- ttkframe(tt, relief="flat")

  frame0.cvs.1 <- tkcanvas(frame0, relief="flat", width=dx - 1, height=dy - 1,
                           background="white", confine=TRUE, closeenough=0,
                           borderwidth=0, highlightthickness=0)
  frame0.ent.2 <- ttkentry(frame0, textvariable=col.var, width=10)


  frame0.but.4 <- ttkbutton(frame0, width=12, text="OK", command=SaveColor)
  frame0.but.5 <- ttkbutton(frame0, width=12, text="Cancel",
                            command=function() {
                              rtn.col <<- NULL
                              tclvalue(tt.done.var) <- 1
                            })

  tkgrid(frame0.cvs.1, frame0.ent.2, "x", frame0.but.4, frame0.but.5,
         pady=c(0, 10))
  tkgrid.columnconfigure(frame0, 2, weight=1)

  tkgrid.configure(frame0.cvs.1, sticky="w", padx=c(11, 1), pady=c(1, 11))
  tkgrid.configure(frame0.ent.2, padx=c(5, 0))

  tkgrid.configure(frame0.but.4, sticky="e", padx=c(0, 4))
  tkgrid.configure(frame0.but.5, sticky="w", padx=c(0, 10), rowspan=2)

  tkpack(frame0, fill="x", side="bottom", anchor="e")

  # Canvas

  frame1 <- ttkframe(tt, relief="flat")
  frame1.cvs <- tkcanvas(frame1, relief="flat", width=w + 1, height=h + 1,
                         background="white", confine=TRUE, closeenough=0,
                         borderwidth=0, highlightthickness=0)
  tkgrid(frame1.cvs, padx=10, pady=10)
  tkpack(frame1)

  DrawColorChart()

  UpdateColor()

  # Binds on canvas

  tkbind(frame1.cvs, "<ButtonPress>", SaveColor)
  tkbind(frame1.cvs, "<Motion>", function(x, y) MouseMotion(x, y))
  tkbind(frame1.cvs, "<Leave>", MouseLeavesCanvas)
  tkbind(frame0.ent.2, "<KeyRelease>", UpdateColor)
  tkbind(frame0.ent.2, "<Return>", SaveColor)

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

  rtn.col
}
