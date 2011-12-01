ChooseColor <- function(col, parent=NULL) {
# A GUI for selecting a graphic color.

  # Additional functions (subroutines)

  # Save color and quit

  SaveColor <- function() {
    col.hex <- Txt2Hex(tclvalue(col.var))
    if (col.hex == "")
      col.hex <- NA
    rtn.col <<- col.hex
    tclvalue(tt.done.var) <- 1
  }

  # Draw polygon on small canvas

  DrawPolygonSmall <- function(fill) {
    tcl(frame0.cvs.1, "delete", "col")
    pts <- .Tcl.args(c(0, 0, dx, 0, dx, dy, 0, dy))
    tkcreate(frame0.cvs.1, "polygon", pts, fill=fill, outline="", tag="col")
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
        DrawPolygonLarge(i, j, fill=d[i, j], outline="black", tag="")
      }
    }
  }

  # Frame cell based on mouse selection

  MouseSelect <- function(x, y) {
    tcl(frame1.cvs, "delete", "browse")
    i <- ceiling((as.numeric(y)) / dy)
    j <- ceiling((as.numeric(x)) / dx)
    if (i == 0)
      i <- 1
    if (j == 0)
      j <- 1
    col.hex <- d[i, j]
    DrawPolygonSmall(col.hex)
    DrawPolygonLarge(i, j, fill="", outline="white", tag="browse")

    col.rgb <- col2rgb(col.hex, alpha=FALSE)
    col.hsv <- rgb2hsv(col.rgb, maxColorValue=255)

    nh <<- col.hsv[1]
    ns <<- col.hsv[2]
    nv <<- col.hsv[3]

    if (is.transparent)
      col.hex <- rgb(col.rgb[1], col.rgb[2], col.rgb[3], na * 255,
                     maxColorValue=255)
    tclvalue(col.var) <- col.hex

    tclvalue(h.scl.var) <- nh
    tclvalue(s.scl.var) <- ns
    tclvalue(v.scl.var) <- nv
    tclvalue(h.ent.var) <- sprintf("%.2f", nh)
    tclvalue(s.ent.var) <- sprintf("%.2f", ns)
    tclvalue(v.ent.var) <- sprintf("%.2f", nv)
  }

  # Update color polygons based on text string in entry-box

  UpdatePolygons <- function(txt) {
    col.hex <- substring(txt, 1, 7)
    tcl(frame1.cvs, "delete", "browse")
    if (col.hex %in% d) {
      ij <- which(d == col.hex, arr.ind=TRUE)[1, ]
      i <- ij[1]
      j <- ij[2]
      DrawPolygonLarge(i, j, fill="", outline="white", tag="browse")
    }
    if (is.na(col.hex) || col.hex == "")
      col.hex <- "#000000"
    DrawPolygonSmall(col.hex)
  }

  # Coerces text string to hexadecimal color string

  Txt2Hex <- function(txt) {
    txt <- CheckColorStr(as.character(txt))
    if (substr(txt, 1, 1) != "#")
      txt <- paste("#", txt, sep="")
    if (is.transparent)
      fmt <- "%08s"
    else
      fmt <- "%06s"
    fmt.txt <- gsub(" ", "0", sprintf(fmt, substr(txt, 2, nchar(txt))))
    txt <- paste("#", fmt.txt, sep="")
    if (inherits(try(col2rgb(txt), silent=TRUE), "try-error")) {
      if (is.transparent)
        txt <- "#000000FF"
      else
        txt <- "#000000"
    }
    txt
  }

  # Coerces numeric values to hexadecimal color string

  Num2Hex <- function() {
    if (is.transparent)
      col.hex <- hsv(h=nh, s=ns, v=nv, alpha=na)
    else
      col.hex <- hsv(h=nh, s=ns, v=nv)
    col.hex
  }

  # Check range of numeric color component

  CheckColorNum <- function(...) {
    num <- as.numeric(...)
    if (is.na(num) || num < 0) {
      num <- 0
    } else if (num > 1) {
      num <- 1
    }
    num
  }

  # Check digits of hexadecimal color character string

  CheckColorStr <- function(txt) {
    txt <- as.character(txt)
    if (is.transparent)
      txt <- substring(txt, 1, 9)
    else
      txt <- substring(txt, 1, 7)
    sep.txt <- strsplit(txt, "")[[1]]
    idxs <- which(sapply(sep.txt, function(i) i %in% hex.digits))
    txt <- paste(sep.txt[idxs], collapse="")
    txt
  }

  # Updates based on change in scale

  ScaleH <- function(...) {
    nh <<- CheckColorNum(...)
    tclvalue(h.ent.var) <- sprintf("%.2f", nh)
    tclvalue(col.var) <- Num2Hex()
    UpdatePolygons(tclvalue(col.var))
  }

  ScaleS <- function(...) {
    ns <<- CheckColorNum(...)
    tclvalue(s.ent.var) <- sprintf("%.2f", ns)
    tclvalue(col.var) <- Num2Hex()
    UpdatePolygons(tclvalue(col.var))
  }

  ScaleV <- function(...) {
    nv <<- CheckColorNum(...)
    tclvalue(v.ent.var) <- sprintf("%.2f", nv)
    tclvalue(col.var) <- Num2Hex()
    UpdatePolygons(tclvalue(col.var))
  }

  ScaleA <- function(...) {
    na <<- CheckColorNum(...)
    tclvalue(a.ent.var) <- sprintf("%.2f", na)
    tclvalue(col.var) <- Num2Hex()
    UpdatePolygons(tclvalue(col.var))
  }

  # Updates based on change in entry of numeric colors

  EntryH <- function() {
    txt <- as.character(tclvalue(h.ent.var))
    nh <<- CheckColorNum(txt)
    tclvalue(h.ent.var) <- txt
    tclvalue(h.scl.var) <- nh
    tclvalue(col.var) <- Num2Hex()
    UpdatePolygons(tclvalue(col.var))
  }

  EntryS <- function() {
    txt <- as.character(tclvalue(s.ent.var))
    ns <<- CheckColorNum(txt)
    tclvalue(s.ent.var) <- txt
    tclvalue(s.scl.var) <- ns
    tclvalue(col.var) <- Num2Hex()
    UpdatePolygons(tclvalue(col.var))
  }

  EntryV <- function() {
    txt <- as.character(tclvalue(v.ent.var))
    nv <<- CheckColorNum(txt)
    tclvalue(v.ent.var) <- txt
    tclvalue(v.scl.var) <- nv
    tclvalue(col.var) <- Num2Hex()
    UpdatePolygons(tclvalue(col.var))
  }

  EntryA <- function() {
    txt <- as.character(tclvalue(a.ent.var))
    na <<- CheckColorNum(txt)
    tclvalue(a.ent.var) <- txt
    tclvalue(a.scl.var) <- na
    tclvalue(col.var) <- Num2Hex()
    UpdatePolygons(tclvalue(col.var))
  }

  # Toggle transparency check-box

  ToggleTransparency <- function() {
    is.transparent <<- as.logical(as.integer(tclvalue(trans.var)))
    if (is.transparent) {
      tkconfigure(frame2.lab.4.1, state="noraml")
      tcl(frame2.scl.4.2, "state", "!disabled")
      tkconfigure(frame2.ent.4.3, state="noraml")
    } else {
      tkconfigure(frame2.lab.4.1, state="disabled")
      tcl(frame2.scl.4.2, "state", "disabled")
      tkconfigure(frame2.ent.4.3, state="disabled")
      na <<- 1
      tclvalue(a.scl.var) <- na
      tclvalue(a.ent.var) <- sprintf("%.2f", na)
    }
    tclvalue(col.var) <- Num2Hex()
  }


  # Main program

  # Color chart information

  w <- 400
  h <- 240
  m <- 12
  n <- 20
  d <- matrix(c("#0247FE", "#000000", "#000000", "#003300", "#006600",
                "#009900", "#00CC00", "#00FF00", "#330000", "#333300",
                "#336600", "#339900", "#33CC00", "#33FF00", "#660000",
                "#663300", "#666600", "#669900", "#66CC00", "#66FF00",
                "#0392CE", "#1B1B1A", "#000033", "#003333", "#006633",
                "#009933", "#00CC33", "#00FF33", "#330033", "#333333",
                "#336633", "#339933", "#33CC33", "#33FF33", "#660033",
                "#663333", "#666633", "#669933", "#66CC33", "#66FF33",
                "#66B032", "#333333", "#000066", "#003366", "#006666",
                "#009966", "#00CC66", "#00FF66", "#330066", "#333366",
                "#336666", "#339966", "#33CC66", "#33FF66", "#660066",
                "#663366", "#666666", "#669966", "#66CC66", "#66FF66",
                "#D0EA2B", "#4E4E4E", "#000099", "#003399", "#006699",
                "#009999", "#00CC99", "#00FF99", "#330099", "#333399",
                "#336699", "#339999", "#33CC99", "#33FF99", "#660099",
                "#663399", "#666699", "#669999", "#66CC99", "#66FF99",
                "#FEFE33", "#666666", "#0000CC", "#0033CC", "#0066CC",
                "#0099CC", "#00CCCC", "#00FFCC", "#3300CC", "#3333CC",
                "#3366CC", "#3399CC", "#33CCCC", "#33FFCC", "#6600CC",
                "#6633CC", "#6666CC", "#6699CC", "#66CCCC", "#66FFCC",
                "#FABC02", "#808081", "#0000FF", "#0033FF", "#0066FF",
                "#0099FF", "#00CCFF", "#00FFFF", "#3300FF", "#3333FF",
                "#3366FF", "#3399FF", "#33CCFF", "#33FFFF", "#6600FF",
                "#6633FF", "#6666FF", "#6699FF", "#66CCFF", "#66FFFF",
                "#FB9902", "#9A999A", "#990000", "#993300", "#996600",
                "#999900", "#99CC00", "#99FF00", "#CC0000", "#CC3300",
                "#CC6600", "#CC9900", "#CCCC00", "#CCFF00", "#FF0000",
                "#FF3300", "#FF6600", "#FF9900", "#FFCC00", "#FFFF00",
                "#FD5308", "#B4B4B4", "#990033", "#993333", "#996633",
                "#999933", "#99CC33", "#99FF33", "#CC0033", "#CC3333",
                "#CC6633", "#CC9933", "#CCCC33", "#CCFF33", "#FF0033",
                "#FF3333", "#FF6633", "#FF9933", "#FFCC33", "#FFFF33",
                "#FE2712", "#CCCCCD", "#990066", "#993366", "#996666",
                "#999966", "#99CC66", "#99FF66", "#CC0066", "#CC3366",
                "#CC6666", "#CC9966", "#CCCC66", "#CCFF66", "#FF0066",
                "#FF3366", "#FF6666", "#FF9966", "#FFCC66", "#FFFF66",
                "#A7194B", "#E6E6E6", "#990099", "#993399", "#996699",
                "#999999", "#99CC99", "#99FF99", "#CC0099", "#CC3399",
                "#CC6699", "#CC9999", "#CCCC99", "#CCFF99", "#FF0099",
                "#FF3399", "#FF6699", "#FF9999", "#FFCC99", "#FFFF99",
                "#8601AF", "#F3F3F3", "#9900CC", "#9933CC", "#9966CC",
                "#9999CC", "#99CCCC", "#99FFCC", "#CC00CC", "#CC33CC",
                "#CC66CC", "#CC99CC", "#CCCCCC", "#CCFFCC", "#FF00CC",
                "#FF33CC", "#FF66CC", "#FF99CC", "#FFCCCC", "#FFFFCC",
                "#3D01A4", "#FFFFFF", "#9900FF", "#9933FF", "#9966FF",
                "#9999FF", "#99CCFF", "#99FFFF", "#CC00FF", "#CC33FF",
                "#CC66FF", "#CC99FF", "#CCCCFF", "#CCFFFF", "#FF00FF",
                "#FF33FF", "#FF66FF", "#FF99FF", "#FFCCFF", "#FFFFFF"),
            nrow=m, ncol=n, byrow=TRUE)

  dx <- w / n
  dy <- h / m

  # All possible digits in color character string

  hex.digits <- list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                     "a", "b", "c", "d", "e", "f",
                     "A", "B", "C", "D", "E", "F", "#")

  # Initialize color to return

  rtn.col <- NULL

  # Account for improper color argument (col)

  if (missing(col) || !inherits(col, "character"))
    col <- "#000000"

 # Transparency status

  if (nchar(col) == 9)
    is.transparent <- TRUE
  else
    is.transparent <- FALSE

  # Color is intially required to be in hexadecimal format

  col.hex <- Txt2Hex(col)

  # Initialize hue, saturation, value, and alpha color components

  col.rgb <- col2rgb(col.hex, alpha=TRUE)
  col.hsv <- rgb2hsv(col.rgb[1:3], maxColorValue=255)
  nh <- col.hsv[1]
  ns <- col.hsv[2]
  nv <- col.hsv[3]
  na <- col.rgb[4] / 255

  # Assign additional variables linked to Tk widgets

  col.var <- tclVar(col.hex)

  trans.var <- tclVar(is.transparent)

  h.scl.var <- tclVar(nh)
  s.scl.var <- tclVar(ns)
  v.scl.var <- tclVar(nv)
  a.scl.var <- tclVar(na)
  h.ent.var <- tclVar(sprintf("%.2f", nh))
  s.ent.var <- tclVar(sprintf("%.2f", ns))
  v.ent.var <- tclVar(sprintf("%.2f", nv))
  a.ent.var <- tclVar(sprintf("%.2f", na))

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

  # Frame 0, ok and cancel buttons

  frame0 <- ttkframe(tt, relief="flat")

  frame0.cvs.1 <- tkcanvas(frame0, relief="flat", width=dx - 1, height=dy - 1,
                           background="white", confine=TRUE, closeenough=0,
                           borderwidth=0, highlightthickness=0)
  frame0.ent.2 <- ttkentry(frame0, textvariable=col.var, width=12)
  frame0.chk.3 <- ttkcheckbutton(frame0, variable=trans.var, text="Transparency",
                                 command=ToggleTransparency)

  frame0.but.5 <- ttkbutton(frame0, width=12, text="OK", command=SaveColor)
  frame0.but.6 <- ttkbutton(frame0, width=12, text="Cancel",
                            command=function() {
                              rtn.col <<- NULL
                              tclvalue(tt.done.var) <- 1
                            })

  tkgrid(frame0.cvs.1, frame0.ent.2,  frame0.chk.3, "x", frame0.but.5, frame0.but.6,
         pady=c(10, 10))
  tkgrid.columnconfigure(frame0, 3, weight=1)

  tkgrid.configure(frame0.cvs.1, sticky="w", padx=c(11, 1), pady=c(11, 11))
  tkgrid.configure(frame0.ent.2, padx=c(4, 4))

  tkgrid.configure(frame0.but.5, sticky="e", padx=c(0, 4))
  tkgrid.configure(frame0.but.6, sticky="w", padx=c(0, 10), rowspan=2)

  tkpack(frame0, fill="x", side="bottom", anchor="e")

  # Frame 1, color chart

  frame1 <- ttkframe(tt, relief="flat")
  frame1.cvs <- tkcanvas(frame1, relief="flat", width=w + 1, height=h + 1,
                         background="black", confine=TRUE, closeenough=0,
                         borderwidth=0, highlightthickness=0)
  tkgrid(frame1.cvs, padx=10, pady=10)
  tkpack(frame1)

  DrawColorChart()
  UpdatePolygons(col.hex)

  # Frame 2, red, blue, green, and alpha sliders

  frame2 <- ttkframe(tt, relief="flat")

  frame2.lab.1.1 <- ttklabel(frame2, text="H:")
  frame2.lab.2.1 <- ttklabel(frame2, text="S:")
  frame2.lab.3.1 <- ttklabel(frame2, text="V:")
  frame2.lab.4.1 <- ttklabel(frame2, text="A:")

  frame2.scl.1.2 <- tkwidget(frame2, "ttk::scale", from=0, to=1,
                             orient="horizontal", value=nh, variable=h.scl.var,
                             command=function(...) ScaleH(...))
  frame2.scl.2.2 <- tkwidget(frame2, "ttk::scale", from=0, to=1,
                             orient="horizontal", value=ns, variable=s.scl.var,
                             command=function(...) ScaleS(...))
  frame2.scl.3.2 <- tkwidget(frame2, "ttk::scale", from=0, to=1,
                             orient="horizontal", value=nv, variable=v.scl.var,
                             command=function(...) ScaleV(...))
  frame2.scl.4.2 <- tkwidget(frame2, "ttk::scale", from=0, to=1,
                             orient="horizontal", value=na, variable=a.scl.var,
                             command=function(...) ScaleA(...))

  frame2.ent.1.3 <- ttkentry(frame2, textvariable=h.ent.var, width=4)
  frame2.ent.2.3 <- ttkentry(frame2, textvariable=s.ent.var, width=4)
  frame2.ent.3.3 <- ttkentry(frame2, textvariable=v.ent.var, width=4)
  frame2.ent.4.3 <- ttkentry(frame2, textvariable=a.ent.var, width=4)

  ToggleTransparency()

  tkgrid(frame2.lab.1.1, frame2.scl.1.2, frame2.ent.1.3, pady=c(0, 5))
  tkgrid(frame2.lab.2.1, frame2.scl.2.2, frame2.ent.2.3, pady=c(0, 5))
  tkgrid(frame2.lab.3.1, frame2.scl.3.2, frame2.ent.3.3, pady=c(0, 5))
  tkgrid(frame2.lab.4.1, frame2.scl.4.2, frame2.ent.4.3)

  tkgrid.configure(frame2.lab.1.1, frame2.lab.2.1, frame2.lab.3.1,
                   frame2.lab.4.1, sticky="e", padx=c(10, 2))
  tkgrid.configure(frame2.scl.1.2, frame2.scl.2.2, frame2.scl.3.2,
                   frame2.scl.4.2, sticky="we", padx=2)
  tkgrid.configure(frame2.ent.1.3, frame2.ent.2.3, frame2.ent.3.3,
                   frame2.ent.4.3, padx=c(10, 10))

  tkgrid.columnconfigure(frame2, 1, weight=1)

  tkpack(frame2, fill="x")

  # Bind events

  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tkbind(frame0.ent.2, "<KeyRelease>",
         function() {
           tclvalue(col.var) <- CheckColorStr(tclvalue(col.var))
           color <- Txt2Hex(tclvalue(col.var))
           UpdatePolygons(color)
         })
  tkbind(frame0.ent.2, "<Return>", SaveColor)

  tkbind(frame1.cvs, "<ButtonPress>", function(x, y) MouseSelect(x, y))

  tkbind(frame2.ent.1.3, "<KeyRelease>", EntryH)
  tkbind(frame2.ent.2.3, "<KeyRelease>", EntryS)
  tkbind(frame2.ent.3.3, "<KeyRelease>", EntryV)
  tkbind(frame2.ent.4.3, "<KeyRelease>", EntryA)

  # GUI control

  tkfocus(tt)
  tkgrab(tt)

  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  rtn.col
}
