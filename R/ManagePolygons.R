ManagePolygons <- function(ply=NULL, encoding=getOption("encoding"),
                           parent=NULL) {
  # A GUI for managing and manipulating polygons that is based
  # on the gpclib package.

  # Additional functions (subroutines)

  # Save polygon

  SavePolygon <- function(type) {
    rtn <<- list()
    if (length(ply) > 0) {
      rtn <<- ply[sapply(ply, function(i) inherits(i, "gpc.poly"))]

      if (exists("Data") && !identical(rtn, Data("poly"))) {
        Data("poly", if (identical(rtn, list())) NULL else Data("poly", rtn))

        Data("poly.data", poly.data)
        Data("poly.crop", poly.crop)

        if (!is.null(poly.data)) {
          missing.poly <- !poly.data %in% names(rtn)
          updated.poly <- !identical(rtn[[poly.data]], old.pdata)
          if (missing.poly | updated.poly) {
            Data("poly.data", NULL)
            Data("data.pts", NULL)
            Data("data.grd", NULL)
          }
        }
        if (!is.null(poly.crop)) {
          missing.poly <- !poly.crop %in% names(rtn)
          updated.poly <- !identical(rtn[[poly.crop]], old.pcrop)
           if (missing.poly | updated.poly) {
            Data("poly.crop", NULL)
            Data("data.grd", NULL)
          }
        }
      }
    }
    if (type == "ok")
      tclvalue(tt.done.var) <- 1
  }

  # Plot polygon

  PlotPolygon <- function(){

    # Draw polygon in canvas

    DrawPolygon <- function(contours, tag="", col.line="", col.fill="") {
      for (cnt in contours) {
        pts <- Xy2mn(cnt$x, cnt$y)
        mn <- rep(NA, length(pts$m) * 2)
        is.odd <- !array(0:1, length(mn))
        mn[ is.odd] <- pts$m
        mn[!is.odd] <- pts$n
        tkcreate(frame2.cvs, "polygon", .Tcl.args(mn), fill=col.fill,
                 outline=col.line, width=1, tag=tag)
      }
    }

    idxs <- as.integer(tkcurselection(frame1.lst)) + 1

    tcl(frame2.cvs, "delete", "all")
    xran <<- NULL
    yran <<- NULL

    tclvalue(area.var) <- ""
    tclvalue(poly.var) <- ""
    tclvalue(hole.var) <- ""
    tclvalue(vert.var) <- ""

    base.ply <<- NULL

    if (length(idxs) == 0)
      return()

    for (idx in idxs) {
      bb <- get.bbox(ply[[idx]])
      xran <- range(c(xran, bb$x))
      yran <- range(c(yran, bb$y))
    }
    xran <<- extendrange(xran, f=0.02)
    yran <<- extendrange(yran, f=0.02)

    cmd <- tclvalue(rb.var)
    base.ply <<- ply[[idxs[1]]]
    for (idx in idxs[-1]) {
      if (cmd == "add") {
        base.ply <<- union(base.ply, ply[[idx]])
      } else if (cmd == "sub") {
        base.ply <<- setdiff(base.ply, ply[[idx]])
      } else if (cmd == "int") {
        base.ply <<- intersect(base.ply, ply[[idx]])
      }
    }
    if (cmd == "exc" && length(idxs) > 1) {
      union.ply <- base.ply
      inter.ply <- base.ply
      for (idx in idxs[-1]) {
        union.ply <- union(union.ply, ply[[idx]])
        inter.ply <- intersect(inter.ply, ply[[idx]])
      }
      base.ply <<- setdiff(union.ply, inter.ply)
    }

    base.pts <- get.pts(base.ply)

    if (length(base.pts) == 0) {
      base.ply <<- NULL
    } else {
      hole <- NULL
      vert <- 0
      for (ctr in base.pts) {
        hole <- append(hole, ctr$hole)
        vert <- vert + length(ctr$x)
      }
      if (!is.null(hole)) {
        DrawPolygon(base.pts[!hole], col.fill="#FDFEC4")
        if (any(hole))
          DrawPolygon(base.pts[hole], col.fill="white")
      }
      tclvalue(area.var) <- format(area.poly(base.ply))
      tclvalue(poly.var) <- length(base.pts)
      tclvalue(hole.var) <- sum(hole)
      tclvalue(vert.var) <- vert
    }

    for (i in idxs)
      DrawPolygon(get.pts(ply[[i]]), tag=names(ply)[i], col.line=col.pal[i])
  }

  # Transform coordinates from real to canvas

  Xy2mn <- function(x, y) {
    m <- w * ((x - xran[1]) / diff(xran))
    n <- h - (h * ((y - yran[1]) / diff(yran)))
    list(m=m, n=n)
  }

  # Transform coordinates from canvas to real

  Mn2xy <- function(m, n) {
    x <- (m * diff(xran) + w * xran[1]) / w
    y <- (h * yran[1] - (n - h) * diff(yran)) / h
    list(x=x, y=y)
  }

  # Scale objects in canvas based on canvas size

  ScaleCanvas <- function() {
    w0 <- w
    h0 <- h
    w <<- as.numeric(tkwinfo("width",  frame2.cvs))
    h <<- as.numeric(tkwinfo("height", frame2.cvs))
    tcl(frame2.cvs, "scale", "all", 0, 0, w / w0, h / h0)
  }

  # Update pointer coordinates

  MouseMotion <- function(x, y) {
    if (!is.null(xran)) {
      pnt <- Mn2xy(as.numeric(x), as.numeric(y))
      tclvalue(xy.var) <- paste(format(pnt$x), format(pnt$y), sep=", ")
    }
  }

  # Remove pointer coordinates after leaving canvas

  MouseLeave <- function() {
    tclvalue(xy.var) <- ""
  }

  # Name polygon

  NamePolygon <- function(old=NULL, nam=NA){
    if (is.na(nam))
      nam <- "New Polygon"
    i <- 1
    hld <- nam
    while (nam %in% old) {
      nam <- paste(hld, " (", i, ")", sep="")
      i <- i + 1
    }
    nam
  }

  # Rename polygon

  RenamePolygon <- function() {
    if (length(ply) == 0)
      return()

    old.names <- names(ply)
    cur.name <- NULL

    idxs <- as.integer(tkcurselection(frame1.lst)) + 1
    if (length(idxs) != 0)
      cur.name <- old.names[idxs[1]]

    new.names <- Rename(old.names, cur.name, "Rename Polygon", tt)

    if (!is.null(new.names) && length(new.names) == length(old.names)) {
      if (identical(new.names, old.names))
        return()
      names(ply) <<- new.names

      if (!is.null(poly.data))
        poly.data <<- new.names[which(old.names %in% poly.data)]
      if (!is.null(poly.crop))
        poly.crop <<- new.names[which(old.names %in% poly.crop)]
    }

    for (i in seq(along=new.names)) {
      tclvalue(list.var) <- tcl("lreplace", tclvalue(list.var),
                                i - 1, i - 1, new.names[i])
    }
  }

  # Save new polygon

  SaveNewPolygon <- function() {
    if (is.null(base.ply))
      return()

    nam <- NamePolygon(old=names(ply))
    ply[[nam]] <<- base.ply

    tcl("lappend", list.var, nam)

    idx <- length(ply) - 1
    tkselection.clear(frame1.lst, 0, idx)
    tkselection.set(frame1.lst, idx, idx)

    tclvalue(rb.var) <- "add"
    PlotPolygon()
  }

  # Select polygon

  SelectPolygon <- function(type) {
    n <- length(ply)
    if (n == 0)
      return()
    nams <- names(ply)
    idxs <- (1:n) - 1
    sel <- as.integer(tkcurselection(frame1.lst))
    if (type == "all") {
      tkselection.set(frame1.lst, 0, max(idxs))
    } else if (type == "none" | type == "inverse") {
      tkselection.clear(frame1.lst, 0, max(idxs))
    }
    if (type == "inverse") {
      for (i in idxs[!(idxs %in% sel)])
        tkselection.set(frame1.lst, i)
    }
    PlotPolygon()
  }

  # Arrange polygon

  ArrangePolygon <- function(type) {
    idxs <- as.integer(tkcurselection(frame1.lst)) + 1
    if (length(idxs) == 0)
      return()
    if (type == "back") {
      ply <<- append(ply[idxs], ply[-idxs])
      new.idxs <- 1:length(idxs)
    } else if (type == "front") {
      ply <<- append(ply[-idxs], ply[idxs])
      new.idxs <- (length(ply) - length(idxs) + 1):length(ply)
    } else if (type == "backward") {
      n <- length(ply)
      new.idxs <- idxs
      all.idxs <- 1:n
      for (i in 1:n) {
        if (i %in% new.idxs)
          all.idxs[c(i - 1, i)] <- all.idxs[c(i, i - 1)]
      }
      ply <<- ply[all.idxs]
      if (length(new.idxs) == 0) {
        new.idxs <- 1
      } else {
        new.idxs <- (1:n)[all.idxs %in% new.idxs]
      }
    } else if (type == "forward") {
      n <- length(ply)
      new.idxs <- idxs
      all.idxs <- 1:n
      for (i in rev(1:n)) {
          if (i %in% new.idxs)
            all.idxs[c(i, i + 1)] <- all.idxs[c(i + 1, i)]
      }
      all.idxs <- na.omit(all.idxs)
      ply <<- ply[all.idxs]
      if (length(new.idxs) == 0) {
        new.idxs <- n
      } else {
        new.idxs <- (1:n)[all.idxs %in% new.idxs]
      }
    }
    for (i in seq(along=ply)) {
      tclvalue(list.var) <- tcl("lreplace", tclvalue(list.var),
                                i - 1, i - 1, names(ply)[i])
    }
    tkselection.clear(frame1.lst, 0, "end")
    for (i in new.idxs - 1)
      tkselection.set(frame1.lst, i)
    PlotPolygon()
  }

  # Clear polygon

  ClearPolygon <- function() {
    idxs <- as.integer(tkcurselection(frame1.lst)) + 1
    if (length(idxs) == 0)
      return()
    for (idx in idxs) {
      i <- as.integer(tcl("lsearch", "-exact", tclvalue(list.var),
                          names(ply)[idx]))
      if (i < 0)
        next
      tclvalue(list.var) <- tcl("lreplace", tclvalue(list.var), i, i)
    }
    ply <<- ply[-idxs]
    n <- length(ply)
    tkselection.clear(frame1.lst, 0, n - 1)
    tkselection.set(frame1.lst, n - 1)
    PlotPolygon()
  }

  # Import polygon

  ImportPolygon <- function() {
    f <- GetFile(cmd="Open", exts=c("ply"), win.title="Open Polygon File(s)",
                 multi=TRUE, parent=tt)
    if (is.null(f))
      return()
    if (!is.list(f[[1]]))
      f <- list(f)
    for (i in seq(along=f)) {
      con <- file(f[[i]]$path, "r", encoding=encoding)
      new.poly <- read.polyfile(con, nohole=FALSE)
      close(con)
      if (!inherits(new.poly, "gpc.poly"))
        next
      nam <- NamePolygon(old=names(ply), nam=f[[i]]$name)
      ply[[nam]] <<- new.poly
      tcl("lappend", list.var, nam)
    }
    idxs <- as.integer(tkcurselection(frame1.lst))
    if (length(idxs) == 0) {
      tkselection.set(frame1.lst, length(ply) - 1)
      PlotPolygon()
    }
  }

  # Export polygon

  ExportPolygon <- function() {
    idxs <- as.integer(tkcurselection(frame1.lst)) + 1
    if (length(idxs) == 0)
      return()

    for (i in idxs) {
      f <- GetFile(cmd="Save As", exts="ply", win.title="Save Polygon As",
                   initialfile=names(ply)[i], defaultextension="ply", parent=tt)
      if (is.null(f))
        next
      write.polyfile(ply[[i]], f$path)
    }
    tkfocus(tt)
  }


  # Main program

  rtn <- NULL

  if (is.null(ply))
    ply <- list()

  w <- 300
  h <- 300

  xran <- NULL
  yran <- NULL

  col.pal <- c("#FA3A3A", "#3EB636", "#000000", "#227CE8", "#F060A8", "#F18D31",
               "#46C5BD", "#AAC704", "#E64804", "#915135", "#4F575A", "#B1CD02",
               "#3DBF34", "#315A5E", "#5E3831", "#FA330C", "#D45B0A", "#494012",
               substr(rainbow(100), 1, 7))

  base.ply <- NULL

  poly.data <- poly.crop <- NULL
  if (exists("Data")) {
    poly.data <- Data("poly.data")
    poly.crop <- Data("poly.crop")
    old.pdata <- if (is.null(poly.data)) NULL else Data(c("poly", poly.data))
    old.pcrop <- if (is.null(poly.crop)) NULL else Data(c("poly", poly.crop))
  }

  # Assign the variables linked to Tk widgets

  rb.var   <- tclVar("add")
  area.var <- tclVar("")
  poly.var <- tclVar("")
  hole.var <- tclVar("")
  vert.var <- tclVar("")
  xy.var   <- tclVar("")
  list.var <- tclVar()
  for (i in names(ply))
    tcl("lappend", list.var, i)

  tt.done.var <- tclVar(0)

  # Open GUI

  tclServiceMode(FALSE)

  tt <- tktoplevel(padx=0, pady=0)

  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    tmp <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(tmp[2]) + 25,
                            "+", as.integer(tmp[3]) + 25, sep=""))
  }

  tktitle(tt) <- "Manage Polygons"

  # Add menus

  top.menu <- tkmenu(tt, tearoff=0)

  menu.file <- tkmenu(tt, tearoff=0, relief="flat")
  tkadd(top.menu, "cascade", label="File", menu=menu.file, underline=0)

  tkadd(menu.file, "command", label="Open", accelerator="Ctrl+O",
        command=ImportPolygon)
  tkadd(menu.file, "command", label="Save as", accelerator="Ctrl+S",
        command=ExportPolygon)

  menu.edit <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Edit", menu=menu.edit, underline=0)
  tkadd(menu.edit, "command", label="Rename", accelerator="Ctrl+R",
        command=RenamePolygon)
  tkadd(menu.edit, "command", label="Delete", accelerator="Del",
        command=ClearPolygon)

  menu.select <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Select", menu=menu.select, underline=0)
  tkadd(menu.select, "command", label="All", accelerator="Ctrl+A",
        command=function() SelectPolygon("all"))
  tkadd(menu.select, "command", label="Deselect", accelerator="Shift+Ctrl+A",
        command=function() SelectPolygon("none"))
  tkadd(menu.select, "command", label="Inverse",
        command=function() SelectPolygon("inverse"))

  menu.arrange <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Arrange", menu=menu.arrange, underline=0)
  tkadd(menu.arrange, "command", label="Send to back",
        accelerator="Shift+Ctrl+[",
        command=function() ArrangePolygon("back"))
  tkadd(menu.arrange, "command", label="Send backward", accelerator="Ctrl+[",
        command=function() ArrangePolygon("backward"))
  tkadd(menu.arrange, "command", label="Bring forward", accelerator="Ctrl+]",
        command=function() ArrangePolygon("forward"))
  tkadd(menu.arrange, "command", label="Bring to front",
        accelerator="Shift+Ctrl+]",
        command=function() ArrangePolygon("front"))

  tkconfigure(tt, menu=top.menu)

  # Frame 0 contains ok and cancel buttons, and size grip

  frame0 <- ttkframe(tt, relief="flat")

  frame0.but.1 <- ttkbutton(frame0, width=12, text="OK",
                            command=function() SavePolygon("ok"))
  frame0.but.2 <- ttkbutton(frame0, width=12, text="Apply",
                            command=function() SavePolygon("apply"))
  frame0.but.3 <- ttkbutton(frame0, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)

  frame0.grp.4 <- ttksizegrip(frame0)

  tkgrid(frame0.but.1, frame0.but.2, frame0.but.3, frame0.grp.4)

  tkgrid.configure(frame0.but.1, frame0.but.2, sticky="e", padx=2, pady=c(5, 8))
  tkgrid.configure(frame0.but.2, padx=c(2, 6))
  tkgrid.configure(frame0.but.3, sticky="w", padx=2, pady=c(5, 8), rowspan=2)
  tkgrid.configure(frame0.grp.4, sticky="se")

  tkpack(frame0, side="bottom", anchor="e")

  # Paned window

  pw <- ttkpanedwindow(tt, orient="horizontal")

  # Frame 1

  frame1 <- ttkframe(pw, relief="flat")

  frame1.lst <- tklistbox(frame1, selectmode="extended", activestyle="none",
                          relief="flat", borderwidth=5, width=20,
                          exportselection=FALSE, listvariable=list.var,
                          highlightthickness=0)
  frame1.ysc <- ttkscrollbar(frame1, orient="vertical")

  tkconfigure(frame1.lst, background="white",
              yscrollcommand=paste(.Tk.ID(frame1.ysc), "set"))
  tkconfigure(frame1.ysc, command=paste(.Tk.ID(frame1.lst), "yview"))

  tkpack(frame1.lst, side="left",  fill="both", expand=TRUE,
         pady=c(6, 0), padx=0)
  tkpack(frame1.ysc, side="right", fill="y", anchor="w",
         pady=c(7, 0), padx=c(0, 6))

  n <- length(ply)
  if (n > 1)
    tkselection.set(frame1.lst, n - 1)

  tkbind(frame1.lst, "<ButtonRelease-1>", PlotPolygon)

  # Frame 2

  frame2 <- ttkframe(pw, relief="flat")

  frame2.cvs <- tkcanvas(frame2, relief="flat", width=w, height=h,
                         background="white", confine=TRUE, closeenough=0,
                         cursor="crosshair", borderwidth=0,
                         highlightthickness=0)

  # Frame 3

  frame3 <- ttkframe(pw, relief="flat")

  frame3a <- ttklabelframe(frame3, relief="flat", borderwidth=3, padding=3,
                           text="Shape modes")

  frame3a.rb.1 <- ttkradiobutton(frame3a, variable=rb.var, command=PlotPolygon,
                                 value="add", text="Unite")
  frame3a.rb.2 <- ttkradiobutton(frame3a, variable=rb.var, command=PlotPolygon,
                                 value="sub", text="Minus front")
  frame3a.rb.3 <- ttkradiobutton(frame3a, variable=rb.var, command=PlotPolygon,
                                 value="int", text="Intersect")
  frame3a.rb.4 <- ttkradiobutton(frame3a, variable=rb.var, command=PlotPolygon,
                                 value="exc", text="Exclude overlapping")

  frame3a.but <- ttkbutton(frame3a, width=12, text="Build",
                           command=SaveNewPolygon)

  tkgrid(frame3a.rb.1, sticky="w")
  tkgrid(frame3a.rb.2, sticky="w")
  tkgrid(frame3a.rb.3, sticky="w")
  tkgrid(frame3a.rb.4, sticky="w")
  tkgrid(frame3a.but, pady=10)

  tcl("grid", "anchor", frame3a, "w")

  frame3b <- ttklabelframe(frame3, relief="flat", padding=3, text="Attributes")

  frame3b.lab.1.1 <- tklabel(frame3b, text="Area")
  frame3b.lab.2.1 <- tklabel(frame3b, text="Polygons")
  frame3b.lab.3.1 <- tklabel(frame3b, text="Holes")
  frame3b.lab.4.1 <- tklabel(frame3b, text="Vertices")

  frame3b.lab.1.2 <- tklabel(frame3b, text=tclvalue(area.var))
  frame3b.lab.2.2 <- tklabel(frame3b, text=tclvalue(poly.var))
  frame3b.lab.3.2 <- tklabel(frame3b, text=tclvalue(hole.var))
  frame3b.lab.4.2 <- tklabel(frame3b, text=tclvalue(vert.var))

  tkconfigure(frame3b.lab.1.2, textvariable=area.var)
  tkconfigure(frame3b.lab.2.2, textvariable=poly.var)
  tkconfigure(frame3b.lab.3.2, textvariable=hole.var)
  tkconfigure(frame3b.lab.4.2, textvariable=vert.var)

  tkgrid(frame3b.lab.1.1, frame3b.lab.1.2)
  tkgrid(frame3b.lab.2.1, frame3b.lab.2.2)
  tkgrid(frame3b.lab.3.1, frame3b.lab.3.2)
  tkgrid(frame3b.lab.4.1, frame3b.lab.4.2)

  tkgrid.configure(frame3b.lab.1.1, frame3b.lab.2.1,
                   frame3b.lab.3.1, frame3b.lab.4.1, sticky="e")

  tkgrid.configure(frame3b.lab.1.2, frame3b.lab.2.2,
                   frame3b.lab.3.2, frame3b.lab.4.2, sticky="w", padx=c(5, 0))

  tcl("grid", "anchor", frame3b, "w")

  frame3c <- ttkframe(frame3, relief="flat", borderwidth=3, padding=3)

  frame3c.lab.1 <- tklabel(frame3c, text=tclvalue(xy.var))
  tkconfigure(frame3c.lab.1, textvariable=xy.var)
  tkgrid(frame3c.lab.1)

  tkpack(frame3a, fill="both", expand=TRUE, padx=5, pady=c(5, 2))
  tkpack(frame3b, fill="both", expand=TRUE, padx=5, pady=c(5, 2))

  tkpack(frame3c)

  # Final layout

  tkgrid(frame2.cvs, sticky="news")

  tkgrid.configure(frame2.cvs, padx=0, pady=c(5, 0))

  tkgrid.rowconfigure(frame2, frame2.cvs, weight=1)
  tkgrid.columnconfigure(frame2, frame2.cvs, weight=1)

  tkadd(pw, frame1, weight=0)
  tkadd(pw, frame2, weight=1)
  tkadd(pw, frame3, weight=0)

  tkpack(pw, fill="both", expand=TRUE, padx=15, pady=c(10, 5))

  # Bind events

  tkbind(frame2.cvs, "<Motion>", function(x, y) MouseMotion(x, y))
  tkbind(frame2.cvs, "<Leave>", MouseLeave)
  tkbind(frame2.cvs, "<Configure>", ScaleCanvas)

  tkbind(tt, "<Control-o>", ImportPolygon)
  tkbind(tt, "<Control-s>", ExportPolygon)

  tkbind(tt, "<Control-a>", function() SelectPolygon("all"))
  tkbind(tt, "<Shift-Control-A>", function() SelectPolygon("none"))

  tkbind(tt, "<Control-]>", function() ArrangePolygon("forward"))
  tkbind(tt, "<Shift-Control-}>", function() ArrangePolygon("front"))
  tkbind(tt, "<Control-[>", function() ArrangePolygon("backward"))
  tkbind(tt, "<Shift-Control-{>", function() ArrangePolygon("back"))

  tkbind(tt, "<BackSpace>", ClearPolygon)
  tkbind(tt, "<Delete>", ClearPolygon)
  tkbind(tt, "<Control-r>", RenamePolygon)

  tkbind(frame1.lst, "<<ListboxSelect>>", PlotPolygon)

  # GUI control

  ScaleCanvas()

  tkfocus(tt)
  tkgrab(tt)
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)
  PlotPolygon()

  tclServiceMode(TRUE)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  rtn
}
