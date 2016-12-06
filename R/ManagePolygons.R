ManagePolygons <- function(polys=NULL, poly.data=NULL, poly.crop=NULL,
                           encoding=getOption("encoding"), parent=NULL) {


  # save polygon
  SavePolygon <- function(type) {
    if (length(polys) > 0)
      polys <- polys[vapply(polys, function(i) inherits(i, "gpc.poly"), TRUE)]
    if (length(polys) == 0) polys <- NULL
    if (!is.null(poly.data) && !poly.data %in% names(polys)) poly.data <- NULL
    if (!is.null(poly.crop) && !poly.crop %in% names(polys)) poly.crop <- NULL
    rtn <<- list(polys=polys, poly.data=poly.data, poly.crop=poly.crop)
    if (type == "ok") tclvalue(tt.done.var) <- 1
  }

  # plot polygon
  PlotPolygon <- function() {

    # draw polygon in canvas
    DrawPolygon <- function(contours, tag="", col.line="", col.fill="") {
      for (cnt in contours) {
        pts <- Xy2mn(cnt$x, cnt$y)
        mn <- rep(NA, length(pts$m) * 2)
        is.odd <- !array(0:1, length(mn))
        mn[ is.odd] <- pts$m
        mn[!is.odd] <- pts$n
        tkcreate(f2.cvs, "polygon", .Tcl.args(mn), fill=col.fill,
                 outline=col.line, width=1, tag=tag)
      }
    }

    idxs <- as.integer(tkcurselection(f1.lst)) + 1L

    tcl(f2.cvs, "delete", "all")
    xran <<- NULL
    yran <<- NULL

    tclvalue(area.var) <- ""
    tclvalue(poly.var) <- ""
    tclvalue(hole.var) <- ""
    tclvalue(vert.var) <- ""

    polys.base <<- NULL

    if (length(idxs) == 0) return()

    for (idx in idxs) {
      bb <- rgeos::get.bbox(polys[[idx]])
      xran <- range(c(xran, bb$x))
      yran <- range(c(yran, bb$y))
    }
    xran <<- extendrange(xran, f=0.02)
    yran <<- extendrange(yran, f=0.02)

    cmd <- tclvalue(rb.var)

    polys.base <<- NULL
    if (cmd == "exc") {
      if (length(idxs) > 1L) {
        union.polys <- polys[[idxs[1]]]
        inter.polys <- polys[[idxs[1]]]
        for (idx in idxs[-1]) {
          union.polys <- try(union(union.polys, polys[[idx]]), silent=TRUE)
          inter.polys <- try(intersect(inter.polys, polys[[idx]]), silent=TRUE)
        }
        if (!inherits(union.polys, "try-error") && !inherits(inter.polys, "try-error"))
          polys.base <<- setdiff(union.polys, inter.polys)
      }
    } else {
      if (cmd == "add") {
        fun <- "union"
      } else if (cmd == "sub") {
        fun <- "setdiff"
      } else if (cmd == "int") {
        fun <- "intersect"
      }
      build.polys <- polys[[idxs[1]]]
      for (idx in idxs[-1]) {
        build.polys <- try(do.call(fun, list(build.polys, polys[[idx]])), silent=TRUE)
      }
      if (!inherits(build.polys, "try-error")) polys.base <<- build.polys
    }

    if (!is.null(polys.base)) {
      base.pts <- rgeos::get.pts(polys.base)
      if (length(base.pts) == 0) polys.base <<- NULL
    }
    if (!is.null(polys.base)) {
      hole <- NULL
      vert <- 0
      for (ctr in base.pts) {
        hole <- append(hole, ctr$hole)
        vert <- vert + length(ctr$x)
      }
      if (!is.null(hole)) {
        DrawPolygon(base.pts[!hole], col.fill="#FDFEC4")
        if (any(hole)) DrawPolygon(base.pts[hole], col.fill="white")
      }
      tclvalue(area.var) <- format(rgeos::area.poly(polys.base))
      tclvalue(poly.var) <- length(base.pts)
      tclvalue(hole.var) <- sum(hole)
      tclvalue(vert.var) <- vert
    }

    for (i in idxs)
      DrawPolygon(rgeos::get.pts(polys[[i]]), tag=names(polys)[i], col.line=col.pal[i])
  }


  # transform coordinates from real to canvas
  Xy2mn <- function(x, y) {
    m <- w * ((x - xran[1]) / diff(xran))
    n <- h - (h * ((y - yran[1]) / diff(yran)))
    return(list(m=m, n=n))
  }


  # transform coordinates from canvas to real
  Mn2xy <- function(m, n) {
    x <- (m * diff(xran) + w * xran[1]) / w
    y <- (h * yran[1] - (n - h) * diff(yran)) / h
    return(list(x=x, y=y))
  }


  # scale objects in canvas based on canvas size
  ScaleCanvas <- function() {
    w0 <- w
    h0 <- h
    w <<- as.numeric(tkwinfo("width",  f2.cvs))
    h <<- as.numeric(tkwinfo("height", f2.cvs))
    tcl(f2.cvs, "scale", "all", 0, 0, w / w0, h / h0)
  }


  # update pointer coordinates
  MouseMotion <- function(x, y) {
    if (!is.null(xran)) {
      pnt <- Mn2xy(as.numeric(x), as.numeric(y))
      tclvalue(xy.var) <- paste(format(pnt$x), format(pnt$y), sep=", ")
    }
  }


  # remove pointer coordinates after leaving canvas
  MouseLeave <- function() {
    tclvalue(xy.var) <- ""
  }


  # name polygon
  NamePolygon <- function(old=NULL, nam=NA){
    if (is.na(nam)) nam <- "New Polygon"
    i <- 1
    hold.nam <- nam
    while (nam %in% old) {
      nam <- paste0(hold.nam, " (", i, ")")
      i <- i + 1
    }
    return(nam)
  }


  # rename polygon
  RenamePolygon <- function() {
    if (length(polys) == 0) return()

    old.names <- names(polys)
    cur.name <- NULL

    idxs <- as.integer(tkcurselection(f1.lst)) + 1
    if (length(idxs) != 0) cur.name <- old.names[idxs[1]]

    new.names <- Rename(old.names, cur.name, "Rename Polygon", tt)

    if (!is.null(new.names) && length(new.names) == length(old.names)) {
      if (identical(new.names, old.names)) return()
      names(polys) <<- new.names
      if (!is.null(poly.data)) poly.data <<- new.names[which(old.names %in% poly.data)]
      if (!is.null(poly.crop)) poly.crop <<- new.names[which(old.names %in% poly.crop)]
    }

    for (i in seq_along(new.names)) {
      tclvalue(list.var) <- tcl("lreplace", tclvalue(list.var), i - 1, i - 1, new.names[i])
    }
  }


  # save new polygon
  SaveNewPolygon <- function() {
    if (is.null(polys.base)) return()
    nam <- NamePolygon(old=names(polys))
    polys[[nam]] <<- polys.base
    tcl("lappend", list.var, nam)
    idx <- length(polys) - 1
    tkselection.clear(f1.lst, 0, idx)
    tkselection.set(f1.lst, idx, idx)
    tclvalue(rb.var) <- "add"
    PlotPolygon()
  }


  # select polygon
  SelectPolygon <- function(type) {
    n <- length(polys)
    if (n == 0) return()
    nams <- names(polys)
    idxs <- (seq_len(n)) - 1L
    sel <- as.integer(tkcurselection(f1.lst))
    if (type == "all") {
      tkselection.set(f1.lst, 0, max(idxs))
    } else if (type == "none" | type == "inverse") {
      tkselection.clear(f1.lst, 0, max(idxs))
    }
    if (type == "inverse") {
      for (i in idxs[!(idxs %in% sel)]) tkselection.set(f1.lst, i)
    }
    PlotPolygon()
  }


  # arrange polygon
  ArrangePolygon <- function(type) {
    idxs <- as.integer(tkcurselection(f1.lst)) + 1
    if (length(idxs) == 0) return()
    if (type == "back") {
      polys <<- append(polys[idxs], polys[-idxs])
      new.idxs <- seq_along(idxs)
    } else if (type == "front") {
      polys <<- append(polys[-idxs], polys[idxs])
      new.idxs <- (length(polys) - length(idxs) + 1):length(polys)
    } else if (type == "backward") {
      n <- length(polys)
      new.idxs <- idxs
      all.idxs <- seq_len(n)
      for (i in all.idxs) {
        if (i %in% new.idxs) all.idxs[c(i - 1, i)] <- all.idxs[c(i, i - 1)]
      }
      polys <<- polys[all.idxs]
      new.idxs <- if (length(new.idxs) == 0) 1 else seq_len(n)[all.idxs %in% new.idxs]
    } else if (type == "forward") {
      n <- length(polys)
      new.idxs <- idxs
      all.idxs <- seq_len(n)
      for (i in rev(all.idxs)) {
        if (i %in% new.idxs) all.idxs[c(i, i + 1)] <- all.idxs[c(i + 1, i)]
      }
      all.idxs <- na.omit(all.idxs)
      polys <<- polys[all.idxs]
      new.idxs <- if (length(new.idxs) == 0) n else seq_len(n)[all.idxs %in% new.idxs]
    }
    for (i in seq_along(polys))
      tclvalue(list.var) <- tcl("lreplace", tclvalue(list.var), i - 1, i - 1, names(polys)[i])
    tkselection.clear(f1.lst, 0, "end")
    for (i in new.idxs - 1) tkselection.set(f1.lst, i)
    PlotPolygon()
  }


  # clear polygon
  ClearPolygon <- function() {
    idxs <- as.integer(tkcurselection(f1.lst)) + 1
    if (length(idxs) == 0) return()
    for (idx in idxs) {
      i <- as.integer(tcl("lsearch", "-exact", tclvalue(list.var), names(polys)[idx]))
      if (i < 0) next
      tclvalue(list.var) <- tcl("lreplace", tclvalue(list.var), i, i)
    }
    polys <<- polys[-idxs]
    n <- length(polys)
    tkselection.clear(f1.lst, 0, n - 1)
    tkselection.set(f1.lst, n - 1)
    PlotPolygon()
  }


  # import polygon
  ImportPolygon <- function() {
    tkconfigure(tt, cursor="watch")
    on.exit(tkconfigure(tt, cursor="arrow"))

    f <- GetFile(cmd="Open", exts=c("ply"), win.title="Open Polygon File(s)",
                 multi=TRUE, parent=tt)
    if (is.null(f)) return()
    if (!is.list(f)) f <- list(f)
    for (i in seq_along(f)) {
      con <- file(f[[i]], "r", encoding=encoding)
      new.poly <- rgeos::read.polyfile(con, nohole=FALSE)
      close(con)
      if (!inherits(new.poly, "gpc.poly")) next
      nam <- NamePolygon(old=names(polys), nam=attr(f[[i]], "name"))
      polys[[nam]] <<- new.poly
      tcl("lappend", list.var, nam)
    }
    idxs <- as.integer(tkcurselection(f1.lst))
    if (length(idxs) == 0) {
      tkselection.set(f1.lst, length(polys) - 1)
      PlotPolygon()
    }
  }


  # export polygon
  ExportPolygon <- function() {
    idxs <- as.integer(tkcurselection(f1.lst)) + 1
    if (length(idxs) == 0) return()
    for (i in idxs) {
      f <- GetFile(cmd="Save As", exts="ply", win.title="Save Polygon As",
                   initialfile=names(polys)[i], defaultextension="ply", parent=tt)
      if (is.null(f)) next
      rgeos::write.polyfile(polys[[i]], f)
    }
    tkfocus(tt)
  }


  rtn <- NULL

  w <- 300
  h <- 300

  xran <- NULL
  yran <- NULL

  col.pal <- c("#FA3A3A", "#3EB636", "#000000", "#227CE8", "#F060A8", "#F18D31",
               "#46C5BD", "#AAC704", "#E64804", "#915135", "#4F575A", "#B1CD02",
               "#3DBF34", "#315A5E", "#5E3831", "#FA330C", "#D45B0A", "#494012",
               substr(rainbow(100), 1, 7))

  polys.base <- NULL

  if (is.null(polys)) polys <- list()

  # assign the variables linked to tk widgets
  rb.var      <- tclVar("add")
  area.var    <- tclVar("")
  poly.var    <- tclVar("")
  hole.var    <- tclVar("")
  vert.var    <- tclVar("")
  xy.var      <- tclVar("")
  list.var    <- tclVar()
  for (i in names(polys)) tcl("lappend", list.var, i)
  tt.done.var <- tclVar(0)

  # open gui
  tclServiceMode(FALSE)

  tt <- tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste0("+", as.integer(geo[2]) + 25,
                             "+", as.integer(geo[3]) + 25))
  }
  tktitle(tt) <- "Manage Polygons"

  # add menus
  top.menu <- tkmenu(tt, tearoff=0)

  menu.file <- tkmenu(tt, tearoff=0, relief="flat")
  tkadd(top.menu, "cascade", label="File", menu=menu.file, underline=0)

  tkadd(menu.file, "command", label="Open\u2026", accelerator="Ctrl+o", command=ImportPolygon)
  tkadd(menu.file, "command", label="Save as\u2026", accelerator="Ctrl+s", command=ExportPolygon)

  menu.edit <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Edit", menu=menu.edit, underline=0)
  tkadd(menu.edit, "command", label="Rename\u2026", accelerator="Ctrl+r", command=RenamePolygon)
  tkadd(menu.edit, "command", label="Delete", accelerator="Delete", command=ClearPolygon)

  menu.select <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Select", menu=menu.select, underline=0)
  tkadd(menu.select, "command", label="All", accelerator="Ctrl+a",
        command=function() SelectPolygon("all"))
  tkadd(menu.select, "command", label="Deselect", accelerator="Shift+Ctrl+a",
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

  # frame 0, ok and cancel buttons, and size grip
  f0 <- ttkframe(tt, relief="flat")

  f0.but.1  <- ttkbutton(f0, width=2, image=GetBitmapImage("top"),
                         command=function() ArrangePolygon("back"))
  f0.but.2  <- ttkbutton(f0, width=2, image=GetBitmapImage("up"),
                         command=function() ArrangePolygon("backward"))
  f0.but.3  <- ttkbutton(f0, width=2, image=GetBitmapImage("down"),
                         command=function() ArrangePolygon("forward"))
  f0.but.4  <- ttkbutton(f0, width=2, image=GetBitmapImage("bottom"),
                         command=function() ArrangePolygon("front"))
  f0.but.5  <- ttkbutton(f0, width=2, image=GetBitmapImage("delete"),
                         command=ClearPolygon)

  f0.but.7  <- ttkbutton(f0, width=12, text="OK",
                         command=function() SavePolygon("ok"))
  f0.but.8  <- ttkbutton(f0, width=12, text="Cancel",
                         command=function() tclvalue(tt.done.var) <- 1)
  f0.but.9  <- ttkbutton(f0, width=12, text="Apply",
                         command=function() SavePolygon("apply"))
  f0.but.10 <- ttkbutton(f0, width=12, text="Help",
                         command=function() {
                           print(help("ManagePolygons", package="RSurvey"))
                         })
  f0.grp.11 <- ttksizegrip(f0)

  tkgrid(f0.but.1, f0.but.2, f0.but.3, f0.but.4, f0.but.5, "x", f0.but.7, f0.but.8,
         f0.but.9, f0.but.10, f0.grp.11)

  tkgrid.columnconfigure(f0, 5, weight=1)
  tkgrid.configure(f0.but.1, f0.but.2, f0.but.3, f0.but.4, f0.but.5,
                   sticky="n", padx=c(0, 2), pady=c(4, 0))
  tkgrid.configure(f0.but.1, padx=c(10, 2))
  tkgrid.configure(f0.but.5, padx=c(20, 0))
  tkgrid.configure(f0.but.7, f0.but.8, f0.but.9, f0.but.10,
                   padx=c(4, 0), pady=c(15, 10))
  tkgrid.configure(f0.but.10, columnspan=2, padx=c(4, 10))
  tkgrid.configure(f0.grp.11, sticky="se")

  tkraise(f0.but.10, f0.grp.11)

  tkpack(f0, fill="x", side="bottom", anchor="e")

  # paned window
  pw <- ttkpanedwindow(tt, orient="horizontal")

  # frame 1
  f1 <- ttkframe(pw, relief="flat")

  f1.lst <- tklistbox(f1, selectmode="extended", activestyle="none",
                      relief="flat", borderwidth=5, width=25,
                      exportselection=FALSE, listvariable=list.var,
                      highlightthickness=0)
  f1.ysc <- ttkscrollbar(f1, orient="vertical")

  tkconfigure(f1.lst, background="white", yscrollcommand=paste(.Tk.ID(f1.ysc), "set"))
  tkconfigure(f1.ysc, command=paste(.Tk.ID(f1.lst), "yview"))

  tkpack(f1.lst, side="left",  fill="both", expand=TRUE)
  tkpack(f1.ysc, side="right", fill="y", anchor="w")

  n <- length(polys)
  if (n > 0) tkselection.set(f1.lst, n - 1)

  # frame 2
  f2 <- ttkframe(pw, relief="flat")

  f2.cvs <- tkcanvas(f2, relief="flat", width=w, height=h,
                     background="white", confine=TRUE, closeenough=0,
                     cursor="crosshair", borderwidth=0, highlightthickness=0)

  # frame 3
  f3 <- ttkframe(pw, relief="flat")

  f3a <- ttklabelframe(f3, relief="flat", borderwidth=5, padding=5, text="Shape modes")

  f3a.rb.1 <- ttkradiobutton(f3a, variable=rb.var, command=PlotPolygon,
                             value="add", text="Unite")
  f3a.rb.2 <- ttkradiobutton(f3a, variable=rb.var, command=PlotPolygon,
                             value="sub", text="Minus front")
  f3a.rb.3 <- ttkradiobutton(f3a, variable=rb.var, command=PlotPolygon,
                             value="int", text="Intersect")
  f3a.rb.4 <- ttkradiobutton(f3a, variable=rb.var, command=PlotPolygon,
                             value="exc", text="Exclude overlapping")

  f3a.but <- ttkbutton(f3a, width=12, text="Build", command=SaveNewPolygon)

  tkgrid(f3a.rb.1, sticky="w")
  tkgrid(f3a.rb.2, sticky="w")
  tkgrid(f3a.rb.3, sticky="w")
  tkgrid(f3a.rb.4, sticky="w")
  tkgrid(f3a.but, pady=10)

  tcl("grid", "anchor", f3a, "w")

  f3b <- ttklabelframe(f3, relief="flat", borderwidth=5, padding=5, text="Attributes")

  f3b.lab.1.1 <- tklabel(f3b, text="Polygons")
  f3b.lab.2.1 <- tklabel(f3b, text="Holes")
  f3b.lab.3.1 <- tklabel(f3b, text="Vertices")
  f3b.lab.4.1 <- tklabel(f3b, text="Area")

  f3b.lab.1.2 <- tklabel(f3b, text=tclvalue(poly.var))
  f3b.lab.2.2 <- tklabel(f3b, text=tclvalue(hole.var))
  f3b.lab.3.2 <- tklabel(f3b, text=tclvalue(vert.var))
  f3b.lab.4.2 <- tklabel(f3b, text=tclvalue(area.var))

  tkconfigure(f3b.lab.1.2, textvariable=poly.var)
  tkconfigure(f3b.lab.2.2, textvariable=hole.var)
  tkconfigure(f3b.lab.3.2, textvariable=vert.var)
  tkconfigure(f3b.lab.4.2, textvariable=area.var)

  tkgrid(f3b.lab.1.1, f3b.lab.1.2)
  tkgrid(f3b.lab.2.1, f3b.lab.2.2)
  tkgrid(f3b.lab.3.1, f3b.lab.3.2)
  tkgrid(f3b.lab.4.1, f3b.lab.4.2)

  tkgrid.configure(f3b.lab.1.1, f3b.lab.2.1, f3b.lab.3.1, f3b.lab.4.1, sticky="w")

  tkgrid.configure(f3b.lab.1.2, f3b.lab.2.2, f3b.lab.3.2, f3b.lab.4.2,
                   sticky="e", padx=c(5, 0))

  tcl("grid", "anchor", f3b, "w")

  f3c <- ttkframe(f3, relief="flat", borderwidth=0, padding=0)

  f3c.lab.1 <- tklabel(f3c, text=tclvalue(xy.var))
  tkconfigure(f3c.lab.1, textvariable=xy.var)
  tkgrid(f3c.lab.1)
  tkgrid.columnconfigure(f3c, 0, weight=1, minsize=13)

  tkpack(f3a, fill="x", pady=c(0, 5))
  tkpack(f3b, fill="x", pady=c(5, 5))
  tkpack(f3c, fill="both", expand=TRUE)

  # final layout
  tkgrid(f2.cvs, sticky="news")

  tkgrid.configure(f2.cvs, padx=5)

  tkgrid.rowconfigure(f2, f2.cvs, weight=1)
  tkgrid.columnconfigure(f2, f2.cvs, weight=1)

  tkadd(pw, f1, weight=0)
  tkadd(pw, f2, weight=1)
  tkadd(pw, f3, weight=0)

  tkpack(pw, fill="both", expand=TRUE, padx=10, pady=c(10, 0))

  # bind events
  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tkbind(f2.cvs, "<Motion>", function(x, y) MouseMotion(x, y))
  tkbind(f2.cvs, "<Leave>", MouseLeave)
  tkbind(f2.cvs, "<Configure>", ScaleCanvas)

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

  tkbind(f1.lst, "<<ListboxSelect>>", PlotPolygon)

  # gui control
  ScaleCanvas()
  PlotPolygon()

  tkfocus(tt)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  return(rtn)
}
