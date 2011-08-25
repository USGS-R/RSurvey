ManageData <- function(cols, vars, parent=NULL) {
  # A GUI for managing and manipulating data

  # Additional functions (subroutines)

  # Save changes and close GUI

  SaveChanges <- function(type) {
    SaveNb()
    if (!identical(cols, old.cols)) {
      Data("cols", cols)
      Data("vars", vars)
    }
    if (type == "ok")
      tclvalue(tt.done.var) <- 1
  }

  # Set variable identification and update functions to reflect this change

  SetVarId <- function(idx=NULL) {
    if (is.null(idx))
      idx <- as.integer(tkcurselection(frame2.lst)) + 1

    # Save name

    nam <- tclvalue(name.var)
    if (nam == "")
      nam <- NULL
    cols[[idx]]$name <<- nam

    # Save units

    unt <- tclvalue(unit.var)
    if (unt == "")
      unt <- NULL
    cols[[idx]]$unit <<- unt

    # Insure content for id

    if (is.null(nam) & is.null(unt))
      cols[[idx]]$name <<- "Unknown"

    # Account for duplicate ids

    new.id <- paste(c(nam, unt), collapse=", ")
    old.id <- cols[[idx]]$id
    old.ids <- sapply(cols, function(i) i$id)

    i <- 1
    hold.new.id <- new.id
    while (new.id %in% old.ids[-idx]) {
      new.id <- paste(hold.new.id, " (", i, ")", sep="")
      i <- i + 1
    }
    cols[[idx]]$id <<- new.id
    tclvalue(list.var) <- tcl("lreplace", tclvalue(list.var),
                              idx - 1, idx - 1, new.id)

    # Update functions

    if (!is.null(old.id)) {
      old.fun <- cols[[idx]]$fun

      str.1 <- paste("DATA[[\"", old.id, "\"]]", sep="")
      str.2 <- paste("DATA[[\"", new.id, "\"]]", sep="")
      funs <- sapply(cols, function(i) gsub(str.1, str.2, i$fun, fixed=TRUE))
      sapply(1:length(cols), function(i) cols[[i]]$fun <<- funs[[i]])

      new.fun <- cols[[idx]]$fun
      if (!identical(old.fun, new.fun)) {
        tkconfigure(frame3.txt.5.2, state="normal")
        tcl(frame3.txt.5.2, "delete", '1.0', 'end')
        tkinsert(frame3.txt.5.2, "end", new.fun)
        tkconfigure(frame3.txt.5.2, state="disabled")
      }
    }
  }

  # Save notebook content

  SaveNb <- function() {
    idx <- as.integer(tkcurselection(frame2.lst)) + 1
    if (length(idx) == 0)
      return()

    # Save units

    old.unt <- cols[[idx]]$unit
    new.unt <- as.character(tclvalue(unit.var))
    if (new.unt == "")
      new.unt <- NULL
    cols[[idx]]$unit <<- new.unt

    # Save decimals

    old.dig <- cols[[idx]]$digits
    new.dig <- as.integer(tclvalue(digs.var))
    if (is.na(new.dig))
      new.dig <- NULL
    cols[[idx]]$digits <<- new.dig

    # Save function

    old.fun <- cols[[idx]]$fun
    new.fun <- as.character(tclvalue(tkget(frame3.txt.5.2, '1.0', 'end-1c')))
    if (new.fun == "")
      new.fun <- "NA"
    cols[[idx]]$fun <<- new.fun

    # Save summary

    is.dig <- !identical(old.dig, new.dig)
    is.unt <- !identical(old.unt, new.unt) && cols[[idx]]$class == "POSIXct"
    is.fun <- !identical(old.fun, new.fun)
    if (is.dig || is.unt || is.fun) {
      if (is.fun)
        sum.dat <- SummarizeData(EvalFunction(new.fun, cols), digits=new.dig,
                                 dt.format=new.unt)
      else
        sum.dat <- SummarizeData(cols[[idx]]$summary, digits=new.dig,
                                 dt.format=new.unt)
      cols[[idx]]$summary <<- sum.dat
    }

    # Save class

    if (!is.null(cols[[idx]]$summary))
      cols[[idx]]$class <<- cols[[idx]]$summary$Class

    # Save comments

    new.comments <- sub("\n$", "", tclvalue(tkget(frame5.txt, '1.0', 'end')))
    if (new.comments == "")
      new.comments <- NULL
    cols[[idx]]$comments <<- new.comments

    # Save name

    SetVarId(idx)
  }

  # Update notebook content

  UpdateNb <- function() {
    idx <- as.integer(tkcurselection(frame2.lst)) + 1
    if (length(idx) == 0) {
      SaveNewVar()
      return()
    }

    # Update name

    saved.name <- cols[[idx]]$name
    if (is.null(saved.name))
      saved.name <- ""
    tclvalue(name.var) <- saved.name

    # Update units

    saved.unit <- cols[[idx]]$unit
    if (is.null(saved.unit))
      saved.unit <- ""
    tclvalue(unit.var) <- saved.unit

    if (cols[[idx]]$class == "POSIXct") {
      tkconfigure(frame3.ent.2.2, state="readonly")
      tkconfigure(frame3.but.2.3, state="normal")
    } else {
      tkconfigure(frame3.ent.2.2, state="normal")
      tkconfigure(frame3.but.2.3, state="disabled")
    }

    # Update decimals

    tkconfigure(frame3.ent.3.2, state="normal")
    tclvalue(digs.var) <- cols[[idx]]$digits
    if (cols[[idx]]$class == "numeric") {
      saved.digs <- cols[[idx]]$digits
      if (is.null(saved.digs))
        saved.digs <- ""
      tclvalue(digs.var) <- saved.digs
    } else {
      tclvalue(digs.var) <- ""
      tkconfigure(frame3.ent.3.2, state="disabled")
    }

    # Update class

    tkconfigure(frame3.ent.4.2, state="normal")
    tclvalue(clas.var) <- cols[[idx]]$class
    tkconfigure(frame3.ent.4.2, state="disabled")

    # Update function

    tkconfigure(frame3.txt.5.2, state="normal")
    tcl(frame3.txt.5.2, "delete", '1.0', 'end')
    tkinsert(frame3.txt.5.2, "end", cols[[idx]]$fun)
    tkconfigure(frame3.txt.5.2, state="disabled")

    s <- "disabled"
    if (is.null(cols[[idx]]$index))
      s <- "normal"
    tkconfigure(frame3.but.5.3, state=s)

    # Update summary

    tkconfigure(frame4.txt, state="normal")
    tcl(frame4.txt, "delete", '1.0', 'end')

    sum.str <- cols[[idx]]$summary$String
    if (!is.null(sum.str))
      tkinsert(frame4.txt, "end", sum.str)

    tkconfigure(frame4.txt, state="disabled")

    DrawHistogram(cols[[idx]]$summary$Hist)

    # Update comments

    tcl(frame5.txt, "delete", '1.0', 'end')
    comments <- cols[[idx]]$comments
    if (!is.null(comments))
      tkinsert(frame5.txt, "end", comments, sep="")
  }

  # Account for change in notebook tab

  ChangeTab <- function() {
    idx <- as.integer(tkcurselection(frame2.lst)) + 1
    if (length(idx) == 0)
      return()

    SaveNb()
    UpdateNb()

    tabid <- tclvalue(tcl(nb, 'select'))

    # Arrive at variable tab

    if (tabid == frame3$ID) {
      tkfocus(frame3)

    # Arrive at comments tab

    } else if (tabid == frame5$ID) {
      tkfocus(frame5.txt)

    # Arrive at summary tab

    } else if (tabid == frame4$ID) {
      tkfocus(frame4)
    }
  }

  # Save new variable

  SaveNewVar <- function() {
    SaveNb()
    id <- "New Variable"
    tcl("lappend", list.var, id)
    n <- length(cols)
    tkselection.clear(frame2.lst, 0, n)
    tkselection.set(frame2.lst, n, n)
    cols[[n + 1]] <<- list(name=id, class="logical", fun="NA")
    UpdateNb()
    SetVarId()

    if (n > 0)
      CallEditFunction()
  }

  # Delete existing variable

  DeleteVar <- function() {
    idx <- as.integer(tkcurselection(frame2.lst)) + 1
    if (length(idx) == 0)
      return()

    var.str <- paste("DATA[[\"", cols[[idx]]$id, "\"]]", sep="")
    funs.with.var <- grep(var.str, sapply(cols, function(i) i$fun), fixed=TRUE)
    dependent.vars <- funs.with.var[!funs.with.var %in% idx]

    if (length(dependent.vars) > 0) {
      msg <- paste("Removal of this variable first requires that the",
                   "following dependent variable(s) be removed:", sep="\n")
      detail <- paste(sapply(cols, function(i) i$id)[dependent.vars],
                      collapse="\n")
      tkmessageBox(icon="info", message=msg, detail=detail,
                   title="Prevented Deletion", type="ok", parent=tt)
      return()
    }

    tclvalue(list.var) <- tcl("lreplace", tclvalue(list.var), idx - 1, idx - 1)

    cols <<- cols[-idx]
    vars <<- vars[!vars %in% idx]
    for (i in seq(along=vars)) {
      if (vars[[i]] > idx)
        vars[[i]] <<- vars[[i]] - 1
    }

    tkselection.clear(frame2.lst, 0, "end")
    if (idx > length(cols))
      tkselection.set(frame2.lst, idx - 2)
    else
      tkselection.set(frame2.lst, idx - 1)
    UpdateNb()
  }

  # Edit a variables function formula

  CallEditFunction <- function() {
    idx <- as.integer(tkcurselection(frame2.lst)) + 1
    if (length(idx) == 0)
      return()

    new.fun <- EditFunction(cols, idx, tt)
    if (is.null(new.fun))
      return()

    tkconfigure(frame3.txt.5.2, state="normal")
    tcl(frame3.txt.5.2, "delete", '1.0', 'end')
    tkinsert(frame3.txt.5.2, "end", new.fun)
    tkconfigure(frame3.txt.5.2, state="disabled")

    SaveNb()
    UpdateNb()
  }

  # Edit date and time format

  CallFormatDateTime <- function() {
    old.unit <- as.character(tclvalue(unit.var))
    new.unit <- FormatDateTime(spec=old.unit, parent=tt)
    if (!is.null(new.unit))
      tclvalue(unit.var) <- new.unit
  }

  # Arrange variables in listbox

  Arrange <- function(type) {
    idx <- as.integer(tkcurselection(frame2.lst)) + 1
    if (length(idx) == 0)
      return()

    n <- length(cols)
    idxs <- 1:n

    if (type == "back") {
      if (idx == 1)
        return()
      new.idxs <- c(idx, idxs[-idx])
      new.idx <- 1
    } else if (type == "front") {
      if (idx == n)
        return()
      new.idxs <- c(idxs[-idx], idx)
      new.idx <- n
    } else if (type == "backward") {
      if (idx == 1)
        return()
      new.idxs <- 1:n
      new.idxs[c(idx - 1, idx)] <- c(idx, idx - 1)
      new.idx <- idx - 1
    } else if (type == "forward") {
      if (idx == n)
        return()
      new.idxs <- 1:n
      new.idxs[c(idx, idx + 1)] <- c(idx + 1, idx)
      new.idx <- idx + 1
    }

    cols <<- cols[new.idxs]
    vars <<- lapply(vars, function(i) idxs[new.idxs %in% i[1]])

    ids <- sapply(cols, function(i) i$id)

    for (i in 1:n)
      tclvalue(list.var) <- tcl("lreplace", tclvalue(list.var),
                                i - 1, i - 1, ids[i])
    tkselection.clear(frame2.lst, 0, "end")
    tkselection.set(frame2.lst, new.idx - 1)
  }

  # Draw histogram in canvas

  DrawHistogram <- function(h) {
    tclServiceMode(FALSE)
    tcl(frame4.cvs, "delete", "all")
    if (is.null(h))
      return()

    bars <- lapply(1:length(h$counts),
                   function(i) list(x=c(h$breaks[i], h$breaks[i + 1]),
                                    y=c(0, h$counts[i])))
    xran <- range(h$breaks, na.rm=TRUE)
    yran <- c(0, max(h$counts, na.rm=TRUE))

    for (bar in bars) {
      pts <- Xy2mn(bar$x, bar$y, xran, yran)
      mn <- rep(NA, length(pts$m) * 2)
      is.odd <- !array(0:1, length(mn))
      mn[ is.odd] <- pts$m
      mn[!is.odd] <- pts$n
      tkcreate(frame4.cvs, "rectangle", .Tcl.args(mn), fill="gray",
               outline="dark gray", width=1, tag="")
    }
    tclServiceMode(TRUE)
  }

  # Transform coordinates from real to canvas

  Xy2mn <- function(x, y, xran, yran) {
    m <- w * ((x - xran[1]) / diff(xran))
    n <- h - (h * ((y - yran[1]) / diff(yran)))
    list(m=m, n=n)
  }

  # Scale objects in canvas based on canvas size

  ScaleCanvas <- function() {
    w0 <- w
    h0 <- h
    w <<- as.numeric(tkwinfo("width",  frame4.cvs))
    h <<- as.numeric(tkwinfo("height", frame4.cvs))
    tcl(frame4.cvs, "scale", "all", 0, 0, w / w0, h / h0)
  }

  # View data for selected variable

  CallViewData <- function() {
    col.names <- as.character(tclvalue(name.var))
    if (col.names == "")
      col.names <- NA
    col.units <- as.character(tclvalue(unit.var))
    if (col.units == "")
      col.units <- NA
    col.digs <- as.integer(tclvalue(digs.var))

    fun <- as.character(tclvalue(tkget(frame3.txt.5.2, '1.0', 'end-1c')))
    d <- as.data.frame(EvalFunction(fun, cols))

    ViewData(d, col.names, col.units, col.digs, parent=tt)
  }


  # Main program

  # Assign variables

  old.cols <- cols
  ids <- sapply(cols, function(i) i$id)

  w <- 300
  h <- 50

  # Assign the variables linked to Tk widgets

  list.var <- tclVar()
  for (i in ids)
    tcl("lappend", list.var, i)

  name.var <- tclVar()
  unit.var <- tclVar()
  digs.var <- tclVar()
  clas.var <- tclVar()

  tt.done.var <- tclVar(0)

  # Open gui

  tclServiceMode(FALSE)
  tt <- tktoplevel(padx=0, pady=0)

  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(geo[2]) + 25,
                            "+", as.integer(geo[3]) + 25, sep=""))
  }
  tktitle(tt) <- "Manage Data"

  # Create menus

  top.menu <- tkmenu(tt, tearoff=0)

  menu.edit <- tkmenu(tt, tearoff=0, relief="flat")
  tkadd(top.menu, "cascade", label="Edit", menu=menu.edit, underline=0)
  tkadd(menu.edit, "command", label="New", accelerator="Ctrl+N",
        command=SaveNewVar)
  tkadd(menu.edit, "command", label="Delete", command=DeleteVar)
  tkadd(menu.edit, "separator")
  tkadd(menu.edit, "command", label="View data", command=CallViewData)

  menu.arrange <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Arrange", menu=menu.arrange, underline=0)
  tkadd(menu.arrange, "command", label="Send to top",
        accelerator="Shift+Ctrl+[", command=function() {Arrange("back")})
  tkadd(menu.arrange, "command", label="Send upward",
        accelerator="Ctrl+[", command=function() {Arrange("backward")})
  tkadd(menu.arrange, "command", label="Bring downward",
        accelerator="Ctrl+]", command=function() {Arrange("forward")})
  tkadd(menu.arrange, "command", label="Bring to bottom",
        accelerator="Shift+Ctrl+]", command=function() {Arrange("front")})

  tkconfigure(tt, menu=top.menu)

  # Frame 0, ok and cancel buttons, and size grip

  frame0 <- ttkframe(tt, relief="flat")

  frame0.but.1 <- ttkbutton(frame0, width=12, text="OK",
                            command=function() SaveChanges("ok"))
  frame0.but.2 <- ttkbutton(frame0, width=12, text="Apply",
                            command=function() SaveChanges("apply"))
  frame0.but.3 <- ttkbutton(frame0, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)

  frame0.grp.4 <- ttksizegrip(frame0)

  tkgrid(frame0.but.1, frame0.but.2, frame0.but.3, frame0.grp.4)

  tkgrid.configure(frame0.but.1, frame0.but.2, sticky="e", padx=2, pady=c(2, 8))
  tkgrid.configure(frame0.but.2, padx=c(2, 6))
  tkgrid.configure(frame0.but.3, sticky="w", padx=c(2, 0), pady=c(2, 8), rowspan=2)
  tkgrid.configure(frame0.grp.4, sticky="se")

  tkpack(frame0, side="bottom", anchor="e")

  # Frame 1, new and view variable buttons

  frame1 <- ttkframe(tt, relief="flat")

  frame1.but.1 <- ttkbutton(frame1, width=2, image=GetBitmapImage("up"),
                            command=function() Arrange("backward"))
  frame1.but.2 <- ttkbutton(frame1, width=2, image=GetBitmapImage("top"),
                            command=function() Arrange("back"))
  frame1.but.3 <- ttkbutton(frame1, width=2, image=GetBitmapImage("bottom"),
                            command=function() Arrange("front"))
  frame1.but.4 <- ttkbutton(frame1, width=2, image=GetBitmapImage("down"),
                            command=function() Arrange("forward"))
  frame1.but.5 <- ttkbutton(frame1, width=2, image=GetBitmapImage("new"),
                            command=SaveNewVar)
  frame1.but.6 <- ttkbutton(frame1, width=2, image=GetBitmapImage("view"),
                            command=CallViewData)
  frame1.but.7 <- ttkbutton(frame1, width=2, image=GetBitmapImage("delete"),
                            command=DeleteVar)

  tkgrid(frame1.but.1, frame1.but.2, frame1.but.3, frame1.but.4,
         frame1.but.5, frame1.but.6, frame1.but.7, padx=c(0, 2))
  tkgrid.configure(frame1.but.7, padx=c(5, 0))

  tkpack(frame1, side="bottom", anchor="w", padx=c(10, 0))

  # Paned window

  pw <- ttkpanedwindow(tt, orient="horizontal")

  # Frame 2, listbox with variable names

  frame2 <- tkframe(pw, relief="flat", padx=0, pady=0)

  frame2.lst <- tklistbox(frame2, selectmode="browse", activestyle="none",
                relief="flat", borderwidth=5, width=25, exportselection=FALSE,
                listvariable=list.var, highlightthickness=0)
  frame2.ysc <- ttkscrollbar(frame2, orient="vertical")
  tkconfigure(frame2.lst, background="white",
              yscrollcommand=paste(.Tk.ID(frame2.ysc), "set"))
  tkconfigure(frame2.ysc, command=paste(.Tk.ID(frame2.lst), "yview"))
  tkpack(frame2.lst, side="left",  fill="both", expand=TRUE, pady=c(2, 2))
  tkpack(frame2.ysc, side="right", fill="y", anchor="w",
         padx=c(0, 2), pady=c(2, 2))

  tkselection.set(frame2.lst, 0)

  tkadd(pw, frame2, weight=0)

  # Notebook with tabs

  nb <- ttknotebook(pw)

  # Frame 3, variable

  frame3 <- ttkframe(nb, relief="flat", padding=10, borderwidth=2)
  tkadd(nb, frame3, text="   Variable   ")

  frame3.lab.1.1 <- ttklabel(frame3, text="Name")
  frame3.lab.2.1 <- ttklabel(frame3, text="Units")
  frame3.lab.3.1 <- ttklabel(frame3, text="Format")
  frame3.lab.4.1 <- ttklabel(frame3, text="Class")
  frame3.lab.5.1 <- ttklabel(frame3, text="Function")

  frame3.ent.1.2 <- ttkentry(frame3, textvariable=name.var)
  frame3.ent.2.2 <- ttkentry(frame3, textvariable=unit.var)
  frame3.ent.3.2 <- ttkentry(frame3, textvariable=digs.var)
  frame3.ent.4.2 <- ttkentry(frame3, textvariable=clas.var, foreground="black")

  fnt <- tkfont.create(family="Courier New", size=9)
  frame3.txt.5.2 <- tktext(frame3, padx=2, pady=2, width=45, height=3,
                           undo=1, wrap="none", foreground="black",
                           background="#ebebe4", borderwidth=1, font=fnt)

  frame3.but.2.3 <- ttkbutton(frame3, text="Edit", width=5,
                              command=CallFormatDateTime)
  frame3.but.5.3 <- ttkbutton(frame3, text="Edit", width=5,
                              command=CallEditFunction)

  tkgrid(frame3.lab.1.1, frame3.ent.1.2, "x")
  tkgrid(frame3.lab.2.1, frame3.ent.2.2, frame3.but.2.3)
  tkgrid(frame3.lab.3.1, frame3.ent.3.2, "x")
  tkgrid(frame3.lab.4.1, frame3.ent.4.2, "x")
  tkgrid(frame3.lab.5.1, frame3.txt.5.2, frame3.but.5.3)

  tkgrid.configure(frame3.lab.1.1, frame3.lab.2.1,
                   frame3.lab.3.1, frame3.lab.4.1, sticky="e")

  tkgrid.configure(frame3.lab.5.1, sticky="ne")

  tkgrid.configure(frame3.ent.1.2, frame3.ent.2.2,
                   frame3.ent.3.2, frame3.ent.4.2,
                   sticky="we", padx=2, pady=2)

  tkgrid.configure(frame3.txt.5.2, padx=2, pady=2, sticky="nswe")

  tkgrid.configure(frame3.but.2.3, sticky="w")
  tkgrid.configure(frame3.lab.5.1, pady=c(4, 0))
  tkgrid.configure(frame3.but.5.3, sticky="nw", pady=c(1, 0))

  tkgrid.columnconfigure(frame3, 1, weight=1, minsize=25)
  tkgrid.rowconfigure(frame3, 4, weight=1, minsize=25)

  tkbind(frame3.ent.1.2, "<Return>", function() SetVarId())
  tkbind(frame3.ent.2.2, "<Return>", function() SetVarId())

  tkbind(frame3.ent.3.2, "<KeyRelease>",
         function() {
           tclvalue(digs.var) <- CheckEntry("integer", tclvalue(digs.var))
         })

  # Frame 4, summary

  frame4 <- ttkframe(nb, relief="flat", padding=0, borderwidth=0)
  tkadd(nb, frame4, text="   Summary   ")

  frame4.xsc <- ttkscrollbar(frame4, orient="horizontal")
  frame4.ysc <- ttkscrollbar(frame4, orient="vertical")

  frame4.txt <- tktext(frame4, bg="white", padx=2, pady=2, width=25, height=8,
                undo=1, wrap="none", foreground="black", relief="flat",
                xscrollcommand=function(...) tkset(frame4.xsc,...),
                yscrollcommand=function(...) tkset(frame4.ysc,...))

  tkconfigure(frame4.xsc, command=paste(.Tk.ID(frame4.txt), "xview"))
  tkconfigure(frame4.ysc, command=paste(.Tk.ID(frame4.txt), "yview"))

  frame4.cvs <- tkcanvas(frame4, relief="flat", width=w, height=h,
                         background="white", confine=TRUE, borderwidth=0,
                         highlightthickness=0)

  tkgrid(frame4.txt, frame4.ysc)
  tkgrid(frame4.xsc, "x")
  tkgrid(frame4.cvs)

  tkgrid.configure(frame4.txt, sticky="news", padx=0, pady=0)
  tkgrid.configure(frame4.ysc, sticky="ns", padx=0, pady=0)
  tkgrid.configure(frame4.xsc, sticky="we", padx=0, pady=0)
  tkgrid.configure(frame4.cvs, sticky="news", padx=2, pady=2, columnspan=2)

  tkgrid.columnconfigure(frame4, 0, weight=1, minsize=25)
  tkgrid.rowconfigure(frame4, 0, weight=1, minsize=25)
  tkgrid.rowconfigure(frame4, 2, weight=1)

  # Frame 5, comment

  frame5 <- ttkframe(nb, relief="flat", padding=0, borderwidth=0)
  tkadd(nb, frame5, text="   Comments   ")

  frame5.xsc <- ttkscrollbar(frame5, orient="horizontal")
  frame5.ysc <- ttkscrollbar(frame5, orient="vertical")

  frame5.txt <- tktext(frame5, bg="white", padx=2, pady=2, width=25, height=8,
                undo=1, wrap="none", foreground="black", relief="flat",
                xscrollcommand=function(...) tkset(frame5.xsc,...),
                yscrollcommand=function(...) tkset(frame5.ysc,...))

  tkconfigure(frame5.xsc, command=paste(.Tk.ID(frame5.txt), "xview"))
  tkconfigure(frame5.ysc, command=paste(.Tk.ID(frame5.txt), "yview"))

  tkpack(frame5.xsc, side="bottom", fill="x", anchor="w", padx=c(0, 15), pady=0)
  tkpack(frame5.ysc, side="right",  fill="y", anchor="w", padx=0, pady=0)
  tkpack(frame5.txt, fill="both", expand=TRUE, pady=0)

  # Insert notebook and paned window

  tkadd(pw, nb, weight=1)
  tkpack(pw, fill="both", expand="yes", padx=c(10, 15), pady=c(10, 2))

  # Bind events

  tkbind(tt, "<Control-n>", SaveNewVar)

  tkbind(tt, "<Control-]>", function() Arrange("forward"))
  tkbind(tt, "<Shift-Control-}>", function() Arrange("front"))
  tkbind(tt, "<Control-[>", function() Arrange("backward"))
  tkbind(tt, "<Shift-Control-{>", function() Arrange("back"))

  tkbind(nb, "<<NotebookTabChanged>>", ChangeTab)
  tkbind(tt, "<Configure>", ScaleCanvas)

  tkbind(frame2.lst, "<ButtonPress-1>", SaveNb)
  tkbind(frame2.lst, "<Up>", SaveNb)
  tkbind(frame2.lst, "<Down>", SaveNb)

  tkbind(frame2.lst, "<<ListboxSelect>>", UpdateNb)

  # GUI control

  UpdateNb()

  tkfocus(tt)
  tkgrab(tt)
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tclServiceMode(TRUE)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)
}
