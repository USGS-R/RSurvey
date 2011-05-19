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
      idx <- as.integer(tkcurselection(frame1.lst)) + 1

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
    hld <- new.id
    while (new.id %in% old.ids[-idx]) {
      new.id <- paste(hld, " (", i, ")", sep="")
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
        tkconfigure(frame2.txt.5.2, state="normal")
        tcl(frame2.txt.5.2, "delete", '1.0', 'end')
        tkinsert(frame2.txt.5.2, "end", new.fun)
        tkconfigure(frame2.txt.5.2, state="disabled")
      }
    }
  }

  # Save notebook content

  SaveNb <- function() {
    idx <- as.integer(tkcurselection(frame1.lst)) + 1
    if (length(idx) == 0)
      return()

    # Save units

    old.unt <- cols[[idx]]$unit
    tmp <- tclvalue(unit.var)
    new.unt <- if (tmp == "") NULL else as.character(tmp)
    cols[[idx]]$unit <<- new.unt

    # Save decimals

    old.dig <- cols[[idx]]$digits
    tmp <- tclvalue(digs.var)
    new.dig <- if (tmp == "") NULL else as.integer(tmp)
    cols[[idx]]$digits <<- new.dig

    # Save function

    old.fun <- cols[[idx]]$fun
    tmp <- as.character(tclvalue(tkget(frame2.txt.5.2, '1.0', 'end-1c')))
    new.fun <- if (tmp == "") "NA" else as.character(tmp)
    cols[[idx]]$fun <<- new.fun

    # Save summary

    is.dig <- !identical(old.dig, new.dig)
    is.unt <- !identical(old.unt, new.unt) && cols[[idx]]$class == "POSIXct"
    is.fun <- !identical(old.fun, new.fun)
    if (is.dig || is.unt || is.fun) {
      if (is.fun)
        sum.dat <- SummarizeData(EvalFunction(new.fun, cols), new.dig, new.unt)
      else
        sum.dat <- SummarizeData(cols[[idx]]$summary, new.dig, new.unt)
      cols[[idx]]$summary <<- sum.dat
    }

    # Save class

    if (!is.null(cols[[idx]]$summary))
      cols[[idx]]$class <<- cols[[idx]]$summary$Class

    # Save comments

    tmp <- sub("\n$", "", tclvalue(tkget(frame4.txt, '1.0', 'end')))
    cols[[idx]]$comments <<- if (tmp == "") NULL else tmp

    # Save name

    SetVarId(idx)
  }

  # Update notebook content

  UpdateNb <- function() {
    idx <- as.integer(tkcurselection(frame1.lst)) + 1
    if (length(idx) == 0) {
      SaveNewVar()
      return()
    }

    # Update name

    tmp <- cols[[idx]]$name
    tclvalue(name.var) <- if (is.null(tmp)) "" else tmp

    # Update units

    tmp <- cols[[idx]]$unit
    tclvalue(unit.var) <- if (is.null(tmp)) "" else tmp

    if (cols[[idx]]$class == "POSIXct") {
      tkconfigure(frame2.ent.2.2, state="readonly")
      tkconfigure(frame2.but.2.3, state="normal")
    } else {
      tkconfigure(frame2.ent.2.2, state="normal")
      tkconfigure(frame2.but.2.3, state="disabled")
    }

    # Update decimals

    tkconfigure(frame2.ent.3.2, state="normal")
    tclvalue(digs.var) <- cols[[idx]]$digits
    if (cols[[idx]]$class == "numeric") {
      tmp <- cols[[idx]]$digits
      tclvalue(digs.var) <- if (is.null(tmp)) "" else tmp
    } else {
      tclvalue(digs.var) <- ""
      tkconfigure(frame2.ent.3.2, state="disabled")
    }

    # Update class

    tkconfigure(frame2.ent.4.2, state="normal")
    tclvalue(clas.var) <- cols[[idx]]$class
    tkconfigure(frame2.ent.4.2, state="disabled")

    # Update function

    tkconfigure(frame2.txt.5.2, state="normal")
    tcl(frame2.txt.5.2, "delete", '1.0', 'end')
    tkinsert(frame2.txt.5.2, "end", cols[[idx]]$fun)
    tkconfigure(frame2.txt.5.2, state="disabled")

    s <- "disabled"
    if (is.null(cols[[idx]]$index))
      s <- "normal"
    tkconfigure(frame2.but.5.3, state=s)

    # Update summary

    tkconfigure(frame3.txt, state="normal")
    tcl(frame3.txt, "delete", '1.0', 'end')

    sum.str <- cols[[idx]]$summary$String
    if (!is.null(sum.str))
      tkinsert(frame3.txt, "end", sum.str)

    tkconfigure(frame3.txt, state="disabled")

    DrawHistogram(cols[[idx]]$summary$Hist)

    # Update comments

    tcl(frame4.txt, "delete", '1.0', 'end')
    comments <- cols[[idx]]$comments
    if (!is.null(comments))
      tkinsert(frame4.txt, "end", comments, sep="")
  }

  # Account for change in notebook tab

  ChangeTab <- function() {
    idx <- as.integer(tkcurselection(frame1.lst)) + 1
    if (length(idx) == 0)
      return()

    SaveNb()
    UpdateNb()

    tabid <- tclvalue(tcl(nb, 'select'))

    # Arrive at variable tab

    if (tabid == frame2$ID) {
      tkfocus(frame2)

    # Arrive at comments tab

    } else if (tabid == frame4$ID) {
      tkfocus(frame4.txt)

    # Arrive at summary tab

    } else if (tabid == frame3$ID) {
      tkfocus(frame3)
    }
  }

  # Save new variable

  SaveNewVar <- function() {
    SaveNb()
    id <- "New Variable"
    tcl("lappend", list.var, id)
    n <- length(cols)
    tkselection.clear(frame1.lst, 0, n)
    tkselection.set(frame1.lst, n, n)
    cols[[n + 1]] <<- list(name=id, class="logical", fun="NA")
    UpdateNb()
    SetVarId()

    if (n > 0)
      CallEditFunction()
  }

  # Delete existing variable

  DeleteVar <- function() {
    idx <- as.integer(tkcurselection(frame1.lst)) + 1
    if (length(idx) == 0)
      return()

    txt <- paste("DATA[[\"", cols[[idx]]$id, "\"]]", sep="")
    tmp <- grep(txt, sapply(cols, function(i) i$fun), fixed=TRUE)
    dependent.vars <- tmp[!tmp %in% idx]

    if (length(dependent.vars) > 0) {
      msg <- paste("Removal of this variable first requires that the",
                   "following dependent variable(s) be removed:", sep="\n")
      detail <- paste(sapply(cols, function(i) i$id)[dependent.vars],
                      collapse="\n")
      tkmessageBox(icon="info", message=msg, detail=detail,
                   title="Deletion Cancelled", type="ok", parent=tt)
      return()
    }

    tclvalue(list.var) <- tcl("lreplace", tclvalue(list.var), idx - 1, idx - 1)

    cols <<- cols[-idx]
    vars <<- vars[!vars %in% idx]
    for (i in seq(along=vars)) {
      if (vars[[i]] > idx)
        vars[[i]] <<- vars[[i]] - 1
    }

    tkselection.clear(frame1.lst, 0, "end")
    if (idx > length(cols))
      tkselection.set(frame1.lst, idx - 2)
    else
      tkselection.set(frame1.lst, idx - 1)
    UpdateNb()
  }

  # Edit a variables function formula

  CallEditFunction <- function() {
    idx <- as.integer(tkcurselection(frame1.lst)) + 1
    if (length(idx) == 0)
      return()

    new.fun <- EditFunction(cols, idx, tt)
    if (is.null(new.fun))
      return()

    tkconfigure(frame2.txt.5.2, state="normal")
    tcl(frame2.txt.5.2, "delete", '1.0', 'end')
    tkinsert(frame2.txt.5.2, "end", new.fun)
    tkconfigure(frame2.txt.5.2, state="disabled")

    SaveNb()
    UpdateNb()
  }

  # Edit date and time format

  CallEditDateFormat <- function() {
    old.unit <- as.character(tclvalue(unit.var))
    new.unit <- EditDateFormat(old.unit, tt)
    if (!is.null(new.unit))
      tclvalue(unit.var) <- new.unit
  }

  # Arrange variables in listbox

  Arrange <- function(type) {
    idx <- as.integer(tkcurselection(frame1.lst)) + 1
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
    tkselection.clear(frame1.lst, 0, "end")
    tkselection.set(frame1.lst, new.idx - 1)
  }

  # Draw histogram in canvas

  DrawHistogram <- function(h) {
    tclServiceMode(FALSE)
    tcl(frame3.cvs, "delete", "all")
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
      tkcreate(frame3.cvs, "rectangle", .Tcl.args(mn), fill="gray",
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
    w <<- as.numeric(tkwinfo("width",  frame3.cvs))
    h <<- as.numeric(tkwinfo("height", frame3.cvs))
    tcl(frame3.cvs, "scale", "all", 0, 0, w / w0, h / h0)
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

    fun <- as.character(tclvalue(tkget(frame2.txt.5.2, '1.0', 'end-1c')))
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
    tmp <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(tmp[2]) + 25,
                            "+", as.integer(tmp[3]) + 25, sep=""))
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

  tkgrid.configure(frame0.but.1, frame0.but.2, sticky="e", padx=2, pady=c(5, 8))
  tkgrid.configure(frame0.but.2, padx=c(2, 6))
  tkgrid.configure(frame0.but.3, sticky="w", padx=2, pady=c(5, 8), rowspan=2)
  tkgrid.configure(frame0.grp.4, sticky="se")

  tkpack(frame0, side="bottom", anchor="e")

  # Paned window

  pw <- ttkpanedwindow(tt, orient="horizontal")

  # Frame 1, listbox with variable names

  frame1 <- tkframe(pw, relief="flat", padx=0, pady=0)

  frame1.lst <- tklistbox(frame1, selectmode="browse", activestyle="none",
                relief="flat", borderwidth=5, width=20, exportselection=FALSE,
                listvariable=list.var, highlightthickness=0)
  frame1.ysc <- ttkscrollbar(frame1, orient="vertical")
  tkconfigure(frame1.lst, background="white",
              yscrollcommand=paste(.Tk.ID(frame1.ysc), "set"))
  tkconfigure(frame1.ysc, command=paste(.Tk.ID(frame1.lst), "yview"))
  tkpack(frame1.lst, side="left",  fill="both", expand=TRUE, pady=c(2, 2))
  tkpack(frame1.ysc, side="right", fill="y", anchor="w",
         padx=c(0, 2), pady=c(2, 2))

  tkselection.set(frame1.lst, 0)

  tkadd(pw, frame1, weight=0)

  # Notebook with tabs

  nb <- ttknotebook(pw)

  # Frame 2, variable

  frame2 <- ttkframe(nb, relief="flat", padding=10, borderwidth=2)
  tkadd(nb, frame2, text="   Variable   ")

  frame2.lab.1.1 <- ttklabel(frame2, text="Name")
  frame2.lab.2.1 <- ttklabel(frame2, text="Units")
  frame2.lab.3.1 <- ttklabel(frame2, text="Decimals")
  frame2.lab.4.1 <- ttklabel(frame2, text="Class")
  frame2.lab.5.1 <- ttklabel(frame2, text="Function")

  frame2.ent.1.2 <- ttkentry(frame2, textvariable=name.var)
  frame2.ent.2.2 <- ttkentry(frame2, textvariable=unit.var)
  frame2.ent.3.2 <- ttkentry(frame2, textvariable=digs.var)
  frame2.ent.4.2 <- ttkentry(frame2, textvariable=clas.var, foreground="black")

  fnt <- tkfont.create(family="Courier New", size=9)
  frame2.txt.5.2 <- tktext(frame2, padx=2, pady=2, width=45, height=3,
                           undo=1, wrap="none", foreground="black",
                           background="#ebebe4", borderwidth=1, font=fnt)

  frame2.but.2.3 <- ttkbutton(frame2, text="Edit", width=5,
                              command=CallEditDateFormat)
  frame2.but.5.3 <- ttkbutton(frame2, text="Edit", width=5,
                              command=CallEditFunction)

  tkgrid(frame2.lab.1.1, frame2.ent.1.2, "x", padx=1, pady=1)
  tkgrid(frame2.lab.2.1, frame2.ent.2.2, frame2.but.2.3, padx=1, pady=1)
  tkgrid(frame2.lab.3.1, frame2.ent.3.2, "x", padx=1, pady=1)
  tkgrid(frame2.lab.4.1, frame2.ent.4.2, "x", padx=1, pady=1)
  tkgrid(frame2.lab.5.1, frame2.txt.5.2, frame2.but.5.3, padx=1, pady=1)

  tkgrid.configure(frame2.lab.1.1, frame2.lab.2.1,
                   frame2.lab.3.1, frame2.lab.4.1,
                   sticky="e", padx=1, pady=1)

  tkgrid.configure(frame2.lab.5.1, sticky="ne")

  tkgrid.configure(frame2.ent.1.2, frame2.ent.2.2,
                   frame2.ent.3.2, frame2.ent.4.2,
                   sticky="we", padx=1, pady=1)

  tkgrid.configure(frame2.txt.5.2, sticky="nswe")

  tkgrid.configure(frame2.but.2.3, frame2.but.5.3, sticky="nw", padx=c(2, 0))

  tkgrid.columnconfigure(frame2, 1, weight=1, minsize=25)
  tkgrid.rowconfigure(frame2, 4, weight=1, minsize=25)

  tkbind(frame2.ent.1.2, "<Return>", function() SetVarId())
  tkbind(frame2.ent.2.2, "<Return>", function() SetVarId())

  tkbind(frame2.ent.3.2, "<KeyRelease>",
         function() {
           tclvalue(digs.var) <- CheckEntry("integer", tclvalue(digs.var))
         })

  # Frame 3, summary

  frame3 <- ttkframe(nb, relief="flat", padding=0, borderwidth=0)
  tkadd(nb, frame3, text="   Summary   ")

  frame3.xsc <- ttkscrollbar(frame3, orient="horizontal")
  frame3.ysc <- ttkscrollbar(frame3, orient="vertical")

  frame3.txt <- tktext(frame3, bg="white", padx=2, pady=2, width=25, height=8,
                undo=1, wrap="none", foreground="black", relief="flat",
                xscrollcommand=function(...) tkset(frame3.xsc,...),
                yscrollcommand=function(...) tkset(frame3.ysc,...))

  tkconfigure(frame3.xsc, command=paste(.Tk.ID(frame3.txt), "xview"))
  tkconfigure(frame3.ysc, command=paste(.Tk.ID(frame3.txt), "yview"))

  frame3.cvs <- tkcanvas(frame3, relief="flat", width=w, height=h,
                         background="white", confine=TRUE, borderwidth=0,
                         highlightthickness=0)

  tkgrid(frame3.txt, frame3.ysc)
  tkgrid(frame3.xsc, "x")
  tkgrid(frame3.cvs)

  tkgrid.configure(frame3.txt, sticky="news", padx=0, pady=0)
  tkgrid.configure(frame3.ysc, sticky="ns", padx=0, pady=0)
  tkgrid.configure(frame3.xsc, sticky="we", padx=0, pady=0)
  tkgrid.configure(frame3.cvs, sticky="news", padx=2, pady=2, columnspan=2)

  tkgrid.columnconfigure(frame3, 0, weight=1, minsize=25)
  tkgrid.rowconfigure(frame3, 0, weight=1, minsize=25)
  tkgrid.rowconfigure(frame3, 2, weight=1)

  # Frame 4, comment

  frame4 <- ttkframe(nb, relief="flat", padding=0, borderwidth=0)
  tkadd(nb, frame4, text="   Comments   ")

  frame4.xsc <- ttkscrollbar(frame4, orient="horizontal")
  frame4.ysc <- ttkscrollbar(frame4, orient="vertical")

  frame4.txt <- tktext(frame4, bg="white", padx=2, pady=2, width=25, height=8,
                undo=1, wrap="none", foreground="black", relief="flat",
                xscrollcommand=function(...) tkset(frame4.xsc,...),
                yscrollcommand=function(...) tkset(frame4.ysc,...))

  tkconfigure(frame4.xsc, command=paste(.Tk.ID(frame4.txt), "xview"))
  tkconfigure(frame4.ysc, command=paste(.Tk.ID(frame4.txt), "yview"))

  tkpack(frame4.xsc, side="bottom", fill="x", anchor="w", padx=c(0, 15), pady=0)
  tkpack(frame4.ysc, side="right",  fill="y", anchor="w", padx=0, pady=0)
  tkpack(frame4.txt, fill="both", expand=TRUE, pady=0)

  # Insert notebook and paned window

  tkadd(pw, nb, weight=1)
  tkpack(pw, fill="both", expand="yes", padx=c(10, 15), pady=c(10, 5))

  # Bind events

  tkbind(tt, "<Control-n>", SaveNewVar)

  tkbind(tt, "<Control-]>", function() Arrange("forward"))
  tkbind(tt, "<Shift-Control-}>", function() Arrange("front"))
  tkbind(tt, "<Control-[>", function() Arrange("backward"))
  tkbind(tt, "<Shift-Control-{>", function() Arrange("back"))

  tkbind(nb, "<<NotebookTabChanged>>", ChangeTab)
  tkbind(tt, "<Configure>", ScaleCanvas)

  tkbind(frame1.lst, "<ButtonPress-1>", SaveNb)
  tkbind(frame1.lst, "<Up>", SaveNb)
  tkbind(frame1.lst, "<Down>", SaveNb)

  tkbind(frame1.lst, "<<ListboxSelect>>", UpdateNb)

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
