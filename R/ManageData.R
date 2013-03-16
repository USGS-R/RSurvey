ManageData <- function(cols, vars, parent=NULL) {
  # A GUI for managing and manipulating data

  # Additional functions (subroutines)

  # Save changes and close GUI

  SaveChanges <- function(type) {
    SaveNb()
    if (!identical(cols, old.cols))
      rtn <<- list(cols=cols, vars=vars)
    if (type == "ok")
      tclvalue(tt.done.var) <- 1
  }

  # Set variable id and update functions to reflect this change

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

      str.1 <- paste("\"", old.id, "\"", sep="")
      str.2 <- paste("\"", new.id, "\"", sep="")
      funs <- sapply(cols, function(i) gsub(str.1, str.2, i$fun, fixed=TRUE))
      sapply(1:length(cols), function(i) cols[[i]]$fun <<- funs[[i]])

      new.fun <- cols[[idx]]$fun
      if (!identical(old.fun, new.fun)) {
        tkconfigure(frame2.txt.5.2, state="normal")
        tcl(frame2.txt.5.2, "delete", '1.0', 'end')
        tkinsert(frame2.txt.5.2, "end", new.fun)
        tkconfigure(frame2.txt.5.2, state="disabled")
      }
      
      query.fun <- Data("query.fun")
      if (!is.null(query.fun)) {
         query.fun <- gsub(str.1, str.2, query.fun, fixed=TRUE)
         Data("query.fun", query.fun)
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
    new.unt <- as.character(tclvalue(unit.var))
    if (new.unt == "")
      new.unt <- NULL
    cols[[idx]]$unit <<- new.unt

    # Save format

    old.fmt <- cols[[idx]]$format
    new.fmt <- as.character(tclvalue(fmt.var))
    if (new.fmt == "")
      new.fmt <- NULL
    cols[[idx]]$format <<- new.fmt

    # Save function

    old.fun <- cols[[idx]]$fun
    new.fun <- as.character(tclvalue(tkget(frame2.txt.5.2, '1.0', 'end-1c')))
    cols[[idx]]$fun <<- new.fun

    # Save summary

    is.fun <- !identical(old.fun, new.fun)
    is.fmt <- !identical(old.fmt, new.fmt)

    if (is.fun) {
      obj <- EvalFunction(new.fun, cols)
      cols[[idx]]$summary <<- SummarizeData(obj, fmt=new.fmt)
      cols[[idx]]$sample <<- na.omit(obj)[1]
    } else if (is.fmt) {
      cols[[idx]]$summary <<- SummarizeData(cols[[idx]]$summary, fmt=new.fmt)
    }

    # Save class

    if (!is.null(cols[[idx]]$summary))
      cols[[idx]]$class <<- cols[[idx]]$summary$Class

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

    saved.name <- cols[[idx]]$name
    if (is.null(saved.name))
      saved.name <- ""
    tclvalue(name.var) <- saved.name

    # Update units

    saved.unit <- cols[[idx]]$unit
    if (is.null(saved.unit))
      saved.unit <- ""
    tclvalue(unit.var) <- saved.unit

    # Update format

    tkconfigure(frame2.ent.3.2, state="normal")
    saved.fmt <- cols[[idx]]$format
    if (is.null(saved.fmt))
      saved.fmt <- ""
    tclvalue(fmt.var) <- saved.fmt
    tkconfigure(frame2.ent.3.2, state="readonly")

    # Update class

    tkconfigure(frame2.ent.4.2, state="normal")
    tclvalue(class.var) <- cols[[idx]]$class
    tkconfigure(frame2.ent.4.2, state="readonly")

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
    tkyview(frame1.lst, n - 1L)
    cols[[n + 1]] <<- list(name=id, class="logical", fun="")
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

    var.str <- paste("\"", cols[[idx]]$id, "\"", sep="")
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
    
    if(is.null(Data("data.raw"))) {
      n <- NULL
    } else {
      n <- nrow(Data("data.raw"))
    }
    
    new.fun <- EditFunction(cols, index=idx, value.length=n, parent=tt)
    
    if (is.null(new.fun)) {
      if (cols[[idx]]$fun == "")
        DeleteVar()
      return()
    }
    
    if (new.fun == "") {
      msg <- paste("Nothing is defined for the function",
                   "and the variable will be deleted.")
      ans <- as.character(tkmessageBox(icon="question", message=msg,
                                       title="Warning", type="okcancel",
                                       parent=tt))
      if (ans == "ok")
        DeleteVar()
      return()
    }

    tkconfigure(frame2.txt.5.2, state="normal")
    tcl(frame2.txt.5.2, "delete", '1.0', 'end')
    tkinsert(frame2.txt.5.2, "end", new.fun)
    tkconfigure(frame2.txt.5.2, state="disabled")

    SaveNb()
    UpdateNb()
  }

  # Edit format

  CallFormat <- function() {
    idx <- as.integer(tkcurselection(frame1.lst)) + 1

    sample.value <- cols[[idx]]$sample

    old.fmt <- as.character(tclvalue(fmt.var))
    if (inherits(sample.value, "POSIXct")) {
      new.fmt <- FormatDateTime(sample=sample.value, fmt=old.fmt, parent=tt)
    } else {
      if (is.null(sample.value))
        sample.value <- NA
      new.fmt <- Format(sample=sample.value, fmt=old.fmt, parent=tt)
    }

    if (!is.null(new.fmt))
      tclvalue(fmt.var) <- new.fmt
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
    tkselection.set(frame1.lst, new.idx - 1L)
    tkyview(frame1.lst, new.idx - 1L)
  }

  # View data for selected variable

  CallViewData <- function() {
    tkconfigure(tt, cursor="watch")
    SaveNb()
    
    nams <- sapply(cols, function(i) ifelse(is.null(i$name), NA, i$name))
    unts <- sapply(cols, function(i) ifelse(is.null(i$unit), NA, i$unit))
    fmts <- sapply(cols, function(i) ifelse(is.null(i$format), NA, i$format))
    funs <- sapply(cols, function(i) ifelse(is.null(i$fun), NA, i$fun))
    d <- lapply(1:length(cols), function(i) EvalFunction(funs[i], cols))
    
    ViewData(as.data.frame(d), nams, unts, fmts, parent=tt)
    tkconfigure(tt, cursor="arrow")
    tkfocus(tt)
  }
  
  # Build historgram
  
  CallBuildHistogram <- function(check.variable=FALSE) {
    SaveNb()
    idx <- as.integer(tkcurselection(frame1.lst)) + 1L
    if (length(idx) == 0)
      return()
    
    cont.classes <- c("integer", "numeric")
    idxs <- which(sapply(cols, function(i) i$class) %in% cont.classes)
    
    if (length(idxs) == 0 | (check.variable & !idx %in% idxs)) {
      msg <- paste("Histogram may only be built for continous variables;",
                   "that is, variables of class \"numeric\" or \"integer\".")
      tkmessageBox(icon="info", message=msg, title="Histogram", type="ok", 
                   parent=tt)
      return()
    }
    
    ids <- sapply(cols, function(i) ifelse(is.null(i$id), NA, i$id))
    funs <- sapply(cols, function(i) ifelse(is.null(i$fun), NA, i$fun))
    d <- sapply(idxs, function(i) EvalFunction(funs[i], cols))
    
    var.names <- ids[idxs]
    if (idx %in% idxs)
      var.default <- ids[idx]
    else
      var.default <- var.names[1]
    
    BuildHistogram(as.data.frame(d), var.names=var.names, 
                   var.default=var.default, parent=tt)
    tkfocus(tt)
  }


  # Main program

  # Assign variables

  rtn <- NULL

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
  fmt.var  <- tclVar()
  class.var <- tclVar()

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
  
  menu.graph <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Graph", menu=menu.graph, underline=0)
  tkadd(menu.graph, "command", label="Histogram", 
        command=function() CallBuildHistogram(FALSE))
  
  tkconfigure(tt, menu=top.menu)

  # Frame 0, ok and cancel buttons, and size grip

  frame0 <- ttkframe(tt, relief="flat")

  frame0.but.1 <- ttkbutton(frame0, width=2, image=GetBitmapImage("up"),
                            command=function() Arrange("backward"))
  frame0.but.2 <- ttkbutton(frame0, width=2, image=GetBitmapImage("top"),
                            command=function() Arrange("back"))
  frame0.but.3 <- ttkbutton(frame0, width=2, image=GetBitmapImage("bottom"),
                            command=function() Arrange("front"))
  frame0.but.4 <- ttkbutton(frame0, width=2, image=GetBitmapImage("down"),
                            command=function() Arrange("forward"))
  
  frame0.but.5 <- ttkbutton(frame0, width=2, image=GetBitmapImage("histogram"),
                            command=function() CallBuildHistogram(TRUE))
  frame0.but.6 <- ttkbutton(frame0, width=2, image=GetBitmapImage("plus"),
                            command=SaveNewVar)
  
  frame0.but.7 <- ttkbutton(frame0, width=2, image=GetBitmapImage("delete"),
                            command=DeleteVar)
  
  frame0.but.9  <- ttkbutton(frame0, width=12, text="OK",
                             command=function() SaveChanges("ok"))
  frame0.but.10 <- ttkbutton(frame0, width=12, text="Apply",
                             command=function() SaveChanges("apply"))
  frame0.but.11 <- ttkbutton(frame0, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)

  frame0.grp.12 <- ttksizegrip(frame0)

  tkgrid(frame0.but.1, frame0.but.2, frame0.but.3, frame0.but.4, frame0.but.5,
         frame0.but.6, frame0.but.7, "x", frame0.but.9, frame0.but.10, 
         frame0.but.11, frame0.grp.12)

  tkgrid.columnconfigure(frame0, 7, weight=1)

  tkgrid.configure(frame0.but.1, frame0.but.2, frame0.but.3, frame0.but.4,
                   frame0.but.5, frame0.but.6, frame0.but.7, sticky="n", 
                   padx=c(0, 2), pady=c(0, 0))
  tkgrid.configure(frame0.but.1, padx=c(10, 2))
  tkgrid.configure(frame0.but.9, frame0.but.10, frame0.but.11,
                   padx=c(0, 4), pady=c(15, 10))
  tkgrid.configure(frame0.but.11, columnspan=2, padx=c(0, 10))
  tkgrid.configure(frame0.grp.12, sticky="se")

  tkraise(frame0.but.11, frame0.grp.12)

  tkpack(frame0, fill="x", side="bottom", anchor="e")

  # Paned window

  pw <- ttkpanedwindow(tt, orient="horizontal")

  # Frame 1, listbox with variable names

  frame1 <- tkframe(pw, relief="flat", padx=0, pady=0)

  frame1.lst <- tklistbox(frame1, selectmode="browse", activestyle="none",
                relief="flat", borderwidth=5, width=25, exportselection=FALSE,
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
  frame2.lab.2.1 <- ttklabel(frame2, text="Unit")
  frame2.lab.3.1 <- ttklabel(frame2, text="Format")
  frame2.lab.4.1 <- ttklabel(frame2, text="Class")
  frame2.lab.5.1 <- ttklabel(frame2, text="Function")

  frame2.ent.1.2 <- ttkentry(frame2, textvariable=name.var)
  frame2.ent.2.2 <- ttkentry(frame2, textvariable=unit.var)
  frame2.ent.3.2 <- ttkentry(frame2, textvariable=fmt.var)
  frame2.ent.4.2 <- ttkentry(frame2, textvariable=class.var)

  fnt <- tkfont.create(family="Courier New", size=9)
  frame2.txt.5.2 <- tktext(frame2, padx=2, pady=2, width=45, height=3,
                           undo=1, wrap="none", foreground="black",
                           background="#ebebe4", borderwidth=1, font=fnt)

  frame2.but.3.3 <- ttkbutton(frame2, text="Edit", width=5,
                              command=CallFormat)
  frame2.but.5.3 <- ttkbutton(frame2, text="Edit", width=5,
                              command=CallEditFunction)

  tkgrid(frame2.lab.1.1, frame2.ent.1.2, "x")
  tkgrid(frame2.lab.2.1, frame2.ent.2.2, "x")
  tkgrid(frame2.lab.3.1, frame2.ent.3.2, frame2.but.3.3)
  tkgrid(frame2.lab.4.1, frame2.ent.4.2, "x")
  tkgrid(frame2.lab.5.1, frame2.txt.5.2, frame2.but.5.3)

  tkgrid.configure(frame2.lab.1.1, frame2.lab.2.1,
                   frame2.lab.3.1, frame2.lab.4.1, sticky="w")

  tkgrid.configure(frame2.lab.5.1, sticky="ne")

  tkgrid.configure(frame2.ent.1.2, frame2.ent.2.2,
                   frame2.ent.3.2, frame2.ent.4.2,
                   sticky="we", padx=2, pady=2)

  tkgrid.configure(frame2.txt.5.2, padx=2, pady=2, sticky="nswe")

  tkgrid.configure(frame2.but.3.3, sticky="w")
  tkgrid.configure(frame2.lab.5.1, pady=c(4, 0))
  tkgrid.configure(frame2.but.5.3, sticky="nw", pady=c(1, 0))

  tkgrid.columnconfigure(frame2, 1, weight=1, minsize=25)
  tkgrid.rowconfigure(frame2, 4, weight=1, minsize=25)

  # Frame 3, summary

  frame3 <- ttkframe(nb, relief="flat", padding=0, borderwidth=0)
  tkadd(nb, frame3, text="   Summary   ")

  frame3.ysc <- ttkscrollbar(frame3, orient="vertical")

  frame3.txt <- tktext(frame3, bg="white", padx=2, pady=2, width=25, height=8,
                undo=1, wrap="none", foreground="black", relief="flat",
                yscrollcommand=function(...) tkset(frame3.ysc,...))

  tkconfigure(frame3.ysc, command=paste(.Tk.ID(frame3.txt), "yview"))

  tkgrid(frame3.txt, frame3.ysc)

  tkgrid.configure(frame3.txt, sticky="news", padx=0, pady=0)
  tkgrid.configure(frame3.ysc, sticky="ns", padx=0, pady=0)

  tkgrid.columnconfigure(frame3, 0, weight=1, minsize=25)
  tkgrid.rowconfigure(frame3, 0, weight=1, minsize=25)

  # Insert notebook and paned window

  tkadd(pw, nb, weight=1)
  tkpack(pw, fill="both", expand="yes", padx=10, pady=c(10, 2))

  # Bind events

  tclServiceMode(TRUE)

  tkbind(tt, "<Control-n>", SaveNewVar)
  tkbind(tt, "<Control-]>", function() Arrange("forward"))
  tkbind(tt, "<Shift-Control-}>", function() Arrange("front"))
  tkbind(tt, "<Control-[>", function() Arrange("backward"))
  tkbind(tt, "<Shift-Control-{>", function() Arrange("back"))
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tkbind(nb, "<<NotebookTabChanged>>", ChangeTab)

  tkbind(frame1.lst, "<ButtonPress-1>", SaveNb)
  tkbind(frame1.lst, "<Up>", SaveNb)
  tkbind(frame1.lst, "<Down>", SaveNb)
  tkbind(frame1.lst, "<<ListboxSelect>>", UpdateNb)

  tkbind(frame2.ent.1.2, "<Return>", function() SetVarId())
  tkbind(frame2.ent.2.2, "<Return>", function() SetVarId())

  # GUI control

  UpdateNb()

  tkfocus(tt)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  rtn
}
