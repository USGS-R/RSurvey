EditFunction <- function(cols, index=NULL, fun=NULL, value.length=NULL, 
                         value.class=NULL, win.title="Edit Function",  
                         parent=NULL) {
  # A GUI for defining a function in the R language with a focus on table data.

  # Additional functions (subroutines)

  # Rebuild list box based on selected class type to show

  RebuildList <- function() {
    idx <- as.integer(tcl(frame1.box.3.1, "current"))
    show.ids <- ids
    if (idx > 0)
      show.ids <- ids[cls %in% classes[idx]]

    tclvalue(variable.var) <- ""
    for (i in seq(along=show.ids))
      tcl("lappend", variable.var, show.ids[i])

    tkselection.clear(frame1.lst.2.1, 0, "end")
    tkfocus(frame2.txt.2.1)
  }

  # Insert character string into text box

  InsertString <- function(txt, sel="<variable>") {
    tcl(frame2.txt.2.1, "edit", "separator")
    seltxt <- as.character(tktag.ranges(frame2.txt.2.1, 'sel'))
    if (length(seltxt) > 1)
      tcl(frame2.txt.2.1, "delete", seltxt[1], seltxt[2])

    cur <- as.character(tkindex(frame2.txt.2.1, "insert"))
    cur <- as.integer(strsplit(cur, ".", fixed=TRUE)[[1]])
    cur.line <- cur[1]
    cur.char <- cur[2]

    tkinsert(frame2.txt.2.1, "insert", txt)
    tkfocus(frame2.txt.2.1)

    if (txt %in% c("()", "[]")) {
      cursor.insert <- paste(cur.line, cur.char + 1, sep=".")
      tkmark.set(frame2.txt.2.1, "insert", cursor.insert)
    } else {
      search.txt <- gregexpr(pattern=sel, txt)[[1]]
      if (search.txt[1] > 0) {
        match.idx <- search.txt[1]
        match.len <- attr(search.txt, "match.length")[1]
        tkfocus(frame2.txt.2.1)

        char <- c(match.idx, match.idx + match.len) + cur.char - 1
        sel0 <- paste(cur.line, char[1], sep=".")
        sel1 <- paste(cur.line, char[2], sep=".")
        tktag.add(frame2.txt.2.1, 'sel', sel0, sel1)
        tkmark.set(frame2.txt.2.1, "insert", sel1)
      }
    }
  }

  # Insert variable into text box

  InsertVar <- function() {
    idx <- as.integer(tkcurselection(frame1.lst.2.1))
    if (length(idx) == 0)
      return()
    id <- as.character(tkget(frame1.lst.2.1, idx, idx))
    txt <- paste("\"", id, "\"", sep="")
    InsertString(txt)
  }

  # Save function

  SaveFunction <- function() {
    txt <- as.character(tclvalue(tkget(frame2.txt.2.1, '1.0', 'end-1c')))
    if (txt == "") {
      new.fun <<- ""
    } else {
      fun <- txt
      pattern <- paste("\"", ids, "\"", sep="")
      replacement <- paste("DATA[[", 1:length(ids), "]]", sep="")
      for (i in seq(along=ids))
        fun <- gsub(pattern[i], replacement[i], fun, fixed=TRUE)
      fun <- paste("function(DATA) {", fun, "}", sep="")
      
      fun <- try(parse(text=fun), silent=TRUE)
      if (inherits(fun, "try-error")) {
        msg <- "There's a problem with function syntax, try revising."
        tkmessageBox(icon="error", message=msg, detail=fun, title="Error",
                     type="ok", parent=tt)
        return()
      }
      val <- EvalFunction(txt, cols)
      if (inherits(val, "try-error")) {
        msg <- "Function results in error during evaluation, try revising."
        tkmessageBox(icon="error", message=msg, detail=val, title="Error",
                     type="ok", parent=tt)
        return()
      }
      
      if (!is.null(value.length) && length(val) != value.length) {
        msg <- paste("Evaluated function must be of length ", value.length, 
                     ", try revising.", sep="")
        dtl <- paste("Resulting object is currently of length ", length(val), 
                     ".", sep="")
        tkmessageBox(icon="error", message=msg, detail=dtl, title="Error",
                     type="ok", parent=tt)
        return()
      }
      
      if (!is.null(value.class) && !inherits(val, value.class)) {
        msg <- paste("A query must result in an object of class \"", value.class, 
                     "\". The evaluated function is an object of class \"", 
                     class(val), "\", please revise.", sep="")
        tkmessageBox(icon="error", message=msg, title="Error", type="ok", 
                     parent=tt)
        return()
      }
      
      new.fun <<- txt
    }
    tclvalue(tt.done.var) <- 1
  }

  # Call date and time format editor

  CallFormatDateTime <- function() {
    spec <- FormatDateTime(parent=tt)
    tkfocus(frame2.txt.2.1)
    if(!is.null(spec))
      InsertString(spec)
  }

  # Text edit functions

  EditUndo <- function() {
    tkfocus(frame2.txt.2.1)
    tcl(frame2.txt.2.1, "edit", "undo")
  }
  EditRedo <- function() {
    tkfocus(frame2.txt.2.1)
    tcl(frame2.txt.2.1, "edit", "redo")
  }
  EditCut <- function() {
    tkfocus(frame2.txt.2.1)
    tcl("tk_textCut", frame2.txt.2.1)
  }
  EditCopy <- function() {
    tkfocus(frame2.txt.2.1)
    tcl("tk_textCopy", frame2.txt.2.1)
  }
  EditPaste <- function() {
    tkfocus(frame2.txt.2.1)
    tcl("tk_textPaste", frame2.txt.2.1)
  }
  EditSelectAll <- function() {
    tkfocus(frame2.txt.2.1)
    tktag.add(frame2.txt.2.1, "sel", "1.0", "end")
  }
  
  # Clear all
  
  ClearAll <- function() {
    tcl(frame2.txt.2.1, "delete", "1.0", "end")
    tkfocus(frame2.txt.2.1)
  }
  
  # Show unique values
  
  ShowUniqueValues <- function() {
    idx <- as.integer(tkcurselection(frame1.lst.2.1))
    if (length(idx) == 0)
      return()
    else
      idx <- idx + 1L
    var.fmt <- cols[[idx]]$format
    
    tkconfigure(tt, cursor="watch")
    var.vals <- unique(EvalFunction(cols[[idx]]$fun, cols))
    var.class <- cols[[idx]]$class

    n <- length(var.vals)
    if (n > 50000) {
      msg <- paste("There are", n, "unique values; this operation can be",
                   "computationally expensive. Would you like to continue?")
      ans <- as.character(tkmessageBox(icon="question", message=msg,
                                       title="Warning", type="okcancel",
                                       parent=tt))
      if (ans == "cancel") {
        tkconfigure(tt, cursor="arrow")
        return()
      }
    }
    
    var.vals <- sort(var.vals, na.last=TRUE)
    if (is.null(var.fmt)) {
      var.vals.txt <- format(var.vals)
    } else if (var.class == "POSIXct") {
      var.vals.txt <- format(var.vals, format=var.fmt)
    } else {
      var.vals.txt <- try(sprintf(var.fmt, var.vals), silent=TRUE)
      if (inherits(var.vals.txt, "try-error"))
        var.vals.txt <- format(var.vals)
    }
    
    tclvalue(value.var) <- ""
    for (i in seq(along=var.vals.txt))
      tcl("lappend", value.var, var.vals.txt[i])
    tkselection.clear(frame1.lst.4.1, 0, "end")
    tkconfigure(frame1.but.5.1, state="disabled")
    tkfocus(frame2.txt.2.1)
    tkconfigure(tt, cursor="arrow")
  }
  
  # Change variable selection
  
  ChangeVar <- function() {
    tclvalue(value.var) <- ""
    tkconfigure(frame1.but.5.1, state="normal")
  }
  
  # Insert value into text box
  
  InsertValue <- function() {
    idx <- as.integer(tkcurselection(frame1.lst.2.1))
    if (length(idx) == 0)
      return()
    else
      idx <- idx + 1L
    var.fmt <- cols[[idx]]$format
    var.class <- cols[[idx]]$class
    
    idx <- as.integer(tkcurselection(frame1.lst.4.1))
    if (length(idx) == 0)
      return()
    val <- as.character(tkget(frame1.lst.4.1, idx, idx))
    
    if (var.class == "factor" && is.na(suppressWarnings(as.numeric(val))))
      var.class <- "character"
    
    if (var.class == "POSIXct") {
      txt <- paste("as.POSIXct(\"", val, "\", format = \"", var.fmt, "\")", 
                   sep="")
    } else {
      val <- gsub("^\\s+|\\s+$", "", val)
      if (var.class == "character" && val != "NA") 
        txt <- paste("\"", val, "\"", sep="")
      else
        txt <- val
    }
    
    InsertString(txt)
  }


  # Main program
  
  if (is.null(index)) {
    old.fun <- fun
  } else {
    old.fun <- cols[[as.integer(index)]]$fun
  }
  new.fun <- NULL

  cls <- sapply(cols, function(i) i$class)
  ids <- sapply(cols, function(i) i$id)
  if (!is.null(index)) {
    edit.fun.id <- ids[index]
    ids <- ids[-index]
    cls <- cls[-index]
  }

  # Class types

  classes <- c("numeric", "integer", "POSIXct", "logical",
               "character", "factor")
  classes <- classes[classes %in% cls]

  # String templates for commands

  cmd <- list()

  cmd$as.numeric <- "as.numeric(<variable>)"
  cmd$as.integer <- "as.integer(<variable>)"
  cmd$as.logical <- "as.logical(<variable>)"
  cmd$as.character <- "as.character(<variable>)"
  cmd$as.factor <- "as.factor(<variable>)"
  cmd$as.POSIXct <- "as.POSIXct(<variable>, format = \"<format>\")"
  
  cmd$abs  <- "abs(<variable>)"
  cmd$sqrt <- "sqrt(<variable>)"
  cmd$exp  <- "exp(<variable>)"
  cmd$log  <- "log(<variable>, base = exp(1))"

  cmd$paste <-  "paste(<variable>, <variable>, sep = \" \")"
  cmd$substr <- paste("substr(<variable>, start = <first element>,",
                      "stop = <last element>)")
  
  cmd$sum  <- "sum(<variable>, na.rm = TRUE)"
  cmd$prod <- "prod(<variable>, na.rm = TRUE)"
  cmd$min  <- "min(<variable>, na.rm = TRUE)"
  cmd$max  <- "max(<variable>, na.rm = TRUE)"

  # Assign variables linked to Tk widgets

  variable.var <- tclVar()
  for (i in seq(along=ids))
    tcl("lappend", variable.var, ids[i]) # must be unique
  value.var <- tclVar()
  tt.done.var <- tclVar(0)

  # Open GUI

  tclServiceMode(FALSE)

  tt <- tktoplevel(padx=0, pady=0)
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste("+", as.integer(geo[2]) + 25,
                            "+", as.integer(geo[3]) + 25, sep=""))
  }
  tktitle(tt) <- win.title

  # Top menu

  top.menu <- tkmenu(tt, tearoff=0)

  # Project menu

  menu.edit <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Edit", menu=menu.edit, underline=0)
  tkadd(menu.edit, "command", label="Undo", accelerator="Ctrl+Z",
        command=EditUndo)
  tkadd(menu.edit, "command", label="Redo", accelerator="Ctrl+Y",
        command=EditRedo)
  tkadd(menu.edit, "separator")
  tkadd(menu.edit, "command", label="Cut", accelerator="Ctrl+X",
        command=EditCut)
  tkadd(menu.edit, "command", label="Copy", accelerator="Ctrl+C",
        command=EditCopy)
  tkadd(menu.edit, "command", label="Paste", accelerator="Ctrl+V",
        command=EditPaste)
  tkadd(menu.edit, "separator")
  tkadd(menu.edit, "command", label="Select all", accelerator="Ctrl+A",
        command=EditSelectAll)
  tkadd(menu.edit, "command", label="Clear all", 
        command=ClearAll)

  menu.class <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Class", menu=menu.class, underline=0)
  tkadd(menu.class, "command", label="As numeric",
        command=function() InsertString(cmd$as.numeric))
  tkadd(menu.class, "command", label="As integer",
        command=function() InsertString(cmd$as.integer))
  tkadd(menu.class, "command", label="As POSIXct",
        command=function() InsertString(cmd$as.POSIXct))
  tkadd(menu.class, "command", label="As logical",
        command=function()  InsertString(cmd$as.logical))
  tkadd(menu.class, "command", label="As character",
        command=function() InsertString(cmd$as.character))
  tkadd(menu.class, "command", label="As factor",
        command=function() InsertString(cmd$as.factor))
  
  menu.math <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Math", menu=menu.math, underline=0)
  tkadd(menu.math, "command", label="Absolute value",
        command=function() InsertString(cmd$abs))
  tkadd(menu.math, "command", label="Square root",
        command=function() InsertString(cmd$sqrt))
  tkadd(menu.math, "command", label="Exponential",
        command=function() InsertString(cmd$exp))
  tkadd(menu.math, "command", label="Logarithm",
        command=function() InsertString(cmd$log))
  
  menu.string <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="String", menu=menu.string, underline=0)
  tkadd(menu.string, "command", label="Concatenate",
        command=function() InsertString(cmd$paste))
  tkadd(menu.string, "command", label="Extract substring",
        command=function() InsertString(cmd$substr))
  
  menu.summary <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Summary", menu=menu.summary, underline=0)
  tkadd(menu.summary, "command", label="Sum",
        command=function() InsertString(cmd$sum))
  tkadd(menu.summary, "command", label="Product",
        command=function() InsertString(cmd$prod))
  tkadd(menu.summary, "command", label="Minimum",
        command=function() InsertString(cmd$min))
  tkadd(menu.summary, "command", label="Maximum",
        command=function() InsertString(cmd$max))
  
  menu.const <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Constant", menu=menu.const, underline=0)
  tkadd(menu.const, "command", label="True",
        command=function() InsertString("TRUE"))
  tkadd(menu.const, "command", label="False",
        command=function() InsertString("FALSE"))
  tkadd(menu.const, "command", label="Not available",
        command=function() InsertString("NA"))
  tkadd(menu.const, "command", label="Pi",
        command=function() InsertString("pi"))
  
  menu.operator <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Operator", menu=menu.operator, underline=0)
  tkadd(menu.operator, "command", label="And",
        command=function() InsertString(" & "))
  tkadd(menu.operator, "command", label="Or",
        command=function() InsertString(" | "))
  tkadd(menu.operator, "command", label="Not",
        command=function() InsertString("!"))
  
  menu.tool <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Tool", menu=menu.tool, underline=0)
  tkadd(menu.tool, "command", label="Build date and time format",
        command=CallFormatDateTime)

  # Finalize top menu

  tkconfigure(tt, menu=top.menu)

  # Frame 0, ok and cancel buttons, and size grip

  frame0 <- tkframe(tt, relief="flat", padx=0, pady=0)

  frame0.but.2 <- ttkbutton(frame0, width=12, text="OK",
                            command=SaveFunction)
  frame0.but.3 <- ttkbutton(frame0, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)
  frame0.grp.4 <- ttksizegrip(frame0)

  tkgrid("x", frame0.but.2, frame0.but.3, frame0.grp.4)

  tkgrid.columnconfigure(frame0, 0, weight=1)

  tkgrid.configure(frame0.but.2, frame0.but.3, padx=c(0, 4), pady=c(15, 10))
  tkgrid.configure(frame0.but.3, columnspan=2, padx=c(0, 10))

  tkgrid.configure(frame0.grp.4, sticky="se")

  tkraise(frame0.but.3, frame0.grp.4)

  tkpack(frame0, fill="x", side="bottom", anchor="e")

  # Paned window

  pw <- ttkpanedwindow(tt, orient="horizontal")

  # Frame 1

  frame1 <- tkframe(pw, relief="flat", padx=0, pady=0)

  frame1.lab.1.1 <- ttklabel(frame1, text="Double click to insert variable",
                             foreground="#414042")
  frame1.lst.2.1 <- tklistbox(frame1, selectmode="browse", activestyle="none",
                              relief="flat", borderwidth=5, width=25, height=8,
                              exportselection=FALSE, listvariable=variable.var,
                              highlightthickness=0)
  frame1.ysc.2.2 <- ttkscrollbar(frame1, orient="vertical")
  box.vals <- "{Show all classes}"
  if (length(classes) > 1)
    box.vals <- c("Show all classes", classes)
  frame1.box.3.1 <- ttkcombobox(frame1, state="readonly", value=box.vals)
  tkconfigure(frame1.lst.2.1, background="white",
              yscrollcommand=paste(.Tk.ID(frame1.ysc.2.2), "set"))
  tkconfigure(frame1.ysc.2.2, command=paste(.Tk.ID(frame1.lst.2.1), "yview"))
  tcl(frame1.box.3.1, "current", 0)
  
  frame1.lst.4.1 <- tklistbox(frame1, selectmode="browse", activestyle="none",
                              relief="flat", borderwidth=5, width=25, height=5,
                              exportselection=FALSE, listvariable=value.var,
                              highlightthickness=0)
  frame1.ysc.4.2 <- ttkscrollbar(frame1, orient="vertical")
  frame1.but.5.1 <- ttkbutton(frame1, width=15, text="Unique Values",
                              command=ShowUniqueValues)
  tkconfigure(frame1.lst.4.1, background="white",
              yscrollcommand=paste(.Tk.ID(frame1.ysc.4.2), "set"))
  tkconfigure(frame1.ysc.4.2, command=paste(.Tk.ID(frame1.lst.4.1), "yview"))
  tkconfigure(frame1.but.5.1, state="disabled")
  
  tkgrid(frame1.lab.1.1, "x")
  tkgrid(frame1.lst.2.1, frame1.ysc.2.2)
  tkgrid(frame1.box.3.1, "x")
  tkgrid(frame1.lst.4.1, frame1.ysc.4.2)
  tkgrid(frame1.but.5.1, "x")

  tkgrid.configure(frame1.lab.1.1, padx=c(10, 0), pady=c(10, 0), sticky="w")
  tkgrid.configure(frame1.lst.2.1, padx=c(10, 0), pady=c(2, 0),  sticky="nsew")
  tkgrid.configure(frame1.ysc.2.2, padx=c(0, 0),  pady=c(2, 0),  sticky="ns")
  tkgrid.configure(frame1.box.3.1, padx=c(10, 0), pady=c(4, 0),  sticky="we")
  tkgrid.configure(frame1.lst.4.1, padx=c(10, 0), pady=c(15, 0), sticky="nsew")
  tkgrid.configure(frame1.ysc.4.2, padx=c(0, 0),  pady=c(15, 0), sticky="ns")
  tkgrid.configure(frame1.but.5.1, padx=c(10, 0), pady=c(2, 0))

  tkgrid.rowconfigure(frame1, 1, weight=1)
  tkgrid.rowconfigure(frame1, 3, weight=1)
  tkgrid.columnconfigure(frame1, 0, weight=1, minsize=20)

  # Frame 2

  frame2 <- tkframe(pw, relief="flat", padx=0, pady=0)

  txt <- "Define function"
  if (!is.null(index))
    txt <- paste(txt, " for \"", edit.fun.id, "\"", sep="")
  frame2.lab.1.1 <- ttklabel(frame2, text=txt, foreground="#414042")
  
  fnt <- tkfont.create(family="Courier New", size=9)
  frame2.txt.2.1 <- tktext(frame2, bg="white", font=fnt, padx=2, pady=2,
                       width=50, height=12, undo=1, wrap="none",
                       foreground="black", relief="flat",
                       xscrollcommand=function(...) tkset(frame2.xsc.3.1,...),
                       yscrollcommand=function(...) tkset(frame2.ysc.2.2,...))

  frame2.ysc.2.2 <- ttkscrollbar(frame2, orient="vertical")
  frame2.xsc.3.1 <- ttkscrollbar(frame2, orient="horizontal")
  tkconfigure(frame2.ysc.2.2, command=paste(.Tk.ID(frame2.txt.2.1), "yview"))
  tkconfigure(frame2.xsc.3.1, command=paste(.Tk.ID(frame2.txt.2.1), "xview"))

  frame2a <- tkframe(frame2, relief="flat", padx=0, pady=0)
  frame2a.but.01 <- ttkbutton(frame2a, width=3, text="\u002b",
                              command=function() InsertString(" + "))
  frame2a.but.02 <- ttkbutton(frame2a, width=3, text="\u2212",
                              command=function() InsertString(" - "))
  frame2a.but.03 <- ttkbutton(frame2a, width=3, text="\u00d7",
                              command=function() InsertString(" * "))
  frame2a.but.04 <- ttkbutton(frame2a, width=3, text="\u00f7",
                              command=function() InsertString(" / "))
  frame2a.but.05 <- ttkbutton(frame2a, width=3, text=">",
                              command=function() InsertString(" > "))
  frame2a.but.06 <- ttkbutton(frame2a, width=3, text="<",
                              command=function() InsertString(" < "))
  frame2a.but.07 <- ttkbutton(frame2a, width=3, text="\u2265",
                              command=function() InsertString(" >= "))
  frame2a.but.08 <- ttkbutton(frame2a, width=3, text="\u2264",
                              command=function() InsertString(" <= "))
  frame2a.but.09 <- ttkbutton(frame2a, width=3, text="=",
                              command=function() InsertString(" == "))
  frame2a.but.10 <- ttkbutton(frame2a, width=3, text="\u2260",
                              command=function() InsertString(" != "))
  frame2a.but.11 <- ttkbutton(frame2a, width=3, text="( )",
                              command=function() InsertString("()"))
  frame2a.but.12 <- ttkbutton(frame2a, width=3, text="[ ]",
                              command=function() InsertString("[]"))
  frame2a.but.13 <- ttkbutton(frame2a, width=2, image=GetBitmapImage("delete"),
                              command=ClearAll)
  
  tkgrid(frame2a.but.01, frame2a.but.02, frame2a.but.03, frame2a.but.04,
         frame2a.but.05, frame2a.but.06, frame2a.but.07, frame2a.but.08, 
         frame2a.but.09, frame2a.but.10, frame2a.but.11, frame2a.but.12, 
         frame2a.but.13, padx=c(0, 2), pady=c(2, 0))

  tkgrid.configure(frame2a.but.01, padx=c(2, 2))
  tkgrid.configure(frame2a.but.05, frame2a.but.11, padx=c(12, 2))
  tkgrid.configure(frame2a.but.13, padx=c(52, 0))

  tkgrid(frame2.lab.1.1, "x")
  tkgrid(frame2.txt.2.1, frame2.ysc.2.2)
  tkgrid(frame2.xsc.3.1, "x")
  tkgrid(frame2a, "x")
  
  tkgrid.configure(frame2.lab.1.1, padx=c(2, 0),  pady=c(10, 0), sticky="w")
  tkgrid.configure(frame2.txt.2.1, padx=c(2, 0),  pady=c(2, 0),  sticky="nsew")
  tkgrid.configure(frame2.ysc.2.2, padx=c(0, 10), pady=c(2, 0),  sticky="ns")
  tkgrid.configure(frame2.xsc.3.1, padx=c(2, 0),  pady=c(0, 0),  sticky="we")
  tkgrid.configure(frame2a, pady=c(0, 0), sticky="we")

  tkgrid.rowconfigure(frame2, 1, weight=1)
  tkgrid.columnconfigure(frame2, 0, weight=1, minsize=20)

  if (!is.null(old.fun) && old.fun != "")
    tkinsert(frame2.txt.2.1, "end", old.fun)

  tcl(frame2.txt.2.1, "edit", "reset")

  tkmark.set(frame2.txt.2.1, "insert", "end")
  
  # Pack frames into paned window

  tkadd(pw, frame1, weight=0)
  tkadd(pw, frame2, weight=1)
  tkpack(pw, fill="both", expand="yes")

  # Bind events

  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)
  
  tkbind(frame1.lst.2.1, "<<ListboxSelect>>", ChangeVar)
  tkbind(frame1.lst.2.1, "<Double-ButtonRelease-1>", InsertVar)
  tkbind(frame1.lst.4.1, "<Double-ButtonRelease-1>", InsertValue)
  tkbind(frame1.box.3.1, "<<ComboboxSelected>>", RebuildList)

  tkbind("Text", "<Control-z>", EditUndo)
  tkbind("Text", "<Control-y>", EditRedo)
  tkbind("Text", "<Control-v>", EditPaste)
  tkbind("Text", "<Control-a>", EditSelectAll)

  # GUI control

  tkfocus(frame2.txt.2.1)
  tkgrab(tt)

  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  new.fun
}
