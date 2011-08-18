EditFunction <- function(cols, index=NULL, parent=NULL) {
  # A GUI for defining a function in the R language with a focus on table data.

  # Additional functions (subroutines)

  # Rebuild list box based on selected class type to show

  RebuildList <- function() {
    idx <- as.integer(tcl(frame0.box.3.1, "current"))
    show.ids <- ids
    if (idx > 0)
      show.ids <- ids[cls %in% classes[idx]]

    tclvalue(list.var) <- ""
    for (i in seq(along=show.ids))
      tcl("lappend", list.var, show.ids[i])

    tkselection.clear(frame0.lst.2.1, 0, "end")
    tkfocus(frame1.txt.2.1)
  }

  # Insert character string into text box

  InsertString <- function(txt, sel="<variable>") {
    tcl(frame1.txt.2.1, "edit", "separator")
    seltxt <- as.character(tktag.ranges(frame1.txt.2.1, 'sel'))
    if (length(seltxt) > 1)
      tcl(frame1.txt.2.1, "delete", seltxt[1], seltxt[2])

    cur <- as.character(tkindex(frame1.txt.2.1, "insert"))
    cur <- as.integer(strsplit(cur, ".", fixed=TRUE)[[1]])
    cur.line <- cur[1]
    cur.char <- cur[2]

    tkinsert(frame1.txt.2.1, "insert", txt)
    tkfocus(frame1.txt.2.1)

    if (txt %in% c("()", "[]")) {
      cursor.insert <- paste(cur.line, cur.char + 1, sep=".")
      tkmark.set(frame1.txt.2.1, "insert", cursor.insert)
    } else {
      search.txt <- gregexpr(pattern=sel, txt)[[1]]
      if (search.txt[1] > 0) {
        match.idx <- search.txt[1]
        match.len <- attr(search.txt, "match.length")[1]
        tkfocus(frame1.txt.2.1)

        char <- c(match.idx, match.idx + match.len) + cur.char - 1
        sel0 <- paste(cur.line, char[1], sep=".")
        sel1 <- paste(cur.line, char[2], sep=".")
        tktag.add(frame1.txt.2.1, 'sel', sel0, sel1)
        tkmark.set(frame1.txt.2.1, "insert", sel1)
      }
    }
  }

  # Insert variable into text box

  InsertVar <- function() {
    idx <- as.integer(tkcurselection(frame0.lst.2.1))
    if (length(idx) == 0)
      return()

    id <- as.character(tkget(frame0.lst.2.1, idx, idx))
    tkselection.clear(frame0.lst.2.1, idx, idx)

    txt <- paste("DATA[[\"", id, "\"]]", sep="")
    InsertString(txt)
  }

  # Save function

  SaveFunction <- function() {
    txt <- as.character(tclvalue(tkget(frame1.txt.2.1, '1.0', 'end-1c')))
    if (txt == "") {
      new.fun <<- "NA"
    } else {
      fun <- try(parse(text=paste("function(DATA) {", txt, "}", sep="")),
                 silent=TRUE)
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
      new.fun <<- txt
    }
    tclvalue(tt.done.var) <- 1
  }

  # Call date and time format editor

  CallEditDateFormat <- function() {
    spec <- EditDateFormat(parent=tt)
    tkfocus(frame1.txt.2.1)
    if(!is.null(spec))
      InsertString(spec)
  }

  # Text edit functions

  EditUndo <- function() {
    tkfocus(frame1.txt.2.1)
    tcl(frame1.txt.2.1, "edit", "undo")
  }
  EditRedo <- function() {
    tkfocus(frame1.txt.2.1)
    tcl(frame1.txt.2.1, "edit", "redo")
  }
  EditCut <- function() {
    tkfocus(frame1.txt.2.1)
    tcl("tk_textCut", frame1.txt.2.1)
  }
  EditCopy <- function() {
    tkfocus(frame1.txt.2.1)
    tcl("tk_textCopy", frame1.txt.2.1)
  }
  EditPaste <- function() {
    tkfocus(frame1.txt.2.1)
    tcl("tk_textPaste", frame1.txt.2.1)
  }
  EditSelectAll <- function() {
    tkfocus(frame1.txt.2.1)
    tktag.add(frame1.txt.2.1, 'sel', '1.0', 'end')
  }


  # Main program

  old.fun <- if (is.null(index)) NULL else cols[[index]]$fun
  new.fun <- NULL

  cls <- sapply(cols, function(i) i$class)
  ids <- sapply(cols, function(i) i$id)
  win.title <- "Edit Function"
  if (!is.null(index)) {
    edit.fun.id <- ids[index]
    ids <- ids[-index]
    cls <- cls[-index]
    win.title <- paste(win.title, " (", edit.fun.id, ")", sep="")
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
  cmd$as.POSIXct <- "as.POSIXct(<variable>, format=\"<format>\")"

  cmd$paste <-  "paste(<variable>, <variable>, sep=\" \")"
  cmd$substr <- paste("substr(<variable>, start=<first element>,",
                      "stop=<last element>)")

  # Assign variables linked to Tk widgets

  list.var <- tclVar()

  for (i in seq(along=ids))
    tcl("lappend", list.var, ids[i]) # must be unique

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

  menu.str <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="String", menu=menu.str, underline=0)
  tkadd(menu.str, "command", label="Concatenate",
        command=function() InsertString(cmd$paste))
  tkadd(menu.str, "command", label="Extract substring",
        command=function() InsertString(cmd$substr))


  menu.tools <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Tools", menu=menu.tools, underline=0)
  tkadd(menu.tools, "command", label="Date and time format",
        command=CallEditDateFormat)

  # Finalize top menu

  tkconfigure(tt, menu=top.menu)

  # Paned window

  pw <- ttkpanedwindow(tt, orient="horizontal")

  # Frame 0

  frame0 <- tkframe(pw, relief="flat", padx=0, pady=0)

  frame0.lab.1.1 <- ttklabel(frame0, text="Click to insert variable",
                             foreground="#414042")
  frame0.lst.2.1 <- tklistbox(frame0, selectmode="browse", activestyle="none",
                              relief="flat", borderwidth=5, width=25, height=8,
                              exportselection=FALSE, listvariable=list.var,
                              highlightthickness=0)
  frame0.ysc.2.2 <- ttkscrollbar(frame0, orient="vertical")

  box.vals <- "{Show all classes}"
  if (length(classes) > 1)
    box.vals <- c("Show all classes", classes)
  frame0.box.3.1 <- ttkcombobox(frame0, state="readonly", value=box.vals)

  tcl(frame0.box.3.1, "current", 0)

  tkconfigure(frame0.lst.2.1, background="white",
              yscrollcommand=paste(.Tk.ID(frame0.ysc.2.2), "set"))
  tkconfigure(frame0.ysc.2.2, command=paste(.Tk.ID(frame0.lst.2.1), "yview"))

  tkgrid(frame0.lab.1.1, "x")
  tkgrid(frame0.lst.2.1, frame0.ysc.2.2)
  tkgrid(frame0.box.3.1, "x")

  tkgrid.configure(frame0.lab.1.1, padx=c(5, 0), pady=c(5, 0), sticky="w")
  tkgrid.configure(frame0.lst.2.1, padx=c(5, 0), pady=c(2, 1), sticky="nsew")
  tkgrid.configure(frame0.ysc.2.2, padx=c(0, 2), pady=c(2, 0), sticky="ns")
  tkgrid.configure(frame0.box.3.1, padx=c(5, 0), pady=c(4, 2), sticky="we")

  tkgrid.rowconfigure(frame0, 1, weight=1)
  tkgrid.columnconfigure(frame0, 0, weight=1, minsize=20)

  tkbind(frame0.lst.2.1, "<ButtonRelease-1>", InsertVar)
  tkbind(frame0.box.3.1, "<<ComboboxSelected>>", RebuildList)

  # Frame 1

  frame1 <- tkframe(pw, relief="flat", padx=0, pady=0)

  txt <- paste("DATA[[\"", edit.fun.id,
               "\"]] <- function(DATA) {<define function below>}", sep="")
  frame1.lab.1.1 <- ttklabel(frame1, text=txt, foreground="#414042")

  fnt <- tkfont.create(family="Courier New", size=9)

  frame1.txt.2.1 <- tktext(frame1, bg="white", font=fnt, padx=2, pady=2,
                       width=75, height=12, undo=1, wrap="none",
                       foreground="black", relief="flat",
                       xscrollcommand=function(...) tkset(frame1.xsc.3.1,...),
                       yscrollcommand=function(...) tkset(frame1.ysc.2.2,...))

  frame1.ysc.2.2 <- ttkscrollbar(frame1, orient="vertical")
  frame1.xsc.3.1 <- ttkscrollbar(frame1, orient="horizontal")
  tkconfigure(frame1.ysc.2.2, command=paste(.Tk.ID(frame1.txt.2.1), "yview"))
  tkconfigure(frame1.xsc.3.1, command=paste(.Tk.ID(frame1.txt.2.1), "xview"))

  frame1a <- tkframe(frame1, relief="flat", padx=0, pady=0)
  frame1a.but.01 <- ttkbutton(frame1a, width=3, text="\u002b",
                              command=function() InsertString("+"))
  frame1a.but.02 <- ttkbutton(frame1a, width=3, text="\u2212",
                              command=function() InsertString("-"))
  frame1a.but.03 <- ttkbutton(frame1a, width=3, text="\u00d7",
                              command=function() InsertString("*"))
  frame1a.but.04 <- ttkbutton(frame1a, width=3, text="\u00f7",
                              command=function() InsertString("/"))
  frame1a.but.05 <- ttkbutton(frame1a, width=3, text="x\u207f",
                              command=function() InsertString("^"))
  frame1a.but.06 <- ttkbutton(frame1a, width=3, text="And",
                              command=function() InsertString("&"))
  frame1a.but.07 <- ttkbutton(frame1a, width=3, text="Or",
                              command=function() InsertString("|"))
  frame1a.but.08 <- ttkbutton(frame1a, width=3, text="Not",
                              command=function() InsertString("!"))
  frame1a.but.09 <- ttkbutton(frame1a, width=3, text=">",
                              command=function() InsertString(">"))
  frame1a.but.10 <- ttkbutton(frame1a, width=3, text="<",
                              command=function() InsertString("<"))
  frame1a.but.11 <- ttkbutton(frame1a, width=3, text="\u2265",
                              command=function() InsertString(">="))
  frame1a.but.12 <- ttkbutton(frame1a, width=3, text="\u2264",
                              command=function() InsertString("<="))
  frame1a.but.13 <- ttkbutton(frame1a, width=3, text="=",
                              command=function() InsertString("=="))
  frame1a.but.14 <- ttkbutton(frame1a, width=3, text="\u2260",
                              command=function() InsertString("!="))
  frame1a.but.15 <- ttkbutton(frame1a, width=3, text="( )",
                              command=function() InsertString("()"))
  frame1a.but.16 <- ttkbutton(frame1a, width=3, text="[ ]",
                              command=function() InsertString("[]"))

  tkgrid(frame1a.but.01, frame1a.but.02, frame1a.but.03, frame1a.but.04,
         frame1a.but.05, frame1a.but.06, frame1a.but.07, frame1a.but.08,
         frame1a.but.09, frame1a.but.10, frame1a.but.11, frame1a.but.12,
         frame1a.but.13, frame1a.but.14, frame1a.but.15, frame1a.but.16,
         padx=c(0, 2), pady=c(2, 0))

  tkgrid.configure(frame1a.but.06, frame1a.but.09, frame1a.but.15, padx=c(8, 2))

  tkgrid(frame1.lab.1.1, "x")
  tkgrid(frame1.txt.2.1, frame1.ysc.2.2)
  tkgrid(frame1.xsc.3.1, "x")
  tkgrid(frame1a, "x")

  tkgrid.configure(frame1.lab.1.1, padx=c(0, 0), pady=c(5, 0), sticky="w")
  tkgrid.configure(frame1.txt.2.1, padx=c(0, 0), pady=c(2, 0), sticky="nsew")
  tkgrid.configure(frame1.ysc.2.2, padx=c(0, 5), pady=c(2, 0), sticky="ns")
  tkgrid.configure(frame1.xsc.3.1, padx=c(0, 0), pady=c(0, 0), sticky="we")
  tkgrid.configure(frame1a, pady=c(2, 0), sticky="we")

  tkgrid.rowconfigure(frame1, 1, weight=1)
  tkgrid.columnconfigure(frame1, 0, weight=1, minsize=20)

  if (!is.null(old.fun) && old.fun != "NA")
    tkinsert(frame1.txt.2.1, "end", old.fun)

  tcl(frame1.txt.2.1, "edit", "reset")

  tkmark.set(frame1.txt.2.1, "insert", "end")

  # Pack frames into paned window

  tkadd(pw, frame0, weight=0)
  tkadd(pw, frame1, weight=1)

  tkpack(pw, fill="both", expand="yes")

  # Frame 2 and size grip

  frame2 <- tkframe(tt, relief="flat", padx=0, pady=0)

  frame2.but.1 <- ttkbutton(frame2, width=12, text="OK",
                            command=SaveFunction)
  frame2.but.2 <- ttkbutton(frame2, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)
  frame2.grp.3 <- ttksizegrip(frame2)

  tkgrid(frame2.but.1, frame2.but.2, frame2.grp.3)

  tkgrid.configure(frame2.but.1, sticky="e", padx=2, pady=c(15, 8))
  tkgrid.configure(frame2.but.2, sticky="w", padx=2, pady=c(15, 8), rowspan=2)
  tkgrid.configure(frame2.grp.3, sticky="se")

  tkpack(frame2, side="bottom", anchor="e")

  # Text bindings

  tkbind("Text", "<Control-z>", EditUndo)
  tkbind("Text", "<Control-y>", EditRedo)
  tkbind("Text", "<Control-v>", EditPaste)
  tkbind("Text", "<Control-a>", EditSelectAll)

  # GUI control

  tkfocus(frame1.txt.2.1)
  tkgrab(tt)
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tclServiceMode(TRUE)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  new.fun
}
