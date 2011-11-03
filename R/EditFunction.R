EditFunction <- function(cols, index=NULL, parent=NULL) {
  # A GUI for defining a function in the R language with a focus on table data.

  # Additional functions (subroutines)

  # Rebuild list box based on selected class type to show

  RebuildList <- function() {
    idx <- as.integer(tcl(frame1.box.3.1, "current"))
    show.ids <- ids
    if (idx > 0)
      show.ids <- ids[cls %in% classes[idx]]

    tclvalue(list.var) <- ""
    for (i in seq(along=show.ids))
      tcl("lappend", list.var, show.ids[i])

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
    tkselection.clear(frame1.lst.2.1, idx, idx)

    txt <- paste("DATA[[\"", id, "\"]]", sep="")
    InsertString(txt)
  }

  # Save function

  SaveFunction <- function() {
    txt <- as.character(tclvalue(tkget(frame2.txt.2.1, '1.0', 'end-1c')))
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
    tktag.add(frame2.txt.2.1, 'sel', '1.0', 'end')
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
    win.title <- paste(win.title, "-", edit.fun.id, sep=" ")
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

  # Frame 0

  frame1 <- tkframe(pw, relief="flat", padx=0, pady=0)

  frame1.lab.1.1 <- ttklabel(frame1, text="Click to insert variable",
                             foreground="#414042")
  frame1.lst.2.1 <- tklistbox(frame1, selectmode="browse", activestyle="none",
                              relief="flat", borderwidth=5, width=25, height=8,
                              exportselection=FALSE, listvariable=list.var,
                              highlightthickness=0)
  frame1.ysc.2.2 <- ttkscrollbar(frame1, orient="vertical")

  box.vals <- "{Show all classes}"
  if (length(classes) > 1)
    box.vals <- c("Show all classes", classes)
  frame1.box.3.1 <- ttkcombobox(frame1, state="readonly", value=box.vals)

  tcl(frame1.box.3.1, "current", 0)

  tkconfigure(frame1.lst.2.1, background="white",
              yscrollcommand=paste(.Tk.ID(frame1.ysc.2.2), "set"))
  tkconfigure(frame1.ysc.2.2, command=paste(.Tk.ID(frame1.lst.2.1), "yview"))

  tkgrid(frame1.lab.1.1, "x")
  tkgrid(frame1.lst.2.1, frame1.ysc.2.2)
  tkgrid(frame1.box.3.1, "x")

  tkgrid.configure(frame1.lab.1.1, padx=c(10, 0), pady=c(10, 0), sticky="w")
  tkgrid.configure(frame1.lst.2.1, padx=c(10, 0), pady=c(2, 0), sticky="nsew")
  tkgrid.configure(frame1.ysc.2.2, padx=c(0, 0), pady=c(2, 0), sticky="ns")
  tkgrid.configure(frame1.box.3.1, padx=c(10, 0), pady=c(4, 0), sticky="we")

  tkgrid.rowconfigure(frame1, 1, weight=1)
  tkgrid.columnconfigure(frame1, 0, weight=1, minsize=20)

  # Frame 1

  frame2 <- tkframe(pw, relief="flat", padx=0, pady=0)

  txt <- paste("DATA[[\"", edit.fun.id,
               "\"]] <- function(DATA) {<define function below>}", sep="")
  frame2.lab.1.1 <- ttklabel(frame2, text=txt, foreground="#414042")

  fnt <- tkfont.create(family="Courier New", size=9)

  frame2.txt.2.1 <- tktext(frame2, bg="white", font=fnt, padx=2, pady=2,
                       width=75, height=12, undo=1, wrap="none",
                       foreground="black", relief="flat",
                       xscrollcommand=function(...) tkset(frame2.xsc.3.1,...),
                       yscrollcommand=function(...) tkset(frame2.ysc.2.2,...))

  frame2.ysc.2.2 <- ttkscrollbar(frame2, orient="vertical")
  frame2.xsc.3.1 <- ttkscrollbar(frame2, orient="horizontal")
  tkconfigure(frame2.ysc.2.2, command=paste(.Tk.ID(frame2.txt.2.1), "yview"))
  tkconfigure(frame2.xsc.3.1, command=paste(.Tk.ID(frame2.txt.2.1), "xview"))

  frame2a <- tkframe(frame2, relief="flat", padx=0, pady=0)
  frame2a.but.01 <- ttkbutton(frame2a, width=3, text="\u002b",
                              command=function() InsertString("+"))
  frame2a.but.02 <- ttkbutton(frame2a, width=3, text="\u2212",
                              command=function() InsertString("-"))
  frame2a.but.03 <- ttkbutton(frame2a, width=3, text="\u00d7",
                              command=function() InsertString("*"))
  frame2a.but.04 <- ttkbutton(frame2a, width=3, text="\u00f7",
                              command=function() InsertString("/"))
  frame2a.but.05 <- ttkbutton(frame2a, width=3, text="x\u207f",
                              command=function() InsertString("^"))
  frame2a.but.06 <- ttkbutton(frame2a, width=3, text="And",
                              command=function() InsertString("&"))
  frame2a.but.07 <- ttkbutton(frame2a, width=3, text="Or",
                              command=function() InsertString("|"))
  frame2a.but.08 <- ttkbutton(frame2a, width=3, text="Not",
                              command=function() InsertString("!"))
  frame2a.but.09 <- ttkbutton(frame2a, width=3, text=">",
                              command=function() InsertString(">"))
  frame2a.but.10 <- ttkbutton(frame2a, width=3, text="<",
                              command=function() InsertString("<"))
  frame2a.but.11 <- ttkbutton(frame2a, width=3, text="\u2265",
                              command=function() InsertString(">="))
  frame2a.but.12 <- ttkbutton(frame2a, width=3, text="\u2264",
                              command=function() InsertString("<="))
  frame2a.but.13 <- ttkbutton(frame2a, width=3, text="=",
                              command=function() InsertString("=="))
  frame2a.but.14 <- ttkbutton(frame2a, width=3, text="\u2260",
                              command=function() InsertString("!="))
  frame2a.but.15 <- ttkbutton(frame2a, width=3, text="( )",
                              command=function() InsertString("()"))
  frame2a.but.16 <- ttkbutton(frame2a, width=3, text="[ ]",
                              command=function() InsertString("[]"))

  tkgrid(frame2a.but.01, frame2a.but.02, frame2a.but.03, frame2a.but.04,
         frame2a.but.05, frame2a.but.06, frame2a.but.07, frame2a.but.08,
         frame2a.but.09, frame2a.but.10, frame2a.but.11, frame2a.but.12,
         frame2a.but.13, frame2a.but.14, frame2a.but.15, frame2a.but.16,
         padx=c(0, 2), pady=c(2, 0))

  tkgrid.configure(frame2a.but.01, padx=c(2, 2))
  tkgrid.configure(frame2a.but.06, frame2a.but.09, frame2a.but.15, padx=c(8, 2))

  tkgrid(frame2.lab.1.1, "x")
  tkgrid(frame2.txt.2.1, frame2.ysc.2.2)
  tkgrid(frame2.xsc.3.1, "x")
  tkgrid(frame2a, "x")

  tkgrid.configure(frame2.lab.1.1, padx=c(2, 0), pady=c(10, 0), sticky="w")
  tkgrid.configure(frame2.txt.2.1, padx=c(2, 0), pady=c(2, 0), sticky="nsew")
  tkgrid.configure(frame2.ysc.2.2, padx=c(0, 10), pady=c(2, 0), sticky="ns")
  tkgrid.configure(frame2.xsc.3.1, padx=c(2, 0), pady=c(0, 0), sticky="we")
  tkgrid.configure(frame2a, pady=c(0, 0), sticky="we")

  tkgrid.rowconfigure(frame2, 1, weight=1)
  tkgrid.columnconfigure(frame2, 0, weight=1, minsize=20)

  if (!is.null(old.fun) && old.fun != "NA")
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

  tkbind(frame1.lst.2.1, "<ButtonRelease-1>", InsertVar)
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
