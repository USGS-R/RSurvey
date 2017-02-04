#' GUI: Edit Text
#'
#' A graphical user interface (\acronym{GUI}) for viewing and editing text.
#'
#' @param txt character.
#'   Text used to populate the window.
#' @param read.only logical.
#'   Specifies whether the text is read only.
#' @param win.title character.
#'   Title of the dialog box.
#' @param is.fixed.width.font logical.
#'   Specifies whether a fixed-width font be used.
#' @param parent tkwin.
#'   \acronym{GUI} parent window
#'
#' @return Returns an object of class character with edited text.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords misc
#'
#' @import tcltk
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   txt <- c("\"Hills cherish the ambition",
#'            "    to turn into partial",
#'            "   differential equations\"",
#'            "",
#'            "        -Donald Hall")
#'   new.txt <- EditText(txt, is.fixed.width.font = TRUE)
#'
#'   EditText(txt, read.only = TRUE)
#' }
#'

EditText <- function(txt, read.only=FALSE, win.title="View Text",
                     is.fixed.width.font=FALSE, parent=NULL) {


  # close gui and return edited text
  SaveText <- function() {
    txt <- as.character(tclvalue(tkget(f1.txt.1.1, "1.0", "end-1c")))
    txt <- strsplit(txt, split="\n", fixed=TRUE)[[1]]
    rtn <<- txt
    tclvalue(tt.done.var) <- 1
  }


  # open file in text console
  OpenFile <- function(is.appended=FALSE) {
    txt <- as.character(tclvalue(tkget(f1.txt.1.1, "1.0", "end-1c")))
    if (txt != "" & !is.appended) {
      msg <- paste("This action will delete existing console text.",
                   "Would you like to continue?", sep="\n")
      ans <- tkmessageBox(icon="question", message=msg, title="Warning", type="yesno", parent=tt)
      ans <- as.character(ans)
      if (ans != "yes") return()
      ClearConsole()
    }
    f <- GetFile(cmd="Open", exts="txt", win.title="Open Text File", parent=tt)
    if (is.null(f)) return()
    txt <- paste(readLines(f), collapse="\n")
    tkinsert(f1.txt.1.1, "end", txt)
  }


  # save current text to file
  SaveAs <- function() {
    txt <- as.character(tclvalue(tkget(f1.txt.1.1, "1.0", "end-1c")))
    f <- GetFile(cmd="Save As", exts="txt", win.title="Save Text As",
                 defaultextension="txt", parent=tt)
    if (is.null(f)) return()
    cat(txt, file=f, sep="\n")
  }


  # edit menu functions
  EditUndo <- function() {
    tkfocus(f1.txt.1.1)
    try(tcl(f1.txt.1.1, "edit", "undo"), silent=TRUE)
  }
  EditRedo <- function() {
    tkfocus(f1.txt.1.1)
    try(tcl(f1.txt.1.1, "edit", "redo"), silent=TRUE)
  }
  EditCut <- function() {
    tkfocus(f1.txt.1.1)
    tcl("tk_textCut", f1.txt.1.1)
  }
  EditCopy <- function() {
    tkfocus(f1.txt.1.1)
    tcl("tk_textCopy", f1.txt.1.1)
  }
  EditPaste <- function() {
    tkfocus(f1.txt.1.1)
    tcl("tk_textPaste", f1.txt.1.1)
  }
  EditSelectAll <- function() {
    tkfocus(f1.txt.1.1)
    tktag.add(f1.txt.1.1, "sel", "1.0", "end")
  }
  ClearConsole <- function() {
    tcl(f1.txt.1.1, "delete", "1.0", "end")
    tkfocus(f1.txt.1.1)
  }


  # assign missing values
  if (missing(txt) || is.null(txt) || length(txt) == 0) txt <- ""
  if (!is.character(txt)) stop("input text argument is not of class character")

  # add end-of-line for vector of character strings and
  # determine the maximum number of characters in a line
  if (length(txt) > 1) {
    txt <- paste(txt, collapse="\n")
    n <- max(vapply(strsplit(txt, split="\n", fixed=TRUE)[[1]], nchar, 0L))
  } else {
    n <- 0
  }

  # determine the width of the text window
  txt.width <- 80

  # determine font type
  font.type <- if (is.fixed.width.font) "TkFixedFont" else "TkTextFont"

  # assigin global variables
  rtn <- NULL

  # assign variables linked to Tk widgets
  tt.done.var <- tclVar(0)

  # open gui
  tclServiceMode(FALSE)
  tt <- tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    geo <- as.integer(geo[2:3]) + 25
    tkwm.geometry(tt, sprintf("+%s+%s", geo[1], geo[2]))
  }
  tktitle(tt) <- win.title

  # start top menu
  top.menu <- tkmenu(tt, tearoff=0)

  # edit menu
  menu.file <- tkmenu(tt, tearoff=0, relief="flat")
  menu.edit <- tkmenu(tt, tearoff=0, relief="flat")

  tkadd(top.menu, "cascade", label="File", menu=menu.file, underline=0)
  tkadd(top.menu, "cascade", label="Edit", menu=menu.edit, underline=0)

  if (read.only) {
    tkadd(menu.file, "command", label="Save as\u2026", accelerator="Ctrl+s", command=SaveAs)
    tkadd(menu.edit, "command", label="Select all", accelerator="Ctrl+a", command=EditSelectAll)
    tkadd(menu.edit, "separator")
    tkadd(menu.edit, "command", label="Copy", accelerator="Ctrl+c", command=EditCopy)
  } else {
    tkadd(menu.file, "command", label="Open\u2026", accelerator="Ctrl+o",
          command=function() OpenFile())
    tkadd(menu.file, "command", label="Open and append\u2026",
          command=function() OpenFile(is.appended=TRUE))
    tkadd(menu.file, "command", label="Save as\u2026", accelerator="Ctrl+s", command=SaveAs)

    tkadd(menu.edit, "command", label="Undo", accelerator="Ctrl+z", command=EditUndo)
    tkadd(menu.edit, "command", label="Redo", accelerator="Ctrl+y", command=EditRedo)
    tkadd(menu.edit, "separator")
    tkadd(menu.edit, "command", label="Cut", accelerator="Ctrl+x", command=EditCut)
    tkadd(menu.edit, "command", label="Copy", accelerator="Ctrl+c", command=EditCopy)
    tkadd(menu.edit, "command", label="Paste", accelerator="Ctrl+v", command=EditPaste)
    tkadd(menu.edit, "separator")
    tkadd(menu.edit, "command", label="Select all", accelerator="Ctrl+a", command=EditSelectAll)
    tkadd(menu.edit, "command", label="Clear console", accelerator="Ctrl+l", command=ClearConsole)
  }

  # finish top menu
  tkconfigure(tt, menu=top.menu)

  # frame 0, ok and cancel buttons
  f0 <- tkframe(tt, relief="flat")

  if (read.only) {
    f0.but.1.2 <- "x"
    f0.but.1.3 <- ttkbutton(f0, width=12, text="Close",
                            command=function() tkdestroy(tt))
  } else {
    f0.but.1.2 <- ttkbutton(f0, width=12, text="OK", command=SaveText)
    f0.but.1.3 <- ttkbutton(f0, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)
  }
  f0.grp.1.4 <- ttksizegrip(f0)

  tkgrid("x", f0.but.1.2, f0.but.1.3, f0.grp.1.4)

  tkgrid.configure(f0.but.1.3, columnspan=2, padx=c(4, 10), pady=10)
  tkgrid.configure(f0.grp.1.4, sticky="se")

  tkraise(f0.but.1.3, f0.grp.1.4)

  tkgrid.columnconfigure(f0, 0, weight=1)

  tkpack(f0, fill="x", side="bottom", anchor="e")

  # frame 1, text window
  f1 <- tkframe(tt, relief="flat")

  f1.txt.1.1 <- tktext(f1, bg="white", font=font.type, padx=2, pady=2,
                       width=txt.width, height=20, undo=1, autoseparators=1,
                       wrap="none", foreground="black", relief="flat",
                       yscrollcommand=function(...) tkset(f1.ysc.1.2, ...),
                       xscrollcommand=function(...) tkset(f1.xsc.2.1, ...))

  f1.ysc.1.2 <- ttkscrollbar(f1, orient="vertical")
  f1.xsc.2.1 <- ttkscrollbar(f1, orient="horizontal")

  tkconfigure(f1.ysc.1.2, command=paste(.Tk.ID(f1.txt.1.1), "yview"))

  tkgrid(f1.txt.1.1, f1.ysc.1.2)
  tkgrid.configure(f1.txt.1.1, sticky="nswe")
  tkgrid.configure(f1.ysc.1.2, sticky="ns")

  if (!read.only || (read.only & n > 120)) {
    tkconfigure(f1.xsc.2.1, command=paste(.Tk.ID(f1.txt.1.1), "xview"))
    tkgrid(f1.xsc.2.1, "x")
    tkgrid.configure(f1.xsc.2.1, sticky="we")
  }

  tkgrid.columnconfigure(f1, 0, weight=1)
  tkgrid.rowconfigure(f1, 0, weight=1)

  tkpack(f1, fill="both", expand="yes")

  tkinsert(f1.txt.1.1, "end", txt)
  tcl(f1.txt.1.1, "edit", "reset")
  tcl(f1.txt.1.1, "edit", "separator")
  if (read.only) tkconfigure(f1.txt.1.1, state="disabled")

  # bind events
  tclServiceMode(TRUE)

  if (!read.only) {
    tkbind("Text", "<Control-s>", SaveAs)
    tkbind("Text", "<Control-z>", EditUndo)
    tkbind("Text", "<Control-y>", EditRedo)
    tkbind("Text", "<Control-v>", EditPaste)
    tkbind("Text", "<Control-l>", ClearConsole)
  }
  tkbind("Text", "<Control-a>", EditSelectAll)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  # gui control
  tkfocus(f1.txt.1.1)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  invisible(rtn)
}
