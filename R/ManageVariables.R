#' GUI: Variable Manager
#'
#' A graphical user interface (\acronym{GUI}) for managing variables in the data table.
#'
#' @param cols list.
#'   See \sQuote{Value} section
#' @param vars list.
#'   See \sQuote{Value} section
#' @param query character.
#'   See \sQuote{Value} section
#' @param changelog data.frame.
#'   See \sQuote{Value} section
#' @param parent tkwin.
#'   \acronym{GUI} parent window
#'
#' @details This \acronym{GUI} lets you:
#'   (1) specify the names and format of variables;
#'   (2) add new variables based on user defined functions, see \code{\link{EditFunction}};
#'   (3) display data in a spreadsheet, see \code{\link{EditData}}; and
#'   (4) remove and (or) reorder variables in the data table.
#'
#' @return Returns an object of class list with components \code{cols} and \code{vars}.
#'   The \code{cols} object is a list whose length is equal to the current number of data variables.
#'   Each component in \code{cols} is linked to a specific variable,
#'   and contains the following components:
#'     \item{name}{variable name}
#'     \item{format}{conversion specification format (optional)}
#'     \item{id}{unique identifier that is created from \code{name}.}
#'     \item{fun}{expression evaluated when computing the variables vector of values.}
#'     \item{index}{variable's component index number in the \code{data.raw} data table, see \code{\link{ImportText}}.
#'       Only required for variables directly linked to data columns in \code{data.raw}.}
#'     \item{class}{data class of the vector object.}
#'     \item{summary}{summary of the variable's descriptive statistics (see \code{\link{summary}}).}
#'     \item{comments}{user comments}
#'   The \code{vars} object is a list with components:
#'     \item{x, y, z, sort.on}{the index number of the corresponding state variable in \code{cols}.
#'       These indexes are updated to reflect the removal and (or) reordering of variables in \code{cols}.}
#'     \item{query}{if required, variable names are updated.}
#'     \item{changelog}{if required, names in the \code{variable} component are updated.}
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
#'   Data(replace.all = obj)
#'   ManageVariables(obj$cols, obj$vars, obj$query, obj$changelog)
#' }
#'

ManageVariables <- function(cols, vars, query, changelog, parent=NULL) {


  # save changes and close GUI
  SaveChanges <- function(type) {
    SaveNb()
    if (!identical(cols, old.cols)) {
      rtn <<- list(cols=cols, vars=vars, query=query, changelog=changelog)
      old.cols <<- cols
    }
    if (type == "ok") tclvalue(tt.done.var) <- 1
  }


  # set variable id and update functions to reflect this change
  SetVarId <- function(idx=NULL) {
    if (is.null(idx)) idx <- as.integer(tkcurselection(f1.lst)) + 1
    if (length(idx) == 0) return()

    # save name
    nam <- tclvalue(name.var)
    cols[[idx]]$name <<- nam
    if (nam == "") nam <- "Unknown"

    # account for duplicate ids
    new.id <- nam
    old.id <- cols[[idx]]$id
    old.ids <- vapply(cols, function(i) i$id, "")
    i <- 1L
    hold.new.id <- new.id
    while (new.id %in% old.ids[-idx]) {
      new.id <- paste0(hold.new.id, " (", i, ")")
      i <- i + 1L
    }
    cols[[idx]]$id <<- new.id
    tclvalue(list.var) <- tcl("lreplace", tclvalue(list.var), idx - 1, idx - 1, new.id)

    # update variable id's used in functions, query, and changelog
    if (!is.null(old.id)) {
      old.fun <- cols[[idx]]$fun
      str.1 <- paste0("\"", old.id, "\"")
      str.2 <- paste0("\"", new.id, "\"")
      funs <- sapply(cols, function(i) gsub(str.1, str.2, i$fun, fixed=TRUE))
      sapply(seq_along(cols), function(i) cols[[i]]$fun <<- funs[[i]])
      new.fun <- cols[[idx]]$fun
      if (!identical(old.fun, new.fun)) {
        tkconfigure(f2.txt.4.2, state="normal")
        tcl(f2.txt.4.2, "delete", "1.0", "end")
        tkinsert(f2.txt.4.2, "end", new.fun)
        tkconfigure(f2.txt.4.2, state="disabled")
      }
      if (!is.null(query))
        query <<- gsub(str.1, str.2, query, fixed=TRUE)
      if (!is.null(changelog))
        changelog[changelog[, "variable"] %in% old.id, "variable"] <<- new.id
    }
  }

  # save notebook content
  SaveNb <- function() {
    idx <- as.integer(tkcurselection(f1.lst)) + 1L
    if (length(idx) == 0) return()

    # save format
    old.fmt <- cols[[idx]]$format
    new.fmt <- as.character(tclvalue(fmt.var))
    cols[[idx]]$format <<- new.fmt

    # save function
    old.fun <- cols[[idx]]$fun
    new.fun <- as.character(tclvalue(tkget(f2.txt.4.2, "1.0", "end-1c")))
    cols[[idx]]$fun <<- new.fun

    # save summary string
    if (!identical(old.fun, new.fun))
      cols[[idx]]$summary <- summary(EvalFunction(new.fun, cols))

    # save name
    SetVarId(idx)
  }


  # update notebook content
  UpdateNb <- function() {
    idx <- as.integer(tkcurselection(f1.lst)) + 1L
    if (length(idx) == 0) return()

    # update name
    saved.name <- cols[[idx]]$name
    if (is.null(saved.name)) saved.name <- ""
    tclvalue(name.var) <- saved.name

    # update class
    tkconfigure(f2.ent.3.2, state="normal")
    saved.class <- cols[[idx]]$class
    tclvalue(class.var) <- paste(saved.class)
    tkconfigure(f2.ent.3.2, state="readonly")

    # update format
    saved.fmt <- cols[[idx]]$format
    tkconfigure(f2.ent.2.2, state="normal")
    tclvalue(fmt.var) <- saved.fmt
    tkconfigure(f2.ent.2.2, state="readonly")

    # update function
    tkconfigure(f2.txt.4.2, state="normal")
    tcl(f2.txt.4.2, "delete", "1.0", "end")
    tkinsert(f2.txt.4.2, "end", cols[[idx]]$fun)
    tkconfigure(f2.txt.4.2, state="disabled")
    s <- "disabled"
    if (is.na(cols[[idx]]$index)) s <- "normal"
    tkconfigure(f2.but.4.3, state=s)

    # update summary
    tkconfigure(f3.txt, state="normal")
    tcl(f3.txt, "delete", "1.0", "end")
    if (!is.null(cols[[idx]]$summary)) {
      txt <- paste(c("", utils::capture.output(cols[[idx]]$summary)), collapse="\n")
      tkinsert(f3.txt, "end", txt)
    }
    tkconfigure(f3.txt, state="disabled")
  }

  # account for change in notebook tab
  ChangeTab <- function() {
    idx <- as.integer(tkcurselection(f1.lst)) + 1L
    if (length(idx) == 0) return()
    SaveNb()
    UpdateNb()
    tabid <- tclvalue(tcl(nb, "select"))

    # arrive at tab
    if (tabid == f2$ID) {
      tkfocus(f2)
    } else if (tabid == f3$ID) {
      tkfocus(f3)
    }
  }


  # delete existing variable
  DeleteVar <- function() {
    idx <- as.integer(tkcurselection(f1.lst)) + 1L
    if (length(idx) == 0) return()

    var.str <- paste0("\"", cols[[idx]]$id, "\"")

    if (!is.null(query) &&  grepl(var.str, query, fixed=TRUE))
      query <<- NULL
    if (!is.null(changelog) &&  grepl(var.str, changelog, fixed=TRUE))
      changelog <<- NULL

    funs.with.var <- grep(var.str, sapply(cols, function(i) i$fun), fixed=TRUE)
    dependent.vars <- funs.with.var[!funs.with.var %in% idx]

    if (length(dependent.vars) > 0) {
      ids <- vapply(cols, function(i) i$id, "")[dependent.vars]
      msg <- paste0("Variables dependent on variable \"", cols[[idx]]$id,
                    "\" include:\n\n  ", paste(ids, collapse=", "),
                    "\n\nThese variables must first be removed before this ",
                    "operation can be completed.")
      tkmessageBox(icon="error", message=msg, title="Deletion Prevented", type="ok", parent=tt)
      return()
    }
    if (!is.na(cols[[idx]]$index)) {
      msg <- paste0("Variable \"", cols[[idx]]$id, "\" corresponds with imported data.\n\n",
                    "Are you sure you want to remove it?")
      ans <- tkmessageBox(icon="question", message=msg, title="Question", type="yesno", parent=tt)
      if (as.character(ans) == "no") return()
    }

    tclvalue(list.var) <- tcl("lreplace", tclvalue(list.var), idx - 1L, idx - 1L)

    cols <<- cols[-idx]
    vars <<- vars[!vars %in% idx]

    if (length(cols) == 0) cols <<- NULL
    if (length(vars) == 0) vars <<- NULL

    for (i in seq_along(vars)) {
      if (vars[[i]] > idx) vars[[i]][1] <<- vars[[i]] - 1
    }

    tkselection.clear(f1.lst, 0, "end")

    n <- length(cols)
    if (n > 0) {
      if (idx > n)
        tkselection.set(f1.lst, idx - 2)
      else
        tkselection.set(f1.lst, idx - 1)
      UpdateNb()
    } else {
      tclvalue(name.var) <- ""
      tclvalue(class.var) <- ""
      tclvalue(fmt.var) <- ""
      tkconfigure(f2.txt.4.2, state="normal")
      tcl(f2.txt.4.2, "delete", "1.0", "end")
      tkconfigure(f2.txt.4.2, state="disabled")
      tkconfigure(f3.txt, state="normal")
      tcl(f3.txt, "delete", "1.0", "end")
      tkconfigure(f3.txt, state="disabled")
    }
  }


  # save new variable
  SaveNewVar <- function() {
    SaveNb()

    new.name <- "New Variable"
    idx <- length(cols) + 1L

    cols[[idx]] <- list(id="", class="")

    m <- if (length(cols) > 1) length(EvalFunction(cols[[1]]$fun, cols)) else NULL

    f <- EditFunction(cols, index=idx, value.length=m, win.title="New Variable", parent=tt)
    if (is.null(f$fun) || f$fun == "") return()

    cols[[idx]] <<- list(id="", name="New Variable", format="", class=f$class,
                         index=NA, fun=f$fun, sample=f$sample, summary=f$summary)

    tcl("lappend", list.var, new.name)
    tkselection.clear(f1.lst, 0, "end")
    tkselection.set(f1.lst, idx - 1L, idx - 1L)
    tkyview(f1.lst, idx - 1L)

    UpdateNb()
    SetVarId(idx)
  }


  # edit a variables function formula
  CallEditFunction <- function() {
    SaveNb()

    idx <- as.integer(tkcurselection(f1.lst)) + 1L
    if (length(idx) == 0) return()

    m <- if (length(cols) > 1) length(EvalFunction(cols[[1]]$fun, cols)) else NULL

    f <- EditFunction(cols, index=idx, value.length=m, parent=tt)

    if (is.null(f$fun)) return()
    if (f$fun == "") {
      msg <- paste0("Nothing has been defined for this function; therefore,\n",
                    "the variable '", cols[[idx]]$name, "' will be removed.")
      ans <- tkmessageBox(icon="question", message=msg, title="Warning", type="okcancel", parent=tt)
      if (as.character(ans) == "ok") DeleteVar()
      return()
    }

    if (!identical(f$class, cols[[idx]]$class)) cols[[idx]]$format <<- ""

    cols[[idx]]$fun     <<- f$fun
    cols[[idx]]$class   <<- f$class
    cols[[idx]]$summary <<- f$summary
    cols[[idx]]$sample  <<- f$sample

    UpdateNb()
  }


  # edit format
  CallFormat <- function() {
    idx <- as.integer(tkcurselection(f1.lst)) + 1L
    if (length(idx) == 0) return()

    sample.value <- cols[[idx]]$sample

    old.fmt <- as.character(tclvalue(fmt.var))
    if (inherits(sample.value, c("POSIXt", "Date"))) {
      new.fmt <- FormatDateTime(sample=sample.value, fmt=old.fmt, parent=tt)
    } else {
      if (is.null(sample.value)) sample.value <- NA
      new.fmt <- Format(sample=sample.value, fmt=old.fmt, parent=tt)
    }

    if (is.null(new.fmt)) new.fmt <- ""
    tclvalue(fmt.var) <- new.fmt
  }


  # arrange variables in listbox
  Arrange <- function(type) {
    idx <- as.integer(tkcurselection(f1.lst)) + 1L
    if (length(idx) == 0) return()

    n <- length(cols)
    idxs <- seq_len(n)

    if (type == "back") {
      if (idx == 1) return()
      new.idxs <- c(idx, idxs[-idx])
      new.idx <- 1
    } else if (type == "front") {
      if (idx == n) return()
      new.idxs <- c(idxs[-idx], idx)
      new.idx <- n
    } else if (type == "backward") {
      if (idx == 1) return()
      new.idxs <- seq_len(n)
      new.idxs[c(idx - 1L, idx)] <- c(idx, idx - 1L)
      new.idx <- idx - 1L
    } else if (type == "forward") {
      if (idx == n) return()
      new.idxs <- seq_len(n)
      new.idxs[c(idx, idx + 1L)] <- c(idx + 1L, idx)
      new.idx <- idx + 1L
    }

    cols <<- cols[new.idxs]

    for (i in seq_along(vars)) {
      vars[[i]][1] <<- idxs[new.idxs %in% vars[[i]][1]]
    }

    ids <- vapply(cols, function(i) i$id, "")

    for (i in seq_len(n))
      tclvalue(list.var) <- tcl("lreplace", tclvalue(list.var), i - 1, i - 1, ids[i])
    tkselection.clear(f1.lst, 0, "end")
    tkselection.set(f1.lst, new.idx - 1L)
    tkyview(f1.lst, new.idx - 1L)
  }


  # assign variables
  rtn <- NULL

  old.cols <- cols
  ids <- vapply(cols, function(i) i$id, "")

  w <- 300
  h <- 50

  # assign the variables linked to Tk widgets
  list.var    <- tclVar()
  for (i in seq_along(ids)) tcl("lappend", list.var, ids[i])
  name.var    <- tclVar()
  fmt.var     <- tclVar()
  class.var   <- tclVar()
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
  tktitle(tt) <- "Variable Manager"

  # create menus
  top.menu <- tkmenu(tt, tearoff=0)
  menu.edit <- tkmenu(tt, tearoff=0, relief="flat")
  tkadd(top.menu, "cascade", label="Edit", menu=menu.edit, underline=0)
  tkadd(menu.edit, "command", label="New\u2026", accelerator="Ctrl+N",
        command=SaveNewVar)
  tkadd(menu.edit, "command", label="Delete", command=DeleteVar)
  menu.arrange <- tkmenu(tt, tearoff=0)
  tkadd(top.menu, "cascade", label="Arrange", menu=menu.arrange, underline=0)
  tkadd(menu.arrange, "command", label="Send to top", accelerator="Ctrl+Shift+[",
        command=function() Arrange("back"))
  tkadd(menu.arrange, "command", label="Send upward", accelerator="Ctrl+[",
        command=function() Arrange("backward"))
  tkadd(menu.arrange, "command", label="Bring downward", accelerator="Ctrl+]",
        command=function() Arrange("forward"))
  tkadd(menu.arrange, "command", label="Bring to bottom", accelerator="Ctrl+Shift+]",
        command=function() Arrange("front"))
  tkconfigure(tt, menu=top.menu)

  # frame 0, ok and cancel buttons, and size grip
  f0 <- ttkframe(tt, relief="flat")
  f0.but.1 <- ttkbutton(f0, width=2, image=GetBitmapImage("top"),
                        command=function() Arrange("back"))
  f0.but.2 <- ttkbutton(f0, width=2, image=GetBitmapImage("up"),
                        command=function() Arrange("backward"))
  f0.but.3 <- ttkbutton(f0, width=2, image=GetBitmapImage("down"),
                        command=function() Arrange("forward"))
  f0.but.4 <- ttkbutton(f0, width=2, image=GetBitmapImage("bottom"),
                        command=function() Arrange("front"))
  f0.but.5 <- ttkbutton(f0, width=2, image=GetBitmapImage("plus"),
                        command=SaveNewVar)
  f0.but.6 <- ttkbutton(f0, width=2, image=GetBitmapImage("delete"),
                        command=DeleteVar)

  f0.but.8 <- ttkbutton(f0, width=12, text="OK",
                        command=function() SaveChanges("ok"))
  f0.but.9 <- ttkbutton(f0, width=12, text="Cancel",
                         command=function() tclvalue(tt.done.var) <- 1)
  f0.but.10 <- ttkbutton(f0, width=12, text="Apply",
                         command=function() SaveChanges("apply"))
  f0.but.11 <- ttkbutton(f0, width=12, text="Help",
                         command=function() {
                           print(utils::help("ManageVariables", package="RSurvey", verbose=FALSE))
                         })
  f0.grp.12 <- ttksizegrip(f0)

  tkgrid(f0.but.1, f0.but.2, f0.but.3, f0.but.4, f0.but.5, f0.but.6, "x",
         f0.but.8, f0.but.9, f0.but.10, f0.but.11, f0.grp.12)

  tkgrid.columnconfigure(f0, 6, weight=1)

  tkgrid.configure(f0.but.1, f0.but.2, f0.but.3, f0.but.4, f0.but.5, f0.but.6,
                   sticky="n", padx=c(0, 2), pady=c(0, 0))
  tkgrid.configure(f0.but.1, padx=c(10, 2))
  tkgrid.configure(f0.but.6, padx=c(26, 0))
  tkgrid.configure(f0.but.8, f0.but.9, f0.but.10, f0.but.11, padx=c(0, 4), pady=c(15, 10))
  tkgrid.configure(f0.but.11, columnspan=2, padx=c(0, 10))
  tkgrid.configure(f0.grp.12, sticky="se")

  tkraise(f0.but.11, f0.grp.12)

  tkpack(f0, fill="x", side="bottom", anchor="e")

  # paned window
  pw <- ttkpanedwindow(tt, orient="horizontal")

  # frame 1, listbox with variable names
  f1 <- tkframe(pw, relief="flat")

  f1.lst <- tklistbox(f1, selectmode="browse", activestyle="none",
                      relief="flat", borderwidth=5, width=25,
                      exportselection=FALSE, listvariable=list.var, highlightthickness=0)
  f1.ysc <- ttkscrollbar(f1, orient="vertical")
  tkconfigure(f1.lst, background="white", yscrollcommand=paste(.Tk.ID(f1.ysc), "set"))
  tkconfigure(f1.ysc, command=paste(.Tk.ID(f1.lst), "yview"))
  tkpack(f1.lst, side="left",  fill="both", expand=TRUE, pady=c(2, 2))
  tkpack(f1.ysc, side="right", fill="y", anchor="w", padx=c(0, 2), pady=c(2, 2))

  tkselection.set(f1.lst, 0)

  tkadd(pw, f1, weight=0)

  # notebook with tabs
  nb <- ttknotebook(pw)

  # frame 2, variable
  f2 <- ttkframe(nb, relief="flat", padding=10, borderwidth=2)
  tkadd(nb, f2, text="   Variable   ")

  f2.lab.1.1 <- ttklabel(f2, text="Name")
  f2.lab.2.1 <- ttklabel(f2, text="Format")
  f2.lab.3.1 <- ttklabel(f2, text="Class")
  f2.lab.4.1 <- ttklabel(f2, text="Function")

  f2.ent.1.2 <- ttkentry(f2, textvariable=name.var)
  f2.ent.2.2 <- ttkentry(f2, textvariable=fmt.var)
  f2.ent.3.2 <- ttkentry(f2, textvariable=class.var)

  f2.txt.4.2 <- tktext(f2, padx=2, pady=2, width=45, height=6, undo=1, wrap="none",
                       relief="flat", foreground="black", background="#ebebe4",
                       borderwidth=1, font="TkFixedFont", state="disabled")
  f2.but.2.3 <- ttkbutton(f2, text="Edit", width=5, command=CallFormat)
  f2.but.4.3 <- ttkbutton(f2, text="Edit", width=5, command=CallEditFunction)

  tkgrid(f2.lab.1.1, f2.ent.1.2, "x")
  tkgrid(f2.lab.2.1, f2.ent.2.2, f2.but.2.3)
  tkgrid(f2.lab.3.1, f2.ent.3.2, "x")
  tkgrid(f2.lab.4.1, f2.txt.4.2, f2.but.4.3)

  tkgrid.configure(f2.lab.1.1, f2.lab.2.1, f2.lab.3.1, sticky="w")

  tkgrid.configure(f2.lab.4.1, sticky="ne")

  tkgrid.configure(f2.ent.1.2, f2.ent.2.2, f2.ent.3.2,
                   sticky="we", padx=2, pady=2)

  tkgrid.configure(f2.txt.4.2, padx=3, pady=3, sticky="nswe")

  tkgrid.configure(f2.but.2.3, sticky="w")
  tkgrid.configure(f2.lab.4.1, pady=c(4, 0))
  tkgrid.configure(f2.but.4.3, sticky="nw", pady=c(1, 0))

  tkgrid.columnconfigure(f2, 1, weight=1, minsize=25)
  tkgrid.rowconfigure(f2, 3, weight=1, minsize=25)

  # frame 3, summary
  f3 <- ttkframe(nb, relief="flat", padding=0, borderwidth=0)
  tkadd(nb, f3, text="   Summary   ")

  f3.ysc <- ttkscrollbar(f3, orient="vertical")

  f3.txt <- tktext(f3, bg="white", padx=2, pady=2, width=60, height=8,
                   undo=1, wrap="none", foreground="black", relief="flat",
                   font="TkFixedFont", yscrollcommand=function(...) tkset(f3.ysc, ...))

  tkconfigure(f3.ysc, command=paste(.Tk.ID(f3.txt), "yview"))

  tkgrid(f3.txt, f3.ysc)

  tkgrid.configure(f3.txt, sticky="news")
  tkgrid.configure(f3.ysc, sticky="ns")

  tkgrid.columnconfigure(f3, 0, weight=1, minsize=25)
  tkgrid.rowconfigure(f3, 0, weight=1, minsize=25)

  # insert notebook and paned window
  tkadd(pw, nb, weight=1)
  tkpack(pw, fill="both", expand="yes", padx=10, pady=c(10, 2))

  # update Notebook
  UpdateNb()

  # bind events
  tclServiceMode(TRUE)

  tkbind(tt, "<Control-n>", SaveNewVar)
  tkbind(tt, "<Control-]>", function() Arrange("forward"))
  tkbind(tt, "<Control-Shift-}>", function() Arrange("front"))
  tkbind(tt, "<Control-[>", function() Arrange("backward"))
  tkbind(tt, "<Control-Shift-{>", function() Arrange("back"))
  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tkbind(nb, "<<NotebookTabChanged>>", ChangeTab)

  tkbind(f1.lst, "<ButtonPress-1>", SaveNb)
  tkbind(f1.lst, "<Up>", SaveNb)
  tkbind(f1.lst, "<Down>", SaveNb)
  tkbind(f1.lst, "<<ListboxSelect>>", UpdateNb)

  tkbind(f2.ent.1.2, "<Return>", function() SetVarId())

  # gui control
  tkfocus(tt)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  return(rtn)
}
