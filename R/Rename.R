#' Rename Values in Character Vector
#'
#' A \acronym{GUI} for renaming values in a vector of character strings.
#'
#' @param names character.
#'    Vector of character strings
#' @param cur.name character.
#'    Sets the combobox value, name must be included in \code{names}.
#' @param win.title character.
#'    String to display as the title of the dialog box.
#' @param parent tkwin.
#'   \acronym{GUI} parent window
#'
#' @return Returns a character vector with updated values of \code{names}.
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
#'   Rename(names = c("Name1", "Name2", "Name3"), cur.name = "Name2")
#' }
#'

Rename <- function(names=NULL, cur.name=NULL, win.title=NULL, parent=NULL) {


  # update entry
  UpdateEntry <- function() {
    if (tclvalue(cur.var) != "" && !(tclvalue(new.var) %in% new.names))
      new.names[names %in% tclvalue(cur.var)] <<- tclvalue(new.var)
    tclvalue(new.var) <- new.names[names %in% tclvalue(old.var)]
    tclvalue(cur.var) <- tclvalue(old.var)
  }


  # save renamed values
  SaveNames <- function() {
    UpdateEntry()
    rtn.names <<- new.names
    tclvalue(tt.done.var) <- 1
  }


  if (is.null(names)) return(NULL)
  rtn.names <- new.names <- names

  # assign the variables linked to tk widgets
  old.var     <- tclVar("")
  new.var     <- tclVar("")
  cur.var     <- tclVar("")
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
  if (!is.null(win.title)) tktitle(tt) <- win.title
  tkwm.resizable(tt, 1, 0)

  # frame 0
  f0 <- ttkframe(tt, relief="flat")

  f0.but.2 <- ttkbutton(f0, width=12, text="OK", command=SaveNames)
  f0.but.3 <- ttkbutton(f0, width=12, text="Cancel",
                        command=function() tclvalue(tt.done.var) <- 1)
  f0.but.4 <- ttkbutton(f0, width=12, text="Help",
                        command=function() {
                          print(utils::help("Rename", package="RSurvey"))
                        })

  tkgrid("x", f0.but.2, f0.but.3, f0.but.4, sticky="se", pady=c(15, 10), padx=c(4, 0))
  tkgrid.columnconfigure(f0, 0, weight=1)
  tkgrid.configure(f0.but.2, padx=c(40, 0))
  tkgrid.configure(f0.but.4, padx=c(4, 10))
  tkpack(f0, fill="x", side="bottom", anchor="e")

  # frame 1
  f1 <- ttkframe(tt, relief="flat")

  f1.lab.1 <- ttklabel(f1, text="Old name")
  f1.lab.2 <- ttklabel(f1, text="New name")

  prep.names <- if (length(names) == 1) paste0("{", names, "}") else names

  f1.box.1 <- ttkcombobox(f1, state="readonly", values=prep.names, textvariable=old.var)
  f1.ent.1 <- ttkentry(f1, textvariable=new.var)

  if (!is.null(cur.name) && cur.name %in% names)
    tcl(f1.box.1, "current", match(cur.name, names) - 1)

  tkgrid(f1.lab.1, f1.box.1)
  tkgrid(f1.lab.2, f1.ent.1, pady=c(10, 0))

  tkgrid.configure(f1.lab.1, f1.lab.2, sticky="w", padx=c(0, 2))
  tkgrid.configure(f1.box.1, f1.ent.1, sticky="we")

  tkgrid.columnconfigure(f1, 1, weight=1, minsize=25)

  tkpack(f1, fill="x", expand=TRUE, padx=10, pady=c(10, 0))

  # bind events
  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  tkbind(f1.box.1, "<<ComboboxSelected>>", UpdateEntry)

  # gui control
  UpdateEntry()

  tkfocus(tt)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  return(rtn.names)
}
