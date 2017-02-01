#' Search
#'
#' A \acronym{GUI} for establishing find and replace arguments in a data table.
#'
#' @param is.replace logical.
#'    If true, the replace component is included.
#' @param defaults list.
#'    See \sQuote{Value} section
#' @param parent tkwin.
#'   \acronym{GUI} parent window
#'
#' @return Returns an object of list class with the following components:
#'     \item{find.what}{string to search for}
#'     \item{replace.with}{replacement string}
#'     \item{is.match.word}{indicates whether matches be restricted to whole words only.}
#'     \item{is.match.case}{indicates whether the search is case sensitive.}
#'     \item{is.reg.exps}{if true, the search is made using \link{regular expression};
#'       that is, a pattern that describes a set of strings.}
#'     \item{is.search.col}{indicates whether the search is limited to a single column.}
#'     \item{is.perl}{indicates whether Perl style regular expressions should be used.}
#'     \item{is.replace.first}{indicates whether to replace for only the first instance.}
#'     \item{is.search.sel}{indicates whether the search limited to selected cells.}
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
#'   ans <- Search()
#' }
#'

Search <- function(is.replace=FALSE, defaults=NULL, parent=NULL) {


  # return find and replace parameters
  ReturnParameters <- function(is.replace.first=FALSE) {
    find.what <- as.character(tclvalue(find.what.var))
    replace.with <- if (is.replace) as.character(tclvalue(replace.with.var)) else NULL
    is.match.word <- as.logical(as.integer(tclvalue(match.word.var)))
    is.match.case <- as.logical(as.integer(tclvalue(match.case.var)))
    is.reg.exps   <- as.logical(as.integer(tclvalue(reg.exps.var)))
    is.search.col <- as.logical(as.integer(tclvalue(search.col.var)))
    is.perl       <- as.logical(as.integer(tclvalue(perl.var)))
    is.search.sel <- as.logical(as.integer(tclvalue(search.sel.var)))
    rtn <<- list(find.what=find.what, replace.with=replace.with,
                 is.match.word=is.match.word, is.match.case=is.match.case,
                 is.reg.exps=is.reg.exps, is.search.col=is.search.col,
                 is.perl=is.perl, is.replace.first=is.replace.first,
                 is.search.sel=is.search.sel)
    tclvalue(tt.done.var) <- 1
  }


  # toggle match word
  ToggleMatchWord <- function() {
    is.match.word <- as.logical(as.integer(tclvalue(match.word.var)))
    if (is.match.word) {
      tclvalue(reg.exps.var) <- FALSE
      tkconfigure(f2.chk.4.1, state="disabled")
    } else {
      tkconfigure(f2.chk.4.1, state="normal")
    }
    ToggleRegExps()
  }


  # toggle regular expression
  ToggleRegExps <- function() {
    is.reg.exps <- as.logical(as.integer(tclvalue(reg.exps.var)))
    if (is.reg.exps) {
      tkconfigure(f2.rad.4.2, state="normal")
      tkconfigure(f2.rad.4.3, state="normal")
    } else {
      tkconfigure(f2.rad.4.2, state="disabled")
      tkconfigure(f2.rad.4.3, state="disabled")
    }
  }


  # assigin global variables
  rtn <- NULL

  # assign variables linked to Tk widgets
  find.what.var     <- tclVar()
  replace.with.var  <- tclVar()
  match.word.var    <- tclVar(0)
  match.case.var    <- tclVar(0)
  reg.exps.var      <- tclVar(0)
  search.col.var    <- tclVar(0)
  search.sel.var    <- tclVar(0)
  perl.var          <- tclVar(0)
  tt.done.var       <- tclVar(0)

  # set default values
  replace.with <- ""
  if (!is.null(defaults) && is.list(defaults)) {
    if (!is.null(defaults$find.what) && is.character(defaults$find.what))
      tclvalue(find.what.var) <- defaults$find.what
    if (!is.null(defaults$replace.with) && is.character(defaults$replace.with))
      tclvalue(replace.with.var) <- defaults$replace.with
    if (!is.null(defaults$is.match.word) && is.logical(defaults$is.match.word))
      tclvalue(match.word.var) <- defaults$is.match.word
    if (!is.null(defaults$is.match.case) && is.logical(defaults$is.match.case))
      tclvalue(match.case.var) <- defaults$is.match.case
    if (!is.null(defaults$is.reg.exps) && is.logical(defaults$is.reg.exps))
      tclvalue(reg.exps.var) <- defaults$is.reg.exps
    if (!is.null(defaults$is.search.col) && is.logical(defaults$is.search.col))
      tclvalue(search.col.var) <- defaults$is.search.col
    if (!is.null(defaults$is.perl) && is.logical(defaults$is.perl))
      tclvalue(perl.var) <- defaults$is.perl
    if (!is.null(defaults$is.search.sel) && is.logical(defaults$is.search.sel))
      tclvalue(search.sel.var) <- defaults$is.search.sel
  }

  # open gui
  tclServiceMode(FALSE)
  tt <- tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    geo <- as.integer(geo[2:3]) + 25
    tkwm.geometry(tt, sprintf("+%s+%s", geo[1], geo[2]))
  }
  tktitle(tt) <- "Search"
  tkwm.resizable(tt, 1, 0)

  # frame 0
  f0 <- ttkframe(tt, relief="flat")

  if (is.replace) {
    f0.but.1.2 <- ttkbutton(f0, width=12, text="Replace",
                            command=function() ReturnParameters(TRUE))
    f0.but.1.3 <- ttkbutton(f0, width=12, text="Replace All",
                            command=function() ReturnParameters(FALSE))
    f0.but.1.4 <- ttkbutton(f0, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)
  } else {
    f0.but.1.2 <- "x"
    f0.but.1.3 <- ttkbutton(f0, width=12, text="Find",
                            command=function() ReturnParameters())
    f0.but.1.4 <- ttkbutton(f0, width=12, text="Cancel",
                            command=function() tclvalue(tt.done.var) <- 1)
  }
  f0.but.1.5 <- ttkbutton(f0, width=12, text="Help",
                          command=function() {
                            print(utils::help("Search", package="RSurvey"))
                          })

  tkgrid("x", f0.but.1.2, f0.but.1.3, f0.but.1.4, f0.but.1.5)

  tkgrid.columnconfigure(f0, 0, weight=1)

  tkgrid.configure(f0.but.1.3, f0.but.1.4, padx=c(4, 0))
  if (is.replace)
    tkgrid.configure(f0.but.1.2, padx=c(10, 0))
  else
    tkgrid.configure(f0.but.1.3, padx=c(10, 0))
  tkgrid.configure(f0.but.1.5, padx=c(4, 10), pady=10)

  tkpack(f0, fill="x", side="bottom", anchor="e")

  # frame 1
  f1 <- ttkframe(tt, relief="flat")

  f1.lab.1.1 <- ttklabel(f1, text="Find what:", foreground="#141414")
  f1.ent.2.1 <- ttkentry(f1, width=10, font="TkFixedFont", textvariable=find.what.var)
  tkgrid(f1.lab.1.1, sticky="w")
  tkgrid(f1.ent.2.1, sticky="we")
  tkgrid.columnconfigure(f1, 0, weight=1)

  if (is.replace) {
    f1.lab.3.1 <- ttklabel(f1, text="Replace with:", foreground="#141414")
    f1.ent.4.1 <- ttkentry(f1, width=10, font="TkFixedFont", textvariable=replace.with.var)
    tkgrid(f1.lab.3.1, sticky="w", pady=c(10, 0))
    tkgrid(f1.ent.4.1, sticky="we")
    tkgrid.rowconfigure(f1, 3, weight=1)
  }

  tkpack(f1, fill="x", expand="yes", padx=10, pady=10)

  # frame 2
  f2 <- ttkframe(tt, relief="flat")

  f2.chk.1.1 <- ttkcheckbutton(f2, text="Match case", variable=match.case.var)
  f2.chk.2.1 <- ttkcheckbutton(f2, text="Match entire cell contents", variable=match.word.var,
                               command=function() ToggleMatchWord())
  f2.chk.3.1 <- ttkcheckbutton(f2, text="Search in selected cells", variable=search.sel.var)
  f2.chk.4.1 <- ttkcheckbutton(f2, text="Search using regular expressions", variable=reg.exps.var,
                               command=function() ToggleRegExps())
  f2.rad.4.2 <- ttkradiobutton(f2, variable=perl.var, value=FALSE, text="Unix")
  f2.rad.4.3 <- ttkradiobutton(f2, variable=perl.var, value=TRUE, text="Perl")

  tkgrid(f2.chk.1.1, "x", "x", sticky="w")
  tkgrid(f2.chk.2.1, "x", "x", sticky="w", pady=2)

  tkgrid(f2.chk.3.1, "x", "x", sticky="w")
  tkgrid(f2.chk.4.1, f2.rad.4.2, f2.rad.4.3, sticky="w", pady=c(2, 10))

  tkgrid.configure(f2.rad.4.2, padx=c(4, 4))

  tkpack(f2, fill="x", padx=10)

  # gui control
  tclServiceMode(TRUE)

  ToggleRegExps()

  tkfocus(f1.ent.2.1)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  return(rtn)
}
