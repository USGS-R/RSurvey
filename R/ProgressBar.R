# A progress bar for providing feedback; derived from
# tcltk::tkProgressBar (v3.0.2)

ProgressBar <- function (win.title="Progress Bar", label="", maximum=100,
                         parent=NULL) {

  MoveProgressBar <- function (x) {
    if (!is.finite(x) || x < 0 || x > maximum)
      return()
    tclvalue(.val) <<- x
    return()
  }

  GetValue <- function () {
    return(.val)
  }

  GetWindowState <- function () {
    return(as.logical(as.integer(tkwinfo("exists", .tt))))
  }

  DestroyWindow <- function () {
    tclServiceMode(FALSE)
    if (!.is.destroyed) {
      tkdestroy(.tt)
      .is.destroyed <<- TRUE
    }
    tclServiceMode(TRUE)
  }

  SetLabel <- function (x) {
    tclvalue(.lab) <- x
  }


  tclServiceMode(FALSE)

  .tt <-tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(.tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(.tt, paste0("+", as.integer(geo[2]) + 25,
                              "+", as.integer(geo[3]) + 25))
  }
  tktitle(.tt) <- win.title
  tkwm.resizable(.tt, 0, 0)

  .is.destroyed <- FALSE
  .val <- tclVar(0)
  .lab <- tclVar(label)

  frame0 <- ttkframe(.tt, relief="flat")
  frame0.lab <- ttklabel(frame0, textvariable=.lab)
  frame0.pbr <- ttkprogressbar(frame0, length=300, variable=.val,
                               maximum=maximum)
  frame0.but <- ttkbutton(frame0, width=12, text="Cancel",
                          command=DestroyWindow)
  tkgrid(frame0.lab, sticky="w")
  tkgrid(frame0.pbr, sticky="we", pady=c(10, 15))
  tkgrid(frame0.but, sticky="e")
  tkpack(frame0, fill="x", padx=10, pady=10)

  tclServiceMode(TRUE)

  tkbind(.tt, "<Destroy>", DestroyWindow)
  tkfocus(.tt)

  lst <- list(GetValue=GetValue, MoveProgressBar=MoveProgressBar,
              SetLabel=SetLabel, DestroyWindow=DestroyWindow,
              GetWindowState=GetWindowState)
  return(structure(lst, class="ProgressBar"))
}


SetProgressBar <- function (pb, value, label=NULL) {
  tclServiceMode(FALSE)
  on.exit(tclServiceMode(TRUE))
  if (!inherits(pb, "ProgressBar"))
    stop("invalid class for 'pb'")
  if (!pb$GetWindowState())
    return(NA)
  old.value <- pb$GetValue()
  pb$MoveProgressBar(value)
  if (!is.null(label))
    pb$SetLabel(label)
  invisible(old.value)
}
