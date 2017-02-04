#' GUI: Progress Bar
#'
#' A progress bar that shows the status of long-running operations.
#'
#' @param win.title character.
#'   String to display as the title of the dialog box.
#' @param label character.
#'   String to display in the dialog box.
#' @param maximum numeric.
#'   Maximum value for the progress bar.
#'   The minimum value is zero.
#' @param nsteps numeric.
#'   Total number of increments the progress bar will make.
#' @param min.nsteps numeric.
#'   Minimum number of increments.
#'   If greater than \code{nsteps}, the dialog box is not opened.
#' @param parent tkwin.
#'   graphical user interface parent window
#' @param pb ProgressBar.
#'   Object returned from \code{ProgressBar}, see \sQuote{Value} section.
#' @param value numeric.
#'   Value for the progress bar, between zero and \code{maximum}.
#' @param step numeric.
#'   Number of progress bar increments.
#'   If equal to \code{nsteps}, the dialog box will close.
#'
#' @return For \code{ProgressBar} an object of class \code{"ProgressBar"} and mode \code{list} is returned.
#'   Components of the list object include:
#'     \item{GetValue}{function that returns the value of the progress bar.}
#'     \item{MoveProgressBar}{function that moves progress bar, passes a numeric argument.}
#'     \item{SetLabel}{function that sets label in the dialog box, passes a character argument.}
#'     \item{DestroyWindow}{function that closes the dialog box.}
#'     \item{GetWindowState}{function that returns false if the dialog box has been closed, otherwise true.}
#'     \item{nsteps}{see \sQuote{Arguments} section}
#'
#'   For \code{SetProgressBar}, the previous value of the progress bar.
#'   An error is returned if the progress has terminated prematurely.
#'
#' @references The code in this function was derived from the \code{\link[tcltk]{tkProgressBar}} function, version v3.0.2.
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
#'   maximum <- 10
#'   label <- "Estimated time to completion is being calculated\u2026"
#'   pb <- ProgressBar(label = label, maximum = maximum, nsteps = maximum)
#'
#'   for (i in seq_len(maximum)) {
#'     est.time <- system.time(Sys.sleep(1))["elapsed"] * (maximum - i)
#'     label <- paste("Estimated time to completion is", round(est.time), "secs")
#'     ans <- try(SetProgressBar(pb, value = i, label = label, step = i))
#'     if (inherits(ans, "try-error")) break
#'   }
#' }
#'

ProgressBar <- function (win.title="Progress Bar", label="", maximum=100,
                         nsteps=NULL, min.nsteps=10L, parent=NULL) {


  MoveProgressBar <- function (x) {
    if (!is.finite(x) || x < 0 || x > maximum) return()
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
    if (!is.null(parent)) tkfocus(parent)
    tclServiceMode(TRUE)
  }


  SetLabel <- function (x) {
    tclvalue(.lab) <- x
  }


  if (is.numeric(nsteps) && is.numeric(min.nsteps) && nsteps < min.nsteps) return()

  tclServiceMode(FALSE)
  on.exit(tclServiceMode(TRUE))

  .tt <-tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(.tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    geo <- as.integer(geo[2:3]) + 25
    tkwm.geometry(.tt, sprintf("+%s+%s", geo[1], geo[2]))
  }
  tktitle(.tt) <- win.title
  tkwm.resizable(.tt, 0, 0)

  .is.destroyed <- FALSE
  .val <- tclVar(0)
  .lab <- tclVar(label)

  frame0 <- ttkframe(.tt, relief="flat")
  frame0.lab <- ttklabel(frame0, textvariable=.lab)
  frame0.pbr <- ttkprogressbar(frame0, length=300, variable=.val, maximum=maximum)
  frame0.but <- ttkbutton(frame0, width=12, text="Cancel", command=DestroyWindow)
  tkgrid(frame0.lab, sticky="w")
  tkgrid(frame0.pbr, sticky="we", pady=c(10, 15))
  tkgrid(frame0.but, sticky="e")
  tkpack(frame0, fill="x", padx=10, pady=10)

  tkbind(.tt, "<Destroy>", DestroyWindow)
  tkfocus(.tt)

  lst <- list(GetValue=GetValue, MoveProgressBar=MoveProgressBar,
              SetLabel=SetLabel, DestroyWindow=DestroyWindow,
              GetWindowState=GetWindowState, nsteps=nsteps)
  return(structure(lst, class="ProgressBar"))
}


#' @rdname ProgressBar
#'
#' @export

SetProgressBar <- function (pb, value, label=NULL, step=NULL) {
  if (!inherits(pb, "ProgressBar")) return()
  tclServiceMode(FALSE)
  on.exit(tclServiceMode(TRUE))
  if (!pb$GetWindowState()) stop("progress bar terminated prematurely")
  old.value <- pb$GetValue()
  pb$MoveProgressBar(value)
  if (!is.null(label)) pb$SetLabel(label)
  if (is.numeric(step) && is.numeric(pb$nsteps) && step >= pb$nsteps) pb$DestroyWindow()
  invisible(old.value)
}
