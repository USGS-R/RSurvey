

ProgressBar <- function (win.title="Progress Bar", label="", parent=NULL) {

  MoveProgressbar <- function (x) {
    if (!is.finite(x) || x < 0 || x > 100)
      return()
    tclvalue(.var) <<- x
    return()
  }

  GetValue <- function () {
    return(.var)
  }

  GetWindowState <- function () {
    return(as.logical(as.integer(tkwinfo("exists", .tt))))
  }

  DestroyWindow <- function () {
    if (!.is.destroyed) {
      tkdestroy(.tt)
      .is.destroyed <<- TRUE
    }
  }

  SetLabel <- function (x) {
    tclvalue(.lab) <- x
  }




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
  .var <- tclVar(0)
  .lab <- tclVar(label)

  frame0 <- ttkframe(.tt, relief="flat")

  frame0.lab <- ttklabel(frame0, textvariable=.lab)
  frame0.pbr <- ttkprogressbar(frame0, length=300, variable=.var)
  frame0.but <- ttkbutton(frame0, width=12, text="Cancel",
                          command=DestroyWindow)


  tkgrid(frame0.lab, sticky="w")
  tkgrid(frame0.pbr, sticky="we", pady=c(10, 15))
  tkgrid(frame0.but, sticky="e")

  tkpack(frame0, fill="x", padx=10, pady=10)



  tkbind(.tt, "<Destroy>", DestroyWindow)

  lst <- list(GetValue=GetValue, MoveProgressbar=MoveProgressbar,
              SetLabel=SetLabel, DestroyWindow=DestroyWindow,
              GetWindowState=GetWindowState)
  return(structure(lst, class="ttkProgressBar"))
}



SetProgressBar <- function (pb, value, label=NULL) {
  if (!inherits(pb, "ttkProgressBar"))
    stop()
  old.value <- pb$GetValue()
  pb$MoveProgressbar(value)
  if (!is.null(label))
    pb$SetLabel(label)
  tcl("update", "idletasks")
  invisible(old.value)
}








library(tcltk)

pb <- ProgressBar(label = "Some information")


vals <- c(0, sort(runif(50, min = 0, max = 100)), 100)
nvals <- length(vals)
old.sys.time <- Sys.time()
for (i in seq_along(vals)) {
  if (!pb$GetWindowState())
    break

  Sys.sleep(0.2)

  if (i == 1L)
    label <- "Some information"
  else
    label <- paste("Estimated time to completion is", format(round(tdiff)))

  SetProgressBar(pb, vals[i], label)

  new.sys.time <- Sys.time()
  tdiff <- (new.sys.time - old.sys.time) * (nvals - i)
  old.sys.time <- new.sys.time
}




