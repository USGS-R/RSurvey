SetSortOrder <- function(col.ids, sort.on=NULL, parent=NULL) {


  # save sort order
  SaveSortOrder <- function() {
    col.id <- as.character(tclvalue(col.id.var))
    decreasing <- as.logical(as.integer(tclvalue(decreasing.var)))
    na.last <- as.integer(tclvalue(na.last.var))
    na.last <- if (na.last %in% 0:1)  as.logical(na.last) else NA
    if (col.id == "") {
      sort.on <- NULL
    } else {
      sort.on <- which(col.ids == col.id)
      attr(sort.on, "decreasing") <- decreasing
      attr(sort.on, "na.last") <- na.last
    }
    rtn <<- sort.on
    tclvalue(tt.done.var) <- 1
  }


  # initialize return value
  rtn <- sort.on

  # assign variables linked to Tk widgets
  col.id.var     <- tclVar()
  decreasing.var <- tclVar(0)
  na.last.var    <- tclVar(1)
  tt.done.var    <- tclVar(0)

  # set variables
  idx <- 0L
  if (is.integer(sort.on) && idx %in% seq_along(col.ids)) {
    idx <- as.integer(sort.on)
    decreasing <- attr(sort.on, "decreasing")
    if (!is.null(decreasing)) tclvalue(decreasing.var) <- as.logical(decreasing)
    na.last <- attr(sort.on, "na.last")
    if (!is.null(na.last)) {
      if (is.logical(na.last)) tclvalue(na.last.var) <- as.integer(na.last)
      if (is.na(na.last)) tclvalue(na.last.var) <- 2
    }
  }

  # open gui
  tclServiceMode(FALSE)
  tt <- tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste0("+", as.integer(geo[2]) + 25,
                             "+", as.integer(geo[3]) + 25))
  }
  tktitle(tt) <- "Sort Order"
  tkwm.resizable(tt, 1, 0)

  # frame 0
  f0 <- tkframe(tt, relief="flat")

  f0.but.2 <- ttkbutton(f0, width=12, text="OK", command=SaveSortOrder)
  f0.but.3 <- ttkbutton(f0, width=12, text="Cancel",
                        command=function() tclvalue(tt.done.var) <- 1)
  f0.but.4 <- ttkbutton(f0, width=12, text="Help",
                        command=function() {
                          print(help("SetSortOrder", package="RSurvey"))
                        })

  tkgrid("x", f0.but.2, f0.but.3, f0.but.4, pady=c(15, 10), padx=c(4, 0))
  tkgrid.columnconfigure(f0, 0, weight=1)
  tkgrid.configure(f0.but.4, padx=c(4, 10))
  tkpack(f0, fill="x", side="bottom", anchor="e")

  # frame 1
  f1 <- ttkframe(tt, relief="flat")

  f1.lab.1.1 <- tklabel(f1, text="Variable to sort on")

  vals <- c("", col.ids)
  if (length(vals) == 1) vals <- paste0("{", vals, "}")
  f1.box.1.2 <- ttkcombobox(f1, state="readonly", textvariable=col.id.var, values=vals)
  tcl(f1.box.1.2, "current", idx)

  f1.lab.2.2 <- ttklabel(f1, text="Order")
  f1.rad.2.3 <- ttkradiobutton(f1, variable=decreasing.var, value=FALSE,
                               text="increasing", width=10)
  f1.rad.3.3 <- ttkradiobutton(f1, variable=decreasing.var, value=TRUE,
                               text="decreasing", width=10)

  f1.lab.2.4 <- ttklabel(f1, text="NAs")
  f1.rad.2.5 <- ttkradiobutton(f1, variable=na.last.var, value=1, text="place last")
  f1.rad.3.5 <- ttkradiobutton(f1, variable=na.last.var, value=0, text="place first")
  f1.rad.4.5 <- ttkradiobutton(f1, variable=na.last.var, value=2, text="remove")

  tkgrid(f1.lab.1.1, f1.box.1.2, pady=c(15, 5))
  tkgrid("x", f1.lab.2.2, f1.rad.2.3, f1.lab.2.4, f1.rad.2.5, "x")
  tkgrid("x", "x", f1.rad.3.3, "x", f1.rad.3.5, "x")
  tkgrid("x", "x", "x", "x", f1.rad.4.5, "x")

  tkgrid.configure(f1.box.1.2, sticky="ew", columnspan=6)
  tkgrid.configure(f1.lab.2.2, padx=c(0, 4))
  tkgrid.configure(f1.lab.2.4, padx=c(20, 4))
  tkgrid.configure(f1.rad.2.5, f1.rad.3.5, f1.rad.4.5, sticky="w")

  tkgrid.columnconfigure(f1, 6, weight=1, minsize=0)

  tkpack(f1, fill="x", padx=10)

  # bind events
  tclServiceMode(TRUE)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

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
