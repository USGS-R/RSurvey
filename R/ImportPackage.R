ImportPackage <- function(classes=NULL, parent=NULL) {


  # load dataset
  LoadDataset <- function() {
    tkconfigure(tt, cursor="watch")
    tclServiceMode(FALSE)
    on.exit(tkconfigure(tt, cursor="arrow"))
    on.exit(tclServiceMode(TRUE), add=TRUE)
    idx <- as.integer(tkcurselection(f1.lst.2.1)) + 1L
    pkg.name <- pkg.names[idx]
    idx <- as.integer(tkcurselection(f1.lst.2.4))
    pkg.item <- as.character(tkget(f1.lst.2.4, idx))
    rtn <<- list(d=eval(parse(text=paste(pkg.name, pkg.item, sep="::"))),
                 src=c(dataset=pkg.item, package=pkg.name,
                       accessed=format(Sys.time())))
    tclvalue(tt.done.var) <- 1
  }


  # load package
  LoadPackage <- function() {
    tkconfigure(tt, cursor="watch")
    tclServiceMode(FALSE)
    on.exit(tkconfigure(tt, cursor="arrow"))
    on.exit(tclServiceMode(TRUE), add=TRUE)
    idx <- as.integer(tkcurselection(f1.lst.2.1)) + 1L
    pkg.name <- pkg.names[idx]

    if (requireNamespace(pkg.name, quietly=TRUE)) {
      idx <- as.integer(tcl(f1.box.3.1, "current"))
      if (idx == 2L) {
        tcl(f1.box.3.1, "current", 0)
        SelectPackageType()
        tkselection.clear(f1.lst.2.1, 0, "end")
        idx <- which(pkg.names %in% pkg.name) - 1L
        tkselection.set(f1.lst.2.1, idx)
        tksee(f1.lst.2.1, idx)
      }
      SelectPackage()
    } else {
      tkmessageBox(icon="error", message="Unable to load package namespace.",
                   title="Error", type="ok", parent=tt)
    }
    tkfocus(f1.lst.2.1)
  }


  # summarize datasets in package
  SummarizePackage <- function() {
    idx <- as.integer(tkcurselection(f1.lst.2.1)) + 1L
    pkg.name <- pkg.names[idx]
    pkg.datasets <- ds.list[[pkg.name]]
    pkg.datasets <- pkg.datasets[order(pkg.datasets[, 1]), , drop=FALSE]
    nmax <- max(nchar(pkg.datasets[, "Item"]))
    if (nmax < 20) nmax <- 20
    items <- sprintf(paste0("%-", nmax, "s"), pkg.datasets[, "Item"])
    txt <- sprintf("Datasets in package %s:\n", sQuote(pkg.name))
    txt <- c(txt, paste(items, pkg.datasets[, "Title"], sep="  "))
    EditText(txt, read.only=TRUE, win.title="Summary of Datasets",
             is.fixed.width.font=TRUE, parent=tt)
    tkfocus(f1.lst.2.1)
  }


  # describe dataset
  DescribeDataset <- function() {
    idx <- as.integer(tkcurselection(f1.lst.2.1)) + 1L
    pkg.name <- pkg.names[idx]
    idx <- as.integer(tkcurselection(f1.lst.2.4))
    pkg.item <- paste(as.character(tkget(f1.lst.2.4, idx)), collapse=" ")
    ans <- try(help(pkg.item, package=(pkg.name)), silent=TRUE)
    if (inherits(ans, "try-error"))
      tkmessageBox(icon="error", message="Problem with dataset documentation.",
                   title="Error", type="ok", parent=tt)
    else
      print(ans)
    tkfocus(f1.lst.2.4)
  }


  # get class of dataset
  GetClass <- function(pkg.name, pkg.item) {
    txt <- paste(pkg.name, pkg.item, sep="::")
    ans <- try(class(eval(parse(text=txt))), silent=TRUE)
    pkg.item.class <- if (inherits(ans, "try-error")) "<Unknown>" else ans
    return(pkg.item.class[1])
  }


  # gui control for select package
  SelectPackage <- function() {
    tkconfigure(tt, cursor="watch")
    tclServiceMode(FALSE)
    on.exit(tkconfigure(tt, cursor="arrow"))
    on.exit(tclServiceMode(TRUE), add=TRUE)
    idx <- as.integer(tkcurselection(f1.lst.2.1)) + 1L
    pkg.name <- pkg.names[idx]
    if (isNamespaceLoaded(pkg.name)) {
      tkconfigure(f1.but.4.2, state="disabled", default="disabled")
      pkg.datasets <- ds.list[[pkg.name]]
      all.pkg.items <- pkg.datasets[, "Item"]
      if (is.null(ds.class[[pkg.name]]))
        ds.class[[pkg.name]] <<- vapply(all.pkg.items, function(i) GetClass(pkg.name, i), "")

      ds.classes <- vapply(ds.class[[pkg.name]], function(i) paste(i, collapse=" "), "")
      ds.in.error <- ds.classes %in% "try-error"
      ds.class.vals <- sort(unique(ds.classes[!ds.in.error]))

      if (as.logical(as.integer(tclvalue(fit.for.loading.var))))
        ds.class.vals <- ds.class.vals[ds.class.vals %in% classes]

      if (length(ds.class.vals) > 0)
        ds.class.vals <- c("Show all classes", ds.class.vals)
      else
        ds.class.vals <- "{Show all classes}"

      old.class <- paste(as.character(tkget(f1.box.3.4)), collapse=" ")
      tkconfigure(f1.box.3.4, value=ds.class.vals, state="readonly")

      idx <- which(ds.class.vals %in% old.class)
      idx <- if (length(idx) > 0) idx - 1L else 0L
      tcl(f1.box.3.4, "current", idx)

      if (idx > 0)
        pkg.items <- all.pkg.items[ds.classes %in% ds.class.vals[idx + 1L]]
      else
        pkg.items <- all.pkg.items[ds.classes %in% ds.class.vals]

      if (length(pkg.items) > 0) pkg.items <- sort(pkg.items)
      tkselection.clear(f1.lst.2.4, 0, "end")
      tclvalue(dataset.var) <- ""
      for (i in seq_along(pkg.items)) tcl("lappend", dataset.var, pkg.items[i])
      if (length(pkg.items) > 0) {
        tkselection.set(f1.lst.2.4, 0)
        tkconfigure(f1.but.4.4, state="normal")
      } else {
        tkconfigure(f1.but.4.4, state="disabled")
      }
    } else {
      tkconfigure(f1.box.3.4, value="{}")
      tcl(f1.box.3.4, "current", 0)
      tkconfigure(f1.box.3.4, state="disabled")

      tkconfigure(f0.but.1.2, state="disabled")
      tkconfigure(f1.but.4.2, state="normal", default="active")
      tkconfigure(f1.but.4.4, state="disabled")
      tkselection.clear(f1.lst.2.4, 0, "end")
      tclvalue(dataset.var) <- ""
    }
    SelectDataset()
  }


  # gui control for select dataset
  SelectDataset <- function() {
    idx <- as.integer(tkcurselection(f1.lst.2.4))
    if (length(idx) == 0) {
      tkconfigure(f0.but.1.2, state="disabled", default="disabled")
      return()
    }
    pkg.item <- paste(as.character(tkget(f1.lst.2.4, idx)), collapse=" ")
    pkg.name <- pkg.names[as.integer(tkcurselection(f1.lst.2.1)) + 1L]
    idx <- which(ds.list[[pkg.name]][, "Item"] %in% pkg.item)
    is.valid <- ds.class[[pkg.name]][idx] %in% classes
    if (is.null(classes) | is.valid)
      tkconfigure(f0.but.1.2, state="normal", default="active")
    else
      tkconfigure(f0.but.1.2, state="disabled", default="disabled")
  }


  # gui control for select package type
  SelectPackageType <- function() {
    tkconfigure(tt, cursor="watch")
    tclServiceMode(FALSE)
    on.exit(tkconfigure(tt, cursor="arrow"))
    on.exit(tclServiceMode(TRUE), add=TRUE)
    idx <- as.integer(tcl(f1.box.3.1, "current"))
    if (idx == 0L) {
      pkg.names <<- all.pkgs
    } else {
      is.pkg.loaded <- all.pkgs %in% loadedNamespaces()
      if (idx == 1L)
        pkg.names <<- all.pkgs[is.pkg.loaded]
      else
        pkg.names <<- all.pkgs[!is.pkg.loaded]
    }
    tkselection.clear(f1.lst.2.1, 0, "end")
    tclvalue(package.var) <- ""
    for (i in seq_along(pkg.names)) tcl("lappend", package.var, pkg.names[i])
    tkselection.set(f1.lst.2.1, 0)
    SelectPackage()
    tkfocus(f1.lst.2.1)
  }


  # gui control for select class type
  SelectClassType <- function() {
    SelectPackage()
    if (length(as.integer(tkcurselection(f1.lst.2.4))) == 0)
      tkfocus(f1.lst.2.1)
    else
      tkfocus(f1.lst.2.4)
  }


  # initialize values
  all.pkgs <- .packages(all.available=TRUE, lib.loc=.libPaths())
  all.pkgs <- all.pkgs[!all.pkgs %in% c("Rcmdr")]

  all.ds <- suppressWarnings(data(package=all.pkgs)$results)
  all.ds[, "Item"] <- gsub(" \\((.*)\\).*", "", all.ds[, "Item"])

  all.pkgs <- sort(unique(all.ds[, "Package"]))

  FUN <- function(i) all.ds[all.ds[, "Package"] == i, c("Item", "Title"), drop=FALSE]
  ds.list <- sapply(all.pkgs, FUN, simplify=FALSE)

  ds.class <- list()

  pkg.type.vals <- paste(c("Show all", "Loaded", "Unloaded"), "packages")
  ds.class.vals <- "{}"

  pkg.names <- NULL
  rtn <- NULL

  # assign variables linked to Tk widgets
  package.var         <- tclVar()
  dataset.var         <- tclVar()
  fit.for.loading.var <- tclVar(1)
  tt.done.var         <- tclVar(0)

  # open gui
  tclServiceMode(FALSE)
  tt <- tktoplevel()
  if (!is.null(parent)) {
    tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tkwm.geometry(parent)), "\\+"))
    tkwm.geometry(tt, paste0("+", as.integer(geo[2]) + 25,
                             "+", as.integer(geo[3]) + 25))
  }
  tktitle(tt) <- "Import Dataset From R Package"

  # frame 0 contains load, cancel, and help buttons, and size grip
  f0 <- ttkframe(tt, relief="flat")

  f0.but.1.2 <- ttkbutton(f0, width=12, text="Load", command=LoadDataset)
  f0.but.1.3 <- ttkbutton(f0, width=12, text="Cancel",
                          command=function() tclvalue(tt.done.var) <- 1)
  f0.but.1.4 <- ttkbutton(f0, width=12, text="Help",
                          command=function() {
                            print(help("ImportPackage", package="RSurvey"))
                          })
  f0.grp.1.5 <- ttksizegrip(f0)

  tkgrid("x", f0.but.1.2, f0.but.1.3, f0.but.1.4, f0.grp.1.5)

  tkgrid.configure(f0.but.1.2, padx=c(10, 0))
  tkgrid.configure(f0.but.1.3, padx=4)
  tkgrid.configure(f0.but.1.4, padx=c(0, 10), columnspan=2)

  tkgrid.configure(f0.but.1.2, f0.but.1.3, f0.but.1.4, pady=c(15, 10))

  tkgrid.configure(f0.grp.1.5, sticky="se")
  tkraise(f0.but.1.4, f0.grp.1.5)
  tkgrid.columnconfigure(f0, 0, weight=1)

  tkpack(f0, fill="x", side="bottom", anchor="e")

  tkconfigure(f0.but.1.2, state="disabled")

  # frame 1, package and dataset
  f1 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0)

  f1.lab.1.1 <- ttklabel(f1, text="Package", foreground="#141414")
  f1.lab.1.4 <- ttklabel(f1, text="Dataset", foreground="#141414")

  f1.lst.2.1 <- tklistbox(f1, selectmode="browse", activestyle="none",
                          relief="flat", borderwidth=5, width=30, height=8,
                          exportselection=FALSE, listvariable=package.var,
                          highlightthickness=0)
  f1.ysc.2.3 <- ttkscrollbar(f1, orient="vertical")
  tkconfigure(f1.lst.2.1, background="white", yscrollcommand=paste(.Tk.ID(f1.ysc.2.3), "set"))
  tkconfigure(f1.ysc.2.3, command=paste(.Tk.ID(f1.lst.2.1), "yview"))

  f1.lst.2.4 <- tklistbox(f1, selectmode="browse", activestyle="none", relief="flat",
                          borderwidth=5, width=30, height=8, exportselection=FALSE,
                          listvariable=dataset.var, highlightthickness=0)
  f1.ysc.2.6 <- ttkscrollbar(f1, orient="vertical")
  tkconfigure(f1.lst.2.4, background="white", yscrollcommand=paste(.Tk.ID(f1.ysc.2.6), "set"))
  tkconfigure(f1.ysc.2.6, command=paste(.Tk.ID(f1.lst.2.4), "yview"))

  f1.box.3.1 <- ttkcombobox(f1, state="readonly", value=pkg.type.vals)
  f1.box.3.4 <- ttkcombobox(f1, state="readonly", value=ds.class.vals)

  f1.but.4.1 <- ttkbutton(f1, width=10, text="Summary", command=SummarizePackage)
  f1.but.4.2 <- ttkbutton(f1, width=10, text="Load", command=LoadPackage)
  f1.but.4.4 <- ttkbutton(f1, width=10, text="Describe", command=DescribeDataset)

  if (is.null(classes))
    f1.chk.4.5 <- "x"
  else
    f1.chk.4.5 <- ttkcheckbutton(f1, text="Fit for loading", variable=fit.for.loading.var,
                                 command=function() {
                                   SelectPackage()
                                   tkfocus(f1.lst.2.1)
                                 })

  tkgrid(f1.lab.1.1, "x", "x", f1.lab.1.4, "x", "x", pady=c(10, 0))
  tkgrid(f1.lst.2.1, "x", f1.ysc.2.3, f1.lst.2.4, "x", f1.ysc.2.6)
  tkgrid(f1.box.3.1, "x", "x", f1.box.3.4, "x", "x", pady=4)
  tkgrid(f1.but.4.1, f1.but.4.2, "x", f1.but.4.4, f1.chk.4.5, "x")

  tkgrid.configure(f1.lab.1.1, columnspan=3)
  tkgrid.configure(f1.lab.1.4, columnspan=3)
  tkgrid.configure(f1.lst.2.1, columnspan=2)
  tkgrid.configure(f1.lst.2.4, columnspan=2)
  tkgrid.configure(f1.box.3.1, columnspan=2)
  tkgrid.configure(f1.box.3.4, columnspan=2)

  tkgrid.configure(f1.lab.1.1, f1.lab.1.4, sticky="w")
  tkgrid.configure(f1.lst.2.1, f1.lst.2.4, sticky="nswe")
  tkgrid.configure(f1.ysc.2.3, f1.ysc.2.6, sticky="ns")
  tkgrid.configure(f1.box.3.1, f1.box.3.4, sticky="we")
  tkgrid.configure(f1.but.4.2, f1.but.4.4, f1.chk.4.5, sticky="w")

  tkgrid.configure(f1.ysc.2.3, padx=c(0, 25))
  tkgrid.configure(f1.but.4.1, padx=c(0, 4))
  tkgrid.configure(f1.but.4.4, padx=c(0, 8))

  tkgrid.columnconfigure(f1, 1, minsize=85, weight=1)
  tkgrid.columnconfigure(f1, 4, minsize=85, weight=1)
  tkgrid.rowconfigure(f1, 1, weight=1)

  tkpack(f1, fill="both", expand=TRUE, anchor="nw", padx=10)

  tkselection.set(f1.lst.2.1, 0)
  tcl(f1.box.3.1, "current", 0)

  # bind events
  tclServiceMode(TRUE)

  tkbind(f1.lst.2.1, "<<ListboxSelect>>",    SelectPackage)
  tkbind(f1.lst.2.4, "<<ListboxSelect>>",    SelectDataset)
  tkbind(f1.box.3.1, "<<ComboboxSelected>>", SelectPackageType)
  tkbind(f1.box.3.4, "<<ComboboxSelected>>", SelectClassType)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  # gui control
  SelectPackageType()

  tkfocus(f1.lst.2.1)
  tkgrab(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  invisible(rtn)
}
