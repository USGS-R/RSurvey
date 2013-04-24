# A GUI for importing data sets from R packages.

ImportPackageData <- function(parent=NULL) {

  # Additional functions (subroutines)
  
  # Load data set
  LoadDataset <- function() {
    idx <- as.integer(tkcurselection(frame1.lst.2.1)) + 1L
    pkg.name <- pkg.names[idx]
    idx <- as.integer(tkcurselection(frame1.lst.2.4))
    pkg.item <- as.character(tkget(frame1.lst.2.4, idx))
    rtn <<- eval(parse(text=paste(pkg.name, pkg.item, sep="::")))
    tclvalue(tt.done.var) <- 1
  }
  
  # Check if package(s) are loaded
  IsPackageLoaded <- function(pkg.names) {
    vapply(pkg.names, function(i) paste("package", i, sep=":") %in% search(), 
           TRUE)
  }
  
  # Load package
  LoadPackage <- function(pkg.name) {
    idx <- as.integer(tkcurselection(frame1.lst.2.1)) + 1L
    pkg.name <- pkg.names[idx]
    lib <- paste("package", pkg.name, sep=":")
    if (!lib %in% search())
      suppressPackageStartupMessages(require(pkg.name, quietly=TRUE, 
                                             warn.conflicts=FALSE, 
                                             character.only=TRUE))
    if (lib %in% search()) {
      idx <- as.integer(tcl(frame1.box.3.1, "current"))
      if (idx == 2L) {
        tcl(frame1.box.3.1, "current", 0)
        SelectPackageType()
        tkselection.clear(frame1.lst.2.1, 0, "end")
        idx <- which(pkg.names %in% pkg.name) - 1L
        tkselection.set(frame1.lst.2.1, idx)
        tksee(frame1.lst.2.1, idx)
      }
      SelectPackage()
    } else {
      tkmessageBox(icon="error", message="Unable to load package.", 
                   title="Error", type="ok", parent=tt)
    }
  }
  
  # Describe package
  DescribePackage <- function() {
    idx <- as.integer(tkcurselection(frame1.lst.2.1)) + 1L
    pkg.name <- pkg.names[idx]
    f <- system.file("DESCRIPTION", package=pkg.name)
    if (file.access(f, 0) < 0 || file.access(f, 4) < 0) {
      tkmessageBox(icon="error", message="Problem with package documentation.", 
                   title="Error", type="ok", parent=tt)
    } else {
      msg <- paste(readLines(f, n=-1L), collapse="\n")
      tkmessageBox(icon="info", message=msg, title="Package Description", 
                   parent=tt)
    }
  }
  
  # Describe data set
  DescribeDataset <- function() {
    idx <- as.integer(tkcurselection(frame1.lst.2.1)) + 1L
    pkg.name <- pkg.names[idx]
    idx <- as.integer(tkcurselection(frame1.lst.2.4))
    pkg.item <- as.character(tkget(frame1.lst.2.4, idx))
    ans <- try(help(pkg.item, package=(pkg.name)), silent=TRUE)
    if (inherits(ans, "try-error"))
      tkmessageBox(icon="error", message="Problem with dataset documentation.", 
                   title="Error", type="ok", parent=tt)
    else
      print(ans)
  }
  
  # GUI control for select package
  SelectPackage <- function() {
    idx <- as.integer(tkcurselection(frame1.lst.2.1)) + 1L
    tclServiceMode(FALSE)
    pkg.name <- pkg.names[idx]
    if (IsPackageLoaded(pkg.name)) {
      tkconfigure(frame1.but.4.2, state="disabled")
      pkg.datasets <- ds.list[[pkg.name]]
      all.pkg.items <- pkg.datasets[, "Item"]
      idx <- as.integer(tcl(frame1.box.3.4, "current"))
      if (idx > 0) 
        valid.classes <- valid.classes[idx]
      fun <- function(i) inherits(try(eval(parse(text=i)), silent=TRUE), 
                                  valid.classes)
      is.valid <- vapply(all.pkg.items, fun, TRUE)
      pkg.items <- all.pkg.items[is.valid]
      if (length(pkg.items) > 0)
        pkg.items <- sort(pkg.items)
      tkselection.clear(frame1.lst.2.4, 0, "end")
      tclvalue(dataset.var) <- ""
      for (i in seq(along=pkg.items))
        tcl("lappend", dataset.var, pkg.items[i])
      if (length(pkg.items) > 0) {
        tkselection.set(frame1.lst.2.4, 0)
        tkconfigure(frame0.but.1.2, state="normal")
        tkconfigure(frame1.but.4.4, state="normal")
      } else {
        tkconfigure(frame0.but.1.2, state="disabled")
        tkconfigure(frame1.but.4.4, state="disabled")
      }
    } else {
      tkconfigure(frame0.but.1.2, state="disabled")
      tkconfigure(frame1.but.4.2, state="normal")
      tkconfigure(frame1.but.4.4, state="disabled")
      tkselection.clear(frame1.lst.2.4, 0, "end")
      tclvalue(dataset.var) <- ""
    }
    tclServiceMode(TRUE)
  }
  
  # GUI control for select package type
  SelectPackageType <- function() {
    idx <- as.integer(tcl(frame1.box.3.1, "current"))
    tclServiceMode(FALSE)
    if (idx == 0L) {
      pkg.names <<- all.pkgs
    } else {
      is.pkg.loaded <- IsPackageLoaded(all.pkgs)
      if (idx == 1L)
        pkg.names <<- all.pkgs[is.pkg.loaded]
      else
        pkg.names <<- all.pkgs[!is.pkg.loaded]
    }
    tkselection.clear(frame1.lst.2.1, 0, "end")
    tclvalue(package.var) <- ""
    for (i in seq(along=pkg.names))
      tcl("lappend", package.var, pkg.names[i])
    tkselection.set(frame1.lst.2.1, 0)
    tclServiceMode(TRUE)
    SelectPackage()
  }


  # Main program

  # Initialize values

  all.pkgs <- .packages(all.available=TRUE, lib.loc=.libPaths())
  all.pkgs <- all.pkgs[!all.pkgs %in% c("RSurvey", "Rcmdr")]
  
  all.ds <- suppressWarnings(data(package=all.pkgs)$results)
  all.pkgs <- sort(unique(all.ds[, "Package"]))
  
  ds.list <- sapply(all.pkgs, 
                    function(i) all.ds[all.ds[, "Package"] == i, 
                                       c("Item", "Title"), drop=FALSE],
                    simplify=FALSE)
  
  pkg.type.vals <- c("All packages with data sets", "Loaded packages", 
                     "Unloaded packages")
  valid.classes <- c("matrix", "data.frame")
  ds.class.vals <- c("All loadable data sets", "Matrices", "Data frames")
  
  pkg.names <- NULL
  rtn <- NULL

  # Assign variables linked to Tk widgets

  package.var <- tclVar()
  dataset.var <- tclVar()

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

  tktitle(tt) <- "Import Data From Package"

  # Frame 0 contains load, cancel, and help buttons, and size grip

  frame0 <- ttkframe(tt, relief="flat")

  frame0.but.1.2 <- ttkbutton(frame0, width=12, text="Load",
                              command=LoadDataset)
  frame0.but.1.3 <- ttkbutton(frame0, width=12, text="Cancel",
                              command=function() tclvalue(tt.done.var) <- 1)
  frame0.but.1.4 <- ttkbutton(frame0, width=12, text="Help",
                              command=function() {
                                print(help("ImportPackageData", 
                                           package="RSurvey"))
                              })
  frame0.grp.1.5 <- ttksizegrip(frame0)

  tkgrid("x", frame0.but.1.2, frame0.but.1.3, frame0.but.1.4, frame0.grp.1.5)

  tkgrid.configure(frame0.but.1.2, padx=c(10, 0))
  tkgrid.configure(frame0.but.1.3, padx=4)
  tkgrid.configure(frame0.but.1.4, padx=c(0, 10), columnspan=2)
  
  tkgrid.configure(frame0.but.1.2, frame0.but.1.3, frame0.but.1.4, 
                   pady=c(15, 10))
  
  tkgrid.configure(frame0.grp.1.5, sticky="se")
  tkraise(frame0.but.1.4, frame0.grp.1.5)
  tkgrid.columnconfigure(frame0, 0, weight=1)

  tkpack(frame0, fill="x", side="bottom", anchor="e")

  tkconfigure(frame0.but.1.2, state="disabled")

  # Frame 1, package and dataset

  frame1 <- ttkframe(tt, relief="flat", padding=0, borderwidth=0)

  frame1.lab.1.1 <- ttklabel(frame1, text="Package", foreground="#414042")
  frame1.lab.1.4 <- ttklabel(frame1, text="Data set", foreground="#414042")
  
  frame1.lst.2.1 <- tklistbox(frame1, selectmode="browse", activestyle="none",
                              relief="flat", borderwidth=5, width=30, height=8,
                              exportselection=FALSE, listvariable=package.var,
                              highlightthickness=0)
  frame1.ysc.2.3 <- ttkscrollbar(frame1, orient="vertical")
  tkconfigure(frame1.lst.2.1, background="white",
              yscrollcommand=paste(.Tk.ID(frame1.ysc.2.3), "set"))
  tkconfigure(frame1.ysc.2.3, command=paste(.Tk.ID(frame1.lst.2.1), "yview"))
  
  frame1.lst.2.4 <- tklistbox(frame1, selectmode="browse", activestyle="none",
                              relief="flat", borderwidth=5, width=30, height=8,
                              exportselection=FALSE, listvariable=dataset.var,
                              highlightthickness=0)
  frame1.ysc.2.5 <- ttkscrollbar(frame1, orient="vertical")
  tkconfigure(frame1.lst.2.4, background="white",
              yscrollcommand=paste(.Tk.ID(frame1.ysc.2.5), "set"))
  tkconfigure(frame1.ysc.2.5, command=paste(.Tk.ID(frame1.lst.2.4), "yview"))
  
  frame1.box.3.1 <- ttkcombobox(frame1, state="readonly", value=pkg.type.vals)
  frame1.box.3.4 <- ttkcombobox(frame1, state="readonly", value=ds.class.vals)
  
  frame1.but.4.1 <- ttkbutton(frame1, width=10, text="Describe",
                              command=DescribePackage)
  frame1.but.4.2 <- ttkbutton(frame1, width=10, text="Load",
                              command=LoadPackage)
  frame1.but.4.4 <- ttkbutton(frame1, width=10, text="Describe",
                              command=DescribeDataset)
  
  tkgrid(frame1.lab.1.1, "x", "x", frame1.lab.1.4, "x", pady=c(10, 0))
  tkgrid(frame1.lst.2.1, "x", frame1.ysc.2.3, frame1.lst.2.4, frame1.ysc.2.5)
  tkgrid(frame1.box.3.1, "x", "x", frame1.box.3.4, "x", pady=c(4, 4))
  tkgrid(frame1.but.4.1, frame1.but.4.2, "x", frame1.but.4.4, "x")
  
  tkgrid.configure(frame1.lab.1.1, columnspan=3)
  tkgrid.configure(frame1.lab.1.4, columnspan=2)
  tkgrid.configure(frame1.lst.2.1, columnspan=2)
  tkgrid.configure(frame1.box.3.1, columnspan=2)
  
  tkgrid.configure(frame1.lab.1.1, frame1.lab.1.4, sticky="w")
  tkgrid.configure(frame1.lst.2.1, frame1.lst.2.4, sticky="nswe")
  tkgrid.configure(frame1.ysc.2.3, frame1.ysc.2.5, sticky="ns")
  tkgrid.configure(frame1.box.3.1, frame1.box.3.4, sticky="we")
  tkgrid.configure(frame1.but.4.2, frame1.but.4.4, sticky="w")
  
  tkgrid.configure(frame1.ysc.2.3, padx=c(0, 25))
  tkgrid.configure(frame1.but.4.1, padx=c(0, 4))
  
  tkgrid.columnconfigure(frame1, 1, minsize=85, weight=1)
  tkgrid.columnconfigure(frame1, 3, minsize=85, weight=1)
  tkgrid.rowconfigure(frame1, 1, weight=1)

  tkpack(frame1, fill="both", expand=TRUE, anchor="nw", padx=10)
  
  tkselection.set(frame1.lst.2.1, 0)
  tcl(frame1.box.3.1, "current", 1)
  tcl(frame1.box.3.4, "current", 0)

  # Bind events

  tclServiceMode(TRUE)
  
  tkbind(frame1.lst.2.1, "<<ListboxSelect>>", SelectPackage)
  
  tkbind(frame1.box.3.1, "<<ComboboxSelected>>", SelectPackageType)
  tkbind(frame1.box.3.4, "<<ComboboxSelected>>", SelectPackage)

  tkbind(tt, "<Destroy>", function() tclvalue(tt.done.var) <- 1)

  # GUI control

  SelectPackageType()

  tkgrab(tt)
  tkfocus(tt)
  tkwait.variable(tt.done.var)

  tclServiceMode(FALSE)
  tkgrab.release(tt)
  tkdestroy(tt)
  tclServiceMode(TRUE)

  invisible(rtn)
}
