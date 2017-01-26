ManagePackages <- function() {


  # install missing packages from cran mirror
  InstallPackages <- function() {
    tkconfigure(tt, cursor="watch")
    on.exit(tkconfigure(tt, cursor="arrow"))
    tclServiceMode(FALSE)
    on.exit(tclServiceMode(TRUE), add=TRUE)
    idx <- which(cran.mirrors$Name %in% as.character(tclvalue(repos.var)))
    repo <- cran.mirrors$URL[idx]
    contriburl <- contrib.url(repos=repo, type=getOption("pkgType"))
    cran.pkgs <- available.packages(contriburl)

    idxs <- as.integer(tkcurselection(f1.lst.2.2)) + 1L
    missing.pkgs <- missing.pkgs[idxs]

    is.on.cran <- missing.pkgs %in% cran.pkgs
    available.pkgs <- missing.pkgs[is.on.cran]
    unavailable.pkgs <- missing.pkgs[!is.on.cran]
    if (length(unavailable.pkgs) > 0) {
      msg <- paste0("The following packages are unavailable on this ",
                    "CRAN mirror:\n\n",
                    paste(paste0("\'", unavailable.pkgs, "\'"), collapse=", "),
                    "\n\nWould you like to try a different CRAN mirror?")
      ans <- tkmessageBox(icon="question", message=msg, title="CRAN", type="yesno", parent=tt)
      if (tolower(substr(ans, 1, 1)) == "y") return()
    }
    if (length(available.pkgs) > 0)
      install.packages(available.pkgs, repos=repo, verbose=TRUE)

    # load name spaces for suggested packages into current session
    for (pkg in available.pkgs) {
      is.loaded <- requireNamespace(pkg, quietly=TRUE)
      msg <- sprintf("unable to load the name space for suggested package: %s", pkg)
      if (!is.loaded) warning(msg, call.=FALSE)
    }

    tclvalue(tt.done.var) <- 1
  }


  # suggested packages
  txt <- readLines(system.file("DESCRIPTION", package="RSurvey"))
  pkgs <- sub(",$", "", strsplit(txt[grep("^Suggests:", txt)], " ")[[1]][-1])

  # account for missing packages
  is.pkg.missing <- !pkgs %in% .packages(all.available=TRUE)
  if (any(is.pkg.missing)) {
    missing.pkgs <- pkgs[is.pkg.missing]
    cran.mirrors <- getCRANmirrors(all=FALSE, local.only=FALSE)
    default.repo <- getOption("repos")["CRAN"]
    idx <- which(sub("/$", "", cran.mirrors$URL) %in% sub("/$", "", default.repo["CRAN"]))
    idx <- if (length(idx) > 0) idx[1] else 1
    repos.var <- tclVar(cran.mirrors$Name[idx])
    rlogo.var <- tclVar()
    tt.done.var <- tclVar(0)
    pkgs.var <- tclVar()
    for (i in seq_along(missing.pkgs)) tcl("lappend", pkgs.var, missing.pkgs[i])

    # open gui
    tclServiceMode(FALSE)
    tt <- tktoplevel()
    tktitle(tt) <- "Package Manager"
    tkwm.resizable(tt, 0, 0)

    # frame 0, ok and cancel buttons
    f0 <- tkframe(tt, relief="flat")
    f0.but.2 <- ttkbutton(f0, width=12, text="OK", default="active", command=InstallPackages)
    f0.but.3 <- ttkbutton(f0, width=12, text="Cancel",
                          command=function() tclvalue(tt.done.var) <- 1)
    tkgrid("x", f0.but.2, f0.but.3, sticky="se", pady=10)
    tkgrid.columnconfigure(f0, 0, weight=1)
    tkgrid.configure(f0.but.2, padx=c(10, 2))
    tkgrid.configure(f0.but.3, padx=c(2, 10))
    tkpack(f0, fill="x", side="bottom", anchor="e")

    # frame 1, message and mirror selection
    f1 <- tkframe(tt, relief="flat", background="white")
    if ("RSurvey" %in% .packages(all.available=TRUE))
      f <- system.file("images/rlogo.gif", package="RSurvey")
    else
      f <- file.path(getwd(), "inst", "images", "rlogo.gif")
    tkimage.create("photo", rlogo.var, format="GIF", file=f)

    f1.lab.1.1 <- ttklabel(f1, image=rlogo.var, background="white")

    txt <- "The following suggested package(s) have not been installed:"
    f1.lab.1.2 <- ttklabel(f1, text=txt, justify="left", background="white")

    f1.lst.2.2 <- tklistbox(f1, selectmode="extended", activestyle="none", relief="groove",
                            height=4, width=25, exportselection=FALSE, listvariable=pkgs.var,
                            highlightthickness=0)
    f1.ysc.2.3 <- ttkscrollbar(f1, orient="vertical")
    tkconfigure(f1.lst.2.2, background="white", yscrollcommand=paste(.Tk.ID(f1.ysc.2.3), "set"))
    tkconfigure(f1.ysc.2.3, command=paste(.Tk.ID(f1.lst.2.2), "yview"))
    tkselection.set(f1.lst.2.2, 0, length(missing.pkgs))

    txt <- paste("These packages are not necessarily needed but are highly recommended for full functionality.",
                 "You can deselect any of the packages listed and they will be excluded from installation.")
    txt <- paste(strwrap(txt, 80), collapse="\n")
    f1.lab.3.2 <- ttklabel(f1, text=txt, justify="left", background="white")

    txt <- paste("Packages will be downloaded from the Comprehensive R Archive Network (CRAN)",
                 "and automatically installed.")
    txt <- paste(strwrap(txt, 80), collapse="\n")
    f1.lab.4.2 <- ttklabel(f1, text=txt, justify="left", background="white")

    txt <- "Choose your preferred CRAN mirror"
    f1.lab.5.2 <- ttklabel(f1, text=txt, justify="left", background="white")
    f1.box.5.3 <- ttkcombobox(f1, textvariable=repos.var, width=25,
                              values=cran.mirrors$Name, state="readonly")
    tcl(f1.box.5.3, "current", 0)

    tkgrid(f1.lab.1.1, f1.lab.1.2, "x", pady=c(30, 10))
    tkgrid("x", f1.lst.2.2, f1.ysc.2.3, pady=c(0, 10))
    tkgrid("x", f1.lab.3.2, "x", pady=c(0, 10))
    tkgrid("x", f1.lab.4.2, "x", pady=c(0, 10))
    tkgrid("x", f1.lab.5.2, f1.box.5.3, pady=c(10, 30))

    tkgrid.configure(f1.lab.1.1, padx=c(20, 20), sticky="n", rowspan=4)
    tkgrid.configure(f1.lab.1.2, padx=c(0, 40), sticky="w", columnspan=2)
    tkgrid.configure(f1.lst.2.2, padx=c(0, 0), sticky="e")
    tkgrid.configure(f1.ysc.2.3, padx=c(0, 40), sticky="nsw")
    tkgrid.configure(f1.lab.3.2, padx=c(0, 40), sticky="w", columnspan=2)
    tkgrid.configure(f1.lab.4.2, padx=c(0, 40), sticky="w", columnspan=2)
    tkgrid.configure(f1.lab.5.2, padx=c(0, 4), sticky="e")
    tkgrid.configure(f1.box.5.3, padx=c(0, 40), sticky="w")

    tkpack(f1)

    # binds events
    tclServiceMode(TRUE)
    tkbind(tt, "<Return>", InstallPackages)
    tkbind(tt, "<Key-space>", InstallPackages)

    # gui control
    tkfocus(force=tt)
    tkgrab(tt)
    tkwait.variable(tt.done.var)
    tclServiceMode(FALSE)
    tkgrab.release(tt)
    tkdestroy(tt)
    tclServiceMode(TRUE)
  }

  # warn if tktable is unavailable
  tcl.pkg <- tryCatch(tcl("package", "require", "Tktable"), error=identity)
  if (inherits(tcl.pkg, "error")) {
    msg <- paste("Tcl package Tktable is missing and is strongly recommended",
                 "for full functionality of RSurvey.\n\n ",
                 "http://tktable.sourceforge.net")
    tkmessageBox(icon="warning", message=msg, title="Missing Tktable", type="ok")
  }
}
