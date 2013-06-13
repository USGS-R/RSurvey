# This function loads R packages required by RSurvey. If a required
# package is unavailable on the local computer an attempt is made to
# acquire the package from CRAN using an existing network connection.

LoadPackages <- function() {

  require.pkgs <- c("tcltk", "sp", "rgl", "rgeos", "MBA")
  suggest.pkgs <- c("rgdal", "tripack", "colorspace", "dichromat")

  pkgs <- c(require.pkgs, suggest.pkgs)

  available.pkgs <- .packages(all.available=TRUE)
  is.missing <- !pkgs %in% available.pkgs

  is.tcl <- "tcltk" %in% available.pkgs
  if (is.tcl)
    require("tcltk")

  if (any(is.missing)) {
    missing.pkgs <- pkgs[is.missing]
    missing.pkgs.str <- paste("  ", paste(paste0("\'", missing.pkgs, "\'"),
                                          collapse=", "))
    msg <- paste("The following package(s) used by RSurvey are missing:",
                 "", missing.pkgs.str, "",
                 "Some features will not be available without these packages.",
                 "Install these packages from CRAN?", sep="\n")

    if (is.tcl) {
      is.install <- FALSE
      repo <- NULL
      InstallPackages <- function() {
        is.install <<- TRUE
        idx <- which(cran.mirrors$Name %in% as.character(tclvalue(repo.var)))
        repo <<- cran.mirrors$URL[idx]
        tclvalue(tt.done.var) <- 1
      }

      repo.var <- tclVar()
      tt.done.var <- tclVar(0)

      cran.mirrors <- getCRANmirrors(all=FALSE, local.only=FALSE)
      default.repo <- getOption("repos")
      idx <- which(sub("/$", "", cran.mirrors$URL) %in%
                   sub("/$", "", default.repo["CRAN"]))
      if (length(idx) > 0)
        tclvalue(repo.var) <- cran.mirrors$Name[idx[1]]
      else
        tclvalue(repo.var) <- cran.mirrors$Name[1]

      tclServiceMode(FALSE)

      tt <- tktoplevel()
      tktitle(tt) <- "Missing Packages"
      tkwm.resizable(tt, 0, 0)

      frame0 <- tkframe(tt, relief="flat")
      frame0.but.2 <- ttkbutton(frame0, width=12, text="Yes", default="active",
                                command=InstallPackages)
      frame0.but.3 <- ttkbutton(frame0, width=12, text="No",
                                command=function() tclvalue(tt.done.var) <- 1)
      tkgrid("x", frame0.but.2, frame0.but.3, sticky="se", pady=10)
      tkgrid.columnconfigure(frame0, 0, weight=1)
      tkgrid.configure(frame0.but.2, padx=c(10, 2))
      tkgrid.configure(frame0.but.3, padx=c(2, 10))
      tkpack(frame0, fill="x", side="bottom", anchor="e")

      frame1 <- tkframe(tt, relief="flat", background="white")
      frame1.lab.1.1 <- ttklabel(frame1, text=msg, justify="left",
                                 background="white")
      frame1.lab.2.1 <- ttklabel(frame1, text="Set CRAN mirror",
                                 justify="left", background="white")
      frame1.box.2.2 <- ttkcombobox(frame1, state="readonly",
                                    textvariable=repo.var,
                                    values=cran.mirrors$Name)
      tkgrid(frame1.lab.1.1, "x", pady=c(30, 20))
      tkgrid(frame1.lab.2.1, frame1.box.2.2, pady=c(0, 30))
      tkgrid.configure(frame1.lab.1.1, columnspan=2, padx=40)
      tkgrid.configure(frame1.lab.2.1, padx=c(40, 4), sticky="e")
      tkgrid.configure(frame1.box.2.2, padx=c(0, 40), sticky="w")
      tkpack(frame1)

      tclServiceMode(TRUE)

      tkbind(tt, "<Return>", InstallPackages)
      tkbind(tt, "<Key-space>", InstallPackages)

      tkfocus(tt)
      tkgrab(tt)
      tkwait.variable(tt.done.var)

      tclServiceMode(FALSE)
      tkgrab.release(tt)
      tkdestroy(tt)
      tclServiceMode(TRUE)

    } else {
      ans <- readline(paste(msg, " (yes/no)  "))
      is.install <- tolower(substr(ans, 1, 1)) == "y"
      if (is.install) {
        chooseCRANmirror(graphics=FALSE)
        repo <- getOption("repos")
      }
    }

    if (is.install) {
      contriburl <- contrib.url(repos=repo, type=getOption("pkgType"))
      cran.pkgs <- available.packages(contriburl)
      if (!all(missing.pkgs %in% cran.pkgs))
        stop("missing package(s) not available at CRAN mirror")
      install.packages(missing.pkgs, repos=repo, quiet=TRUE)
    }
  }

  for (pkg in pkgs) {
    is.suggested <- pkg %in% suggest.pkgs
    is.pkg <- suppressWarnings(require(pkg, character.only=TRUE,
                                       warn.conflicts=!is.suggested,
                                       quietly=is.suggested))
    if (!is.pkg && !is.suggested)
      stop("package required")
  }

  # Additional Tcl/Tk packages
  tcl.pkg <- tryCatch(tcl("package", "require", "Tktable"), error=identity)
  if (inherits(tcl.pkg, "error")) {
    msg <- paste("Tcl package Tktable is missing and is strongly recommended",
                 "for full functionality of RSurvey.\n ",
                 "http://tktable.sourceforge.net")
    if (is.tcl)
      tkmessageBox(icon="warning", message=msg, title="Missing Tktable",
                   type="ok")
    else
      warning(msg, domain=NA)
  }
}
