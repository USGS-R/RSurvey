LoadPackages <- function(repo="http://cran.r-project.org") {
  # This function loads R packages required by RSurvey. If a required
  # package is unavailable on the local computer an attempt is made to
  # acquire the package from CRAN using an existing network connection.

  require.pkgs <- c("tcltk", "sp", "rgl", "MBA", "colorspace", "rgeos")
  suggest.pkgs <- c("rgdal", "dichromat", "tripack")

  pkgs <- c(require.pkgs, suggest.pkgs)

  available.pkgs <- .packages(all.available=TRUE)
  is.missing <- !pkgs %in% available.pkgs

  is.tcl <- "tcltk" %in% available.pkgs
  if (is.tcl)
    require("tcltk")

  if (any(is.missing)) {
    missing.pkgs <- pkgs[is.missing]
    msg <- paste("The following package(s) used by RSurvey are missing:",
                 paste(" ", paste(missing.pkgs, collapse=", ")),
                 "Some features will not be available without these packages.",
                 "Install these packages from CRAN?", sep="\n")
    if (is.tcl)
      ans <- as.character(tkmessageBox(icon="question", message=msg,
                                       title="Missing Packages", type="yesno"))
    else
      ans <- readline(paste(msg, " (yes/no)  "))
    is.install <- tolower(substr(ans, 1, 1)) == "y"

    if (is.install) {
      contriburl <- contrib.url(repos=repo, type=getOption("pkgType"))
      cran.pkgs <- available.packages(contriburl)
      if (!all(missing.pkgs %in% cran.pkgs))
        repo <- NULL
      install.packages(missing.pkgs, repos=repo)
    }
  }

  for (pkg in pkgs) {
    is.pkg <- require(pkg, character.only=TRUE)
    if (!is.pkg && pkg %in% require.pkgs)
      stop(paste("package", pkg, "is required"))
  }

  # Additional Tcl/Tk packages

  tcl.pkg <- tryCatch(tcl("package", "require", "Tktable"), error=identity)
  if (inherits(tcl.pkg, "error")) {
    msg <- paste("Tcl package Tktable is missing and is strongly recommended",
                 "for full functionality of RSurvey.\n ",
                 "http://tktable.sourceforge.net")
    if (is.tcl)
      tkmessageBox(icon="warning", message=msg, title="Missing Tktable", type="ok")
    else
      warning(msg, domain=NA)
  }
}
