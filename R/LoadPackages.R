LoadPackages <- function(repo="http://cran.r-project.org") {
  # This function loads R packages required by RSurvey. If a required
  # package is unavailable on the local computer an attempt is made to
  # acquire the package from CRAN using an existing network connection.

  require.pkgs <- c("tcltk", "sp", "gpclib", "rgl", "MBA", "tripack",
                    "colorspace")
  suggest.pkgs <- c("rgdal")

  pkgs <- c(require.pkgs, suggest.pkgs)

  available.pkgs <- .packages(all.available=TRUE)
  is.missing <- !pkgs %in% available.pkgs

  is.tcl <- "tcltk" %in% available.pkgs
  if (is.tcl)
    suppressPackageStartupMessages(require("tcltk"))

  if (any(is.missing)) {
    contriburl <- contrib.url(repos=repo, type=getOption("pkgType"))
    missing.pkgs <- pkgs[is.missing]

    msg <- paste("The following packages used by RSurvey are missing:",
                 paste(" ", paste(missing.pkgs, collapse=", ")),
                 "Without these packages, some features will not be available.",
                 "Install these packages from CRAN?", sep="\n")

    if (is.tcl)
      ans <- as.character(tkmessageBox(icon="error", message=msg,
                                       title="Missing Packages", type="yesno"))
    else
      ans <- readline(paste(msg, " (yes/no)  "))
    is.install <- tolower(substr(ans, 1, 1)) == "y"

    if (is.install) {
      cran.pkgs <- available.packages(contriburl)
      if (!all(missing.pkgs %in% cran.pkgs))
        repo <- NULL
      install.packages(missing.pkgs, repos=repo)
    }
  }

  for (pkg in pkgs) {
    is.pkg <- suppressPackageStartupMessages(require(pkg, character.only=TRUE))
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
