LoadPackages <- function(repo="http://cran.r-project.org") {
  # This function loads R packages required by RSurvey. If a required
  # package is unavailable on the local computer an attempt is made to
  # acquire the package from CRAN using an existing network connection.

  require.pkgs <- c("tcltk", "sp", "gpclib", "rgl", "MBA", "tripack")
  suggest.pkgs <- c("rgdal")

  # Packages that may be useful for future work:
  # "udunits", "mgcv", "RColorBrewer", "classInt", "maptools"

  available.pkgs <- .packages(all.available=TRUE)

  if (any(!suggest.pkgs %in% available.pkgs)) {
    contriburl <- contrib.url(repos=repo, type=getOption("pkgType"))
    cran.pkgs <- available.packages(contriburl)
    is.available <- suggest.pkgs %in% available.pkgs |
                    suggest.pkgs %in% cran.pkgs
    suggest.pkgs <- suggest.pkgs[is.available]
  }

  pkgs <- c(require.pkgs, suggest.pkgs)

  install.pkgs <- pkgs[!pkgs %in% available.pkgs]
  if (length(install.pkgs) > 0)
    install.packages(install.pkgs, repos=repo)

  for (pkg in pkgs) {
    is.pkg <- suppressPackageStartupMessages(require(pkg, character.only=TRUE))
    if (!is.pkg && pkg %in% require.pkgs)
      stop(paste("package", pkg, "is required"))
  }

  # Additional Tcl/Tk packages

  tcl.pkg <- tryCatch(tcl("package", "require", "Tktable"), error=identity)
  if (inherits(tcl.pkg, "error")) {
    txt <- paste("Tcl package Tktable not found and is strongly recommended",
                 "for full functionality (http://tktable.sourceforge.net/).")
    warning(txt, domain=NA)
  }
}
