LoadPackages <- function() {
  # This function loads R packages required by RSurvey. If a required
  # package is unavailable on the local computer an attempt is made to
  # acquire the package from CRAN using an existing network connection.

  require.pkgs <- c("tcltk", "sp", "gpclib", "rgl", "MBA", "tripack")

  suggest.pkgs <- c("rgdal")

  # Packages that may be useful for future work:
  # "udunits", "mgcv", "RColorBrewer", "classInt", "maptools"

  pkgs <- append(require.pkgs, suggest.pkgs)
  install.pkgs <- pkgs[!(pkgs %in% .packages(all.available=TRUE))]

  if (length(install.pkgs) > 0)
    install.packages(install.pkgs)

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
