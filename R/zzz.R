.onAttach <- function(lib, pkg) {
  if (interactive()) {
    ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
    msg <- "USGS Orphan Package:
            https://owi.usgs.gov/R/packages.html#orphan
            Deprecated - Development of this package has halted."
    packageStartupMessage(paste(strwrap(msg), collapse="\n"))
    LaunchGui()
  } else {
    packageStartupMessage("The RSurvey GUI is launched only in interactive sessions.")
  }
  raster::rasterOptions(standardnames=FALSE)
  invisible()
}

.onLoad <- function(...) {
  if (interactive()) ManagePackages()
  invisible()
}
