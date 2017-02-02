
if (interactive()) {
  d <- read.dcf(file=file.path(getwd(), "DESCRIPTION"), fields=c("Imports", "Suggests"), all=TRUE)
  s <- paste(d$Imports, d$Suggest, sep=",")
  pkgs <- strsplit(gsub("[\r\n]", "", s), ",")[[1]]
  options(defaultPackages=pkgs)
}

.First <- function() {
  if (interactive()) {
    source(file.path(getwd(), "R", "RestoreSession.R"), .GlobalEnv)
    RestoreSession(file.path(getwd(), "R"))
    tcltk::tcl("package", "require", "Tktable")
  }
}
