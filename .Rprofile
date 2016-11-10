
d <- read.dcf(file=file.path(getwd(), "DESCRIPTION"),
              fields = c("Imports", "Suggests"), all=TRUE)
s <- paste(d$Imports, d$Suggest, sep=",")
pkgs <- strsplit(gsub("[\r\n]", "", s), ",")[[1]]
options(defaultPackages=pkgs)


.First <- function() {
  RSurvey::RestoreSession(file.path(getwd(), "R"))
  tcltk::tcl("package", "require", "Tktable")
}
