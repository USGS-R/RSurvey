l <- readLines(file.path(getwd(), "DESCRIPTION"))

id <- "Depends"
txt <- sub(paste0("^", id, ": "), "", l[substr(l, 1L, nchar(id)) == id])
pkgs <- strsplit(txt, ",")[[1]][-1]

id <- "Imports"
txt <- sub(paste0("^", id, ": "), "", l[substr(l, 1L, nchar(id)) == id])
pkgs <- c(pkgs, if (length(txt) > 0) strsplit(txt, ",")[[1]] else NULL)

id <- "Suggests"
txt <- sub(paste0("^", id, ": "), "", l[substr(l, 1L, nchar(id)) == id])
pkgs <- c(pkgs, if (length(txt) > 0) strsplit(txt, ",")[[1]] else NULL)

packages <- sub("^\\s+", "", pkgs)

options(defaultPackages=c(getOption("defaultPackages"), packages))

.First <- function() {
  RSurvey::RestoreSession(file.path(getwd(), "R"))
  tcltk::tcl("package", "require", "Tktable")
}
