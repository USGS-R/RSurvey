l <- readLines(file.path(getwd(), "DESCRIPTION"))

depends <- sub("^Depends: ", "", l[substr(l, 1L, 7L) == "Depends"])
depends <- strsplit(depends, ",")[[1]][-1]

imports <- sub("^Imports: ", "", l[substr(l, 1L, 7L) == "Imports"])
imports <- if (length(imports) > 0) strsplit(imports, ",")[[1]] else NULL

suggests <- sub("^Suggests: ", "", l[substr(l, 1L, 8L) == "Suggests"])
suggests <- if (length(suggests) > 0) strsplit(suggests, ",")[[1]] else NULL

packages <- sub("^\\s+", "", c(depends, imports, suggests))

options(defaultPackages=c(getOption("defaultPackages"), packages))

.First <- function() {
  RSurvey::RestoreSession(file.path(getwd(), "R"))
  tcltk::tcl("package", "require", "Tktable")
}
