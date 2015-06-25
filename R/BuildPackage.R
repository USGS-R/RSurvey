# Writes a series of commands for creating an R package to a Windows batch file.
# Software requirements for building R packages in a Windows OS include:
#   Rtools;      http://cran.r-project.org/bin/windows/Rtools/
#   MiKTeX;      http://www.miktex.org/download
#   QPDF;        http://qpdf.sourceforge.net/
#   Ghostscript; http://www.ghostscript.com/
# Place QPDF and Ghostscript in the 'Path' environmental variable.

BuildPackage <- function(check.cran=FALSE, no.vignettes=FALSE) {
  if (.Platform$OS.type != "windows")
    stop(call.=FALSE, "This function requires a Windows platform.")

  pkg <- basename(getwd())
  file.name <- pkg

  build.option <- "--resave-data"
  check.option <- ""
  if (check.cran) {
    file.name <- paste0(file.name, "-cran")
    check.option <- paste(check.option, "--as-cran")
  }
  if (no.vignettes) {
    file.name <-  paste0(file.name, "-no_vignettes")
    build.option <- paste(build.option, "--no-build-vignettes")
    check.option <- paste(check.option, "--no-build-vignettes")
  }
  if (!check.cran && !no.vignettes)
    check.option <- paste(check.option, "--no-build-vignettes")

  description <- readLines("DESCRIPTION")
  ver <- strsplit(grep("Version:", description, value=TRUE), " ")[[1]][2]

  path.pkg <- shQuote(getwd())

  temp.dir <- normalizePath(Sys.getenv("TEMP"), winslash = "/")
  path.tmp <- shQuote(file.path(temp.dir, pkg))
  path.git <- shQuote(file.path(temp.dir, pkg, ".git"))
  path.tar <- shQuote(file.path(temp.dir, paste0(pkg, "_", ver, ".tar.gz")))
  path.chk <- shQuote(file.path(temp.dir, paste0(pkg, ".Rcheck")))
  path.cmd <- paste0(R.home(component="bin"), "/R CMD")
  file.zip <- shQuote(paste0(pkg, "_*"))

  cs <- paste(Sys.getenv("COMSPEC"), "/c")

  cmd <- paste("CD /d", path.pkg)
  cmd <- append(cmd, paste0(cs, " RM -f ", pkg, "*"))
  cmd <- append(cmd, paste("CD /d", shQuote(temp.dir)))
  cmd <- append(cmd, paste(cs, path.cmd, "REMOVE", pkg))
  cmd <- append(cmd, paste(cs, "RMDIR /S /Q", path.tmp))
  cmd <- append(cmd, paste(cs, "CP -r", path.pkg, shQuote(temp.dir)))
  cmd <- append(cmd, paste(cs, "RMDIR /S /Q", path.chk))
  cmd <- append(cmd, paste(cs, "RMDIR /S /Q", path.git))
  cmd <- append(cmd, paste(cs, path.cmd, "build", build.option, path.tmp))
  cmd <- append(cmd, paste(cs, path.cmd, "check", check.option, path.tar))
  cmd <- append(cmd, paste(cs, path.cmd, "INSTALL --build", path.tar))
  cmd <- append(cmd, paste(cs, "RMDIR /S /Q", path.tmp))
  cmd <- append(cmd, paste(cs, "MOVE /Y", file.zip, path.pkg))
  cmd <- append(cmd, "pause")

  f <- tcl("tk_getSaveFile", defaultextension=".bat",
           title="Save Batch file As", initialfile=paste0(file.name, ".bat"),
           initialdir=file.path(getwd(), ".."))
  f <- as.character(f)
  if (length(f) == 0)
    return()

  cat(cmd, file=f, sep="\n")
}
