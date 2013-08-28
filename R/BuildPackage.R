# Writes a series of commands for creating an R package to a Windows batch file.
# Software requirements for building R packages in Windows XP Pro:
#   Download and install the Rtools installer (choose to update Search Path)
#     http://www.murdoch-sutherland.com/Rtools/installer.html
#   Download and install the MiKTeX installation program
#     http://www.miktex.org/setup.html

BuildPackage <- function() {
  if (.Platform$OS.type != "windows")
    stop(call.=FALSE, "This function requires a Windows platform.")

  pkg <- basename(getwd())
  description <- readLines("DESCRIPTION")
  ver <- strsplit(grep("Version:", description, value=TRUE), " ")[[1]][2]

  path.pkg <- shQuote(getwd())
  path.tmp <- shQuote(file.path("C:", pkg))
  path.git <- shQuote(file.path("C:", pkg, ".git"))
  path.tar <- shQuote(paste0("C:/", pkg, "_", ver, ".tar.gz"))
  path.chk <- shQuote(paste0("C:/", pkg, ".Rcheck"))
  path.cmd <- paste0(R.home(component="bin"), "/Rcmd")
  file.zip <- shQuote(paste0(pkg, "_*"))

  cmd <- NULL
  cmd <- append(cmd, paste0("RM -f ", getwd(), "/", pkg, "*"))
  cmd <- append(cmd, paste(path.cmd, "REMOVE", pkg))
  cmd <- append(cmd, paste("CP -r", path.pkg, shQuote("C:/")))
  cmd <- append(cmd, paste("RMDIR /S /Q", path.chk))
  cmd <- append(cmd, paste("RMDIR /S /Q", path.git))
  cmd <- append(cmd, paste(path.cmd, "build", path.tmp, "--resave-data"))
  cmd <- append(cmd, paste(path.cmd, "check", path.tar))
  cmd <- append(cmd, paste(path.cmd, "INSTALL --build", path.tmp))
  cmd <- append(cmd, paste("RMDIR /S /Q", path.tmp))
  cmd <- append(cmd, paste("MOVE /Y", file.zip, path.pkg))

  cmd <- paste(Sys.getenv("COMSPEC"), "/c", cmd)

  f <- tcl("tk_getSaveFile", defaultextension=".bat",
           title="Save Batch file As", initialfile=paste0(pkg, ".bat"),
           initialdir=file.path(getwd(), ".."))
  f <- as.character(f)

  if (length(f) == 0)
    return()

  cat(c("CD /d C:/", cmd, "pause"), file=f, sep="\n")
}
