BuildPackage <- function() {
  # Writes a series of commands for creating an R package to
  # a Windows batch file.

  # Software requirements for building R packages in Windows XP Pro:
  #   Download and install the Rtools installer (choose to update Search Path)
  #     http://www.murdoch-sutherland.com/Rtools/installer.html
  #   Download and install the MiKTeX installation program
  #     http://www.miktex.org/setup.html

  require(tcltk)

  if (.Platform$OS.type != "windows")
    stop(call.=FALSE, "This function requires a Windows platform.")

  pkg <- rev(unlist(strsplit(getwd(), "/")))[1]

  path.pkg <- shQuote(getwd())
  path.tmp <- shQuote(paste("C:/", pkg, sep=""))
  path.chk <- shQuote(paste("C:/", pkg, "/", pkg, ".Rcheck", sep=""))
  path.zip <- shQuote(paste(pkg, "_*", sep=""))
  path.cmd <- paste(R.home(component="bin"), "/Rcmd", sep="")

  cmd <- NULL
  cmd <- append(cmd, paste("RM -f ", getwd(), "/", pkg, "*", sep=""))
  cmd <- append(cmd, paste(path.cmd, "REMOVE", pkg, sep=" "))
  cmd <- append(cmd, paste("CP -r", path.pkg, shQuote("C:/"), sep=" "))
  cmd <- append(cmd, paste("RMDIR /S /Q", path.chk, sep=" "))
  cmd <- append(cmd, paste(path.cmd, "build", path.tmp, sep=" "))
  cmd <- append(cmd, paste(path.cmd, "INSTALL --build", path.tmp, sep=" "))
  cmd <- append(cmd, paste(path.cmd, "check", path.tmp, sep=" "))
  cmd <- append(cmd, paste("RMDIR /S /Q", path.tmp, sep=" "))
  cmd <- append(cmd, paste("MOVE /Y", path.zip, path.pkg, sep=" "))

  cmd <- paste(Sys.getenv("COMSPEC"), "/c", cmd, sep=" ")

  f <- tcl("tk_getSaveFile", defaultextension=".bat",
           title="Save Batch file As", initialfile=paste(pkg, ".bat", sep=""),
           initialdir=paste(getwd(), "/..", sep=""))
  f <- as.character(f)

  if (length(f) == 0)
    return()

  cat(c(cmd, "pause"), file=f, sep="\n")
}
