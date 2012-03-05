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

  pkg <- basename(getwd())
  description <- readLines("DESCRIPTION")
  ver <- strsplit(grep("Version:", description, value=TRUE), " ")[[1]][2]

  path.pkg <- shQuote(getwd())
  path.tmp <- shQuote(file.path("C:", pkg))
  path.git <- shQuote(file.path("C:", pkg, ".git"))
  path.tar <- shQuote(paste("C:/", pkg, "_", ver, ".tar.gz", sep=""))
  path.chk <- shQuote(paste("C:/", pkg, ".Rcheck", sep=""))
  path.cmd <- paste(R.home(component="bin"), "/Rcmd", sep="")
  file.zip <- shQuote(paste(pkg, "_*", sep=""))

  cmd <- NULL
  cmd <- append(cmd, paste("RM -f ", getwd(), "/", pkg, "*", sep=""))
  cmd <- append(cmd, paste(path.cmd, "REMOVE", pkg, sep=" "))
  cmd <- append(cmd, paste("CP -r", path.pkg, shQuote("C:/"), sep=" "))
  cmd <- append(cmd, paste("RMDIR /S /Q", path.chk, sep=" "))
  cmd <- append(cmd, paste("RMDIR /S /Q", path.git, sep=" "))
  cmd <- append(cmd, paste(path.cmd, "build", path.tmp, "--resave-data",
                           sep=" "))
  cmd <- append(cmd, paste(path.cmd, "check", path.tar, sep=" "))
  cmd <- append(cmd, paste(path.cmd, "INSTALL --build", path.tmp, sep=" "))
  cmd <- append(cmd, paste("RMDIR /S /Q", path.tmp, sep=" "))
  cmd <- append(cmd, paste("MOVE /Y", file.zip, path.pkg, sep=" "))

  cmd <- paste(Sys.getenv("COMSPEC"), "/c", cmd, sep=" ")

  f <- tcl("tk_getSaveFile", defaultextension=".bat",
           title="Save Batch file As", initialfile=paste(pkg, ".bat", sep=""),
           initialdir=file.path(getwd(), ".."))
  f <- as.character(f)

  if (length(f) == 0)
    return()

  cat(c("CD /d C:/", cmd, "pause"), file=f, sep="\n")
}
