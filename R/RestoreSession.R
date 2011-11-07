RestoreSession <- function(path, save.objs, fun.call) {
  # This function restores local objects within the current R session.

  if (missing(path)) {
    if (exists("Data")) {
      require(tcltk)

      initial.dir <- getwd()
      if("R" %in% dir(path=initial.dir, full.names=FALSE))
        initial.dir <- file.path(initial.dir, "R")

      path <- tclvalue(tkchooseDirectory(initialdir=initial.dir,
                                         title="Choose R Source Directory..."))
      if (path == "")
        return()
      Data("default.dir", path)
    } else {
      path <- file.path(getwd(), "R")
      if (!file.exists(path))
        return()
    }
  }

  if (missing(save.objs))
    save.objs <- NULL

  all.objs <- ls(all.names=FALSE, envir=as.environment(1))
  cur.objs <- all.objs[!all.objs %in% save.objs]

  graphics.off()

  rm(list=cur.objs, envir=as.environment(1))

  r.files <- list.files(path, pattern="[.][R]$", full.names=TRUE,
                        recursive=TRUE, ignore.case=TRUE)

  err.msg <- "\n"

  cat("\n")
  for (i in r.files) {
    file.name <- tail(unlist(strsplit(i, "/")), 1)
    obj <- substr(file.name, 1, nchar(file.name) - 2)

    if (!obj %in% save.objs) {
      ans <- try(source(i), silent=TRUE)
      if (inherits(ans, "try-error"))
        err.msg <- paste(err.msg, i, "\n", ans, sep="")
      else
        cat(i, "\n")
    }
  }

  cat(err.msg)

  if (!missing(fun.call))
    eval(parse(text=fun.call))()
}
