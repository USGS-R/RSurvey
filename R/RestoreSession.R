RestoreSession <- function(path, save.objs, fun.call) {
  # This function restores local objects within the current R session.

  if (missing(path)) {
    if (exists("Data")) {
      require(tcltk)
      path <- tclvalue(tkchooseDirectory(initialdir=Data("default.dir"),
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

  tmp <- ls(all.names=FALSE, envir=as.environment(1))
  cur.objs <- tmp[!tmp %in% save.objs]

  graphics.off()

  rm(list=cur.objs, envir=as.environment(1))

  r.files <- list.files(path, pattern="[.][R]$", full.names=TRUE,
                        recursive=TRUE, ignore.case=TRUE)

  err.msg <- "\n"

  cat("\n")
  for (i in r.files) {
    tmp <- tail(unlist(strsplit(i, "/")), 1)
    obj <- substr(tmp, 1, nchar(tmp) - 2)

    if (!obj %in% save.objs) {
      ans <- try(source(i), silent=TRUE)
      if (inherits(ans, "try-error")) {
        err.msg <- paste(err.msg, i, "\n", ans, sep="")
      } else {
        cat(i, "\n")
      }
    }
  }

  cat(err.msg)

  if (!missing(fun.call))
    eval(parse(text=fun.call))()
}
