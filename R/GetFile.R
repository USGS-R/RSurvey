GetFile <- function(cmd="Open", file=NULL, exts=NULL, initialdir=NULL,
                    initialfile=NULL, defaultextension=NULL, win.title=cmd,
                    multi=FALSE, parent=NULL) {
  # A GUI for selecting files to open or save.

  # Additional functions (subroutines)

  # Determine file extension

  FileExt <- function(x) {
      brk <- unlist(strsplit(basename(x), "\\."))
      ext <- ""
      if (length(brk) != 1)
        ext <- tolower(tail(brk, 1))
  }


  # Main program

  all.filters <- list(bmp  = "Windows Bitmap Files",
                      csv  = "Text Files",
                      dat  = "Text Files",
                      eps  = "Encapsulated Postscript Files",
                      grd  = "Interpolated Grid Text Files",
                      gz   = "Compressed Text Files",
                      pdf  = "PDF Files",
                      ply  = "Polygon Text Files",
                      png  = "Png Files",
                      jpg  = "Jpeg Files",
                      jpeg = "Jpeg Files",
                      ps   = "Postscript Files",
                      r    = "R Source Files",
                      rda  = "RSurvey Project Files",
                      shp  = "ESRI Shapefiles",
                      tif  = "TIFF Files",
                      tiff = "TIFF Files",
                      txt  = "Text Files"
                  )

  if (!is.null(file)) {
    if (inherits(file, "connection"))
      file <- summary.connection(file)$description
    dir <- dirname(file)
    ext <- FileExt(file)
    nam <- sub(paste(".", ext, "$", sep=""), "", basename(file))
    typ <- all.filters[[ext]]
    f <- list(path=file, dir=dir, name=nam, ext=ext, type=typ)
    Data("default.dir", dir)
    return(f)
  }

  if (is.null(initialdir))
    initialdir <- Data("default.dir")

  filters <- matrix(nrow=0, ncol=2)

  if (!is.null(exts)) {
    for (ext in tolower(exts)) {
      typ <- all.filters[[ext]]
      if (is.null(typ))
        typ <- toupper(ext)
      filters <- rbind(filters, c(typ, paste(".", ext, sep="")))
    }
  }

  filters   <- rbind(filters, c("All files", "*"))
  filters[] <- paste("{", filters, "}", sep="")
  filters   <- apply(filters, 1, paste, collapse=" ")
  filters   <- paste(paste("{", filters, "}", sep=""), collapse=" ")

  if (tolower(substr(cmd, 1, 4)) == "open") {
    args <- list("tk_getOpenFile", title=win.title, multiple=multi)
  } else {
    args <- list("tk_getSaveFile", title=win.title)
  }

  if (!is.null(parent))
    args[["parent"]] <- parent

  if (!is.null(defaultextension))
    args <- c(args, defaultextension=defaultextension)
  if (!is.null(initialdir))
    args <- c(args, initialdir=initialdir)
  if (!is.null(initialfile))
    args <- c(args, initialfile=initialfile)

  args <- c(args, filetypes=filters)

  res <- tclvalue(do.call(tcl, args))

  if (!nzchar(res))
    return()

  if (multi) {
    ans <- character()
    pat <- "([^{])*\\{([^}]*)\\}(.*)"
    while (grepl(pat, res)) {
      ans <- c(ans, sub(pat, "\\2", res))
      res <- sub(pat, "\\1\\3", res)
    }
    ans <- c(ans, strsplit(res, " ", fixed = TRUE)[[1]])
    ans <- ans[nzchar(ans)]
  } else {
    ans <- res
  }

  n <- length(ans)
  if (n > 1)
    f <- list()
  for (i in seq(along=ans)) {
    pth <- ans[i]
    dir <- dirname(pth)
    ext <- FileExt(pth)
    nam <- sub(paste(".", ext, "$", sep=""), "", basename(pth))
    typ <- all.filters[[ext]]
    val <- list(path=pth, dir=dir, name=nam, ext=ext, type=typ)
    if (n > 1) {
      f[[i]] <- val
    } else {
      f <- val
    }
  }

  if (!is.null(f))
    Data("default.dir", dir)

  f
}
