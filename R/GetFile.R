# A GUI for selecting files to open or save.

GetFile <- function(cmd="Open", file=NULL, exts=NULL, initialdir=NULL,
                    initialfile=NULL, defaultextension=NULL, win.title=cmd,
                    multi=FALSE, parent=NULL) {

  # Additional functions (subroutines)

  # Determine file extension
  FileExt <- function(x) {
    ext <- tolower(tail(unlist(strsplit(basename(x), "\\."))[-1], 1))
    if (length(ext) == 0L)
      ext <- ""
    ext
  }


  # Main program

  # Initialize file filters
  all.filters <- list(bmp  = "Windows Bitmap Files",
                      csv  = "Text Files",
                      eps  = "Encapsulated Postscript Files",
                      grd  = "Interpolated Grid Text Files",
                      gz   = "Compressed Text Files",
                      pdf  = "PDF Files",
                      ply  = "Polygon Text Files",
                      png  = "PNG Files",
                      jpg  = "Jpeg Files",
                      jpeg = "Jpeg Files",
                      ps   = "Postscript Files",
                      r    = "R Source Files",
                      rda  = "RSurvey Project Files",
                      shp  = "ESRI Shapefiles",
                      tab  = "Text Files",
                      tif  = "TIFF Files",
                      tiff = "TIFF Files",
                      txt  = "Text Files"
                  )

  # Process connection and return
  if (!is.null(file)) {
    if (inherits(file, "connection"))
      val <- summary.connection(file)$description
    else
      val <- file
    ext <- FileExt(val)
    attr(val, "directory") <- dirname(val)
    attr(val, "extension") <- ext
    attr(val, "name") <- sub(paste(".", ext, "$", sep=""), "", basename(val))
    attr(val, "type") <- all.filters[[ext]]
    Data("default.dir", attr(val, "directory"))
    return(val)
  }

  # Establish initial directory
  if (is.null(initialdir))
    initialdir <- Data("default.dir")

  # Build filters
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

  # Build arguments
  if (tolower(substr(cmd, 1, 4)) == "open")
    args <- list("tk_getOpenFile", title=win.title, multiple=multi)
  else
    args <- list("tk_getSaveFile", title=win.title)
  if (!is.null(parent))
    args[["parent"]] <- parent
  if (!is.null(defaultextension))
    args <- c(args, defaultextension=defaultextension)
  if (!is.null(initialdir))
    args <- c(args, initialdir=initialdir)
  if (!is.null(initialfile))
    args <- c(args, initialfile=initialfile)
  args <- c(args, filetypes=filters)

  # Open file dialog gui
  res <- tclvalue(do.call(tcl, args))
  if (!nzchar(res))
    return()

  # Account for mutiple files
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

  # Package results
  n <- length(ans)
  if (n > 1)
    f <- list()

  for (i in seq(along=ans)) {
    val <- ans[i]
    ext <- FileExt(val)
    attr(val, "directory") <- dirname(val)
    attr(val, "extension") <- ext
    attr(val, "name") <- sub(paste(".", ext, "$", sep=""), "", basename(val))
    attr(val, "type") <- all.filters[[ext]]
    if (n > 1)
      f[[i]] <- val
    else
      f <- val
  }

  # Set default directory
  if (!is.null(f))
    Data("default.dir", attr(val, "directory"))

  f
}
