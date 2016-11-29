GetFile <- function(cmd="Open", file=NULL, exts=NULL, initialdir=NULL,
                    initialfile=NULL, defaultextension=NULL, win.title=cmd,
                    multi=FALSE, parent=NULL) {

  # initialize file filters
  all.filters <- list(bmp   = "Windows Bitmap",
                      bz2   = "Compressed Text",
                      csv   = "Text",
                      eps   = "Encapsulated Postscript",
                      gz    = "Compressed Text",
                      pdf   = "PDF",
                      ply   = "Polygon",
                      png   = "PNG",
                      jpg   = "Jpeg",
                      jpeg  = "Jpeg",
                      ps    = "Postscript",
                      Rda   = "R Data",
                      rda   = "R Data",
                      RData = "RSurvey Project",
                      shp   = "ESRI Shapefile",
                      tab   = "Text",
                      tif   = "TIFF",
                      tiff  = "TIFF",
                      tsv   = "Text",
                      txt   = "Text",
                      xlsx  = "Open XML Spreadsheet",
                      xz    = "Compressed Text",
                      zip   = "Compressed Text"
                  )

  # process connection and return
  if (!is.null(file)) {
    if (inherits(file, "connection"))
      val <- summary.connection(file)$description
    else
      val <- file
    ext <- tools::file_ext(val)
    attr(val, "directory") <- dirname(val)
    attr(val, "extension") <- ext
    attr(val, "name") <- sub(paste0(".", ext, "$"), "", basename(val))
    attr(val, "type") <- all.filters[[ext]]
    Data("default.dir", attr(val, "directory"))
    return(val)
  }

  # establish initial directory
  if (is.null(initialdir)) initialdir <- Data("default.dir")

  # build filters
  filters <- matrix(nrow=0, ncol=2)
  if (!is.null(exts)) {
    for (ext in exts) {
      typ <- all.filters[[ext]]
      if (is.null(typ)) typ <- toupper(ext)
      filters <- rbind(filters, c(typ, paste0(".", ext)))
    }
  }
  filters   <- rbind(filters, c("All files", "*"))
  filters[] <- paste0("{", filters, "}")
  filters   <- apply(filters, 1, paste, collapse=" ")
  filters   <- paste(paste0("{", filters, "}"), collapse=" ")

  # build arguments
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

  # open file dialog gui
  res <- tclvalue(do.call(tcl, args))
  if (!nzchar(res)) return()

  # account for mutiple files
  if (multi) {
    ans <- character()
    pat <- "([^{])*\\{([^}]*)\\}(.*)"
    while (grepl(pat, res)) {
      ans <- c(ans, sub(pat, "\\2", res))
      res <- sub(pat, "\\1\\3", res)
    }
    ans <- c(ans, strsplit(res, " ", fixed=TRUE)[[1]])
    ans <- ans[nzchar(ans)]
  } else {
    ans <- res
  }

  # package results
  n <- length(ans)
  if (n > 1) f <- list()

  for (i in seq_along(ans)) {
    val <- ans[i]
    ext <- tools::file_ext(val)
    attr(val, "directory") <- dirname(val)
    attr(val, "extension") <- ext
    attr(val, "name") <- sub(paste0(".", ext, "$"), "", basename(val))
    attr(val, "type") <- all.filters[[ext]]
    if (n > 1)
      f[[i]] <- val
    else
      f <- val
  }

  # set default directory
  if (!is.null(f)) Data("default.dir", attr(val, "directory"))

  return(f)
}
