WriteFile <- function(ext="txt") {
  # Exports post-processed data to a file.

  # Check for necessary information

  if ("shp" %in% ext) {
    is.pkg <- suppressPackageStartupMessages(require("rgdal",
                                                     character.only=TRUE,
                                                     quietly=TRUE))
    if (!is.pkg)
      ext <- ext[-which(ext %in% "shp")]
  }

  if (is.null(Data("data.raw")) || is.null(ext))
    return()
  if (is.null(Data("data.pts")) && "shp" %in% ext)
    return()
  if (is.null(Data("data.grd")) && "grd" %in% ext)
    return()

  # Select output file

  f <- GetFile(cmd="Save As", exts=ext, file=NULL, win.title="Save Data As",
               defaultextension="txt")
  if (is.null(f))
    return()

  ext <- f$ext

  # Open connection

  enc <- Data("encoding")

  if (ext == "gz") {
    con <- gzfile(description=f$path, open="w", encoding=enc, compression=6)
  } else if (ext != "shp") {
    con <- file(description=f$path, open="w", encoding=enc)
  }

  # Organize data

  vars <- Data("vars")
  cols <- Data("cols")

  ncols <- length(cols)

  col.ids  <- sapply(1:ncols, function(i) cols[[i]]$id)
  col.funs <- sapply(1:ncols, function(i) cols[[i]]$fun)
  col.clas <- sapply(1:ncols, function(i) cols[[i]]$class)
  col.nams <- sapply(1:ncols, function(i) cols[[i]]$name)
  col.unts <- sapply(1:ncols,
                     function(i) {
                       rtn <- cols[[i]]$unit
                       if (is.null(rtn))
                         rtn <- NA
                       rtn
                     })
  col.digs <- sapply(1:ncols,
                     function(i) {
                       rtn <- cols[[i]]$digits
                       if (is.null(rtn))
                         rtn <- NA
                       rtn
                     })

  # Prepare for export

  if (ext == "grd") {
    d <- Data("data.grd")
  } else {

    # Get row indexs

    idxs <- row.names(Data("data.pts"))
    if (is.null(idxs))
      idxs <- row.names(Data("data.raw"))

    idxs <- as.integer(idxs)

    # Initialize data table for export

    d <- as.data.frame(matrix(NA, nrow=length(idxs), ncol=length(cols)))

    # Evaluate functions

    for (i in 1:ncols)
      d[, i] <- EvalFunction(cols[[i]]$fun, cols)[idxs]

    # Format data

    for (i in 1:ncols) {
      if (col.clas[i] == "numeric") {
        dig <- col.digs[i]
        if (is.na(dig))
          dig <- getOption("digits")
        d[, i] <- format(round(d[, i], dig), nsmall=dig)
        if (ext == "shp")
          d[, i] <- as.numeric(d[, i])
      } else if (col.clas[i] == "integer") {
        d[, i] <- format(d[, i], nsmall=0)
        if (ext == "shp")
          d[, i] <- as.integer(d[, i])
      } else if (col.clas[i] == "POSIXct") {
        fmt <- col.unts[i]
        if (is.na(fmt))
          fmt <- "%Y-%m-%d %H:%M:%S"
        d[, i] <- format(d[, i], format=fmt)
      }
    }
  }

  # Write data to ouput file

  if (ext == "grd") {
      dput(d, file=con)
  } else if (ext == "shp") {

      # Names are finicky for shapefiles, rules are convoluted,
      # 8-bit names and no periods

      col.names <- gsub("\\.", "", make.names(substr(col.ids, 1, 7),
                        unique=TRUE))
      colnames(d) <- col.names
      coordinates(d) <- col.names[c(vars$x, vars$y)]

      writeOGR(obj=d, dsn=f$dir, layer=f$name, driver="ESRI Shapefile",
               verbose=TRUE)
  } else {

      # Construct header

      h <- t(col.nams)
      if (!all(is.na(col.unts)))
        h <- rbind(h, col.unts)
      if (!all(is.na(col.digs)))
        h <- rbind(h, col.digs)

      # Value seperator

      sep <- ifelse(ext == "csv", ",", "\t")

      # Write table to file

      write.table(h, file=con, append=FALSE, quote=FALSE, row.names=FALSE,
                  col.names=FALSE, sep=sep)
      write.table(d, file=con, append=TRUE,  quote=FALSE, row.names=FALSE,
                  col.names=FALSE, sep=sep)
  }

  if (exists("con"))
    close(con)
}
