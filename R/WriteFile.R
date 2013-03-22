WriteFile <- function(file.type="text", file.name=NULL, col.ids=NULL,
                      is.processed=TRUE, headers=c(FALSE, FALSE), 
                      sep="\t", is.compressed=FALSE, 
                      encoding=getOption("encoding")) {
  # Exports post-processed data to a file.

  # Check for necessary information

  if (file.type == "shape") {
    is.pkg <- "rgdal" %in% .packages(all.available=TRUE) &&
              require(rgdal)
    if (!is.pkg)
      stop("package rgdal required for shapefile support")
  }

  if (is.null(Data("data.raw")))
    return()
  if (is.null(Data("data.grd")) && file.type == "grid")
    return()

  # Establish connection

  if (is.null(file.name)) {
    if (file.type == "text") {
      ext <- "txt"
    } else if (file.type == "shape") {
      ext <- "shp"
    } else if (file.type == "grid") {
      ext <- "grd"
    }
    f <- GetFile(cmd="Save As", exts=ext, file=NULL,
                 win.title="Save Data As", defaultextension=ext)
    if (is.null(f))
      return()
    file.name <- f
  }

  if (file.type %in% c("text", "grid")) {
    if (is.compressed)
      con <- gzfile(description=file.name, open="w", encoding=encoding,
                    compression=6)
    else
      con <- file(description=file.name, open="w", encoding=encoding)
    if (!inherits(con, "connection"))
      stop()
    on.exit(close(con))
  }

  # Organize data

  vars <- Data("vars")
  cols <- Data("cols")

  n <- length(cols)

  if (is.null(col.ids)) {
    col.idxs <- 1:n
  } else {
    all.col.ids <- sapply(1:n, function(i) cols[[i]]$id)
    if (file.type == "shape") {
      if (is.null(vars$x) | is.null(vars$y))
        stop("Shapefiles require coordinate values")
      id.x <- all.col.ids[vars$x]
      id.y <- all.col.ids[vars$y]
      if (!id.x %in% col.ids)
        col.ids <- c(col.ids, id.x)
      if (!id.y %in% col.ids)
        col.ids <- c(col.ids, id.y)
    }
    col.idxs <- which(all.col.ids %in% col.ids)
  }

  col.ids  <- sapply(col.idxs, function(i) cols[[i]]$id)
  col.funs <- sapply(col.idxs, function(i) cols[[i]]$fun)
  col.nams <- sapply(col.idxs, function(i) cols[[i]]$name)
  col.fmts <- sapply(col.idxs,
                     function(i) {
                       rtn <- cols[[i]]$format
                       if (is.null(rtn))
                         rtn <- NA
                       rtn
                     })

  # Identify data set and records

  if (file.type == "grid") {
    d <- Data("data.grd")
  } else {
    if (is.processed & !is.null(Data("data.pts")))
      row.idxs <- as.integer(row.names(Data("data.pts")))
    else
      row.idxs <- as.integer(row.names(Data("data.raw")))

    n <- length(col.idxs)
    m <- length(row.idxs)
    d <- as.data.frame(matrix(NA, nrow=m, ncol=n))

    for (i in 1:n) {
      obj <- EvalFunction(col.funs[i], cols)[row.idxs]

      # Format data

      fmt <- col.fmts[i]
      if (inherits(obj, "POSIXt")) {
        if (is.na(fmt))
          d[, i] <- format(obj)
        else
          d[, i] <- format(obj, format=fmt)
      } else if (file.type == "shape") {
        d[, i] <- obj
      } else {
        if (is.na(fmt)) {
          d[, i] <- format(obj)
        } else {
          ans <- try(sprintf(fmt, obj), silent=TRUE)
          if (inherits(ans, "try-error"))
            d[, i] <- format(obj)
          else
            d[, i] <- ans
        }
      }
    }
  }

  # Write data to ouput file

  if (file.type == "grid") {
    dput(d, file=con)
  } else if (file.type == "shape") {

    # Names are finicky for shapefiles, rules are convoluted,
    # 8-bit names and no periods

    column.names <- gsub("\\.", "", make.names(substr(col.ids, 1L, 7L),
                         unique=TRUE))
    colnames(d) <- column.names

    idx.x <- which(col.ids %in% id.x)
    idx.y <- which(col.ids %in% id.y)
    coordinates(d) <- column.names[c(idx.x, idx.y)]

    file.dir <- dirname(file.name)
    file.base <- basename(file.name)

    file.ext <- tolower(tail(unlist(strsplit(file.base, "\\."))[-1L], 1L))
    if (length(file.ext) == 0L)
      file.layer <- basename(file.name)
    else
      file.layer <- sub(paste(".", file.ext, "$", sep=""), "", file.base)

    writeOGR(obj=d, dsn=file.dir, layer=file.layer, driver="ESRI Shapefile",
             verbose=TRUE, overwrite_layer=TRUE)
  } else {

    # Construct header

    is.header <- any(headers)
    if (is.header) {
      m <- sum(as.integer(headers))
      n <- ncol(d)
      h <- as.data.frame(matrix(NA, nrow=m, ncol=n))
      i <- 1L
      if (headers[1]) {
        h[i, ] <- col.nams
        i <- i + 1L
      }
      if (headers[2])
        h[i, ] <- col.fmts

      write.table(h, file=con, append=FALSE, quote=FALSE, row.names=FALSE,
                  col.names=FALSE, sep=sep)
    }

    write.table(d, file=con, append=is.header, quote=FALSE, row.names=FALSE,
                col.names=FALSE, sep=sep)
  }
}
