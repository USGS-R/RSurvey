ReadData <- function(con, headers=c(FALSE, FALSE), sep="\t",
                     quote="\"'", nrows=-1, na.strings=c("", "NA"), skip=0,
                     comment.char="#", encoding=getOption("encoding")) {
  # Reads table formatted data from a connection and creates a
  # data frame from it.

  # Clear previous data
  Data(clear.data=TRUE)
  
  # Connection
  if (!inherits(con, "connection")) {
    con <- file(description=con, open="r", encoding=encoding)
    on.exit(close(con))
  }

  # Track computational time
  elapsed.time <- system.time({

    # Establish arguments to pass to read.table
    args <- list(file=con, header=FALSE, sep=sep, quote=quote, row.names=NULL,
                 na.strings=na.strings, check.names=TRUE, fill=TRUE,
                 strip.white=TRUE, blank.lines.skip=TRUE,
                 comment.char=comment.char, allowEscapes=TRUE, flush=TRUE,
                 fileEncoding="", encoding=encoding)

    # Load headers

    col.classes <- "character"
    nheaders <- sum(headers)
    if (nheaders > 0L) {
      h <- try(do.call(read.table, c(args, skip=skip, nrows=nheaders,
                                     colClasses=col.classes)), silent=TRUE)
      if (inherits(h, "try-error"))
        return(h)
      
      i <- 1L
      if (headers[1]) {
        nams <- as.character(h[i, ])
        nams[is.na(nams)] <- "Unknown"
        i <- i + 1L
      }
      if (headers[2]) {
        fmts <- as.character(h[i, ])

        # Use formats to determine column classes
        n <- ncol(h)
        col.classes <- rep("character", n)
        for (i in 1:n) {
          fmt <- fmts[i]

          test <- try(sprintf(fmt, 1), silent=TRUE)
          is.error <- inherits(test, "try-error")
          if (!is.error) {
            is.num <- !is.na(suppressWarnings(as.numeric(test)))
            if (is.num) {
              s <- paste(substr(fmt, 1, 1),
                         substr(fmt, nchar(fmt), nchar(fmt)), sep="")
              if (s %in% c("%d", "%i")) {
                col.classes[i] <- "integer"
              } else if (s %in% c("%f", "%e", "%E")) {
                col.classes[i] <- "numeric"
              }
            }
          }
        }
        col.classes[fmts %in% "%Y-%m-%d %H:%M:%S"] <- "POSIXct"
      }

      skip <- 0L
      nrows <- nrows - nheaders
    }

    # Load data
    d <- try(do.call(read.table, c(args, skip=skip, nrows=nrows,
                                   list(colClasses=col.classes))), silent=TRUE)
    if (inherits(d, "try-error"))
      return(d)
    
    # Initialize missing headers
    n <- ncol(d)
    if (!headers[1])
      nams <- rep("Unknown", n)
    if (!headers[2])
      fmts <- rep(NA, n)

    # Reset row names
    rownames(d) <- 1:nrow(d)

    # Initialize variables
    cols <- list()
    vars <- list()
    ids <- NULL

    # Establish column types
    for (j in 1:n) {
      val <- d[, j]
      fmt <- if (is.na(fmts[j])) NULL else fmts[j]

      # Try to determine class of character variables
      if (inherits(val, "character")) {
        is.date <- FALSE
        if (!is.null(fmt) && !all(is.na(val))) {
          date.time <- as.POSIXct(val, format=fmt)
          is.date <- all(!is.na(date.time[!is.na(val)]))
        }
        if (is.date)
          val <- date.time
        else
          val <- type.convert(d[, j], as.is=TRUE)
      }
      
      # Determine default x-, y-, z-axis variables
      if (inherits(val, c("numeric", "integer"))) {
        val[!is.finite(val)] <- NA
        if (is.null(vars$x)) {
          vars$x <- j
        } else if (is.null(vars$y)) {
          vars$y <- j
        } else if (is.null(vars$z)) {
          vars$z <- j
        }
      }
      
      # Set variable class
      cls <- class(val)[1]
         
      # Set missing formats
      if (is.null(fmt)) {
        if (cls %in% c("character", "logical")) {
          fmt <- "%s"
        } else if (cls == "numeric") {
          fmt <- "%f"
        } else if (cls == "integer") {
          fmt <- "%d"
        }
      }

      # Additional attributes

      nam <- nams[j]
      id <- nam
      i <- 1L
      hold.id <- id
      while (id %in% ids) {
        id <- paste(hold.id, " (", i, ")", sep="")
        i <- i + 1L
      }
      ids <- c(ids, id)

      cols[[j]] <- list()

      cols[[j]]$id      <- id
      cols[[j]]$name    <- nam
      cols[[j]]$format  <- fmt
      cols[[j]]$class   <- cls
      cols[[j]]$index   <- j
      cols[[j]]$fun     <- paste("\"", id, "\"", sep="")
      cols[[j]]$sample  <- na.omit(val)[1]
      cols[[j]]$summary <- SummarizeData(val, fmt=fmt)

      d[, j] <- val
    }

    # Store data
    Data("data.raw", d)
    Data("cols", cols)
    Data("vars", vars)
  })

  ans <- paste("\nTime required to import data:",
               format(elapsed.time['elapsed']), "seconds\n", "\n")
  invisible(ans)
}
