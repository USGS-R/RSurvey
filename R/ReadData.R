ReadData <- function(con, headers=c(FALSE, FALSE, FALSE), sep="\t",
                     quote="\"'", nrows=-1, na.strings=c("", "NA"), skip=0,
                     comment.char="#", encoding=getOption("encoding")) {
  # Reads table formatted data from a connection and creates a
  # data frame from it.

  # Clear previous data

  Data(clear.data=TRUE)

  # Track computational time

  elapsed.time <- system.time({

    # Connection

    if (!inherits(con, "connection")) {
      con <- file(description=con, open="r", encoding=encoding)
      on.exit(close(con))
    }

    # Load data

    d <- try(read.table(con, header=FALSE, sep=sep, quote=quote,
                        row.names=NULL, na.strings=na.strings,
                        colClasses="character", nrows=nrows,
                        skip=skip, check.names=TRUE, fill=TRUE,
                        strip.white=TRUE, blank.lines.skip=TRUE,
                        comment.char=comment.char,
                        allowEscapes=TRUE, flush=TRUE,
                        fileEncoding="", encoding=encoding), silent=TRUE)

    if (inherits(d, "try-error"))
      return(d)

    # Remove columns containing all NA values

    is.all.na <- sapply(seq(along=d), function(i) all(is.na(d[, i])))
    d <- d[, !is.all.na, drop=FALSE]

    # Determine the number of columns

    n <- ncol(d)

    # Address file header

    if (headers[1]) {
      nams <- as.character(d[1, ])
      nams[is.na(nams)] <- "Unknown"
      d <- d[-1, , drop=FALSE]
    } else {
        nams <- rep("Unknown", n)
    }

    if (headers[2]) {
      unts <- as.character(d[1, ])
      d <- d[-1, , drop=FALSE]
    } else {
      unts <- rep(NA, n)
    }

    if (headers[3]) {
      digs <- suppressWarnings(as.integer(d[1, ]))
      digs[is.na(digs) | (digs < 0 | digs > 20)] <- NA
      d <- d[-1, , drop=FALSE]
    } else {
      digs <- rep(NA, n)
    }

    # Reset row names

    rownames(d) <- 1:nrow(d)

    # Initialize variables

    cols <- list()
    vars <- list()
    ids <- NULL

    # Establish column types

    for (idx in 1:n) {
      val <- d[, idx]
      unt <- if (is.na(unts[idx])) NULL else unts[idx]
      dig <- if (is.na(digs[idx])) NULL else digs[idx]

      is.date <- FALSE
      if (!is.null(unt) && !all(is.na(val))) {
        date.time <- as.POSIXct(val, format=unt)
        is.date <- all(!is.na(date.time[!is.na(val)]))
      }

      # Convert value to assumed format

      val <- if (is.date) date.time else type.convert(d[, idx], as.is=TRUE)

      # Class integer or numeric

      if (inherits(val, c("integer", "numeric"))) {
        val[!is.finite(val)] <- NA
        if (is.null(vars$x)) {
          vars$x <- idx
        } else if (is.null(vars$y)) {
          vars$y <- idx
        } else if (is.null(vars$z)) {
          vars$z <- idx
        }

      # Class POSIXct

      } else if (inherits(val, "POSIXct")) {
        if (is.null(vars$t))
          vars$t <- idx
      }

      # Additional attributes

      nam <- nams[idx]

      id <- paste(c(nam, unt), collapse=", ")
      i <- 1

      hold.id <- id
      while (id %in% ids) {
        id <- paste(hold.id, " (", i, ")", sep="")
        i <- i + 1
      }
      ids <- c(ids, id)

      cols[[idx]] <- list()
      cols[[idx]]$id <- id
      cols[[idx]]$name <- nam
      cols[[idx]]$unit <- unt
      cols[[idx]]$digits <- dig
      cols[[idx]]$class <- class(val)[1]
      cols[[idx]]$index <- idx
      cols[[idx]]$summary <- SummarizeData(val, digits=dig, dt.format=unt)
      cols[[idx]]$fun <- paste("DATA[[\"", id, "\"]]", sep="")

      d[, idx] <- val
    }

    # Store data

    Data("data.raw", d)
    Data("cols", cols)
    Data("vars", vars)
  })

  msg <- paste("\nTime required to import data:",
               format(elapsed.time['elapsed']), "seconds\n", "\n")
  msg
}
