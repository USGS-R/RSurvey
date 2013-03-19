SummarizeData <- function(obj, fmt=NULL) {
  # Summarizes the descriptive statistics of an array object.

  # Additional functions (subroutines)

  # Format value
  FormatValue <- function(i) {
    fmt <- dic[[i]]$fmt
    if (is.null(fmt) || s$Class == "integer") {
      val <- try(format(s[[i]]), silent=TRUE)
    } else if (s$Class == "POSIXct") {
      val <- try(format(s[[i]], format=fmt), silent=TRUE)
    } else {
      val <- try(sprintf(fmt, s[[i]]), silent=TRUE)
    }
    if (inherits(val, "try-error"))
      val <- ""
    val <- gsub("(^ +)|( +$)", "", val)
    return(val)
  }
  
  # Build text string
  BuildString <- function(s) {
    s.names <- names(s)
    s.names <- s.names[!s.names %in% "String"]
    descriptions <- paste(sapply(s.names, function(i) dic[[i]]$id), ": ", sep="")
    fmt <-  paste("%-", max(nchar(descriptions)), "s", sep="")
    descriptions <- sprintf(fmt, descriptions)
    values <- sapply(s.names, FormatValue)
    fmt <- paste("%", max(nchar(values)), "s", sep="")
    values <- sprintf(fmt, values)
    string <- paste(paste(paste(descriptions, values, sep=""), collapse="\n"), 
                    "\n", sep="")
    return(string)
  }


  # Main program

  # Account for missing values
  if (is.null(obj))
    return(NULL)
  if (is.null(fmt) || is.na(fmt) || fmt == "")
    fmt <- NULL

  # Build dictionary with summary components
  dic <- list()
  dic$"Class"     <- list(id="Class")
  dic$"Time Per." <- list(id="Length of period")
  dic$"NA's"      <- list(id="Number of NA's", fmt="%d")
  dic$"Count"     <- list(id="Count", fmt="%d")
  dic$"TRUE"      <- list(id="Number of TRUE values", fmt="%d")
  dic$"FALSE"     <- list(id="Number of FALSE values", fmt="%d")
  dic$"Unique"    <- list(id="Number of unique values",fmt="%d")
  dic$"Mean"      <- list(id="Mean", fmt=fmt)
  dic$"Sum"       <- list(id="Sum", fmt=fmt)
  dic$"St. Dev."  <- list(id="Standard deviation", fmt=fmt)
  dic$"Min."      <- list(id="Minimum", fmt=fmt)
  dic$"1st Qu."   <- list(id="Lower quartile", fmt=fmt)
  dic$"Median"    <- list(id="Median", fmt=fmt)
  dic$"3rd Qu."   <- list(id="Upper quartile", fmt=fmt)
  dic$"Max."      <- list(id="Maximum", fmt=fmt)

  # Reformat old summary string
  is.summary.list <- inherits(obj, "list") && !is.null(obj$String)
  if (is.summary.list) {
    s <- obj
    s$String <- BuildString(s)
    return(s)
  }

  # New summary

  if (inherits(obj, "list")) {
    s <- obj
    s$"String" <- NULL
  } else {

    # Common parameters

    s <- list()
    s$Class <- class(obj)[1]
    s$Count <- length(obj)
    s$"NA's" <- length(which(is.na(obj)))

    h <- NULL

    # Specific parameters

    if (inherits(obj, c("integer", "numeric", "POSIXct"))) {
      s$"Unique" <- length(unique(na.omit(obj)))
      quan <- quantile(obj, probs=seq(0, 1, 0.25), na.rm=TRUE, names=FALSE)
      s$"Min."    <- quan[1]
      s$"1st Qu." <- quan[2]
      s$"Median"  <- quan[3]
      s$"3rd Qu." <- quan[4]
      s$"Max."    <- quan[5]

      s$"Mean" <- mean(obj, na.rm=TRUE)

      if (inherits(obj, c("integer", "numeric"))) {
        s$"St. Dev." <- sd(obj, na.rm=TRUE)
        s$"Sum" <- sum(as.numeric(obj), na.rm=TRUE)
      } else {
        s$"Time Per." <- format(s$"Max." - s$"Min.", units="auto")
      }

    } else if (inherits(obj, "logical")) {
      s$"FALSE" <- length(which(!obj))
      s$"TRUE" <- length(which(obj))
      
    } else if (inherits(obj, c("character", "factor"))) {
      if (inherits(obj, "character"))
        obj <- as.factor(obj)
      s$"Unique" <- length(levels(obj))
    }
  }
  s$String <- BuildString(s)
  return(s)
}
