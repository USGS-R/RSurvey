SummarizeData <- function(obj, digits=NULL, big.mark="", scientific=FALSE,
                          dt.format=NULL) {
  # Summarizes the descriptive statistics of an array object.

  # Additional functions (subroutines)

  # Construct text string

  BuildString <- function(i) {
    if (s$Class == "POSIXct") {
      if (is.null(dic[[i]]$dt.format))
        val <- format(s[[i]])
      else
        val <- format(s[[i]], format=dic[[i]]$dt.format)
    } else {
      if (is.null(dic[[i]]$digits))
        val <- format(s[[i]])
      else
        val <- format(round(s[[i]], dic[[i]]$digits), nsmall=dic[[i]]$digits,
                      trim=TRUE, big.mark=dic[[i]]$big.mark,
                      scientific=dic[[i]]$scientific)
    }
    paste(dic[[i]]$id, val, sep=": ")
  }


  # Main program

  # Account for missing values

  if (is.null(obj))
    return(NULL)

  if (!is.numeric(digits) || (digits < 0 | digits > 20))
    digits <- getOption("digits")
  else
    digits <- as.integer(digits)

  # Determine object class

  is.list.int <- inherits(obj, "list") && obj$Class == "integer"
  if(inherits(obj, "integer") || is.list.int)
    digits <- 0L

  # Build dictionary with summary components

  dic <- list()

  dic$"Class"     <- list(id="Class")
  dic$"Time Per." <- list(id="Length of period")

  add <- list(digits=0L, big.mark=",", scientific=FALSE)
  dic$"NA's"      <- c(id="Number of NA's", add)
  dic$"Count"     <- c(id="Count", add)
  dic$"TRUE"      <- c(id="Number of TRUE values", add)
  dic$"FALSE"     <- c(id="Number of FALSE values", add)
  dic$"Unique"    <- c(id="Number of unique values",add)

  add <- list(digits=digits, big.mark=big.mark, scientific=scientific)
  dic$"Mean"      <- c(id="Mean", add)
  dic$"Sum"       <- c(id="Sum", add)
  dic$"St. Dev."  <- c(id="Standard deviation", add)

  add <- c(add, dt.format=dt.format)
  dic$"Min."      <- c(id="Minimum", add)
  dic$"1st Qu."   <- c(id="Lower quartile", add)
  dic$"Median"    <- c(id="Median", add)
  dic$"3rd Qu."   <- c(id="Upper quartile", add)
  dic$"Max."      <- c(id="Maximum", add)

  # A list indicates that only formatting on the summary text is desired

  if (inherits(obj, "list") && !is.null(obj$String)) {
    s <- obj
    nms <- names(s)[!names(s) %in% c("Hist", "String")]
    s$String <- paste(paste(sapply(nms, BuildString), collapse="\n"),
                      "\n", sep="")
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

    if (s$Class %in% c("integer", "numeric", "POSIXct")) {
      s$"Unique" <- length(unique(na.omit(obj)))
      quan <- quantile(obj, probs=seq(0, 1, 0.25), na.rm=TRUE, names=FALSE)

      s$"Min."    <- quan[1]
      s$"1st Qu." <- quan[2]
      s$"Median"  <- quan[3]
      s$"3rd Qu." <- quan[4]
      s$"Max."    <- quan[5]

      s$"Mean" <- mean(obj, na.rm=TRUE)

      if (s$Class %in% c("integer", "numeric")) {
        if (s$Class == "numeric")
          s$"St. Dev." <- sd(obj, na.rm=TRUE)
        s$"Sum" <- sum(as.numeric(obj), na.rm=TRUE)
        s$"Hist" <- hist(obj, plot=FALSE)

      } else {
        s$"Time Per." <- format(s$"Max." - s$"Min.", units="auto")
        s$"Hist" <- hist(obj, breaks=25, plot=FALSE)
      }

    } else if (s$Class == "logical") {
      s$"FALSE" <- length(which(!obj))
      s$"TRUE" <- length(which(obj))
      if(!all(is.na(obj)))
        s$"Hist" <- hist(as.integer(na.omit(obj)) + 1, seq(0, 2), plot=FALSE)

    } else if (s$Class %in% c("character", "factor")) {
      if (s$Class == "character")
        obj <- as.factor(obj)
      s$"Unique" <- length(levels(obj))
      s$"Hist" <- hist(na.omit(as.integer(obj)), plot=FALSE)
    }
  }

  nms <- names(s)[!names(s) %in% "Hist"]
  s$String <- paste(paste(sapply(nms, BuildString), collapse="\n"),
                    "\n", sep="")

  s
}
