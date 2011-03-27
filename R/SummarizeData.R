SummarizeData <- function(obj, digits=NULL, units=NULL) {
  # Summarizes the descriptive statistics of an array object.

  # Additional functions (subroutines)

  # Construct text string

  BuildString <- function(i) {
    if (s$Class == "POSIXct") {
      if (is.na(dic[[i]][[3]])) {
        val <- format(s[[i]])
      } else {
        val <- format(s[[i]], format=dic[[i]][[3]])
      }
    } else {
      if (is.na(dic[[i]][[2]])) {
        val <- format(s[[i]])
      } else {
        dig <- as.integer(dic[[i]][[2]])
        val <- format(round(s[[i]], dig), nsmall=dig, trim=TRUE)
      }
    }
    paste(dic[[i]][[1]], val, sep=": ")
  }


  # Main program

  # Account for missing values

  if (is.null(obj))
    return(NULL)
  if (is.null(digits) || (digits < 0 | digits > 20))
    digits <- getOption("digits")
  if (is.null(units))
    units <- ""

  # Determine object class

  is.list.int <- inherits(obj, "list") && obj$Class == "integer"
  if(inherits(obj, "integer") || is.list.int)
    digits <- 0

  # Dictionary for element names in list

  dic <- list()
  dic$"Class"     <- list("Class", NA, NA)
  dic$"Time Per." <- list("Length of period", NA, NA)
  dic$"NA's"      <- list("Number of NA's", 0, NA)
  dic$"Count"     <- list("Count", 0, NA)
  dic$"TRUE"      <- list("Number of TRUE values", 0, NA)
  dic$"FALSE"     <- list("Number of FALSE values", 0, NA)
  dic$"Unique"    <- list("Number of unique values", 0, NA)
  dic$"Mean"      <- list("Mean", digits, NA)
  dic$"Sum"       <- list("Sum", digits, NA)
  dic$"Min."      <- list("Minimum", digits, units)
  dic$"Max."      <- list("Maximum", digits, units)
  dic$"1st Qu."   <- list("Lower quartile", digits, units)
  dic$"3rd Qu."   <- list("Upper quartile", digits, units)
  dic$"Median"    <- list("Median", digits, units)
  dic$"St. Dev."  <- list("Standard deviation", digits, NA)

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
