# Convert objects from POSIXct to character class

POSIXct2Character <- function(obj, fmt="%Y-%m-%d %H:%M:%OS3") {
  pos <- gregexpr("%OS[[:digit:]]+", fmt)[[1]]
  if (pos > 0) {
    dec.digits <- as.integer(substr(fmt,
                                    pos + 3L,
                                    pos + attr(pos, "match.length")))
    obj <- as.POSIXlt(obj, tz="GMT")
    obj$sec <- round(obj$sec, dec.digits) + 10^(-dec.digits - 1L)
  }
  return(format(obj, format=fmt))
}
