# Convert objects from POSIXct to character class

POSIXct2Character <- function(obj, fmt="%Y-%m-%d %H:%M:%OS3") {
  match.location <- gregexpr("%OS[[:digit:]]+", fmt)[[1]]
  if (match.location > 0) {
    dec.digits <- as.integer(substr(fmt,
                                    match.location + 3L,
                                    match.location + attr(match.location,
                                                          "match.length")))
    obj <- as.POSIXlt(obj)
    obj$sec <- round(obj$sec, dec.digits) + 10^(-dec.digits - 1L)
  }
  return(format(obj, format=fmt))
}
