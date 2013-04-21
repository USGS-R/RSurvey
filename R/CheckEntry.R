# Controls the character string content within a Tk entry widget.

CheckEntry <- function (ent.typ, ent.str="") {

  if (ent.str == "")
    return("")

  chr <- unlist(strsplit(ent.str, split=""))

  if (ent.typ == "numeric") {
    accept.vals <- c(as.character(0:9), ".", "-")
  } else if (ent.typ == "second") {
    accept.vals <- c(as.character(0:9), ".")
  } else if (ent.typ %in% c("integer", "hour", "minute")) {
    accept.vals <- c(as.character(0:9))
  } else if (ent.typ == "date") {
    accept.vals <- c("a", "A", "b", "c", "C", "d", "D", "e", "E", "F", "g",
                     "G", "h", "H", "I", "j", "k", "l", "m", "M", "n", "O",
                     "p", "r", "R", "S", "t", "T", "u", "U", "V", "w", "W",
                     "x", "X", "y", "Y", "z", "Z", "/", "-", ":", "%", "#",
                     " ", as.character(0:9))
  }

  if (all(chr %in% accept.vals))
    rtn <- ent.str
  else
    rtn <- paste(chr[chr %in% accept.vals], collapse="")

  if (rtn == "")
   return(rtn)

  if (ent.typ == "hour") {
    if (as.integer(rtn) > 23)
      rtn <- "23"
  } else if (ent.typ == "minute") {
    if (as.integer(rtn) > 59)
      rtn <- "59"
  } else if (ent.typ == "second") {
    if (as.numeric(rtn) < 0) {
      rtn <- "0"
    } else if (as.numeric(rtn) > 59.999) {
      rtn <- "59.999"
    }
  }

  rtn
}
