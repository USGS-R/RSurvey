#' Control Content in Entry Widget
#'
#' This function enforces content control on entry widgets.
#'
#' @param obj.class character.
#'   Name of object class, either \var{real}, \var{integer}, or \var{logical}
#' @param ent.str character.
#'   Value from entry widget
#'
#' @return Returns a character string that can be easily converted to the desired object class.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords manip
#'
#' @export
#'
#' @examples
#' CheckEntry("numeric", "3.14ab")
#' CheckEntry("integer", "3.")
#'

CheckEntry <- function(obj.class, ent.str="") {

  if (ent.str == "") return(ent.str)

  if ("numeric" %in% obj.class) {
    accept.vals <- c(as.character(0:9), "-", "e", "E", ".", "N", "A")
  } else if ("integer" %in% obj.class) {
    accept.vals <- c(as.character(0:9), "-", "e", "E", "N", "A")
  } else if ("logical" %in% obj.class) {
    accept.vals <- c("T", "R", "U", "E", "F", "A", "L", "S", "N")
  } else {
    return(ent.str)
  }

  chr <- unlist(strsplit(ent.str, split=""))
  if (all(chr %in% accept.vals))
    rtn <- ent.str
  else
    rtn <- paste(chr[chr %in% accept.vals], collapse="")

  return(rtn)
}
