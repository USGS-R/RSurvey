#' Parse and Evaluate an RSurvey Expression
#'
#' This function parses and evaluates a character string representation of an \pkg{RSurvey} expression.
#'
#' @param txt character.
#'   A string representation of an \R function.
#' @param cols list.
#'   See \code{\link{ManageVariables}}
#'
#' @return Returns the result of evaluating the text expression.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{parse}}, \code{\link{eval}}
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' d <- list(x = 1:10, y = 10:1)
#' Data("data.raw", d)
#' cols <- list()
#' cols[[1]] <- list(id = "X", index = 1, fun = "\"X\"")
#' cols[[2]] <- list(id = "Y", index = 2, fun = "\"Y\"")
#' EvalFunction("\"Y\"", cols)
#' EvalFunction("\"X\" + \"Y\"", cols)
#' EvalFunction("rnorm(12)", cols)
#'

EvalFunction <- function(txt, cols) {
  d <- list()
  ids <- vapply(cols, function(i) i$id, "")
  for (i in seq_along(ids)) {
    if (regexpr(paste0("\"", ids[i], "\""), txt, fixed=TRUE)[1] >= 0) {
      if (is.na(cols[[i]]$index))
        d[[i]] <- EvalFunction(cols[[i]]$fun, cols)
      else
        d[[i]] <- Data("data.raw")[[cols[[i]]$index]]
    }
  }
  fun <- txt
  for (i in seq_along(ids))
    fun <- gsub(paste0("\"", ids[i], "\""), paste0("DATA[[", i, "]]"), fun, fixed=TRUE)
  fun <- eval(parse(text=paste0("function(DATA) {", fun, "}")))
  ans <- try(fun(d), silent=TRUE)
  return(ans)
}
