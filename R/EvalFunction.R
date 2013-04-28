# Evaluate an RSurvey expression.
EvalFunction <- function(txt, cols) {

  d <- list()

  ids <- vapply(cols, function(i) i$id, "")

  for (i in seq(along=ids)) {
    id.quoted <- paste0("\"", ids[i], "\"")
    if (regexpr(id.quoted, txt, fixed=TRUE)[1] >= 0) {
      if (is.na(cols[[i]]$index)) {
        d[[i]] <- EvalFunction(cols[[i]]$fun, cols)
      } else {
        d[[i]] <- Data("data.raw")[, cols[[i]]$index]
      }
    }
  }

  fun <- txt
  pattern <- paste0("\"", ids, "\"")
  replacement <- paste0("DATA[[", 1:length(ids), "]]")
  for (i in seq(along=ids))
    fun <- gsub(pattern[i], replacement[i], fun, fixed=TRUE)
  fun <- paste0("function(DATA) {", fun, "}")
  fun <- eval(parse(text=fun))

  ans <- try(fun(d), silent=TRUE)

  if (inherits(ans, "try-error"))
    return(ans)

  if (is.numeric(ans))
    ans[is.infinite(ans) | is.nan(ans)] <- NA

  ans
}
