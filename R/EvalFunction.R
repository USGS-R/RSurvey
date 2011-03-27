EvalFunction <- function(txt, cols) {
  # Evaluate R expression

  d <- list()

  ids <- sapply(cols, function(i) i$id)

  for (i in seq(along=ids)) {
    id.quoted <- paste("\"", ids[i], "\"", sep="")
    if (regexpr(id.quoted, txt, fixed=TRUE)[1] >= 0) {
      if (is.null(cols[[i]]$index)) {
        d[[i]] <- EvalFunction(cols[[i]]$fun, cols)
      } else {
        d[[i]] <- Data("data.raw")[, cols[[i]]$index]
        if (!is.null(cols[[i]]$digits))
          d[[i]] <- round(d[[i]], cols[[i]]$digits)
      }
    }
  }

  fun <- txt
  ids.quoted <- paste("\"", ids, "\"", sep="")
  for (i in seq(along=ids.quoted))
    fun <- gsub(ids.quoted[i], i, fun, fixed=TRUE)

  fun <- eval(parse(text=paste("function(DATA) {", fun, "}", sep="")))

  ans <- try(fun(d), silent=TRUE)

  if (inherits(ans, "try-error"))
    return(ans)

  if (is.numeric(ans))
    ans[is.infinite(ans) | is.nan(ans)] <- NA

  ans
}
