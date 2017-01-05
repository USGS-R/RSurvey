Data <- local({

  # store data locally
  dat <- list()

  # set default values
  default <- list("nlevels"       = 20,
                  "width"         = 7,
                  "cex.pts"       = 1,
                  "default.dir"   = getwd(),
                  "sep"           = "\t",
                  "rkey"          = 0,
                  "show.poly"     = 0,
                  "img.contour"   = 0,
                  "show.lines"    = 0,
                  "show.2.axes"   = 0,
                  "minor.ticks"   = 0,
                  "ticks.inside"  = 0,
                  "rm.pnt.line"   = 0,
                  "color.palette" = grDevices::terrain.colors,
                  "crs"           = sp::CRS(as.character(NA))
              )

  function(option, value, which.attr=NULL, clear.proj=FALSE, clear.data=FALSE, replace.all=NULL) {

    # replace all values
    if (is.list(replace.all)) {
      dat <<- replace.all
      return(invisible())
    }

    # save parameters
    if (clear.proj | clear.data) {
      save.params <- c("default.dir", "win.loc", "width", "cex.pts")
      if (clear.data)
        save.params <- c(save.params, "nlevels", "asp.yx", "asp.zx", "rkey",
                         "show.poly", "img.contour", "show.lines",
                         "date.fmt", "polys", "proj.file", "show.2.axes",
                         "minor.ticks", "ticks.inside", "color.palette", "rm.pnt.line")
      save.params <- save.params[save.params %in% names(dat)]
      dat <<- sapply(save.params, function(i) list(dat[[i]]))
      return(invisible())
    }

    # return all data
    if (missing(option)) return(dat)

    # check indices for numeric option elements
    if (is.numeric(option)) {
      option <- sapply(option, as.integer)
      option.new <- option[1L]
      if (option.new > length(dat)) option.new <- NULL
      if (!is.null(option.new) && length(option) > 1) {
        for (i in 2:length(option)) {
          if (option[i] > length(dat[[option.new[-i]]]))
            break
          else
            option.new <- c(option.new, option[i])
        }
      }

    # determine numeric indices from character option element
    } else {
      idx <- match(option[1], names(dat))
      option.new <- idx
      if (is.na(option.new)) option.new <- NULL
      if (!is.null(option.new) && length(option) > 1) {
        for (i in 2:length(option)) {
          idx <- match(option[i], names(dat[[option.new[-i]]]))
          if (is.na(idx)) break
          option.new <- c(option.new, idx)
        }
      }
    }

    # determine number of options
    noption     <- length(option)
    noption.new <- length(option.new)

    # return value
    if (missing(value)) {
      if (noption.new < noption) {
        if (noption == 1 && option %in% names(default))
          return(default[[option]])
        else
          return(NULL)
      }
      if (is.null(which.attr))
        return(dat[[option.new]])
      else
        return(attr(dat[[option.new]], which.attr, exact=TRUE))

    # set value
    } else {
      if (noption.new == noption || (noption.new == (noption - 1)
          && is.list(if (is.null(option.new)) dat else dat[[option.new]]))) {
        if (is.null(which.attr))
          dat[[option]] <<- value
        else
          attr(dat[[option]], which.attr) <<- value
      }
    }
  }
})
