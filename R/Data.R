Data <- local({

  # store data locally
  dat <- list()

  # set default values
  default <- list("win.loc"         = NULL,
                  "default.dir"     = getwd(),
                  "palette.pts"     = function(n, c=80, l=60, start=0, end=300) {
                                        colorspace::rainbow_hcl(n, c, l, start, end)
                                      },
                  "palette.grd"     = function(n, h=c(300, 75), c.=c(35, 95), l=c(15, 90),
                                               power=c(0.8, 1.2)) {
                                        colorspace::heat_hcl(n, h, c., l, power)
                                      },
                  "crs"             = sp::CRS(as.character(NA)),
                  "sep"             = "\t",
                  "cex.pts"         = 1,
                  "nlevels"         = NULL,
                  "asp.yx"          = NULL,
                  "asp.zx"          = NULL,
                  "legend.loc"      = NULL,
                  "scale.loc"       = NULL,
                  "arrow.loc"       = NULL,
                  "scale.loc"       = NULL,
                  "bg.lines"        = 0,
                  "useRaster"       = 1,
                  "contour.lines"   = 0,
                  "dms.tick"        = 0,
                  "make.intervals"  = 0,
                  "proportional"    = 0,
                  "quantile.breaks" = 0,
                  "draw.key"        = 0,
                  "max.dev.dim"     = c(43L, 56L))

  function(option, value, which.attr=NULL, clear.proj=FALSE, clear.data=FALSE, replace.all=NULL) {

    # replace all values
    if (is.list(replace.all)) {
      dat <<- replace.all
      return(invisible())
    }

    # save parameters
    if (clear.proj || clear.data) {
      save.params <- if (clear.data) names(default) else c("win.loc", "default.dir")
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
    noption <- length(option)
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
