Data <- local({
  # A function used to set or query data and parameters.

  # Store data locally

  dat <- list()

  default <- list("nlevels"       = 20,
                  "width"         = 7,
                  "cex.pts"       = 1,
                  "default.dir"   = getwd(),
                  "sep"           = "\t",
                  "rkey"          = 0,
                  "show.poly"     = 0,
                  "img.contour"   = 0,
                  "show.lines"    = 0,
                  "show.points"   = 0,
                  "vuni"          = 0,
                  "show.2.axes"   = 0,
                  "minor.ticks"   = 0,
                  "ticks.inside"  = 0,
                  "rm.pnt.line"   = 0,
                  "grid.res"      = list(x=NA, y=NA),
                  "grid.mba"      = list(n=NA, m=NA, h=11),
                  "encoding"      = getOption("encoding"),
                  "color.palette" = colorspace::diverge_hcl
              )

  # Main program

  function(option, value, clear.proj=FALSE, clear.data=FALSE,
           replace.all=NULL) {

    # Replace all values

    if (is.list(replace.all)) {
      dat <<- replace.all
      return(invisible())
    }

    # Save parameters

    if (clear.proj | clear.data) {
      save.params <- c("default.dir", "win.loc", "csi", "width", "cex.pts")
      if (clear.data)
        save.params <- c(save.params, "nlevels", "asp.yx", "asp.zx",
                         "vmax", "vxby", "vyby", "rkey", "show.poly",
                         "img.contour", "show.lines", "show.points",
                         "vuni", "date.fmt", "poly", "proj.file",
                         "show.2.axes", "minor.ticks", "ticks.inside",
                         "color.palette", "rm.pnt.line")
      save.params <- save.params[save.params %in% names(dat)]
      dat <<- sapply(save.params, function(i) list(dat[[i]]))
      return(invisible())
    }

    # Return all data

    if (missing(option))
      return(dat)

    # Numeric indices specifying option elements

    if (is.numeric(option)) {
      option <- sapply(option, as.integer)

      opt <- option[1]
      if (opt > length(dat))
        opt <- NULL

      if (!is.null(opt) && length(option) > 1) {
        for (i in 2:length(option)) {
          if (option[i] > length(dat[[opt[-i]]]))
            break
          else
            opt <- c(opt, option[i])
        }
      }
    } else {
      idx <- match(option[1], names(dat))
      opt <- idx
      if (is.na(opt))
        opt <- NULL

      if (!is.null(opt) && length(option) > 1) {
        for (i in 2:length(option)) {
          idx <- match(option[i], names(dat[[opt[-i]]]))
          if (is.na(idx))
            break
          opt <- c(opt, idx)
        }
      }
    }

    # Return or set value

    nopt <- length(opt)
    noption <- length(option)

    if (missing(value)) {
      if (nopt < noption) {
        if (noption == 1 && option %in% names(default))
          return(default[[option]])
        return(NULL)
      }
      return(dat[[opt]])
    } else {
      if (nopt == noption || (nopt == (noption - 1)
          && is.list(if (is.null(opt)) dat else dat[[opt]]))) {
        dat[[option]] <<- value
      }
    }
  }
})

