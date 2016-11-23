# RSurvey

[![Travis-CI Build Status](https://travis-ci.org/jfisher-usgs/RSurvey.svg?branch=master)](https://travis-ci.org/jfisher-usgs/RSurvey)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/RSurvey)](http://cran.r-project.org/package=RSurvey)

## Overview

The [R](http://www.r-project.org/) package **RSurvey** is a processing program for spatially distributed data.
**RSurvey** features graphing, data management, query building, and polygon clipping tools.
A graphical user interface (GUI) is provided and requires R operate as an SDI application,
using multiple top-level windows for the console, graphics, and pager.
Immediate goals for software development include:

- add the ability to manipulate geospatial data;
- revise management of graphic devices; and
- create a GUI for geostatistical modeling.

## Install

If R is not already installed on your computer, download and install the latest binary distribution from
[CRAN](http://cran.r-project.org/ "The Comprehensive R Archive Network").
Windows users should set R to operate as an SDI application during installation
by choosing to customize the start-up options and specifying the SDI interface (not the default).

You can install the stable version of the **RSurvey** package from
[CRAN](https://CRAN.R-project.org/package=RSurvey) using the following command:

```r
install.packages("RSurvey")
```

In addition to the required packages, **RSurvey** is dependent on a number of functions in the suggested packages.
If any of the suggested packages are missing, **RSurvey** will offer to install them when it first starts up.
Note that the license for the suggested **tripack** package explicitly forbids commercial use.

Support for displaying table data is provided by [tktable](http://tktable.sourceforge.net/ "tktable"),
a spreadsheet-like [Tcl/Tk](http://www.tcl.tk/ "Tcl/Tk") widget (typically included with the binary distribution of R).
The following command will indicate whether tktable is available for use:

```r
print(inherits(tcltk::tclRequire("Tktable", warn = FALSE), "tclObj"))
```

## Run

Load **RSurvey** in the current R session

```r
library(RSurvey)
```

Launch a GUI session

```r
StartGui()
```

## Bugs

Please consider reporting bugs and asking questions on the
[Issues page](https://github.com/jfisher-usgs/RSurvey/issues).

## License

GPL-2 or GPL-3.
These are "copy-left" licenses.
This means that anyone who distributes the code in a bundle must license the whole bundle in a GPL-compatible way.
Additionally, anyone who distributes modified versions of the code (derivative works) must also make the source code available.
GPL-3 is a little stricter than GPL-2, closing some older loopholes.

[![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://img.shields.io/badge/License-GPL%20v2-blue.svg)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
