# RSurvey

[![Travis-CI Build Status](https://travis-ci.org/jfisher-usgs/RSurvey.svg?branch=master)](https://travis-ci.org/jfisher-usgs/RSurvey)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/RSurvey)](https://CRAN.R-project.org/package=RSurvey)

## Overview

The [R](https://www.r-project.org/) package **RSurvey** is a geographic information system (GIS) graphical user interface (GUI)
that provides data viewing, management, and analysis tools.

## Install

If R is not already installed on your computer, download and install the latest binary distribution from
the Comprehensive R Archive Network ([CRAN](https://cran.r-project.org/)).
Windows users should set R to operate as a single document interface (SDI) application during installation
by choosing to customize the start-up options and specifying the SDI interface (not the default).

If your operating system is OS X, download and install [XQuartz](https://www.xquartz.org/), and reboot your computer.

**RSurvey** uses the [Tk](http://www.tkdocs.com/) toolkit for GUI rendering,
access to Tk is provided by the **tcltk** package.
To check if Tk is available, startup an R session and type the following at the command prompt

```r
capabilities("tcltk")
```

Support for viewing and editing table data is provided by [Tktable](http://tktable.sourceforge.net/),
a spreadsheet-like Tk widget (typically included with the binary distribution of R).
To check if Tktable is available, use the command

```r
inherits(tcltk::tclRequire("Tktable", FALSE), "tclObj")
```

Finally, install the stable version of **RSurvey** from CRAN using the command

```r
install.packages("RSurvey")
```

In addition to its required packages, **RSurvey** can make use of the functionality in its suggested packages.
If any of the suggested packages are missing, you will be prompted to install them when it first starts up.

## Run

Load **RSurvey** in the current R session and launch its GUI using the command

```r
library(RSurvey)
```

## Bugs

Please consider reporting bugs and asking questions on the
[Issues page](https://github.com/jfisher-usgs/RSurvey/issues).

## Disclaimer

This software is in the public domain because it contains materials that originally came from the USGS,
an agency of the United States Department of Interior.
For more information, see the
[official USGS copyright policy](https://www2.usgs.gov/visual-id/credit_usgs.html "official USGS copyright policy").

Although this software program has been used by the USGS, no warranty, expressed or implied,
is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty,
and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

[![CC0](https://i.creativecommons.org/p/zero/1.0/88x31.png)](https://creativecommons.org/publicdomain/zero/1.0/)
