# RSurvey

[![Travis Build Status](https://travis-ci.org/USGS-R/RSurvey.svg?branch=master)](https://travis-ci.org/USGS-R/RSurvey)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/2iqeunfhx593megv?svg=true)](https://ci.appveyor.com/project/jfisher-usgs/rsurvey)
[![CRAN Version](https://www.r-pkg.org/badges/version/RSurvey)](https://CRAN.R-project.org/package=RSurvey)
[![](https://cranlogs.r-pkg.org/badges/RSurvey?color=brightgreen)](https://CRAN.R-project.org/package=RSurvey)
[![CC0](https://img.shields.io/badge/license-CC0--1.0-brightgreen.svg)](https://creativecommons.org/publicdomain/zero/1.0/)

## Overview

**RSurvey** is a geographic information system (GIS) graphical user interface (GUI) that provides data viewing, management, and analysis tools.
The cross-platform application was designed to be simple enough for non-technical users.
**RSurvey**-generated graphics attempt to meet the exact standards for illustrations in reports of the United States Geological Survey
([USGS](https://www.usgs.gov/)).

## Install

If [R](https://www.r-project.org/) is not already installed on your computer, download and install the latest binary distribution from
the Comprehensive R Archive Network ([CRAN](https://cran.r-project.org/)).
Windows users should set R to operate as a single document interface (SDI) application during installation
by choosing to customize the start-up options and specifying the SDI interface (not the default).

If your operating system is macOS, download and install [XQuartz](https://www.xquartz.org/), and reboot your computer.

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

To install the stable version of **RSurvey** from [CRAN](https://CRAN.R-project.org/package=RSurvey) use the command

```r
install.packages("RSurvey")
```

Or use **devtools** to install the development version from GitHub.

```r
devtools::install_github("USGS-R/RSurvey")
```

In addition to its required packages, **RSurvey** can make use of the functionality in its suggested packages.
If any of the suggested packages are missing, you will be prompted to install them when it first starts up.

If you're running into difficulties with package installation,
see the R Commander [installation notes](https://socserv.mcmaster.ca/jfox/Misc/Rcmdr/installation-notes.html) for possible solutions.
R Commander is another GUI implemented as an R package.

## Run

Load **RSurvey** in the current R session and launch its main GUI using the command

```r
library("RSurvey")
```

## Contribute

Report bugs, suggest new features on the [Issues](https://github.com/USGS-R/RSurvey/issues) page,
and propose improvements with pull requests.
A tentative list of features proposed for future releases is given on the
[Future Plans](https://github.com/USGS-R/RSurvey/blob/master/inst/misc/future-plans.md) page.
The set of standards used for coding **RSurvey** is documented in
[Google's R Style Guide](https://google.github.io/styleguide/Rguide.xml "Google's R Style Guide").

## Disclaimer

This software is in the public domain because it contains materials that originally came from the USGS,
an agency of the United States Department of Interior.
For more information, see the
[official USGS copyright policy](https://www2.usgs.gov/visual-id/credit_usgs.html "official USGS copyright policy").

Although this software program has been used by the USGS, no warranty, expressed or implied,
is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and
related program material nor shall the fact of distribution constitute any such warranty,
and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."
