# RSurvey

[![Travis Build Status](https://travis-ci.org/USGS-R/RSurvey.svg?branch=master)](https://travis-ci.org/USGS-R/RSurvey)
[![CRAN Version](https://www.r-pkg.org/badges/version/RSurvey)](https://CRAN.R-project.org/package=RSurvey)
[![](https://cranlogs.r-pkg.org/badges/RSurvey?color=brightgreen)](https://www.rpackages.io/package/RSurvey)
[![USGS Category](https://img.shields.io/badge/USGS-Orphan-red.svg)](https://owi.usgs.gov/R/packages.html#orphan)

## Deprecated

Development of this package has halted.
If you are interested in taking over maintainer status for the package, please email the author.

## Overview

**RSurvey** is a geographic information system (GIS) graphical user interface (GUI) that provides data viewing, management, and analysis tools.
The cross-platform application was designed to be simple enough for non-technical users.

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
see the R Commander [installation notes](http://socserv.socsci.mcmaster.ca/jfox/Misc/Rcmdr/installation-notes.html) for possible solutions.
R Commander is another GUI implemented as an R package.

## Run

Load **RSurvey** in the current R session and launch its main GUI using the command

```r
library("RSurvey")
```

## Disclaimer

This information is preliminary or provisional and is subject to revision.
It is being provided to meet the need for timely best science.
The information has not received final approval by the U.S. Geological Survey (USGS)
and is provided on the condition that neither the USGS nor the U.S. Government
shall be held liable for any damages resulting from the authorized or unauthorized use of the information.

Although this software program has been used by the U.S. Geological Survey (USGS),
no warranty, expressed or implied, is made by the USGS or the U.S. Government
as to the accuracy and functioning of the program and related program material
nor shall the fact of distribution constitute any such warranty,
and no responsibility is assumed by the USGS in connection therewith.
