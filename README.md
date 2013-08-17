RSurvey
=======

This [R](http://www.r-project.org/ "R") package is a processing program for
spatially distributed data.
[**RSurvey**](http://cran.r-project.org/web/packages/RSurvey/index.html "RSurvey")
features graphing tools, query building, and
polygon clipping. A graphical user interface (GUI) is provided and
requires R operate as an SDI application, using multiple
top-level windows for the console, graphics, and pager.

The set of standards used for coding **RSurvey** is documented in
[Google's R Style Guide](http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html "Google's R Style Guide").

Install
-------

If R is not already installed on your
computer, download and install the latest binary distribution from
[CRAN](http://cran.r-project.org/ "The Comprehensive R Archive Network").
Windows users should set R to operate as an SDI application during installation
by choosing to customize the startup options and specifying the SDI interface
(not the default).

Open an R session and install the required packages from CRAN:

    > install.packages('RSurvey')

In addition to the required packages, **RSurvey** uses functions in a number of
recommended packages:
[**rgdal**](http://cran.r-project.org/web/packages/rgdal/index.html "rgdal")
for shapefile support,
[**colorspace**](http://cran.r-project.org/web/packages/colorspace/index.html "colorspace")
for color palettes based on HCL colors,
[**dichromat**](http://cran.r-project.org/web/packages/dichromat/index.html "dichromat")
for color-blind safe palettes, and
[**tripack**](http://cran.r-project.org/web/packages/tripack/index.html "tripack")
for auto-cropping. Note that the license for **tripack** explicitly forbids
commercial use. If any of these recommended packages are missing, **RSurvey**
will offer to install them when it first starts up.

Support for displaying table data is provided by
[tktable](http://tktable.sourceforge.net/ "tktable"),
a spreadsheet-like [Tcl/Tk](http://www.tcl.tk/ "Tcl/Tk") widget
(included with the Windows binary distribution of R).
The following call with indicate whether tktable is available for use:

    > capabilities()['tcltk']

Run
---

Load **RSurvey** in the current R session to activate the main GUI:

    > library(RSurvey)

Example data sets are provided
[here](https://github.com/jfisher-usgs/RSurvey/tree/master/inst/extdata).
