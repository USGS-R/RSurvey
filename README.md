RSurvey
=======

This [R](http://www.r-project.org/ "R") package is a processing program for spatially distributed data.
[**RSurvey**](https://cran.r-project.org/package=RSurvey "RSurvey") features graphing, data management, query building, and polygon clipping tools.
A graphical user interface (GUI) is provided and requires R operate as an SDI application, using multiple top-level windows for the console, graphics, and pager.
The set of standards used for coding **RSurvey** is documented in [Google's R Style Guide](https://google.github.io/styleguide/Rguide.xml "Google's R Style Guide").
Immediate goals for software development include:

* adding the ability to manipulate geospatial data;
* revising management of graphic devices; and
* creating a GUI for geostatistical modeling.

Install
-------

If R is not already installed on your computer, download and install the latest binary distribution from [CRAN](http://cran.r-project.org/ "The Comprehensive R Archive Network").
Windows users should set R to operate as an SDI application during installation by choosing to customize the start-up options and specifying the SDI interface (not the default).

Open an R session and install **RSurvey** and its dependent packages from CRAN using the following commands:

```r
install.packages("RSurvey", dependencies = TRUE, type = "both")
```

In addition to the required packages, **RSurvey** uses functions in a number of suggested packages.
If any of these packages are missing, **RSurvey** will offer to install them when it first starts up.
Note that the license for the suggested **tripack** package explicitly forbids commercial use.

Support for displaying table data is provided by [tktable](http://tktable.sourceforge.net/ "tktable"), a spreadsheet-like [Tcl/Tk](http://www.tcl.tk/ "Tcl/Tk") widget (typically included with the binary distribution of R).
The following command will indicate whether tktable is available for use:

    print(inherits(tcltk::tclRequire("Tktable", warn = FALSE), "tclObj"))

Run
---

Load **RSurvey** in the current R session to activate the main GUI:

    library(RSurvey)

Example data sets are provided in the following directory:

    system.file("extdata", package = "RSurvey")

