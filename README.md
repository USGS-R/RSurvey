RSurvey: A processing program for spatially distributed data
============================================================

This [R](http://www.r-project.org/ "R") package is a processing program for
spatially distributed data.
[RSurvey](http://cran.r-project.org/web/packages/RSurvey/index.html "RSurvey")
features graphing tools, query building, and
polygon clipping. A graphical user interface (GUI) is provided and
requires R operate as an SDI application, using multiple
top-level windows for the console, graphics, and pager.

Files can be one of four types as indicated by their extension: tables
(_.txt_, _.csv_, _.dat_, or _.shp_), grids (_.grd_), polygons (_.ply_),
or binary project images (_.rda_). Tables (_.txt_, _.csv_, _.dat_)
can be compressed by [gzip](http://www.gzip.org/ "gzip")
with additional extension _.gz_.
Shapefles (_.shp_) and interpolated grid files (_.grd_) are limited to data
export. Support for programmatic manipulation of measurement units is only
provided for date-time values; therefore, the bulk of unit consistency is tasked
to the user. Time zones, spatial datum's and projections are not supported.

The set of standards used for coding RSurvey is documented in
[Google's R Style Guide](http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html "Google's R Style Guide").

Install
-------

If R is not already installed on your
computer, download and install the latest binary distribution from
[CRAN](http://cran.r-project.org/ "The Comprehensive R Archive Network").
Windows users should set R to operate as an SDI application during installation
by choosing to customize the startup options and specifying the SDI interface
(not the default).

Install required R packages from CRAN using a simple call to
`install.packages`:

    > install.packages('RSurvey')

Install optional R packages
[rgdal](http://cran.r-project.org/web/packages/rgdal/index.html "rgdal")
for shapefile support and
[dichromat](http://cran.r-project.org/web/packages/dichromat/index.html "dichromat")
for color-blind safe palettes:

    > install.packages(c('rgdal', 'dichromat'))

Support for displaying table data is provided by
[tktable](http://tktable.sourceforge.net/ "tktable"),
a spreadsheet-like [Tcl/Tk](http://www.tcl.tk/ "Tcl/Tk") widget
(included with the Windows binary distribution of R).
A call to `tclRequire` will indicate whether tktable is available for use:

    > tclRequire('Tktable')

Run
---

Load RSurvey in the current R session:

    > library(RSurvey)

Activate the main GUI:

    > OpenRSurvey()

Example data sets are provided in `inst/extdata`.
