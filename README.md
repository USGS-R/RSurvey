RSurvey: A processing program for spatially distributed data
============================================================

Description
-----------

This [R](http://www.r-project.org/ "R") package is a processing program for
spatially distributed data. It features graphing tools, query building, and
polygon clipping. A graphical user interface (GUI) is provided.
The GUI requires R operate as an SDI application, using multiple
top-level windows for the console, graphics, and pager.

Files can be one of four types as indicated by their extension: tables
(_.txt_, _.csv_, _.dat_, or _.shp_), grids (_.grd_), polygons (_.ply_), or
project images (_.rda_). Tables (_.txt_, _.csv_, _.dat_) can be compressed
by [gzip](http://www.gzip.org/ "gzip") with additional extension _.gz_.
Shapefles (_.shp_) and interpolated grid files (_.grd_) are limited to data
export. Support for programmatic manipulation of measurement units is only
provided for date-time values; therefore, the bulk of unit consistency is tasked
to the user. Time zones, spatial datum's and projections are not supported.

The set of standards used for coding RSurvey is documented in
[Google's R Style Guide](http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html "Google's R Style Guide").

Installation
------------

If R is not already installed on your
computer, download and install the latest binary distribution from
[CRAN](http://cran.r-project.org/ "The Comprehensive R Archive Network").
Windows users should set R to operate as an SDI application during installation
by choosing to customize the startup options and specifying the SDI interface
(not the default).

Install required R packages from CRAN using a simple call to
`install.packages()`:

    install.packages(c('tcltk', 'sp', 'gpclib', 'rgl', 'MBA', 'tripack', 'RSurvey'))

Summary descriptions of
[sp](http://cran.r-project.org/web/packages/sp/index.html "sp"),
[gpclib](http://cran.r-project.org/web/packages/gpclib/index.html "gpclib"),
[rgl](http://cran.r-project.org/web/packages/rgl/index.html "rgl"),
[MBA](http://cran.r-project.org/web/packages/MBA/index.html "MBA"),
[tripack](http://cran.r-project.org/web/packages/tripack/index.html "tripack"), and
[RSurvey](http://cran.r-project.org/web/packages/RSurvey/index.html "RSurvey")
are available on CRAN.

Install the optional R package
[rgdal](http://cran.r-project.org/web/packages/rgdal/index.html "rgdal")
for shapefile support:

    install.packages('rgdal')

Support for displaying table data is provided by
[tktable](http://tktable.sourceforge.net/ "tktable"),
a spreadsheet-like [Tcl/Tk](http://www.tcl.tk/ "Tcl/Tk") widget
(included with the Windows binary distribution of R). 
A call to `tclRequire()` will indicate whether tktable is available for use:

    tclRequire('Tktable', warn = TRUE)

Running
-------

Load RSurvey in the current R session:

    library(RSurvey)

Activate the main GUI:

    OpenRSurvey()

Example data sets are provided in 
a binary project image file `data/project.rda` 
and text files `inst/extdata/` containing table and polygon data.
