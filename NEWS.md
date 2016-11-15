# RSurvey 0.8.3.9000

- Stop GUI from automatically launching during the **RSurvey** package load.

- Change function name from `OpenRSurvey` to `StartGui`.

- Remove vector components from state variables.

- Add CITATION file to be used by the `citation` function.

- Remove `BuildPackage` function

- Add Makefile for automating package-development tasks.

- Change NEWS file to markdown format.

- Change format for package version numbering from #.#-# to #.#.#

- Fix bug that prevents data import from text files.

# RSurvey 0.8-3

- Revised GUI in the `LoadPackages` function to make it more obvious that the
  missing packages are not necessary (thanks, @geneorama, #1).

- Changed `donttest` to `dontrun` in the man files.

# RSurvey 0.8-2

- Removed `require` for conditioning in package code, now using `requireNamespace`.

# RSurvey 0.8-1

- Fixed a bug in the `EditData` function that resulted from not updating the
  search index when a new cell selection was made.

- Improved handling of time zones in variables of class 'POSIXt'.

- Added the `ProgressBar` function to show the status for long-running operations.
  The progress bar is implemented when indexing search results.

- In the `EditData` function, the class of the object used to populate the
  spreadsheet-like table has been changed from a 'data.frame' to a 'list'
  with vector components of equal length.
  This change has resulted in a noticeable performance boost.

# RSurvey 0.8-0

- Fixed bug that converted all exported data fields to character class when
  writing Shapefiles and R data files.

- Added `ImportSpreadsheet` function, imports a worksheet from an Office open XML Workbook file (*.xlsx).

- Show individual observations and density estimate in histogram plot.

# RSurvey 0.7-9

- In the DESCRIPTION file, packages declared in the 'Depends' field were moved to the 'Imports' field.

- Redesigned GUI layout in the Format function.

# RSurvey 0.7-8

- Variables of class 'Date' are now handled correctly.

- Removed dependency on the **rgl** package.

- Improved performance for counting the number of lines in a file.

- Fixed bug that was preventing data imports using paste operation.

- Improved performance in the `EditData` function by removing its reliance on a
  copy of the data frame that was coerced into character-class components.

- The `base::format` function is now used when a format conversion specification string is not supplied.
  Previously defaulted to a general string representation; for example,
  numeric objects were formatted using the '%f' string.

- Removed `SummarizeVariable` function and replaced it with the more general
  `base::summary` and `utils::str` functions.

# RSurvey 0.7-5

- Graphics opened in a platform-independent way using the `grDevices::dev.new` function.

- Function name changes: `ViewText` to `EditText`, and `ViewData` to `EditData`.

- Calendar date-time variables are accurately converted to character strings
  using the `POSIXct2Character` function.

- Added ability to import, edit, and export comment string.

- Added ability to edit raw data in a spreadsheet with final edits are saved in a change log.

- Added GUI for importing data from an R package. R data sets may also be imported from a file.

- RSurvey project files are specified using the *.RData file extension.
  The *.rda extension is reserved for R data files.

- Functions `ReadFile` and `WriteFile` have been embedded into the
  `ImportText` and `ExportData` functions, respectively.

- Additional arguments have been added to the `ExportData` GUI.

- In the `ImportText` GUI, added decimal and encoding arguments, and custom
  entries for separator, NA strings, and comment parameters.

# RSurvey 0.7-4

- Main GUI is launched automatically when **RSurvey** is loaded.

- Added ability to view single variable in the `ImportText` GUI.

- It is no longer required to have a data set loaded to open the `DataManagement` GUI.

- Fixed bug with finding unique values in `EditFunction` GUI.

- The header line specifying variable names now comes before the format header line.
  Users need to update header lines in their input text files to reflect this change.

- Measurement units have been removed.
  Users need to remove unit headers from their input text files.

- Added GUI for sorting the processed data set.

# RSurvey 0.7-3

- Removed dependency on the **tripack** package because of its restricted
  license which explicitly forbids commercial use.
