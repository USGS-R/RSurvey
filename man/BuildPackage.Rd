\name{BuildPackage}

\alias{BuildPackage}

\title{R Package Builder in Windows OS}

\description{
Writes a series of commands for creating an \R package to a Windows batch file.
}

\usage{BuildPackage(check.cran = FALSE, no.vignettes = FALSE)}

\arguments{
\item{check.cran}{logical; if \code{TRUE}, uses command-line options similar to
  \acronym{CRAN}.}
\item{no.vignettes}{logical; if \code{TRUE}, uses command-line options to
  prevent build of vignettes.}
}

\details{
This function is intended for package developers and has only been tested on a
Windows operating system (\acronym{OS}) within the package source directory.
The user is tasked with executing the batch file (\file{.bat}).
}

\author{J.C. Fisher}

\examples{\dontrun{BuildPackage()}}

\keyword{internal}
