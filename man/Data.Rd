\name{Data}

\alias{Data}

\docType{data}

\title{Set or Query Data and Parameters}

\description{
A function to set or query all data and parameters used in \pkg{RSurvey}.
}

\usage{
Data(option, value, clear.all = FALSE, clear.proj = FALSE,
     clear.data = FALSE)
}

\arguments{
\item{option}{character; the parameter name, see \sQuote{Parameters}.}
\item{value}{a parameter value specified for \code{option}.}
\item{clear.all}{logical; if \code{TRUE} all parameters are cleared from
  \code{Data}, its default is \code{FALSE}.}
\item{clear.proj}{logical; if \code{TRUE} basic \acronym{GUI} preferences will
  be saved and all other data removed, its default is \code{FALSE}.}
\item{clear.data}{logical; if \code{TRUE} only data sets will be removed,
  its default is \code{FALSE}.}
}

\section{Data}{
Imported raw data is saved to the data frame \code{data.raw}
(see \code{\link{ReadData}}). Processed point data is saved to the data frame
\code{data.pts} and interpolated surface data to the list \code{data.grd}
(see \code{\link{ProcessData}}).
}

\section{Parameters}{
Parameters undefined elsewhere in this documentation include:
  \describe{
    \item{\code{ver}}{character; the package version number.}
    \item{\code{win.loc}}{character; the default horizontal and vertical
      location for \acronym{GUI} placement in pixels.}
  }
}

\value{
If \code{value} is given the object specified by \code{option} is returned.
A \code{NULL} value is returned for objects not yet assigned a value and where
no default value is available.
}

\author{Fisher, J.C.}

\examples{
# To set a parameter
Data("test1", 3.14159265)
Data("test2", list(id = "PI", val = 3.14159265))
# To retrieve a parameter value
Data("test1")
Data("test2")
Data(c("test2", "id"))
Data(c("test2", "val"))
# To get all parameter values
d <- Data()
# To clear all parameters
Data(clear.all = TRUE)
}

\keyword{sysdata}
