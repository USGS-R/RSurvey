\name{Data}

\alias{Data}

\title{Set or Query Data and Parameters}

\description{
A function to set or query parameters and their attributes.
}

\usage{
Data(option, value, which.attr = NULL, clear.proj = FALSE, clear.data = FALSE,
     replace.all = NULL)
}

\arguments{
\item{option}{character; the parameter name, see \sQuote{Parameters}.}
\item{value}{a parameter value specified for \code{option} (optional).}
\item{which.attr}{character, a non-empty character string specifying which
                  attribute is to be accessed (optional).}
\item{clear.proj}{logical; if \code{TRUE} basic \acronym{GUI} preferences will
  be saved and all other data removed; its default is \code{FALSE}.}
\item{clear.data}{logical; if \code{TRUE} only data sets will be removed,
  its default is \code{FALSE}.}
\item{replace.all}{list; a replacement list of parameter values (optional).}
}

\section{Data}{
Imported raw data is saved to the data frame \code{data.raw},
see \code{\link{ImportText}}. Processed point data is saved to the data frame
\code{data.pts} and interpolated surface data is saved to the list
\code{data.grd}, see \code{\link{ProcessData}}.
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
no default value is available. Default values are specified internally within
this function.
}

\author{J.C. Fisher}

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
# To remove all saved parameter values
Data(replace.all = list())
# To recover saved parameter values
Data(replace.all = d)
}

\keyword{sysdata}
