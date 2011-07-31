\name{ManageData}

\alias{ManageData}

\title{Manage Data}

\description{
A \acronym{GUI} for managing, querying, and formatting data.
}

\usage{
ManageData(cols, vars, parent = NULL)
}

\arguments{
\item{cols}{list; see \sQuote{Value}.}
\item{vars}{list; see \sQuote{Value}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\details{
This \acronym{GUI} lets you:
(1) specify the names, measurement units, and decimal precision of variables;
(2) add new variables based on user defined functions
    (see \code{\link{EditFunction}}); and
(3) remove and (or) reorder variables in the data table.
}

\value{
Queries and sets the \code{cols} component of
\code{\link{Data}}, a list whose length is equal to
the current number of data variables. Each component in \code{cols} is linked
to a specific variable, and contains the following components:
\item{name}{character; variable name.}
\item{unit}{character; measurement units (optional); programmatic manipulation
  of measurement units is only supported for date and time variables.}
\item{id}{character; a unique identifier that is created from a
  string concatenation of \code{name} and \code{unit}.}
\item{fun}{character; the expression evaluated when computing the variables
  vector of values.}
\item{index}{integer; the variables component index number in the
  \code{data.raw} data frame (see \code{\link{ImportData}}). Only required
  for variables directly linked to data columns in \code{data.raw}.}
\item{class}{character; the data class of the vector object.}
\item{digs}{integer; the precision of the variable, defined as the number of
  fractional digits or decimal places (optional).}
\item{summary}{list; a summary of the variables descriptive statistics (see
  \code{\link{SummarizeData}}).}
\item{comments}{character; user comments.}
The \code{vars} object is a list with components:
\item{x, y, z, t, vx, vy}{integer; the index number of the corresponding
  state variable in \code{cols}.}
The \code{vars} component of \code{\link{Data}} is only updated to reflect
the removal and (or) reordering of variables (\code{\link{OpenRSurvey}}).
}

\seealso{
\code{\link{EditFunction}}
}

\author{Fisher, J.C.}

\examples{
\donttest{data(project)
ManageData(project$cols, project$vars)}
}

\keyword{misc}
