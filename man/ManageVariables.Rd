\name{ManageVariables}

\alias{ManageVariables}

\title{Manage Variables}

\description{
A \acronym{GUI} for managing variables in the data table.
}

\usage{
ManageVariables(cols, vars, query, changelog, parent = NULL)
}

\arguments{
\item{cols}{list; see \sQuote{Value}.}
\item{vars}{list; see \sQuote{Value}.}
\item{query}{character; see \sQuote{Value}.}
\item{changelog}{data.frame; see \sQuote{Value}.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\details{
This \acronym{GUI} lets you:
(1) specify the names and format of variables;
(2) add new variables based on user defined functions,
    see \code{\link{EditFunction}};
(3) display data in a spreadsheet, see \code{\link{EditData}}; and
(4) remove and (or) reorder variables in the data table.
}

\value{
Returns a list with components \code{cols} and \code{vars}.
The \code{cols} object is a list whose length is equal to
the current number of data variables. Each component in \code{cols} is linked
to a specific variable, and contains the following components:
\item{name}{character; variable name.}
\item{format}{character; the conversion specification format (optional).}
\item{id}{character; a unique identifier that is created from \code{name}.}
\item{fun}{character; the expression evaluated when computing the variables
  vector of values.}
\item{index}{integer; the variables component index number in the
  \code{data.raw} data frame, see \code{\link{ImportData}}. Only required
  for variables directly linked to data columns in \code{data.raw}.}
\item{class}{character; the data class of the vector object.}
\item{summary}{list; a summary of the variables descriptive statistics (see
  \code{\link{summary}}).}
\item{comments}{character; user comments.}
The \code{vars} object is a list with components:
\item{x, y, z, vx, vy, sort.on}{integer; the index number of the corresponding
  state variable in \code{cols}. These indexes are updated to reflect
  the removal and (or) reordering of variables in \code{cols}.}
\item{query}{character; if required, variable names are updated.}
\item{changelog}{data.frame; if required, names in \code{variable} component are
  updated.}
}

\author{J.C. Fisher}

\examples{
\donttest{data(ex.project)
Data(replace.all = ex.project)
ManageVariables(ex.project$cols, ex.project$vars, ex.project$query,
                ex.project$changelog)}
}

\keyword{misc}
