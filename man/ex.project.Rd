\name{ex.project}

\alias{ex.project}

\docType{data}

\title{Example Project}

\description{This is an example project for \pkg{RSurvey}.}

\usage{ex.project}

\format{
A list containing typical components in a project; these components are
described throughout the \pkg{RSurvey} documentation.
}

\examples{\donttest{
data(ex.project)
str(ex.project)

# Programmatically load a project into RSurvey using the following command:
Data(replace.all = ex.project)
}}

\keyword{datasets}
