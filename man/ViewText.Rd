\name{ViewText}

\alias{ViewText}

\title{View Text}

\description{
A \acronym{GUI} for viewing text.
}

\usage{
ViewText(txt, read.only = FALSE, win.title = "View Text", parent = NULL)
}

\arguments{
\item{txt}{character; the data used to populate the table.}
\item{read.only}{logical; specifies whether the text is read only.}
\item{win.title}{character; a string to display as the title of the dialog box.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\details{
Something
}

\author{J.C. Fisher}

\examples{\donttest{
txt <- c("\"Hills cherish the ambition",
         "    to turn into partial",
         "   differential equations\"",
         "",
         "        -Donald Hall")
new.txt <- ViewText(txt)

ViewText(txt, read.only = TRUE)
}}

\keyword{misc}
