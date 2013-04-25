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
txt <- paste("Unicorns envy their cousin",
             "horses a smooth forehead.",
             "Horses weep for lack of horns.",
             "",
             "Hills cherish the ambition",
             "to turn into partial",
             "differential equations,",
             "",
             "which want to be poems, or dogs,",
             "or the Pacific Ocean,",
             "or whiskey, or a gold ring.",
             "",
             "The man wearing the noose",
             "envies an other who fondles",
             "a pistol in a motel room.",
             "",
             "-Donald Hall", sep="\n")
ViewText(txt, read.only = TRUE)
new.txt <- ViewText(txt)
}}

\keyword{misc}
