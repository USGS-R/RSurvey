\name{GetBitmapImage}

\alias{GetBitmapImage}

\title{Icon Bitmap Image}

\description{
Create a small \acronym{Tk} bitmap image.
}

\usage{
GetBitmapImage(type)
}

\arguments{
\item{type}{character; icon image type, see \sQuote{Details}.}
}

\details{
Icon image types include: \var{left}, \var{right}, \var{up}, \var{down},
\var{top}, \var{bottom}, \var{upleft}, \var{upright}, \var{downleft},
\var{downright}, \var{next}, \var{previous}, \var{copy}, \var{paste},
\var{find}, \var{delete}, \var{view}, \var{info}, \var{plus}, \var{minus},
\var{print}, and \var{histogram}.
A recommended editor for bitmap design is Paul Obermeier's
\href{http://www.posoft.de/html/poBitmapMain.html}{poBitmap} tool.
}

\value{
An image of class \code{\link{tclObj}}.
}

\author{J.C. Fisher}

\seealso{
\code{\link{tkimage.create}}
}

\examples{\donttest{
image.names <- c("left", "right", "up", "down", "top", "bottom", "upleft", 
                 "upright", "downleft", "downright", "next", "previous", 
                 "copy", "paste", "find", "delete", "view", "info", "plus", 
                 "minus", "print", "histogram")
i <- 0
j <- 0
d <- 5
tt <- tktoplevel(padx = 50, pady = 50)
for (k in seq(along=image.names)) {
  img <- paste("img", k, sep = ".")
  but <- paste("but", k, sep = ".")
  assign(img, GetBitmapImage(image.names[k]))
  assign(but, ttkbutton(tt, width = 2, image = get(img)))
  tkgrid(get(but), row = i, column = j)
  i <- k \%/\% d
  j <- j + 1
  if (j > d - 1)
    j <- 0 
}
}}

\keyword{misc}
