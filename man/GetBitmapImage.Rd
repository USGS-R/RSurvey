\name{GetBitmapImage}

\alias{GetBitmapImage}

\title{Create Icon Bitmap Image}

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

\examples{
\donttest{types <- c("left", "right", "up", "down", "top", "bottom", "upleft", "upright",
           "downleft", "downright", "next", "previous", "copy", "paste", "find",
           "delete", "view", "info", "plus", "minus", "print", "histogram")
fun <- function(k) print(types[k])
tt <- tcltk::tktoplevel(padx = 50, pady = 50)
i <- 0
j <- 0
d <- 5
for (k in seq(along = types)) {
  img <- paste("img", k, sep = ".")
  but <- paste("but", k, sep = ".")
  assign(img, GetBitmapImage(types[k]))
  assign(but, tcltk::ttkbutton(tt, width = 2, image = get(img),
                               command = local({k <- k; function() fun(k)})))
  tcltk::tkgrid(get(but), row = i, column = j, padx = 5, pady = 5)
  i <- k \%/\% d
  j <- ifelse(j < d - 1, j + 1, 0)
}
}}

\keyword{misc}
