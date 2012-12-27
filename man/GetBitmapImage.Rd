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

\examples{
\donttest{image.obj <- GetBitmapImage("left")
tt <- tktoplevel(padx = 50, pady = 50)
button <- ttkbutton(tt, width = 2, image = image.obj)
tkpack(button)
}
}

\keyword{misc}
