\name{ChoosePalette}

\alias{ChoosePalette}

\title{Set Color Palette}

\description{
A \acronym{GUI} for viewing or manipulating color palettes.
}

\usage{
ChoosePalette(pal = terrain_hcl, n = 7L, parent = NULL)
}

\arguments{
\item{pal}{function; the initial palette, see \sQuote{Value} below.}
\item{n}{integer; the number of colors in the palette.}
\item{parent}{tkwin; the \acronym{GUI} parent window.}
}

\details{
Computes palettes based on the HCL (\code{\link{polarLUV}}) color space.
}

\value{
Returns a function that takes an integer argument and returns that number
of colors interpolating the given sequence.
}

\author{J.C. Fisher and A. Zeileis}

\references{
Zeileis A., Hornik K., and Murrell P., 2009, \emph{Escaping RGBland: Selecting
Colors for Statistical Graphics}, Computational Statistics & Data Analysis,
53, p. 3259-3270.
}

\seealso{
\code{\link{rainbow_hcl}}
}

\examples{
\donttest{pal <- ChoosePalette(terrain_hcl)
filled.contour(volcano, color.palette = pal, asp = 1)}
}

\keyword{misc}
