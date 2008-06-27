\name{clean}
\alias{clean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cleans character strings from spaces at the start and end}
\description{
Clean is a helper function for the read.isi package. It deletes spaces at the start and the end of a single character string.
}
\usage{
clean(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Single character string}
}
\details{
Can handle single character strings only. To use on a vector of characters x, use sapply(x, FUN=clean)
}
\value{
Returns a character string
}
\references{More information on usage and background of the development can be found on www.rensenieuwenhuis.nl/r-project/my-functions/read-isi/}
\author{Rense Nieuwenhuis (contact@rensenieuwenhuis.nl)}

\examples{

a <- "  messy string   "
clean(a)

b <- c("  First", "Second   ", " and last ...")
sapply(b, FUN=clean)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ connection }