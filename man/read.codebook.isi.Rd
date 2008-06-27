\name{read.codebook.isi}
\alias{read.codebook.isi}
\title{Reads ISI-formatted code-books}
\description{
Reads ISI-formatted code-books and converts the information contained in these. This function returns a list containing the information distilled from the code-book to other functions in the read.isi package. This is basically the core-function of the read.isi package.
}
\usage{
read.codebook.isi(input.file)
}
\arguments{
  \item{input.file}{External text file (.dct) containing ISI-formatted code-book}
}
\value{
Returns a list containing two items:
  \item{converted.codebook}{Contains indicators of the name of variables in the code-book ('name'), the starting position ('start') and number of positions taken by each variable in the file ('positions'), the values indicating missing values ($'missing_a' and'missing_b'$) the variable label ('variable.description'), and an indication from which variable the value labels need to be copied ('label.sub')}
  \item{converted.labels}{Contains for each value label in the code-book the name of the variable it belongs to ('name'), the value to which it is attached ('value') and the actual value labels ('label)}
 
}
\references{More information on usage and background of the development can be found on www.rensenieuwenhuis.nl/r-project/read-isi/}
\author{Rense Nieuwenhuis (contact@rensenieuwenhuis.nl)}

\examples{ }
\keyword{ connection }
