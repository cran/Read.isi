\name{read.isi}
\alias{read.isi}
\title{Reads ISI-formatted code-books into R-Project}
\description{
Reads ISI-formatted code-books and uses this to read fixed-width formatted data-files into R-Project.
}
\usage{
read.isi(input.file, dat.file, add.missings = TRUE, add.value.labels = TRUE, ...)
}
\arguments{
  \item{input.file}{Location of the ISI-formatted .dct code-book file.}
  \item{dat.file}{Location of the fixed-width data-file to load.}
  \item{add.missings}{Should value labels indicating missing values be transformed to NA? Defaults to TRUE.}
  \item{add.value.labels}{Should value labels be appended to the variables? Defaults to TRUE.}
  \item{\dots}{Further arguments transferred to read.fwf and read.table, such as 'n' and 'skip' (see below).}
}

\value{
Returns a data.frame containing the data stored in 'dat.file'. Variables are either numeric vectors or factors. 
}
\references{More information on usage and background of the development can be found on www.rensenieuwenhuis.nl/r-project/my-functions/read-isi/}
\author{Rense Nieuwenhuis (contact@rensenieuwenhuis.nl)}

\section{Warning }{Using read.isi is computationally demanding; older systems with low levels of available RAM are easily brought to a halt. Instead use convert.isi to convert code-book to SPSS-interpretable syntax, or use 'n' and 'skip' sub-commands to subsequently read parts of the data-file (i.e. 1000 records at a time)}

\examples{ 
## Examples can only be run using external code-book and data-files
## Basic reading of data based on code-book:
# read.isi("filename-codebook.dct", "filename-data.dat")

## Value labels and missing values, as indicated in the code-book, are not assigned:
# read.isi("filename-codebook.dct", "filename-data.dat", value.labels=FALSE, missings=FALSE)

## Only records 501 thru 1500 are read, if available
# read.isi("filename-codebook.dct", "filename-data.dat", skip=500, n=1000)

}

\keyword{ connection }
