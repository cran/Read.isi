\name{convert.isi}
\alias{convert.isi}
\title{Converts ISI-formatted code-books into SPSS-readable syntax}
\description{
Converts ISI-formatted code-books, associated with certain fixed-width data-files, and converts this to SPSS-readable syntax. Based on this syntax, SPSS is able to read the fixed-width data-files, associate variable names, value labels and missing values.  
}
\usage{
convert.isi(input.file, output.file, dat.file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input.file}{Location and name of the ISI-formatted .dct code-book file.}
  \item{output.file}{Location and name of the SPSS syntax file. }
  \item{dat.file}{Location of the fixed-width data-file. This file is not loaded by R, but the location is forwarded to the SPSS syntax.}
}

\value{
Returns a SPSS syntax file on the location indicated by `output.file'. This syntax is directly executable by SPSS and contains information needed to read fixed-width data, assign variable names and value labels, and missing values.
}
\references{More information on usage and background of the development can be found on www.rensenieuwenhuis.nl/r-project/my-functions/read-isi/}
\author{Rense Nieuwenhuis (contact@rensenieuwenhuis.nl)}

\examples{ 
## Examples can only be run using external code-book and data-files
## Basic conversion of the code-book to SPSS-executable syntax:
# convert.isi("filename-codebook.dct", "filename-spss-syntax.sps", "filename-data.dat")
}

\keyword{ connection } 
