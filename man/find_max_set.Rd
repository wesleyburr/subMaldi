\name{find_max_set}
\alias{find_max_set}
\title{
Find Most Intense Peak of a Set
}
\description{
Analyzes spectral data and returns information about the most intense peak in all of the spectral set. Indicates the most intense spectrum, the \emph{m/z} value, and intesnity of the maximum. Userust specify at least two spectra. 
}
\usage{
find_max_set(dat, mass_dat, spec1, spec2, spec3 = NULL, 
              spec4 = NULL, spec5 = NULL, spec6 = NULL)
}
\arguments{
  \item{dat}{The name of the spectral data frame, containing \code{m/z} data in the first column and spectral intensity data in subsequent columns.}
  \item{mass_dat}{A character string; the name of the column in \code{dat} containing the \emph{m/z} data for the spectrum.}
  \item{spectra_cols}{A character vector; the names of the column in \code{dat} containing the intensity data for the spectra-of-interest.}
}

\value{Returns a data frame indidcating the peak maximum of the spectral set. The sample number, \emph{m/z} value, and intensity data are returned.
}
\references{
https://github.com/wesleyburr/subMaldi
}
\author{
Kristen Yeh <kristenyeh@trentu.ca>
Wesley Burr <wburr@trentu.ca>
Sophie Castel <sophie.castel@ontariotechu.net>
}

\examples{
## Load sample dataset "Master.rda"
data("Master")

## Find the most intense peak of six spectra
find_max_set(dat = Master, mass_dat = "full_mz", spectra_cols = c("After1", "Blank2", 
            "Before1", "Blank1", "After2", "Before2")
}

\keyword{ methods }
