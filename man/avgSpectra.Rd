\name{avgSpectra}
\alias{avgSpectra}
\title{
Average Spectral Replicates
}
\description{
Combines spectral replicates either by averaging (method = "mean") or summing (method = "sum) the intensity values across each row representing a mass-to-charge value in \code{full_mz}. 
}
\usage{
avgSpectra(dat, method = "mean", spectra_cols)
}
\arguments{
  \item{dat}{The mapped spectral data frame, containing \code{full_mz} in the first column.}
  \item{method}{A character string; the method used to combine the spectra. Methods include "sum" and "mean". Default = "mean."}
  \item{spectra_cols}{A character vector; the names of the column in \code{dat} containing the intensity data for the spectra-of-interest.}.}
}

\value{
Returns a new column in the input data frame containing the averaged intensity data.
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

## Average blank spectrum 1 and 2 using the method "mean"
ex <- avgSpectra(Master, method = "mean", spectra_cols = c("Blank1", "Blank2"))

## Average blank spectrum 1 and 2 using the method "sum"
ex <- avgSpectra(Master, method = "sum", spectra_cols = c("Blank1", "Blank2"))
}
\keyword{ manip }
\keyword{ methods }
