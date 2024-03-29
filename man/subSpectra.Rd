% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subSpectra.R
\name{subSpectra}
\alias{subSpectra}
\title{Subtract Blank Peaks from Sample Spectra}
\usage{
subSpectra(dat, Blank_Var, Sample, Sub_Sample, showNeg = FALSE)
}
\arguments{
\item{dat}{The spectral data frame, containing \code{full_mz} in the first
column and intensity data in the subsequent columns.}

\item{Blank_Var}{A character string; the name of the blank column that will
be subtracted from the sample column.}

\item{Sample}{A character string; the name of the sample column that the
blank data will be subtracted from.}

\item{Sub_Sample}{A character string; the name of the column to be filled
with the subtracted spectrum.}

\item{showNeg}{Logical; if \code{showNeg = TRUE}, then negative values
produced by the subtraction will be kept in the dataset. If \code{showNeg =
FALSE}, then negative values created by the subtraction are set to 0 in the
output dataset.}
}
\value{
Returns a vector of intensity values that are filled into the
\code{Sub_Sample} column of the mapped data frame at the corresponding rows
of \code{full_mz}.
}
\description{
This function takes the intensity values in the blank column of the mapped
spectral data frame and subtracts them from the intensity values in the
sample column. If the intensity value in the blank spectrum is greater than
that in the sample spectrum, the intensity of the sample peak is given an
intensity of 0.
}
\examples{

## Load sample datasets "Master.rda"
data("Master")

## Make subtraction column for new data
Master <- transform(Master, "Subtracted" = 0)

## Subtract Blank1 from Before1
Master <- subSpectra(dat = Master, Blank_Var = "Blank1", 
                      Sample = "Before1", Sub_Sample = "Subtracted")


}
\references{
https://github.com/wesleyburr/subMaldi
}
\seealso{
\code{\link{createSpecDF}}, \code{\link{mapSpectrum}}
}
\author{
Kristen Yeh <kristenyeh@trentu.ca> Wesley Burr <wesleyburr@trentu.ca>
}
\keyword{manip}
\keyword{methods}
