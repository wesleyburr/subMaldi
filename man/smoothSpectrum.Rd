% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smoothSpectrum.R
\name{smoothSpectrum}
\alias{smoothSpectrum}
\title{Smooth Noise in Spectrum}
\usage{
smoothSpectrum(
  dat,
  mass_dat,
  intensity_dat,
  method = NULL,
  p = NULL,
  n = NULL,
  m = 0,
  ts = 1
)
}
\arguments{
\item{dat}{The name of the spectral data frame, containing \code{m/z} data
in the first column and spectral intensity data in subsequent columns.}

\item{mass_dat}{A character string; the name of the column in \code{dat}
containing the \emph{m/z} data for the spectrum.}

\item{intensity_dat}{A character string; the name of the column in
\code{dat} containing the intensity data for the spectrum to be smoothed.}

\item{method}{A character string; the method to be used for smoothing.
Available methods include a Savitzky-Golay filter (\code{"sgolay"}) and a
moving average filter (\code{"mov_avg."})}

\item{p}{Single numeric value. If \code{method = "sgolay"}, the filter
order of smoothing. Default = NULL.}

\item{n}{Single odd numeric value. If \code{method = "sgolay"}, the length
of the smoothing filter. If \code{method = "mov_avg"}, the window span
size. Default = NULL.}

\item{m}{Single numeric value. If \code{method = "sgolay"}, returns the
m-th derivative of the filter coefficients. Default = 0.}

\item{ts}{Single numeric value. If \code{method = "sgolay"}, the time
scaling factor. Default = 1.}
}
\value{
Returns a new data frame containing the smoothed spectral data.
}
\description{
Offers two different methods for smoothing noise in raw spectral data: a
moving average filter and the Savitzky-Golay filter (1).
}
\examples{

## Load sample dataset "Before1.rda"
data("Before1")

## Testing method "sgolay"
# test <- smoothSpectrum(dat = Before1, mass_dat = "mass",
#                       intensity_dat = "Intensity", 
#                       method = "sgolay", p = 4, 
#                       n = 25, m = 0)

## Testing method "mov_avg"                        
# test <- smoothSpectrum(dat = Before1, mass_dat = Before1$mass,
#                       intensity_dat = Before1$Intensity,
#                       method = "mov_avg", n = 25)
#                  

}
\references{
https://github.com/wesleyburr/subMaldi (1) A. Savitzky, M.J.E.
Golay, Smoothing and differentiation of data by simplified least-squares
procedures, Anal. Chem. 36 (8) (1964) 1627-1639.
}
\author{
Kristen Yeh <kristenyeh@trentu.ca> Wesley Burr <wesleyburr@trentu.ca>
}
\keyword{manip}
\keyword{methods}
