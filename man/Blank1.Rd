\name{Blank1}
\alias{Blank1}
\docType{data}
\title{
Pairwise Spectral Data from Matrix Blank
}
\description{
Raw spectral data in binary dataframe. The first column contains the \emph{m/z} data for the spectrum, while the second contains the intensity data. Spectrum is a MALDI matrix blank.
}
\usage{data("Blank1")}
\format{
  A data frame with 105717 observations on the following 2 variables.
  \describe{
    \item{\code{mass}}{A numeric vector; the \emph{m/z} data for the spectrum.}
    \item{\code{Intensity}}{A numeric vector; the intensity data for the spectrum.}
  }
}
\source{
Yeh, K., Stock N. L., Burr, W. & Stotesbury, T. Preliminary analysis of latent fingerprints recovered from underneath bloodstains using Matrix-Assisted Laser Desportion/Ionization Fourier-Transform Ion Cyclotron Resonance Mass Spectrometry Imaging (MALDI FT-ICR MSI). Forensic Chemistry (2020). In press.
}
\references{
https://github.com/wesleyburr/subMaldi
}
\examples{
data(Blank1)
}
\keyword{datasets}
