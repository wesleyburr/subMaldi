\name{Before2}
\alias{Before2}
\docType{data}
\title{
Pairwise Spectral Data Frame from Sample
}
\description{
Raw spectral data in binary dataframe. The first column contains the \emph{m/z} data for the spectrum, while the second contains the intensity data. Spectrum is obtained from a biological sample, prior to chemical intervention.
}
\usage{data("Before2")}
\format{
  A data frame with 213593 observations on the following 2 variables.
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
data(Before2)
}
\keyword{datasets}
