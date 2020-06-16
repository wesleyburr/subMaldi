\name{Master}
\alias{Master}
\docType{data}
\title{
Master Data Frame (High Resolution)
}
\description{
<<<<<<< HEAD
A mapped spectral data frame created using \code{createSpecDF()} and \code{mapSpectrum()}. Contains the baseline corrected (by linear interpolation) intensity data for 6 spectra, with \emph{m/z} data recorded up to 4 decimal places. 
=======
A mapped spectral data frame created using \code{createSpecDF()} and \code{mapSpectrum()}. Contains the intensity data for 6 spectra, with \emph{m/z} data recorded up to 4 decimal places. 
>>>>>>> 3c63485... Added data documentation
}
\usage{data("Master")}
\format{
  A data frame with 980506 observations on the following 7 variables.
  \describe{
    \item{\code{full_mz}}{A numeric vector; the \emph{m/z} data for all spectra in the data frame.}
    \item{\code{Blank1}}{A numeric vector; the intensity data for the first spectrum of the dataset. A MALDI matrix blank spectrum.}
    \item{\code{Blank2}}{A numeric vector; the intensity data for the second spectrum in the dataset. A MALDI matrix blank spectrum.}
    \item{\code{Before1}}{A numeric vector; the intensity data for the third spectrum in the dataset. Spectrum obtained prior to chemical intervention.}
    \item{\code{Before2}}{A numeric vector; the intensity data for the fourth spectrum in the dataset. Spectrum obtained prior to chemical intervention.}
    \item{\code{After1}}{A numeric vector; the intensity data for the fifth spectrum in the dataset. Spectrum obtained after chemical intervention.}
    \item{\code{After2}}{A numeric vector; the intensity data for the sixth spectrum in the dataset. Spectrum obtained after chemical intervention.}
  }
}
<<<<<<< HEAD

\source{
Yeh, K., Stock N. L., Burr, W. & Stotesbury, T. Preliminary analysis of latent fingerprints recovered from underneath bloodstains using Matrix-Assisted Laser Desportion/Ionization Fourier-Transform Ion Cyclotron Resonance Mass Spectrometry Imaging (MALDI FT-ICR MSI). Forensic Chemistry (2020). In press.
=======
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{

>>>>>>> 3c63485... Added data documentation
}
\references{
https://github.com/wesleyburr/subMaldi
}
\examples{
data(Master)

## Separate samples
blanks <- select(Master, "full_mz", "Blank1", "Blank2")
precon <- select(Master, "full_mz", "Before1", "Before2")
postcon <- select(Master, "full_mz", "After1", "After2")
}
\keyword{datasets}
