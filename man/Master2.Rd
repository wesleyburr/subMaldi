\name{Master2}
\alias{Master2}
\docType{data}
\title{
Master Data Frame (Low Resolution)
}
\description{
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
A mapped spectral data frame created using \code{createSpecDF()} and \code{mapSpectrum()}. Contains the baseline corrected (by linear interpolation) intensity data for 6 spectra, with \emph{m/z} data recorded up to 2 decimal places. 
=======
A mapped spectral data frame created using \code{createSpecDF()} and \code{mapSpectrum()}. Contains the intensity data for 6 spectra, with \emph{m/z} data recorded up to 2 decimal places. 
>>>>>>> 3c63485... Added data documentation
=======
A mapped spectral data frame created using \code{createSpecDF()} and \code{mapSpectrum()}. Contains the baseline corrected (by linear interpolation) intensity data for 6 spectra, with \emph{m/z} data recorded up to 2 decimal places. 
>>>>>>> a20b013... Corrected master datasets & custom normalization
=======
A mapped spectral data frame created using \code{createSpecDF()} and \code{mapSpectrum()}. Contains the intensity data for 6 spectra, with \emph{m/z} data recorded up to 2 decimal places. 
>>>>>>> 27861a2... Added data documentation
=======
A mapped spectral data frame created using \code{createSpecDF()} and \code{mapSpectrum()}. Contains the baseline corrected (by linear interpolation) intensity data for 6 spectra, with \emph{m/z} data recorded up to 2 decimal places. 
>>>>>>> 33752d5... Corrected master datasets & custom normalization
=======
A mapped spectral data frame created using \code{createSpecDF()} and \code{mapSpectrum()}. Contains the intensity data for 6 spectra, with \emph{m/z} data recorded up to 2 decimal places. 
>>>>>>> 3c63485... Added data documentation
=======
A mapped spectral data frame created using \code{createSpecDF()} and \code{mapSpectrum()}. Contains the baseline corrected (by linear interpolation) intensity data for 6 spectra, with \emph{m/z} data recorded up to 2 decimal places. 
>>>>>>> a20b013... Corrected master datasets & custom normalization
}
\usage{data("Master2")}
\format{
  A data frame with 91346 observations on the following 7 variables.
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
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
\source{
Yeh, K., Stock N. L., Burr, W. & Stotesbury, T. Preliminary analysis of latent fingerprints recovered from underneath bloodstains using Matrix-Assisted Laser Desportion/Ionization Fourier-Transform Ion Cyclotron Resonance Mass Spectrometry Imaging (MALDI FT-ICR MSI). Forensic Chemistry (2020). In press.
=======
=======
>>>>>>> 27861a2... Added data documentation
=======
>>>>>>> 3c63485... Added data documentation
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
<<<<<<< HEAD
<<<<<<< HEAD
>>>>>>> 3c63485... Added data documentation
=======
\source{
Yeh, K., Stock N. L., Burr, W. & Stotesbury, T. Preliminary analysis of latent fingerprints recovered from underneath bloodstains using Matrix-Assisted Laser Desportion/Ionization Fourier-Transform Ion Cyclotron Resonance Mass Spectrometry Imaging (MALDI FT-ICR MSI). Forensic Chemistry (2020). In press.
>>>>>>> 5831098... Added pre-processing vignette
=======
>>>>>>> 27861a2... Added data documentation
=======
\source{
Yeh, K., Stock N. L., Burr, W. & Stotesbury, T. Preliminary analysis of latent fingerprints recovered from underneath bloodstains using Matrix-Assisted Laser Desportion/Ionization Fourier-Transform Ion Cyclotron Resonance Mass Spectrometry Imaging (MALDI FT-ICR MSI). Forensic Chemistry (2020). In press.
>>>>>>> 2931ed6... Added pre-processing vignette
=======
>>>>>>> 3c63485... Added data documentation
=======
\source{
Yeh, K., Stock N. L., Burr, W. & Stotesbury, T. Preliminary analysis of latent fingerprints recovered from underneath bloodstains using Matrix-Assisted Laser Desportion/Ionization Fourier-Transform Ion Cyclotron Resonance Mass Spectrometry Imaging (MALDI FT-ICR MSI). Forensic Chemistry (2020). In press.
>>>>>>> 5831098... Added pre-processing vignette
}
\references{
https://github.com/wesleyburr/subMaldi
}
\examples{
data(Master2)
data(Master)

## Separate samples
blanks <- select(Master2, "full_mz", "Blank1", "Blank2")
precon <- select(Master2, "full_mz", "Before1", "Before2")
postcon <- select(Master2, "full_mz", "After1", "After2")
}
\keyword{datasets}
