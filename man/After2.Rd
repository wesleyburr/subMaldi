\name{After2}
\alias{After2}
\docType{data}
\title{
Pairwise Spectral Data Frame from Sample
}
\description{
Raw spectral data in binary dataframe. The first column contains the \emph{m/z} data for the spectrum, while the second contains the intensity data. Spectrum is obtained from a biological sample, after chemical intervention.
}
\usage{data("After2")}
\format{
  A data frame with 176209 observations on the following 2 variables.
  \describe{
    \item{\code{mass}}{A numeric vector; the \emph{m/z} data for the spectrum.}
    \item{\code{Intensity}}{A numeric vector; the intensity data for the spectrum.}
  }
}

\references{
https://github.com/wesleyburr/subMaldi
}
\examples{
data(After2)
}
\keyword{datasets}
