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
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
https://github.com/wesleyburr/subMaldi
}
\examples{
data(Before2)
}
\keyword{datasets}
