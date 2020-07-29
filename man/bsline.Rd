\name{bsline}
\alias{bsline}
\docType{data}
\title{
Raw Spectrum with Irregular Baseline
}
\description{
A spectrum acquired using ESI FT-ICR-MS with an irregular baseline.
}
\usage{data("bsline")}
\format{
  A data frame with 23807 observations on the following 2 variables.
  \describe{
    \item{\code{mass}}{a numeric vector. The \emph{m/z} data of the spectrum.}
    \item{\code{raw}}{A numeric vector. The raw, unfiltered or corrected intensity of the spectrum.}
  }
}

\references{
https://github.com/wesleyburr/subMaldi/
}
\examples{
data(bsline)
}
\keyword{datasets}
