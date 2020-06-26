\name{.normMethod_quantile}
\alias{.normMethod_quantile}
\title{
Normalize Distribution of Intensities
}
\description{
Called internally by \code{normSpectra()}. Normalizes the distributions of the values in each spectrum in a set.  Sorts the intensity data of each spectrum and evaluates the average intensity for each rank. The intensity values are then replaces with the averaged intensities, rearranged in their original order. Capable of analyzing 2-6 spectra at once.
}
\usage{
.normMethod_quantile(dat, mass_dat, spec1, spec2 = NULL, 
            spec3 = NULL, spec4 = NULL, spec5 = NULL, spec6 = NULL)
}
\arguments{
  \item{dat}{The name of the spectral data frame, containing \code{m/z} data in the first column and spectral intensity data in subsequent columns.}
  \item{mass_dat}{A character string; the name of the column in \code{dat} containing the \emph{m/z} data for the spectrum.}
  \item{spec1}{A character string; the name of the column in \code{dat} containing the intensity data for the spectrum to be analyzed.}
  \item{spec2}{A character string; the name of the column in \code{dat} containing the intensity data for the second spectrum to be normalized. Default = NULL.}
  \item{spec3}{A character string; the name of the column in \code{dat} containing the intensity data for the third spectrum to be normalized. Default = NULL.}
  \item{spec4}{A character string; the name of the column in \code{dat} containing the intensity data for the fourth spectrum to be normalized. Default = NULL.}
  \item{spec5}{A character string; the name of the column in \code{dat} containing the intensity data for the fifth spectrum to be normalized. Default = NULL.}
  \item{spec6}{A character string; the name of the column in \code{dat} containing the intensity data for the sixth spectrum to be normalized. Default = NULL.}
}
\details{

}
\value{
Returns a new data frame including the original \emph{m/z} data and normalized intensity data.
}
\references{
https://github.com/wesleyburr/subMaldi
}
\author{
Kristen Yeh <kristenyeh@trentu.ca>
Wesley Burr <wburr@trentu.ca>
}
\note{
%%  ~~further notes~~
}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Load sample dataset "Master.rda"
data("Master")

## Normalize the spectra "Before1" and "Before2"
# using the quantile method
ex <- normSpectra(dat = Master, mass_dat = "full_mz", method = "quantile",
                  spec1 = "Before1", spec2 = "Before2")

}

\keyword{ methods }
\keyword{ manip }
