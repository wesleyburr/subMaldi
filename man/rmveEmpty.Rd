\name{rmveEmpty}
\alias{rmveEmpty}
\title{
Remove Empty Rows
}
\description{
Data frames created using createSpecDF can have thousands to millions of rows, especially when dealing with high resolution mass spectrometry data. This can be quite taxing to the speed of executing certain functions, especially when it comes to visualizing data. This function removes empty rows to reduce the computational load and increase the ease of use of functions on the data frame. Any row where all elements (but the first, which is \code{full_mz}) equal 0 are removed from the data frame.
}
\usage{
rmveEmpty(dat)
}
\arguments{
  \item{dat}{The spectral data frame, containing \code{full_mz} in the first column, to be trimmed of all-zero rows.}
}

\value{
Returns the input data frame without its all-zero rows.
}
\references{
https://github.com/wesleyburr/subMaldi
}
\author{
Kristen Yeh <kristenyeh@trentu.ca>
}
\note{
Avoid truncating a dataframe until all samples to be compared have been mapped. 
}

\seealso{
\code{\link{createSpecDF}},\code{\link{mapSpectrum}}
}
\examples{
## Load sample dataset "Mastre.rda"
data("Master")

## Select only the spectra "Before1" and "Before2"
ex <- select(Master, "full_mz", "Before1", "Before2")

## Use rmveEmpty(x) on those data frames to reduce computational load for use with
# other functions and packages
ex <- rmveEmpty(dat = ex)

}
\keyword{ manip }
\keyword{ methods }
