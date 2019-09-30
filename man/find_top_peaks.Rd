\name{find_top_peaks}
\alias{find_top_peaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the Most Intense Peaks
}
\description{
Finds the most intense peaks, within an 100 bin range. This function is called by \code{plotSpectrum} and \code{plotSpectra} to generate the\emph{m/z} labels for the desired spectral peaks.
}
\usage{
find_top_peaks(x, y, n_peaks)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the column of the mapped spectral data frame containing the \emph{m/z} values for the given spectrum. Should be contained in the first column of the spectral data frame, \code{full_mz}.}
  \item{y}{the column of the mapped spectral data frame containing the intesnity values for the given spectrum.}
  \item{n_peaks}{single numeric value; the number of peaks that should be labelled.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a matrix of 3 columns containing the \emph{m/z}, intensity, and index data for the most intense peaks.}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Wesley Burr <wburr@trentu.ca>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plotSpectrum}}, \code{\link{plotSpectra}}
}
\examples{
## This is what is called internally by plotSpectrum and plotSpectra
label_peaks <- find_top_peaks(x = dat$full_mz,
                                  y = dat[[spec1]],
                                  n_peaks = n_peaks)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
