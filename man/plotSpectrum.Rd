\name{plotSpectrum}
\alias{plotSpectrum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a Single Spectrum
}
\description{
Plots a single spectrum from a mapped spectral data frame.
}
\usage{
plotSpectrum(dat, mass_dat = full_mz, intensity_dat, colour = "slategrey", span = 5, thresh = 0.1, min_mz = 0, max_mz = 1000, lbls = FALSE, x_ticks = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{the mapped spectral data frame, containing \code{full_mz} in the first column.}
  \item{mass_dat}{a character string; the name of the column in \code{dat} containing the \emph{m/z}data for the spectrum that is to be plotted. Default = full_mz}
  \item{intensity_dat}{a character string; the name of the column in \code{dat} containing the intensity data for the spectrum that is to be plotted.}
  \item{colour}{a character string; the name of the colour the spectrum should be plotted in. Default = "slategrey".}
  \item{span}{single numeric value; the distance between peak maxima that are labelled if lbls = TRUE. Default = 5.}
  \item{thresh}{single numeric value between 0 and 1; the intensity threshold of peaks in the plotted spectrum whih should be labelled. Default = 0.1.}
  \item{min_mz}{single numeric value; minimum \emph{m/z} value of the observed range. Default = 0.}
  \item{max_mz}{single numeric value; upper end of \emph{m/z} range to be observed in the plotted spectra. Default = 1000.}
  \item{lbls}{logical; should labels indicating the \emph{m/z} of the most intense peaks be included in the plot? Default = FALSE. }
  \item{x_ticks}{single numeric value; the increment between ticks on the x-axis. Default = 100.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a line plot of the input spectrum.}
\references{
%% ~put references to the literature/web site here ~
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
## Plotting using the sample dataset "Before1.rda"
data("Before1")
plotSpectrum(dat = Before1, mass_dat = "mass", intensity_dat = "Intensity")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
