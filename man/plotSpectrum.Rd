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
plotSpectrum(dat, spec, min_mz = 50, max_mz = 1100, x_ticks = 100, y_ticks = 25, xlbl = expression(italic("m/z")), ylbl = "Intensity", colour = "black", n_peaks = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{the mapped spectral data frame, containing \code{full_mz} in the first column.}
  \item{spec}{a character string; the name of the column in \code{dat} containing the intensity data for the spectrum that is to be plotted.}
  \item{min_mz}{single numeric value; minimum \emph{m/z} value of the observed range. Default = 50.}
  \item{max_mz}{single numeric value; upper end of \emph{m/z} range observed in spectra. Default = 1100.}
  \item{x_ticks}{single numeric value; the number of ticks and gridlines desired on the x-axis. Default = 100.}
  \item{y_ticks}{single numeric value; the number of ticks and gridlines desired on the y-axis. Default = 25.}
  \item{xlbl}{a character string; the label on the x-axis. Default = \emph{m/z}.}
  \item{ylbl}{a character string; the label on the y-axis. Default = Intensity.}
  \item{colour}{a character string; the name of the colour the spectrum should be plotted in. Default = "black".}
  \item{n_peaks}{single numeric value; the number of most intense peaks that should be labelled. Peaks are found by \code{find_top_peaks}, from which labels are generated from.}
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
## Plotting using the sample dataset "Subtracted_Sample_01.rda"
plotSpectrum(dat = test, spec = "Sample_01", 
             min_mz = 50, max_mz = 100, 
             x_ticks = 100, y_ticks = 25, 
             colour = "pink", n_peaks = 20)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
