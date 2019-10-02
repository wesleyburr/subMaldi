\name{plotSpectra}
\alias{plotSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot and Compare Multiple Spectra
}
\description{
Plots two spectra for comparison purposes. Two different methods for comparison are available: overlaid and stacked.
}
\usage{
plotSpectra(dat, spec1, spec2, method, min_mz = 50, max_mz = 1100, x_ticks = 100, y_ticks = 25, sub_ttl1 = "Original", sub_ttl2 = "Subtracted", xlbl = expression(italic("m/z")), ylbl = "Intensity", colour = "black", overlay_colour = "blue", n_peaks = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{the mapped spectral data frame, containing \code{full_mz} in the first column.}
  \item{spec1}{a character string; the name of the column in \code{dat} containing the intensity data for the spectrum that is to be plotted.}
  \item{spec2}{a character string; the name of the column in \code{dat} containing the intensity data for the spectrum that is to be overlaid (if method = "overlay").}
  \item{method}{a character string; the method used to compare spectra. Methods include "overlay" and "stacked".}
  \item{min_mz}{single numeric value; minimum \emph{m/z} value of the observed range. Default = 50.}
  \item{max_mz}{single numeric value; upper end of \emph{m/z} range observed in spectra. Default = 1100.}
  \item{x_ticks}{single numeric value; the number of ticks and gridlines desired on the x-axis. Default = 100.}
  \item{y_ticks}{single numeric value; the number of ticks and gridlines desired on the y-axis. Default = 25.}
  \item{sub_ttl1}{a character string; the title for the plot of \code{spec1}.}
  \item{sub_ttl2}{a character string; the title for the plot of \code{spec2}.}
  \item{xlbl}{a character string; the label on the x-axis. Default = \emph{m/z}.}
  \item{ylbl}{a character string; the label on the y-axis. Default = Intensity.}
  \item{colour}{a character string; the name of the colour the spectrum should be plotted in. Default = "black".}
  \item{overlay_colour}{a character string; the name of the colour \code{spec2} should be plotted in. If method = "overlay", then this colour will be shown at half opacity. If method = "stacked", this is simply the colour that the spectrum will be plotted in. Default = "blue".}
  \item{n_peaks}{single numeric value; the number of most intense peaks that should be labelled. Peaks are found by \code{find_top_peaks}, from which labels are generated from.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a line plot of the input spectra.}
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Plotting using the sample dataset "Subtracted_Sample_01.rda"
## Method = "overlay"
plotSpectra(dat = test, spec1 = "Sample_01", 
                        spec2 = "SubSamp_Avg", 
                        method = "overlay")

## Method = "stacked"
plotSpectra(dat = test, spec1 = "Sample_01", 
                        spec2 = "SubSamp_Avg", 
                        method = "stacked", 
                        n_peaks = 10)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
