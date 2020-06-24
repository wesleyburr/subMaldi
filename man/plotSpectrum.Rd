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
plotSpectrum(dat, mass_dat, intensity_dat, colour = "slategrey", 
            span = 5, thresh = 0.1, min_mz = 0, max_mz = 1000, 
            lbls = FALSE, x_ticks = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{The mapped spectral data frame, containing \code{full_mz} in the first column.}
  \item{mass_dat}{The name of the column in \code{dat} containing the \emph{m/z}data for the spectrum that is to be plotted. Should be input as dat$mass_column. By default, the mass column in a mapped data frame is "full_mz". If our spectral data frame is called my_spec, then our input for mass_dat = my_spec$full_mz.}
  \item{intensity_dat}{The name of the column in \code{dat} containing the intensity data for the spectrum that is to be plotted. Should be input as dat$intensity_column, as above for mass_dat.}
  \item{colour}{A character string; the name of the colour the spectrum should be plotted in. Default = "slategrey".}
  \item{span}{Single numeric value; the distance between peak maxima that are labelled if lbls = TRUE. Default = 5.}
  \item{thresh}{Single numeric value between 0 and 1; the intensity threshold of peaks in the plotted spectrum whih should be labelled. Default = 0.1.}
  \item{min_mz}{Single numeric value; minimum \emph{m/z} value of the observed range. Default = 0.}
  \item{max_mz}{Single numeric value; upper end of \emph{m/z} range to be observed in the plotted spectra. Default = 1000.}
  \item{lbls}{Logical; should labels indicating the \emph{m/z} of the most intense peaks be included in the plot? Default = FALSE. }
  \item{x_ticks}{Single numeric value; the increment between ticks on the x-axis. Default = 100.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a line plot of the input spectrum.}
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
## Plotting using the sample dataset "Before1.rda"
data("Before1")

plotSpectrum(dat = Before1, mass_dat = Before1$mass, 
              intensity_dat = Before1$Intensity)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
