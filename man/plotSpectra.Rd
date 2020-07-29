\name{plotSpectra}
\alias{plotSpectra}
\title{
Plot and Compare Multiple Spectra
}
\description{
Plots multiple spectra on the same \emph{x}-axis scale for easier comparison. Allows for 2 to 4 spectra per plot, stacked on top of one another.
}
\usage{
plotSpectra(dat, mass_dat, spec1, spec2, spec3 = NULL, spec4 = NULL, 
            colour1 = wes_palette(n=1, name = "GrandBudapest2"), 
            colour2 = wes_palette(n=1, name = "GrandBudapest1"), 
            colour3 = wes_palette(n=1, name = "Moonrise1"), 
            colour4 = wes_palette(n=1, name = "Moonrise3"), 
            span = 5, thresh = 0.1, lbls = FALSE, lbl.fmt = "\%3.4f", min_mz = 0, 
            max_mz = 1000, min_I = 0, max_I = max(dat), x_ticks = 100,   
            intensity_scale = "free_y")
}
\arguments{
  \item{dat}{The name of the spectral data frame, containing the \emph{m/z} data in the first column.}
  \item{mass_dat}{A character string; the name of the column in \code{dat} containing the \emph{m/z} data.}
  \item{spec1}{A character string; the name of the column in \code{dat} containing the intensity data for the first spectrum that is to be plotted.}
  \item{spec2}{A character string; the name of the column in \code{dat} containing the intensity data for the second spectrum to be plotted.}
  \item{spec3}{A character string; the name of the column in \code{dat} containing the intensity data for the third spectrum to be plotted. Default = NULL.}
  \item{spec4}{A character string; the name of the column in \code{dat} containing the intensity data for the fourth spectrum to be plotted. Default = NULL.}
  \item{colour1}{A character string indicating the colour the first spectrum should be plotted in. Default = wes_palette(n=1, name = "GrandBudapest2").}
  \item{colour2}{A charater string indicating the colour of the second spectrum in the plot. Default = wes_palette(n=1, name = "GrandBudapest1").}
  \item{colour3}{A charater string indicating the colour of the third spectrum in the plot. Default = wes_palette(n=1, name = "Moonrise1").}
  \item{colour4}{A charater string indicating the colour of the fourth spectrum in the plot. Default = wes_palette(n=1, name = "Moonrise3").}
  \item{span}{Single numeric value; the span of peak maximas in between each label. Default = 5 (ignores two peak maxima on either side of each label).}
  \item{thresh}{Single numeric value (0-100); the threshold of peak itensities which should be labelled. Default = 0.1.}
  \item{lbls}{Logical. If \code{lbls = TRUE}, labels indicating the \emph{m/z} value of each peak maxima within the indicated \code{span} will be included in the output plot. If \code{lbls = FALSE}, labels are not shown. Default = FALSE.}
  \item{lbl.fmt}{Character string in the format "\%a.bf", where \emph{a} is the number of figures displayed to the left of decimal places in labels, and \emph{b} is the number of figures displayed to the right of decimal places in labels.  Default = "\%3.4f".}
  \item{min_mz}{Single numeric value; minimum \emph{m/z} value of the observed range. Default = 0.}
  \item{max_mz}{Single numeric value; upper end of \emph{m/z} range observed in spectra. Default = 1000.}
  \item{min_I}{Single numeric value; minimum intensity value of the spectrum. Default = 0.}
  \item{max_I}{Single numeric value; upper end of the intensity range observed in spectra.}
  \item{x_ticks}{Single numeric value; the space between ticks on x-axis. Default = 100.}
  \item{intensity_scale}{A character string; the method that should be used for determining the y-axis scales for each spectrum. If \code{method = "free_y"}, each spectrum will be plotted with its own intensity scale. If \code{method = "fixed"}, each spectrum will be plotted with the y-axis of the most intense spectrum in the set.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a line plot of the input spectra.}
\references{
https://github.com/wesleyburr/subMaldi}
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
## Plotting using the sample dataset "Master.rda"
data("Master")
plotSpectra(dat = Master, mass_dat = "full_mz",
            spec1 = "Blank1", spec2 = "Blank2",
            intensity_scale = "free_y", lbls = TRUE)
}


\keyword{methods }
\keyword{ aplot }
