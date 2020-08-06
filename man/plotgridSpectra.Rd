\name{plotgridSpectra}
\alias{plotgridSpectra}
\title{
Plot and Compare Multiple Spectra in a Grid
}
\description{
Plots 4 or 6 spectra in a grid format.
}
\usage{
plotgridSpectra(dat, mass_dat, spec1, spec2, spec3, spec4, spec5 = NULL, 
                spec6 = NULL, colour1 = wes_palette(n = 1, name = "GrandBudapest2"), 
                colour2 = wes_palette(n = 1, name = "GrandBudapest1"), 
                colour3 = wes_palette(n = 1, name = "Moonrise1"), 
                colour4 = wes_palette(n = 1, name = "Moonrise3"), 
                colour5 = wes_palette(n = 1, name = "IsleofDogs1"), 
                colour6 = wes_palette(n = 1, name = "FantasticFox1"), 
                span = 5, thresh = 0.1, lbls = FALSE, lbl.fmt = "\%3.4f",
                columns = 2, min_mz = 0, max_mz = 1000, 
                x_ticks = 100, intensity_scale = "free_y")
}
\arguments{
  \item{dat}{The name of the spectral data frame, containing the \emph{m/z} data in the first column.}
  \item{mass_dat}{A character string; the name of the column in \code{dat} containing the \emph{m/z} data.}
  \item{spec1}{A character string; the name of the column in \code{dat} containing the intensity data for the first spectrum that is to be plotted.}
  \item{spec2}{A character string; the name of the column in \code{dat} containing the intensity data for the second spectrum to be plotted.}
  \item{spec3}{A character string; the name of the column in \code{dat} containing the intensity data for the third spectrum to be plotted.}
  \item{spec4}{A character string; the name of the column in \code{dat} containing the intensity data for the fourth spectrum to be plotted.}
  \item{spec5}{A character string; the name of the column in \code{dat} containing the intensity data for the fifth spectrum to be plotted. Default = NULL.}
  \item{spec6}{A character string; the name of the column in \code{dat} containing the intensity data for the sixth spectrum to be plotted. Default = NULL.}
  \item{colour1}{A character string indicating the colour the first spectrum should be plotted in. Default = wes_palette(n=1, name = "GrandBudapest2").}
  \item{colour2}{A charater string indicating the colour of the second spectrum in the plot. Default = wes_palette(n=1, name = "GrandBudapest1").}
  \item{colour3}{A charater string indicating the colour of the third spectrum in the plot. Default = wes_palette(n=1, name = "Moonrise1").}
  \item{colour4}{A charater string indicating the colour of the fourth spectrum in the plot. Default = wes_palette(n=1, name = "Moonrise3").}
  \item{colour5}{A character string indicating the colour of the fifth spectrum in the plot. Default = wes_palette(n = 1, name = "IsleofDogs1").}
  \item{colour6}{A character string indicating the colour of the sixth spectrum in the plot. Default = wes_palette(n = 1, name = "FantasticFox1").}
  \item{span}{Single numeric value; the span of peak maximas in between each label. Default = 5 (ignores two peak maxima on either side of each label).}
  \item{thresh}{Single numeric value (0-100); the threshold of peak itensities which should be labelled. Default = 0.1.}
  \item{lbls}{Logical. If \code{lbls = TRUE}, labels indicating the \emph{m/z} value of each peak maxima within the indicated \code{span} will be included in the output plot. If \code{lbls = FALSE}, labels are not shown. Default = FALSE.}
   \item{lbl.fmt}{Character string in the format \code{"\%a.bf"}, where \emph{a} is the number of figures displayed to the left of decimal places in labels, and \emph{b} is the number of figures displayed to the right of decimal places in labels.  Default = "\%3.4f".}
  \item{columns}{Single numeric value; either 2 or 3. The number of columns spectral plots should be displayed in. Default = 2.}
  \item{min_mz}{Single numeric value; minimum \emph{m/z} value of the observed range. Default = 0.}
  \item{max_mz}{Single numeric value; upper end of \emph{m/z} range observed in spectra. Default = 1000.}
  \item{x_ticks}{Single numeric value; the space between ticks on x-axis. Default = 100.}
  \item{intensity_scale}{A character string; the method that should be used for determining the y-axis scales for each spectrum. If \code{method = "free_y"}, each spectrum will be plotted with its own intensity scale. If \code{method = "fixed"}, each spectrum will be plotted with the y-axis of the most intense spectrum in the set.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a line plot of the input spectra arranged in two columns.
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

## Plot four spectra
plotgridSpectra(dat = Master, mass_dat = "full_mz",
                spec1 = "Before1", spec2 = "Before1", 
                spec3 = "After1", spec4 = "After2", lbls = TRUE)

## Plot six spectra
plotgridSpectra(dat = Master, mass_dat = "full_mz",
                spec1 = "Blank1", spec2 = "Blank2", 
                spec3 = "Before1", spec4 = "Before2", spec5 = "After1", 
                spec6 = "After2", lbls = TRUE)

}

\keyword{ methods }
\keyword{ aplot }
