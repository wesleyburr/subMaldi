##' Plot a Single Spectrum
##' 
##' Plots a single spectrum from a mapped spectral data frame.
##' 
##' 
##' @param dat The mapped spectral data frame, containing \code{full_mz} in the
##' first column.
##' @param mass_dat The name of the column in \code{dat} containing the
##' \emph{m/z}data for the spectrum that is to be plotted. Should be input as
##' dat$mass_column. By default, the mass column in a mapped data frame is
##' "full_mz". If our spectral data frame is called my_spec, then our input for
##' mass_dat = my_spec$full_mz.
##' @param intensity_dat The name of the column in \code{dat} containing the
##' intensity data for the spectrum that is to be plotted. Should be input as
##' dat$intensity_column, as above for mass_dat.
##' @param colour A character string; the name of the colour the spectrum
##' should be plotted in.
##' @param span Single numeric value; the distance between peak maxima that are
##' labelled if lbls = TRUE. Default = 5.
##' @param thresh Single numeric value between 0 and 1; the intensity threshold
##' of peaks in the plotted spectrum which should be labelled. Default = 0.1.
##' @param min_mz Single numeric value; minimum \emph{m/z} value of the
##' observed range.
##' @param max_mz Single numeric value; upper end of \emph{m/z} range to be
##' observed in the plotted spectra.
##' @param min_I Single numeric value; minimum intensity value of the spectrum.
##' @param max_I Single numeric value; upper end of the intensity range
##' observed in spectra.
##' @param lbls Logical; should labels indicating the \emph{m/z} of the most
##' intense peaks be included in the plot?
##' @param lbl.fmt Character string in the format "\%a.bf", where \emph{a} is
##' the number of figures displayed to the left of decimal places in labels,
##' and \emph{b} is the number of figures displayed to the right of decimal
##' places in labels.
##' @param x_ticks Single numeric value; the increment between ticks on the
##' x-axis.
##' @return Returns a line plot of the input spectrum.
##' @author Kristen Yeh <kristenyeh@@trentu.ca> Wesley Burr <wburr@@trentu.ca>
##' @references https://github.com/wesleyburr/subMaldi
##' @keywords methods aplot
##' @examples
##' 
##' ## Plotting using the sample dataset "Before1.rda"
##' data("Before1")
##' 
##' plotSpectrum(dat = Before1, mass_dat = Before1$mass, 
##'               intensity_dat = Before1$Intensity)


# -----------------------------------------------------------------------
# Last Updated: July 15, 2020
# Author: Kristen Yeh
# Title: subMALDI: Plot a Single Spectrum
# -----------------------------------------------------------------------


plotSpectrum <- function(dat, 
                         mass_dat,
                         intensity_dat,
                         colour = "black",
                         span = 5,
                         thresh = 0.1,
                         min_mz = min(mass_dat),
                         max_mz = max(mass_dat),
                         min_I = 0,
                         max_I = max(intensity_dat),
                         lbls = FALSE,
                         lbl.fmt = "%3.4f",
                         x_ticks = 500) {
  mass <- mass_dat
  
  # If lbls = TRUE
  if(lbls == TRUE){
    tst <- .test_lbl(mass_dat = mass, lbl.fmt = lbl.fmt)
    # Check that labels have less than or equal to decimal places as data
    if(tst == TRUE){
      stop('Label format indicates more decimal places than given in data.') }
    else{
      options(warn=-1)
      
      ggplot(dat, aes(mass_dat, intensity_dat), group = 1) +
        geom_line(col = colour) +
        labs(x = expression(italic("m/z")), y = "Intensity") +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_x_continuous( limits = c(min_mz, max_mz), 
                            breaks = seq(min_mz, max_mz, by = x_ticks)) +
        scale_y_continuous(limits = c(min_I, max_I)) +
        theme_bw() + theme( panel.border = element_blank(),
                            strip.background = element_blank(),
                            strip.text.x = element_blank(),
                            axis.line = element_line(colour = "grey85", 
                                                     size = 0.5)) +
        stat_peaks(aes(x = mass_dat, y = intensity_dat, group = 1), 
                   ignore_threshold = thresh, x.label.fmt = lbl.fmt,  span = span, 
                   geom = "text", check_overlap = TRUE, color = "black", cex = 3.0) 
    }
  } else if(lbls == FALSE){
    options(warn=-1)
    
    ggplot(dat, aes(mass_dat, intensity_dat), group = 1) +
      geom_line(col = colour) +
      labs(x = expression(italic("m/z")), y = "Intensity") +
      theme(axis.text.x = element_text(angle = 90)) +
      scale_x_continuous( limits = c(min_mz, max_mz), 
                          breaks = seq(min_mz, max_mz, by = x_ticks)) +
      scale_y_continuous(limits = c(min_I, max_I)) +
      theme_bw() + theme( panel.border = element_blank(),
                          strip.background = element_blank(),
                          strip.text.x = element_blank(),
                          axis.line = element_line(colour = "grey85", 
                                                   size = 0.5))
  }
}

# -----------------------------------------------------------------------


