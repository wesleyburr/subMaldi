##' Plot and Compare Multiple Spectra in a Grid
##' 
##' Plots 4 or 6 spectra in a grid format.
##' 
##' 
##' @param dat The name of the spectral data frame, containing the \emph{m/z}
##' data in the first column.
##' @param mass_dat A character string; the name of the column in \code{dat}
##' containing the \emph{m/z} data.
##' @param spec1 A character string; the name of the column in \code{dat}
##' containing the intensity data for the first spectrum that is to be plotted.
##' @param spec2 A character string; the name of the column in \code{dat}
##' containing the intensity data for the second spectrum to be plotted.
##' @param spec3 A character string; the name of the column in \code{dat}
##' containing the intensity data for the third spectrum to be plotted.
##' @param spec4 A character string; the name of the column in \code{dat}
##' containing the intensity data for the fourth spectrum to be plotted.
##' @param spec5 A character string; the name of the column in \code{dat}
##' containing the intensity data for the fifth spectrum to be plotted. Default
##' = NULL.
##' @param spec6 A character string; the name of the column in \code{dat}
##' containing the intensity data for the sixth spectrum to be plotted. Default
##' = NULL.
##' @param colour1 A character string indicating the colour the first spectrum
##' should be plotted in.
##' @param colour2 A charater string indicating the colour of the second
##' spectrum in the plot.
##' @param colour3 A charater string indicating the colour of the third
##' spectrum in the plot.
##' @param colour4 A charater string indicating the colour of the fourth
##' spectrum in the plot.
##' @param colour5 A character string indicating the colour of the fifth
##' spectrum in the plot.
##' @param colour6 A character string indicating the colour of the sixth
##' spectrum in the plot.
##' @param span Single numeric value; the span of peak maximas in between each
##' label. Default = 5 (ignores two peak maxima on either side of each label).
##' @param thresh Single numeric value (0-100); the threshold of peak
##' itensities which should be labelled. Default = 0.1.
##' @param lbls Logical. If \code{lbls = TRUE}, labels indicating the
##' \emph{m/z} value of each peak maxima within the indicated \code{span} will
##' be included in the output plot. If \code{lbls = FALSE}, labels are not
##' shown. Default = FALSE.
##' @param lbl.fmt Character string in the format \code{"%a.bf"}, where
##' \emph{a} is the number of figures displayed to the left of decimal places
##' in labels, and \emph{b} is the number of figures displayed to the right of
##' decimal places in labels.
##' @param columns Single numeric value; either 2 or 3. The number of columns
##' spectral plots should be displayed in.
##' @param min_mz Single numeric value; minimum \emph{m/z} value of the
##' observed range.
##' @param max_mz Single numeric value; upper end of \emph{m/z} range observed
##' in spectra.
##' @param min_I Single numeric value; minimum intensity value to be plotted.
##' @param max_I Single numeric value; upper end of the intensity range to be
##' plotted.
##' @param x_ticks Single numeric value; the space between ticks on x-axis.
##' @param intensity_scale A character string; the method that should be used
##' for determining the y-axis scales for each spectrum. If \code{method =
##' "free_y"}, each spectrum will be plotted with its own intensity scale. If
##' \code{method = "fixed"}, each spectrum will be plotted with the y-axis of
##' the most intense spectrum in the set.
##' @return Returns a line plot of the input spectra arranged in two columns.
##' @author Kristen Yeh <kristenyeh@@trentu.ca> Wesley Burr <wburr@@trentu.ca>
##' @references https://github.com/wesleyburr/subMaldi
##' @keywords methods aplot
##' @examples
##' 
##' ## Load sample dataset "Master.rda"
##' data("Master")
##' 
##' ## Plot four spectra
##' plotgridSpectra(dat = Master, mass_dat = "full_mz",
##'                 spec1 = "Before1", spec2 = "Before1", 
##'                 spec3 = "After1", spec4 = "After2", lbls = TRUE)
##' 
##' ## Plot six spectra
##' plotgridSpectra(dat = Master, mass_dat = "full_mz",
##'                 spec1 = "Blank1", spec2 = "Blank2", 
##'                 spec3 = "Before1", spec4 = "Before2", spec5 = "After1", 
##'                 spec6 = "After2", lbls = TRUE)
##' 


# -----------------------------------------------------------------------
# Last Updated: July 15, 2020
# Author: Kristen Yeh
# Title: subMALDI: Plot and Compare Multiple Spectra in a Grid
# -----------------------------------------------------------------------


plotgridSpectra<- function(dat, mass_dat, 
                           spec1, spec2,
                           spec3, spec4,
                           spec5 = NULL, spec6 = NULL,
                           colour1 = "steelblue1",
                           colour2 = "lightpink1",
                           colour3 = "coral",
                           colour4 = "goldenrod1",
                           colour5 = "gold",
                           colour6 = "seagreen3",
                           span = 5,
                           thresh = 0.1,
                           lbls = FALSE, 
                           lbl.fmt = "%3.4f", 
                           columns = 2,
                           min_mz = 0,
                           max_mz = 10000,
                           min_I = 0,
                           max_I = NULL,
                           x_ticks = 500,
                           intensity_scale = "free_y"){
  options(warn=-1)
  mass <- dat[[mass_dat]]
  
  if(min_mz < min(mass_dat)){
    min_mz <- floor(min(mass)) }
  
  if(max_mz > max(mass_dat)){
    max_mz <- ceiling(max(mass)) }
  
  spec <- list(spec1, spec2, spec3, spec4, spec5, spec6)
  nulls <- c()
  
  for(i in 1:length(spec)){
    if(is.null(spec[[i]])){
      null <- i
      nulls <- append(nulls, null, after = length(nulls)) } 
  }
  
  n <- length(spec)-length(nulls)
  
  if(n < 4){
    stop('Less than 4 input spectra. Please use plotSpectra().')
  }
  
  if(n == 4){
    # ensure spec names are columns in dat
    stopifnot(is.character(spec1), is.character(spec2),
              is.character(spec3), is.character(spec4),
              spec1 %in% names(dat), spec2 %in% names(dat), 
              spec3 %in% names(dat), spec4 %in% names(dat))
    
    names(dat) <- c("full_mz", spec1, spec2, spec3, spec4)
    
    # reformat the data frame for easy faceting
    sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                     all_of(spec1), all_of(spec2), all_of(spec3),
                     all_of(spec4), factor_key= TRUE) }
  
  if(n == 5){
    stop('Only 5 input spectra. Please add or remove a spectrum,
         or use plotSpectra().')
  }
  
  if(n == 6){
    # ensure spec names are columns in dat
    stopifnot(is.character(spec1), is.character(spec2),
              is.character(spec3), is.character(spec4),
              is.character(spec5), is.character(spec6),
              spec1 %in% names(dat), spec2 %in% names(dat), 
              spec3 %in% names(dat), spec4 %in% names(dat), 
              spec5 %in% names(dat), spec6 %in% names(dat))
    
    names(dat) <- c("full_mz", spec1, spec2, spec3, spec4,
                    spec5, spec6)
    
    # reformat the data frame for easy faceting
    sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                     all_of(spec1), all_of(spec2), all_of(spec3),
                     all_of(spec4), all_of(spec5), all_of(spec6),
                     factor_key= TRUE) }
  
  if(columns == 2){
    # If lbls = TRUE
    if(lbls == TRUE & is.null(max_I)){
      tst <- .test_lbl(mass_dat = mass, lbl.fmt = lbl.fmt)
      # Check that labels have <= decimal places as data
      if(tst == TRUE){
        stop('Label format indicates more decimal places 
           than given in data.') 
      } else{
        # plot the sorted data
        ggplot(sorted, 
               aes(x = full_mz, y = Intensity, colour = Spectra)) +
          geom_line() +
          labs(x = expression(italic("m/z")), y = "Intensity") +
          facet_wrap(~Spectra, ncol = 2, scales = intensity_scale) +
          scale_x_continuous(limits = c(min_mz, max_mz), 
                             breaks = seq(min_mz, max_mz, 
                                          by = x_ticks)) +
          theme_bw() + theme(panel.border = element_blank(),
                             strip.background = element_blank(),
                             strip.text.x = element_blank(),
                             legend.position = "bottom",
                             axis.line = element_line(colour = "grey85",
                                                      size = 0.5)) +
          scale_color_manual(values = c(colour1,colour2, colour3,
                                        colour4,colour5,colour6)) +
          stat_peaks(aes(x = full_mz, y = Intensity, group = 1),
                     ignore_threshold = thresh, span = span,
                     x.label.fmt = lbl.fmt, geom = "text",
                     check_overlap = TRUE, color = "black", cex = 3.0) }
      
    } else if(lbls == FALSE & is.null(max_I)){
      # plot the sorted data
      ggplot(sorted, aes(x = full_mz, y = Intensity, 
                         colour = Spectra)) +
        geom_line() +
        labs(x = expression(italic("m/z")), y = "Intensity") +
        facet_wrap(~Spectra, ncol = 2, scales = intensity_scale) +
        scale_x_continuous(limits = c(min_mz, max_mz), 
                           breaks = seq(min_mz, max_mz, by = x_ticks)) +
        theme_bw() + theme(panel.border = element_blank(),
                           strip.background = element_blank(),
                           strip.text.x = element_blank(),
                           legend.position = "bottom",
                           axis.line = element_line(colour = "grey85", 
                                                    size = 0.5)) +
        scale_color_manual(values = c(colour1,colour2,colour3,
                                      colour4,colour5,colour6)) 
    } else if(lbls == TRUE){
      ggplot(sorted, 
             aes(x = full_mz, y = Intensity, colour = Spectra)) +
        geom_line() +
        labs(x = expression(italic("m/z")), y = "Intensity") +
        facet_wrap(~Spectra, ncol = 2, scales = intensity_scale) +
        scale_x_continuous(limits = c(min_mz, max_mz), 
                           breaks = seq(min_mz, max_mz, 
                                        by = x_ticks)) +
        scale_y_continuous(limits = c(min_I, max_I)) +
        theme_bw() + theme(panel.border = element_blank(),
                           strip.background = element_blank(),
                           strip.text.x = element_blank(),
                           legend.position = "bottom",
                           axis.line = element_line(colour = "grey85",
                                                    size = 0.5)) +
        scale_color_manual(values = c(colour1,colour2, colour3,
                                      colour4,colour5,colour6)) +
        stat_peaks(aes(x = full_mz, y = Intensity, group = 1),
                   ignore_threshold = thresh, span = span,
                   x.label.fmt = lbl.fmt, geom = "text",
                   check_overlap = TRUE, color = "black", cex = 3.0)
    } else if(lbls == FALSE){
      ggplot(sorted, aes(x = full_mz, y = Intensity, 
                         colour = Spectra)) +
        geom_line() +
        labs(x = expression(italic("m/z")), y = "Intensity") +
        facet_wrap(~Spectra, ncol = 2, scales = intensity_scale) +
        scale_x_continuous(limits = c(min_mz, max_mz), 
                           breaks = seq(min_mz, max_mz, by = x_ticks)) +
        scale_y_continuous(limits = c(min_I, max_I)) +
        theme_bw() + theme(panel.border = element_blank(),
                           strip.background = element_blank(),
                           strip.text.x = element_blank(),
                           legend.position = "bottom",
                           axis.line = element_line(colour = "grey85", 
                                                    size = 0.5)) +
        scale_color_manual(values = c(colour1,colour2,colour3,
                                      colour4,colour5,colour6))
    }
    
    # three columns
  } else{
    # If lbls = TRUE
    if(lbls == TRUE & is.null(max_I)){
      tst <- .test_lbl(mass_dat = mass, lbl.fmt = lbl.fmt)
      # Check that labels have <= decimal places as data
      if(tst == TRUE){
        stop('Label format indicates more decimal places 
           than given in data.') 
      } else{
        # plot the sorted data
        ggplot(sorted, 
               aes(x = full_mz, y = Intensity, colour = Spectra)) +
          geom_line() +
          labs(x = expression(italic("m/z")), y = "Intensity") +
          facet_wrap(~Spectra, ncol = 3, scales = intensity_scale) +
          scale_x_continuous(limits = c(min_mz, max_mz), 
                             breaks = seq(min_mz, max_mz, 
                                          by = x_ticks)) +
          theme_bw() + theme(panel.border = element_blank(),
                             strip.background = element_blank(),
                             strip.text.x = element_blank(),
                             legend.position = "bottom",
                             axis.line = element_line(colour = "grey85",
                                                      size = 0.5)) +
          scale_color_manual(values = c(colour1,colour2, colour3,
                                        colour4,colour5,colour6)) +
          stat_peaks(aes(x = full_mz, y = Intensity, group = 1),
                     ignore_threshold = thresh, span = span,
                     x.label.fmt = lbl.fmt, geom = "text",
                     check_overlap = TRUE, color = "black", cex = 3.0) }
      
    } else if(lbls == FALSE & is.null(max_I)){
      # plot the sorted data
      ggplot(sorted, aes(x = full_mz, y = Intensity, 
                         colour = Spectra)) +
        geom_line() +
        labs(x = expression(italic("m/z")), y = "Intensity") +
        facet_wrap(~Spectra, ncol = 3, scales = intensity_scale) +
        scale_x_continuous(limits = c(min_mz, max_mz), 
                           breaks = seq(min_mz, max_mz, by = x_ticks)) +
        theme_bw() + theme(panel.border = element_blank(),
                           strip.background = element_blank(),
                           strip.text.x = element_blank(),
                           legend.position = "bottom",
                           axis.line = element_line(colour = "grey85", 
                                                    size = 0.5)) +
        scale_color_manual(values = c(colour1,colour2,colour3,
                                      colour4,colour5,colour6)) 
    } else if(lbls == TRUE){
      tst <- .test_lbl(mass_dat = mass, lbl.fmt = lbl.fmt)
      # Check that labels have <= decimal places as data
      if(tst == TRUE){
        stop('Label format indicates more decimal places 
           than given in data.') 
      } else{
        # plot the sorted data
        ggplot(sorted, 
               aes(x = full_mz, y = Intensity, colour = Spectra)) +
          geom_line() +
          labs(x = expression(italic("m/z")), y = "Intensity") +
          facet_wrap(~Spectra, ncol = 3, scales = intensity_scale) +
          scale_x_continuous(limits = c(min_mz, max_mz), 
                             breaks = seq(min_mz, max_mz, 
                                          by = x_ticks)) +
          scale_y_continuous(limits = c(min_I, max_I)) +
          theme_bw() + theme(panel.border = element_blank(),
                             strip.background = element_blank(),
                             strip.text.x = element_blank(),
                             legend.position = "bottom",
                             axis.line = element_line(colour = "grey85",
                                                      size = 0.5)) +
          scale_color_manual(values = c(colour1,colour2, colour3,
                                        colour4,colour5,colour6)) +
          stat_peaks(aes(x = full_mz, y = Intensity, group = 1),
                     ignore_threshold = thresh, span = span,
                     x.label.fmt = lbl.fmt, geom = "text",
                     check_overlap = TRUE, color = "black", cex = 3.0) }
    } else if(lbls == FALSE){
      ggplot(sorted, aes(x = full_mz, y = Intensity, 
                         colour = Spectra)) +
        geom_line() +
        labs(x = expression(italic("m/z")), y = "Intensity") +
        facet_wrap(~Spectra, ncol = 3, scales = intensity_scale) +
        scale_x_continuous(limits = c(min_mz, max_mz), 
                           breaks = seq(min_mz, max_mz, by = x_ticks)) +
        scale_y_continuous(limits = c(min_I, max_I)) +
        theme_bw() + theme(panel.border = element_blank(),
                           strip.background = element_blank(),
                           strip.text.x = element_blank(),
                           legend.position = "bottom",
                           axis.line = element_line(colour = "grey85", 
                                                    size = 0.5)) +
        scale_color_manual(values = c(colour1,colour2,colour3,
                                      colour4,colour5,colour6))
    }
  }
}


# -----------------------------------------------------------------------

