# -----------------------------------------------------------------------
# Last Updated: July 15, 2020
# Author: Kristen Yeh
# Title: subMALDI Plotting Functions
# -----------------------------------------------------------------------


# ------------------------
# PLOT A SINGLE SPECTRUM
# ------------------------


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


# ---------------------
# PLOT 2 to 6 SPECTRA
# ---------------------


plotSpectra <- function(dat, mass_dat,
                        spec1, spec2 = NULL,
                        spec3 = NULL,
                        spec4 = NULL,
                        spec5 = NULL,
                        spec6 = NULL,
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
                        min_mz = 0,
                        max_mz = 10000,
                        min_I = 0,
                        max_I = NULL,
                        x_ticks = 100,
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
  
  if(n == 1){
    stop('Only one input spectrum. Please use plotSpectrum().')
  }
  
  if(n == 2){
    # ensure spec names are columns in dat
    stopifnot(is.character(spec1), is.character(spec2),
              spec1 %in% names(dat), spec2 %in% names(dat))
    
    names(dat) <- c("full_mz", spec1, spec2)
    
    # reformat the data frame for easy faceting
    sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                     all_of(spec1), all_of(spec2), 
                     factor_key= TRUE) }
  
  if(n == 3){
    # ensure spec names are columns in dat
    stopifnot(is.character(spec1), is.character(spec2),
              is.character(spec3), spec1 %in% names(dat), 
              spec2 %in% names(dat), spec3 %in% names(dat))
    
    names(dat) <- c("full_mz", spec1, spec2, spec3)
    
    # reformat the data frame for easy faceting
    sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                     all_of(spec1), all_of(spec2), all_of(spec3),
                     factor_key= TRUE) }
  
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
    # ensure spec names are columns in dat
    stopifnot(is.character(spec1), is.character(spec2),
              is.character(spec3), is.character(spec4),
              is.character(spec5), spec1 %in% names(dat), 
              spec2 %in% names(dat), spec3 %in% names(dat), 
              spec4 %in% names(dat), spec5 %in% names(dat))
    
    names(dat) <- c("full_mz", spec1, spec2, spec3, spec4,
                    spec5)
    
    # reformat the data frame for easy faceting
    sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                     all_of(spec1), all_of(spec2), all_of(spec3),
                     all_of(spec4), all_of(spec5), factor_key= TRUE) }
  
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
        facet_wrap(~Spectra, ncol = 1, scales = intensity_scale) +
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
      facet_wrap(~Spectra, ncol = 1, scales = intensity_scale) +
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
        facet_wrap(~Spectra, ncol = 1, scales = intensity_scale) +
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
      facet_wrap(~Spectra, ncol = 1, scales = intensity_scale) +
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


# ----------------------
# PLOT 4 or 6 SPECTRA
# ----------------------


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
