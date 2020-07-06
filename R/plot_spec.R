# -----------------------------------------------------------------------
# Last Updated: May 25, 2020
# Author: Kristen Yeh
# Title: subMALDI Plotting Functions
# -----------------------------------------------------------------------



# ------------------------
# PLOT A SINGLE SPECTRUM
# ------------------------


plotSpectrum <- function(dat, 
                         mass_dat,
                         intensity_dat,
                         colour = "slategrey",
                         span = 5,
                         thresh = 0.1,
                         min_mz = 0,
                         max_mz = 1000,
                         lbls = FALSE,
                         lbl.fmt = "%3.4f",
                         x_ticks = 100) {
  ggplot(dat, aes(mass_dat, intensity_dat), group = 1) +
    geom_line(col = colour) +
    labs(x = expression(italic("m/z")), y = "Intensity") +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_x_continuous(limits = c(min_mz, max_mz), breaks = seq(min_mz, max_mz, by = x_ticks)) +
    theme_bw() + theme( panel.border = element_blank(),
                        strip.background = element_blank(),
                        strip.text.x = element_blank(),
                        axis.line = element_line(colour = "grey85", 
                                                 size = 0.5)) +
    if(lbls == TRUE){
      stat_peaks(aes(x = mass_dat, y = intensity_dat, group = 1), 
                 ignore_threshold = thresh, x.label.fmt = lbl.fmt,  span = span, 
                 geom = "text", check_overlap = TRUE, color = "black", cex = 3.0)
    } else {  
      stat_peaks(aes(x = mass_dat, y = intensity_dat, group = 1), 
                 ignore_threshold = 1000000, span = span, 
                 geom = "text", check_overlap = TRUE, color = "black", cex = 3.0)
    }
}


# ---------------------
# PLOT 1 to 4 SPECTRA
# ---------------------


plotSpectra <- function(dat, mass_dat,
                        spec1, spec2,
                        spec3 = NULL,
                        spec4 = NULL,
                        colour1 = wes_palette(n=1, name = "GrandBudapest2"),
                        colour2 = wes_palette(n=1, name = "GrandBudapest1"),
                        colour3 = wes_palette(n=1, name = "Moonrise1"),
                        colour4 = wes_palette(n=1, name = "Moonrise3"),
                        span = 5,
                        thresh = 0.1,
                        lbls = FALSE, 
                        lbl.fmt = "%3.4f",
                        min_mz = 0,
                        max_mz = 1000,
                        x_ticks = 100,
                        intensity_scale = "free_y"){
  
  if(is.null(spec4)) {
    if(is.null(spec3)) {
      # ensure spec1 and spec2 are columns in dat
      stopifnot(is.character(spec1), is.character(spec2),
                spec1 %in% names(dat), spec2 %in% names(dat))
      
      # reformat the data frame for easy faceting
      sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                       all_of(spec1), all_of(spec2), factor_key= TRUE)
      
      # plot the sorted data
      ggplot(sorted, aes(x = full_mz, y = Intensity, colour = Spectra)) +
        geom_line() +
        labs(x = expression(italic("m/z")), y = "Intensity") +
        facet_wrap(~Spectra, ncol = 1, scales = intensity_scale) +
        scale_x_continuous(limits = c(min_mz, max_mz), breaks=seq(min_mz,max_mz,by = x_ticks)) +
        theme_bw() + theme( panel.border = element_blank(),
                            strip.background = element_blank(),
                            strip.text.x = element_blank(),
                            legend.position = "bottom",
                            axis.line = element_line(colour = "grey85", 
                                                     size = 0.5)) +
        scale_color_manual(values = c(colour1,colour2)) +
        
        if(lbls == TRUE){ stat_peaks(aes(x = full_mz, y = Intensity, group = 1), 
                                     ignore_threshold = thresh, span = span, x.label.fmt = lbl.fmt, 
                                     geom = "text", check_overlap = TRUE, color = "black", cex = 3.0) } 
      else { stat_peaks(aes(x = full_mz, y = Intensity, group = 1), 
                        ignore_threshold = 100, span = span, 
                        geom = "text", check_overlap = TRUE, color = "black", cex = 3.0) }
    } 
    
    else {
      # spec3 is not null but spec4 is
      stopifnot(is.character(spec1), is.character(spec2), is.character(spec3),
                spec1 %in% names(dat), spec2 %in% names(dat), spec3 %in% names(dat))
      
      # reformat the data frame for easy faceting
      sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                       all_of(spec1), all_of(spec2), all_of(spec3), factor_key= TRUE)
      
      # plot the sorted data
      ggplot(sorted, aes(x = full_mz, y = Intensity, colour = Spectra)) +
        geom_line() +
        labs(x = expression(italic("m/z")), y = "Intensity") +
        facet_wrap(~Spectra, ncol = 1, scales = intensity_scale) +
        scale_x_continuous(limits = c(min_mz, max_mz),breaks=seq(min_mz,max_mz,by = x_ticks)) +
        theme_bw() + theme( panel.border = element_blank(),
                            strip.background = element_blank(),
                            strip.text.x = element_blank(),
                            legend.position = "bottom",
                            axis.line = element_line(colour = "grey85", 
                                                     size = 0.5)) +
        scale_color_manual(values = c(colour1,colour2,colour3)) +
        
        if(lbls == TRUE){ stat_peaks(aes(x = full_mz, y = Intensity, group = 1), 
                                     ignore_threshold = thresh, span = span, x.label.fmt = lbl.fmt,
                                     geom = "text", check_overlap = TRUE, color = "black", cex = 3.0) } 
      else {  stat_peaks(aes(x = full_mz, y = Intensity, group = 1), 
                         ignore_threshold = 100, span = span, geom = "text", 
                         check_overlap = TRUE, color = "black", cex = 3.0)  } 
    }
  } else {
    # if spec4 is NOT null
    stopifnot(is.character(spec1), is.character(spec2), is.character(spec3),
              is.character(spec4), spec1 %in% names(dat), spec2 %in% names(dat), 
              spec3 %in% names(dat), spec4 %in% names(dat))
    
    # reformat the data frame for easy faceting
    sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                     all_of(spec1), all_of(spec2), all_of(spec3),
                     all_of(spec4), factor_key= TRUE)
    
    # plot the sorted data
    ggplot(sorted, aes(x = full_mz, y = Intensity, colour = Spectra)) +
      geom_line() +
      labs(x = expression(italic("m/z")), y = "Intensity") +
      facet_wrap(~Spectra, ncol = 1, scales = intensity_scale) +
      scale_x_continuous(limits = c(min_mz, max_mz),breaks=seq(min_mz,max_mz,by = x_ticks)) +
      theme_bw() + theme( panel.border = element_blank(),
                          strip.background = element_blank(),
                          strip.text.x = element_blank(),
                          legend.position = "bottom",
                          axis.line = element_line(colour = "grey85", 
                                                   size = 0.5)) +
      scale_color_manual(values = c(colour1,colour2,colour3,colour4)) +
      
      if(lbls == TRUE){ stat_peaks(aes(x = full_mz, y = Intensity, group = 1), 
                                   ignore_threshold = thresh, span = span, x.label.fmt = lbl.fmt,
                                   geom = "text", check_overlap = TRUE, color = "black", cex = 3.0) }
    else { stat_peaks(aes(x = full_mz, y = Intensity, group = 1), 
                      ignore_threshold = 100, span = span, 
                      geom = "text", check_overlap = TRUE, color = "black", cex = 3.0) }
  }
}


# ----------------------
# PLOT 4 or 6 SPECTRA
# ----------------------

plotgridSpectra<- function(dat, mass_dat, 
                           spec1, spec2,
                           spec3, spec4,
                           spec5 = NULL, spec6 = NULL,
                           colour1 = wes_palette(n=1, name = "GrandBudapest2"),
                           colour2 = wes_palette(n=1, name = "GrandBudapest1"),
                           colour3 = wes_palette(n=1, name = "Moonrise1"),
                           colour4 = wes_palette(n=1, name = "Moonrise3"),
                           colour5 = wes_palette(n = 1, name = "IsleofDogs1"),
                           colour6 = wes_palette(n = 1, name = "FantasticFox1"),
                           span = 5,
                           thresh = 0.1,
                           lbls = FALSE, 
                           lbl.fmt = "%3.4f", 
                           columns = 2,
                           min_mz = 0,
                           max_mz = 1000,
                           x_ticks = 100,
                           intensity_scale = "free_y"){
  
  if(is.null(spec6)) {
    
    # make sure grabbing the right spec
    stopifnot(is.character(spec1), is.character(spec2), is.character(spec3),
              is.character(spec4), spec1 %in% names(dat), spec2 %in% names(dat), 
              spec3 %in% names(dat), spec4 %in% names(dat))
    
    # reformat the data frame for easy faceting
    sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                     all_of(spec1), all_of(spec2), 
                     all_of(spec3), all_of(spec4), factor_key= TRUE)
    
    # plot the sorted data
    ggplot(sorted, aes(x = full_mz, y = Intensity, colour = Spectra)) +
      geom_line() +
      labs(x = expression(italic("m/z")), y = "Intensity") +
      facet_wrap(~Spectra, ncol = 2, scales = intensity_scale) +
      scale_x_continuous(limits = c(min_mz, max_mz), breaks=seq(min_mz,max_mz,by = x_ticks)) +
      theme_bw() + theme( panel.border = element_blank(),
                          strip.background = element_blank(),
                          strip.text.x = element_blank(),
                          legend.position = "bottom",
                          axis.line = element_line(colour = "grey85", 
                                                   size = 0.5,)) +
      scale_color_manual(values = c(colour1,colour2,colour3,colour4)) +
      if(lbls == TRUE){ stat_peaks(aes(x = full_mz, y = Intensity, group = 1), 
                                   ignore_threshold = thresh, span = span, x.label.fmt = lbl.fmt,
                                   geom = "text", check_overlap = TRUE, color = "black", cex = 3.0) }
    else {  stat_peaks(aes(x = full_mz, y = Intensity, group = 1), 
                       ignore_threshold = 100, span = span, 
                       geom = "text", check_overlap = TRUE, color = "black", cex = 3.0) }
  } 
  
  else if(columns < 3){
    
    # make sure grabbing the right spec
    stopifnot(is.character(spec1), is.character(spec2), is.character(spec3),
              is.character(spec4), is.character(spec5), is.character(spec6),
              spec1 %in% names(dat), spec2 %in% names(dat), spec3 %in% names(dat), 
              spec4 %in% names(dat), spec5 %in% names(dat), spec6 %in% names(dat))
    
    # reformat the data frame for easy faceting
    sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                     all_of(spec1), all_of(spec2), all_of(spec3), 
                     all_of(spec4), all_of(spec5), all_of(spec6),
                     factor_key= TRUE)
    
    # plot the sorted data
    ggplot(sorted, aes(x = full_mz, y = Intensity, colour = Spectra)) +
      geom_line() +
      labs(x = expression(italic("m/z")), y = "Intensity") +
      facet_wrap(~Spectra, ncol = 2, scales = intensity_scale) +
      scale_x_continuous(limits = c(min_mz, max_mz), breaks=seq(min_mz,max_mz,by = x_ticks)) +
      theme_bw() + theme( panel.border = element_blank(),
                          strip.background = element_blank(),
                          strip.text.x = element_blank(),
                          legend.position = "bottom",
                          axis.line = element_line(colour = "grey85", 
                                                   size = 0.5)) +
      scale_color_manual(values = c(colour1,colour2,colour3,colour4,colour5,colour6)) +
      if(lbls == TRUE){ stat_peaks(aes(x = full_mz, y = Intensity, group = 1), 
                                   ignore_threshold = thresh, span = span, x.label.fmt = lbl.fmt,
                                   geom = "text", check_overlap = TRUE, color = "black", cex = 3.0) }
    else {  stat_peaks(aes(x = full_mz, y = Intensity, group = 1), 
                       ignore_threshold = 100, span = span, 
                       geom = "text", check_overlap = TRUE, color = "black", cex = 3.0) }} 
  
  # 3 columns
  else {
    
    # make sure grabbing the right spec
    stopifnot(is.character(spec1), is.character(spec2), is.character(spec3),
              is.character(spec4), is.character(spec5), is.character(spec6),
              spec1 %in% names(dat), spec2 %in% names(dat), spec3 %in% names(dat), 
              spec4 %in% names(dat), spec5 %in% names(dat), spec6 %in% names(dat))
    
    # reformat the data frame for easy faceting
    sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                     all_of(spec1), all_of(spec2), all_of(spec3), 
                     all_of(spec4), all_of(spec5), all_of(spec6),
                     factor_key= TRUE)
    
    # plot the sorted data
    ggplot(sorted, aes(x = full_mz, y = Intensity, colour = Spectra)) +
      geom_line() +
      labs(x = expression(italic("m/z")), y = "Intensity") +
      facet_wrap(~Spectra, ncol = 3, scales = intensity_scale) +
      scale_x_continuous(limits = c(min_mz, max_mz), breaks=seq(min_mz,max_mz,by = x_ticks)) +
      theme_bw() + theme( panel.border = element_blank(),
                          strip.background = element_blank(),
                          strip.text.x = element_blank(),
                          legend.position = "bottom",
                          axis.line = element_line(colour = "grey85", 
                                                   size = 0.5)) +
      scale_color_manual(values = c(colour1,colour2,colour3,colour4,colour5,colour6)) +
      if(lbls == TRUE){ stat_peaks(aes(x = full_mz, y = Intensity, group = 1), 
                                   ignore_threshold = thresh, span = span, x.label.fmt = lbl.fmt,
                                   geom = "text", check_overlap = TRUE, color = "black", cex = 3.0) }
    else {  stat_peaks(aes(x = full_mz, y = Intensity, group = 1), 
                       ignore_threshold = 100, span = span, 
                       geom = "text", check_overlap = TRUE, color = "black", cex = 3.0) }}
  }

# -----------------------------------------------------------------------
