#############################################################################
#
# PLOT A SINGLE SPECTRUm
#
plotSpectrum <- function(dat, 
                         spec, 
                         min_mz = 50.0, 
                         max_mz = 1100.0, 
                         x_ticks = 100, 
                         y_ticks = 25,
                         xlbl = expression(italic("m/z")), 
                         ylbl = "Intensity", 
                         colour = "black",
                         n_peaks = 5) {
  
  # user sets min/max: verify against data
  min_mz <- max(min_mz, min(dat$full_mz))
  max_mz <- min(max_mz, max(dat$full_mz))

  stopifnot(max_mz - min_mz > 10)

  # subset to the user-requested limits
  dat <- dat[dat$full_mz >= min_mz & dat$full_mz <= max_mz, ]

  x_axe <- pretty(seq(min_mz, max_mz, by = x_ticks),
                  n = round((max_mz - min_mz) / x_ticks))
  v_lines <- pretty(seq(min_mz, max_mz, by = x_ticks),
                  n = 2 * round((max_mz - min_mz) / x_ticks)) 

  par(mar = c(4,4,1,1))
  par(mfrow = c(1,1))
  plot(NA, NA,
       type = "l",
       xaxt = "n",
       ylab = ylbl,
       xlab = xlbl,
       xlim = c(min_mz, max_mz), 
       ylim = c(min(dat[[spec]]), max(dat[[spec]])))
  axis(side = 1, at = x_axe, labels = TRUE)

  # hack to fix grid
  grid(nx = NA, ny = NULL, lty = "solid", col = "grey85")
  abline(v = v_lines, col = "grey85", lty = "solid")
  lines(x = dat$full_mz,
        y = dat[[spec]],
        col = colour)

  # crazy bit for labels
  label_peaks <- find_top_peaks(x = dat$full_mz,
                                y = dat[[spec]],
                                n_peaks = n_peaks)
  #label_peaks <- jitter(label_peaks$l, factor = 2)
  # return from this will be the top n_peaks m/z values
  for(j in 1:n_peaks) {
    text(x = label_peaks$x[j] * (1 + rnorm(1, sd = 0.01)), 
         y = label_peaks$y[j] * (1 + rnorm(1, sd = 0.01)),
         label = label_peaks$l[j], 
         cex = 0.7, pos = 4, offset = 0,
         col = "black")
  }
}

#############################################################################
#
# PLOT MULTIPLE SPECTRA
#

plotSpectra <- function(dat, 
                        spec1, 
                        spec2, 
                        method,
                        min_mz = 50, 
                        max_mz = 1100, 
                        x_ticks = 100, 
                        y_ticks = 25,
                        sub_ttl1 = "Original", 
                        sub_ttl2 = "Subtracted", 
                        xlbl = expression(italic("m/z")),
                        ylbl = "Intensity", 
                        colour = "black", 
                        overlay_colour = "blue",
                        n_peaks = 5) {

  # user sets min/max: verify against data
  min_mz <- max(min_mz, min(dat$full_mz))
  max_mz <- min(max_mz, max(dat$full_mz))
  stopifnot(max_mz - min_mz > 10)

  # subset to the user-requested limits
  dat <- dat[dat$full_mz >= min_mz & dat$full_mz <= max_mz, ]

  x_axe <- pretty(seq(min_mz, max_mz, by = x_ticks),
                  n = round((max_mz - min_mz) / x_ticks))
  v_lines <- pretty(seq(min_mz, max_mz, by = x_ticks),
                  n = 2 * round((max_mz - min_mz) / x_ticks)) 

  if(method == "overlay") {

     rgb.val.o <- col2rgb(overlay_colour)
     o.col <- rgb(rgb.val.o[1], rgb.val.o[2], rgb.val.o[3],
                 max = 255,
                 alpha = 80)
     rgb.val.u <- col2rgb(colour)
     u.col <- rgb(rgb.val.u[1], rgb.val.u[2], rgb.val.u[3],
                 max = 255,
                 alpha = 80)


     plot(NA, NA,
          type = "l",
          xaxt = "n",
          ylab = ylbl,
          xlab = xlbl,
          xlim = c(min_mz, max_mz), 
          ylim = c(min(min(dat[[spec1]]), 
                       min(dat[[spec2]])),
                   max(max(dat[[spec1]]),
                       max(dat[[spec2]]))))
     axis(side = 1, at = x_axe, labels = TRUE)
 
     # hack to fix grid
     grid(nx = NA, ny = NULL, lty = "solid", col = "grey85")
     abline(v = v_lines, col = "grey85", lty = "solid")
     lines(x = dat$full_mz,
           y = dat[[spec1]],
           col = u.col)
     lines(x = dat$full_mz,
           y = dat[[spec2]],
           col = o.col)
     legend("topleft",
            c(sub_ttl1, sub_ttl2),
            fill =c(colour, overlay_colour))
 
     # crazy bit for labels
     label_peaks <- find_top_peaks(x = dat$full_mz,
                                   y = dat[[spec2]],
                                   n_peaks = n_peaks)
     # return from this will be the top n_peaks m/z values
     for(j in 1:n_peaks) {
       text(x = label_peaks$x[j] * (1 + rnorm(1, sd = 0.01)), 
            y = label_peaks$y[j] * (1 + rnorm(1, sd = 0.01)),
            label = label_peaks$l[j], 
            cex = 0.7, pos = 4, offset = 0)
     } 
  } else {

    # Spec 1
    par(mfrow = c(2,1)) 
    par(mar = c(1,4,1,1))
    plot(NA, NA,
         type = "l",
         xaxt = "n",
         main = NA, 
         ylab = ylbl,
         xlab = xlbl,
         xlim = c(min_mz, max_mz),
         ylim = c(min(min(dat[[spec1]]), 
                       min(dat[[spec2]])),
                   max(max(dat[[spec1]]),
                       max(dat[[spec2]]))))
    axis(side = 1, at = x_axe, labels = FALSE)

    # hack to fix grid
    grid(nx = NA, ny = NULL, lty = "solid", col = "grey85")
    abline(v = v_lines, col = "grey85", lty = "solid")
    lines(x = dat$full_mz,
          y = dat[[spec1]],
          col = colour)
    legend(x = "topright", legend = sub_ttl1,
           box.col = "black", bg = "white")
    # crazy bit for labels
    label_peaks <- find_top_peaks(x = dat$full_mz,
                                  y = dat[[spec1]],
                                  n_peaks = n_peaks)
    # return from this will be the top n_peaks m/z values
    for(j in 1:n_peaks) {
      text(x = label_peaks$x[j] * (1 + rnorm(1, sd = 0.01)), 
           y = label_peaks$y[j] * (1 + rnorm(1, sd = 0.01)),
           label = label_peaks$l[j], 
           cex = 0.7, pos = 4, offset = 0)
    }
    # end of first spectrum, start of second 
    par(mar = c(4,4,1,1))
    plot(NA, NA,
         type = "l",
         xaxt = "n",
         ylab = ylbl,
         main = NA, 
         xlab = xlbl,
         xlim = c(min_mz, max_mz),
         ylim = c(min(min(dat[[spec1]]), 
                       min(dat[[spec2]])),
                   max(max(dat[[spec1]]),
                       max(dat[[spec2]]))))
      axis(side = 1, at = x_axe, labels = TRUE)
      # hack to fix grid
      grid(nx = NA, ny = NULL, lty = "solid", col = "grey85")
      abline(v = v_lines, col = "grey85", lty = "solid")
      lines(x = dat$full_mz,
            y = dat[[spec2]],
            col = overlay_colour)
      legend(x = "topright", legend = sub_ttl2,
             box.col = "black",bg = "white")
      # crazy bit for labels
      label_peaks <- find_top_peaks(x = dat$full_mz,
                                    y = dat[[spec2]],
                                    n_peaks = n_peaks)
      # return from this will be the top n_peaks m/z values
      for(j in 1:n_peaks) {
        text(x = label_peaks$x[j] * (1 + rnorm(1, sd = 0.01)), 
             y = label_peaks$y[j] * (1 + rnorm(1, sd = 0.01)),
             label = label_peaks$l[j], 
             cex = 0.7, pos = 4, offset = 0)
      }
      # end of second spectrum
      par(mfrow=c(1,1))
  }
}

