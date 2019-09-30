#############################################################################
#
# PLOT A SINGLE SPECTRUm
#

plotSpectrum <- function(dat, spec, min_mz = 50.0, max_mz = 1100.0, x_ticks = 100, y_ticks = 25,
                         xlbl = expression(italic("m/z")), ylbl = "Intensity", colour = "black",
                         n_peaks = 5) {

  x_axe <- seq(min_mz, max_mz, by = x_ticks)
  v_lines <- seq(min_mz, max_mz, by = y_ticks)

  par(mar = c(4,4,1,1))
  plot(NA, NA,
       type = "l",
       xaxt = "n",
       ylab = ylbl,
       xlab = xlbl,
       xlim = c(min(dat$full_mz), max(dat$full_mz)),
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
  label_peaks <- jitter(label_peaks$l, factor = 1)
  # return from this will be the top n_peaks m/z values
  print(label_peaks)
  for(j in 1:n_peaks) {
    text(x = label_peaks$x[j] * (1 + rnorm(1, sd = 0.01)), y = label_peaks$y[j] + (1 * rnorm(1, sd = 0.01)),
         label = label_peaks$l[j], cex = 1, pos = 4, offset = 0.2)
  }
}

#############################################################################
#
# PLOT MULTIPLE SPECTRA
#

plotSpectra <- function(dat, spec1, spec2, method,
                        min_mz = 50, max_mz = 1100, x_ticks = 100, y_ticks = 25,
                       sub_ttl1 = "Original", sub_ttl2 = "Subtracted", xlbl = expression(italic("m/z")),
                        ylbl = "Intensity", colour = "black", overlay_colour = "blue",
                        n_peaks = 5){

  x_axe <- seq(min_mz, max_mz, by = x_ticks)
  v_lines <- seq(min_mz, max_mz, by = y_ticks)

  if(method == "overlay"){

    rgb.val <- col2rgb(overlay_colour)
    o.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
                 max = 255,
                 alpha = 50)

    plot(NA, NA,
         type = "l",
         xaxt = "n",
         ylab = ylbl,
         xlab = xlbl,
         xlim = c(min(dat$full_mz), max(dat$full_mz)),
         ylim = c(min(dat[[spec]]), max(dat[[spec]])))
    axis(side = 1, at = x_axe, labels = TRUE)

    # hack to fix grid
    grid(nx = NA, ny = NULL, lty = "solid", col = "grey85")
    abline(v = v_lines, col = "grey85", lty = "solid")
    lines(x = dat$full_mz,
          y = dat[[spec1]],
          col = colour)
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
    print(label_peaks)
    for(j in 1:n_peaks) {
      text(x = label_peaks$x[j] * (1 + rnorm(1, sd = 0.01)), y = label_peaks$y[j] + (1 * rnorm(1, sd = 0.01)),
           label = label_peaks$l[j], cex = 1, pos = 4, offset = 0.2)

  } else {

    # Spec 1
    par(mfrow = c(2,1), mar = c(4,4,1,1))
    plot(NA, NA,
         type = "l",
         xaxt = "n",
         main = sub_ttl1,
         ylab = ylbl,
         xlab = xlbl,
         xlim = c(min(dat$full_mz), max(dat$full_mz)),
         ylim = c(min(dat[[spec1]]), max(dat[[spec1]])))
    axis(side = 1, at = x_axe, labels = TRUE)
    # hack to fix grid
    grid(nx = NA, ny = NULL, lty = "solid", col = "grey85")
    abline(v = v_lines, col = "grey85", lty = "solid")
    lines(x = dat$full_mz,
          y = dat[[spec1]],
          col = colour)
    # crazy bit for labels
    label_peaks <- find_top_peaks(x = dat$full_mz,
                                  y = dat[[spec1]],
                                  n_peaks = n_peaks)
    # return from this will be the top n_peaks m/z values
    print(label_peaks)
    for(j in 1:n_peaks) {
      text(x = label_peaks$x[j] * (1 + rnorm(1, sd = 0.01)), y = label_peaks$y[j] + (1 * rnorm(1, sd = 0.01)),
           label = label_peaks$l[j], cex = 1, pos = 4, offset = 0.2)

    # Spec 2
      plot(NA, NA,
           type = "l",
           xaxt = "n",
           ylab = ylbl,
           main = sub_ttl2,
           xlab = xlbl,
           xlim = c(min(dat$full_mz), max(dat$full_mz)),
           ylim = c(min(dat[[spec2]]), max(dat[[spec2]])))
      axis(side = 1, at = x_axe, labels = TRUE)
      # hack to fix grid
      grid(nx = NA, ny = NULL, lty = "solid", col = "grey85")
      abline(v = v_lines, col = "grey85", lty = "solid")
      lines(x = dat$full_mz,
            y = dat[[spec2]],
            col = overlay_colour)
      # crazy bit for labels
      label_peaks <- find_top_peaks(x = dat$full_mz,
                                    y = dat[[spec2]],
                                    n_peaks = n_peaks)
      # return from this will be the top n_peaks m/z values
      print(label_peaks)
      for(j in 1:n_peaks) {
        text(x = label_peaks$x[j] * (1 + rnorm(1, sd = 0.01)), y = label_peaks$y[j] + (1 * rnorm(1, sd = 0.01)),
             label = label_peaks$l[j], cex = 1, pos = 4, offset = 0.2)
  }

}
}
}


