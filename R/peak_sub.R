# ----------------------------------------------------------------------------
# Last Updated: August 5, 2020
# Author: Kristen Yeh
# Title: subMALDI - Peak Detection Methods
# ----------------------------------------------------------------------------


# ----
# SNR
# ----

# Noise is estimated as the median of the absolute deviation (MAD) 
# of points within a window.

.peak_snr <- function(dat, mass_dat, intensity_dat, n, SNR_thresh = 0.1){
  
  stopifnot(mass_dat %in% colnames(dat),
            intensity_dat %in% colnames(dat),
            !(is.null(n)),
            SNR_thresh >= 0) 
  
  
  x <- dat[[mass_dat]]
  y <- dat[[intensity_dat]]
  
  # Separate into windows of size n
  y_seg <- split(y, ceiling(seq_along(x)/n))
  
  if(length(y_seg) == 1){
    stop(paste0("Chosen window covers entire spectrum. To avoid this, please select a value of n much less than ", length(x),"."))
  }
  
  noise <- c()
  # Calculate MAD within each window
  for(i in 1:length(y_seg)){
    m <- mad(unlist(y_seg[i]))
    noise <- append(noise, m, after = length(noise))
  }
  
  snr <- c()
  # Calculate signal-to-noise ratio for all peaks in window
  for(j in 1:length(noise)){
    if(noise[j] == 0){
      snr_seg <- unlist(y_seg[j]) / noise[j] # Values will be SNR = Inf since there is no noise present
      snr_seg[is.na(snr_seg)] <- 0 # Replacing NAs with zero (arising from 0/0)
      snr <- append(snr, snr_seg, after = length(snr))
    } else{
      snr_seg <- unlist(y_seg[j]) / noise[j]
      snr <- append(snr, snr_seg, after = length(snr))}
  }
  
  
  idx <- data.frame(x,y,snr)
  removed <- logical(nrow(idx))
  # Remove all signals below input SNR 
  for(k in 1:nrow(idx)){
    if(idx[k, "snr"] < SNR_thresh){
      idx[k, "y"] <- 0
      removed[k] <- TRUE
    } else{
      idx[k, "y"] <- idx[k, "y"]
    }
  }
  
  out <- idx[1:2]
  
  if(sum(removed) == 0){
    stop(paste0("No signals detected below specified SNR. Minimum SNR in this spectrum is ", min(idx$snr),"."))
  } else{
    message(paste0(sum(removed), " peaks removed below SNR_thresh = ", SNR_thresh))
  }
  
  names(out) <- c("mz", "peaks")
  return(out)
}


# ----------------
# SLOPES OF PEAKS
# ----------------

# This criterion uses the shape of peaks to remove false peak candidates. 
# In order to compute the left slope and the right slope of a peak, 
# both the left end point and the right end point of the peak need to 
# be identified. Peak candidate is discarded if both left slope and right 
# slope are less than a threshold. The threshold is defined as half of 
# the local noise level.

.peak_slope <- function(dat, mass_dat, intensity_dat, n){
  
  options(warn = -1)
  
  stopifnot(mass_dat %in% colnames(dat),
            intensity_dat %in% colnames(dat),
            !(is.null(n))) 
  
  x <- dat[[mass_dat]]
  y <- dat[[intensity_dat]]
  
  # Identify left and right endpoints using avg. slopes
  slopes <- diff(y)/diff(x)
  # account for NaN in slopes (in case diff(x) = 0)
  if(any(diff(x)) == 0){ stop('Xi - Xi+1 = 0. Please remove duplicated m/z value.')
  } else{
    
    slopes <- sign(slopes)
    slopes <- as.numeric(stats::filter(slopes, rep(1 / 2, 2)))
    slopes <- slopes[!is.na(slopes)]
    slopes <- append(slopes, 1, after = 0)
    slopes <- append(slopes, 0, after = length(slopes))
    
    idx <- cbind(x,y,slopes)
    L_endpoints <- c()
    R_endpoints <- c()
    
    for(i in 1:nrow(idx)){
      if(idx[i,"slopes"] > 0){ 
        # if slope of A > 0, find nearest point B w/ intensity < A
        a <- i
      
        # find nearest peak w/ slope -1
        b <- which(idx[,"slopes"] < 0)
        b <- b[which(b > i)]
        b <- min(b)
        
        b[which(b == Inf)] <- nrow(idx)
        
        # valley to left of A = left end point
        # valley to right of B = right end point
        left_end <- a-1
        right_end <- b+1
        
        if(right_end > nrow(idx)){
          right_end <- nrow(idx)
        }
        
        # check that next non-0 point to left of a is == -1
        e <- which(idx[,"slopes"] != 0)
        e <- e[which(e < a)]
        e <- max(e)
        e[e == -Inf] <- nrow(idx)
        
        # if next non-0 is still 1, ignore points
        if(idx[e,"slopes"] != 1){
          
          # if right_end has slope -1, move to next 0 valley
          if(idx[right_end,"slopes"] < 0){
            d <- which(idx[,"slopes"] == 0)
            d <- d[which(d > right_end)]
            right_end <- min(d)
          }
          
          # check that next non-0 point to right of b is == 1
          c <- which(idx[,"slopes"] != 0)
          c <- c[which(c > right_end)]
          c <- min(c)
          c[c == Inf] <- nrow(idx)
          
          # if next non-0 is still -1, record this as right_end
          if(idx[c,"slopes"] != 1){
            right_end <- c+1 
          }
          
          # if left_end is 0, move to first row
          left_end[left_end < 1] <- 1
          # if right_end is Inf, move to last row
          right_end[right_end == Inf] <- length(right_end)
          
          L_endpoints <- append(L_endpoints, left_end, after = length(L_endpoints)) 
          R_endpoints <- append(R_endpoints, right_end, after = length(R_endpoints))
        }
      }
    }  
    
    slopes <- diff(y) / diff(x)
    L_slopes <- c()
    R_slopes <- c()
    
    # Compute slopes of left and right endpoints
    for(j in 1:length(L_endpoints)){
      L_slope <- slopes[L_endpoints[j]]
      R_slope <- slopes[R_endpoints[j]]
      L_slopes <- append(L_slopes, L_slope, after = length(L_slopes))
      R_slopes <- append(R_slopes, R_slope, after = length(R_slopes))
    }
    
    R_slopes[is.na(R_slopes)] <- 0
    L_slopes[is.na(L_slopes)] <- 0
    
    # Define threshold: half of noise level in window
    # Separate into windows of size n
    y_seg <- split(y, ceiling(seq_along(x)/n))
    
    if(length(y_seg) == 1){
      stop(paste0("Chosen window covers entire spectrum. To avoid this, please select a value of n much less than ", length(x),"."))
    }
    
    noise <- c()
    # Calculate noise within each window
    for(k in 1:length(y_seg)){
      m <- mad(unlist(y_seg[k]))
      noise <- append(noise, m, after = length(noise))
    }
    
    thresh <- noise/2
    thresh[is.na(thresh)] <- 0
    
    # Discard peak candidates w/slopes less than threshold
    
    # Compare each thresh to slopes within window
    # Use endpoints vectors to gauge which window
    
    peaks <- data.frame(L_endpoints, R_endpoints, L_slopes, R_slopes)
    peaks <- transform(peaks, "L_thresh" = FALSE, "R_thresh" = FALSE)
    
    seg <- seq(1, nrow(idx), by = 1)
    seg <- split(seg, ceiling(seq_along(seg)/n))
    
    for(q in 1:length(seg)){
      # grab all L_endpoints %in% seg[q][[1]]
      m <- which(peaks[,"L_endpoints"] %in% seg[q][[1]])
      for(l in m){
        # compare thresh[q] to L_slope[l]
        if(peaks[l, "L_slopes"] < thresh[q]){
          # if L slope is less than thresh in window
          # label for discard (L_thresh = TRUE)
          peaks[l, "L_thresh"] = TRUE
        } else{
          # if greater than threshold
          # label thresh as "FALSE"
          peaks[l, "L_thresh"] = FALSE
        }
      }
      
      # grab all R_endpoints %in% seg[q][[1]]
      m <- which(peaks[,"R_endpoints"] %in% seg[q][[1]])
      for(l in m){
        # compare thresh[q] to R_slope[l]
        if(peaks[l, "R_slopes"] < thresh[q]){
          # if R slope is less than thresh in window
          # label for discard (R_thresh = TRUE)
          peaks[l, "R_thresh"] <- TRUE
        } else{
          # if greater than threshold
          # label thresh as "FALSE"
          peaks[l, "R_thresh"] <- FALSE
        }
      }

    }
      
    # if any thresh in row = TRUE
    # make intensity between endpoints 0
    L_disc <- peaks[which(peaks$L_thresh == TRUE | peaks$R_thresh == TRUE), "L_endpoints"]
    R_disc <- peaks[which(peaks$R_thresh == TRUE | peaks$L_thresh == TRUE), "R_endpoints"]
    disc <- data.frame(L_disc, R_disc)
    
    # for each row of discard, grab points in idx between L and R
    # make those points <- 0
    for(w in 1:nrow(disc)){
      L <- disc[w,"L_disc"]
      R <- disc[w, "R_disc"]
      win <- L:R
      idx[win, "y"] <- 0
    }
    out <- data.frame(idx[,"x"], idx[,"y"])
    names(out) <- c("mz", "peaks")
    return(out)
  } 
}


# ----------------------------------------------------------------------------