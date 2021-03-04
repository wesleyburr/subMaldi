# ----------------------------------------------------------------------------
# Last Updated: July 29, 2020
# Author: Kristen Yeh
# Title: subMALDI - Monotone Minimum Baseline Correction
# ----------------------------------------------------------------------------

# Compute difference between points to determine slope of each point
# Start from leftmost point A and continue until rightmost point:

# If slope of local point A < 0, a nearest point B to the right
# of A whose slope > 0 is located. All points between A and B
# serve as baseline between A and B.

# If slope of A > 0, a nearest point B to the right of A
# whose intensity is smaller than A is located. The intensity
# of every point on the result baseline between A and B equals
# to the intensity of A.
# Let A = B.

.base_mono <- function(dat, mass_dat, intensity_dat){
  
  options(warn=-1)
  x <- dat[[mass_dat]]
  y <- dat[[intensity_dat]]
  
  slopes <- diff(y)/diff(x)
  # account for NaN in slopes (in case diff(x) = 0)
  if(any(diff(x)) == 0) {  
     stop('Xi - Xi+1 = 0. Please remove duplicated m/z value.')
  } else {
    
    slopes <- sign(slopes)
    slopes <- as.numeric(stats::filter(slopes, rep(1 / 2, 2)))
    slopes <- slopes[!is.na(slopes)]
    slopes <- append(slopes, 1, after = 0)
    slopes <- append(slopes, 0, after = length(slopes))
    # peaks and valleys = 0
    
    idx <- cbind(x,y,slopes)
    
    peaks <- c()
    valleys <- c()
    
    for(i in 1:nrow(idx)){
      if(idx[i,"slopes"] > 0){ 
        # if slope of A > 0, find nearest point B w/ intensity < A
        # intensity between A and B recorded as peak
        a <- i
        
        # zeroes between 1, -1 are peaks
        # find nearest peak w/ slope -1
        b <- which(idx[,"slopes"] < 0)
        b <- b[which(b > i)]
        b <- min(b)
        
        b[which(b == Inf)] <- nrow(idx)
        
        c <- a+1
        d <- b-1
        
        # grab zeroes in between that = peaks
        if(c == d){
          peak <- c()
          peak <- idx[c, "x"]
          peaks <- append(peaks, peak, after = length(peaks))
        } else{
          l <- c(c:d)
          for(m in l){
            if(idx[m, "slopes"] == 0){
              peak <- idx[m, 1]
              peaks <- append(peaks, peak, after = length(peaks)) 
            }
          }
        }
      } else if(idx[i,"slopes"] < 0){ 
        # if slope of A < 0, find nearest point B of slope > 0
        # all points between A and B serve as baseline between A & B
        a <- i
        
        # zeroes between -1, 1 are baseline
        # find nearest peak w/ slope 1
        b <- which(idx[,"slopes"] > 0)
        b <- b[which(b > i)]
        b <- min(b)
        
        # fix last loop
        b[which(b == Inf)] <- nrow(idx)
        
        c <- a+1
        d <- b-1
        
        # grab zeroes in between for valleys
        if(c == d){
          valley <- c()
          valley <- idx[c, "x"]
          valleys <- append(valleys, valley, after = length(valleys))
        } else{
          l <- c(c:d)
          for(m in l){
            if(idx[m, "slopes"] == 0 && m %in% l){
              valley <- idx[m, "x"]
              valleys <- append(valleys, valley, after = length(valleys)) 
            }
          }
        } 
      } 
    }
    peaks <- unique(peaks)
    valleys <- unique(valleys)
    
    out <- data.frame(x,y)
    names(out) <- c("mz", "baseline")
    
    for(n in 1:nrow(out)){
      if(out$mz[n] %in% valleys){
        # if m/z is in valleys,
        # subtract the intensity of n by valley intensity
        base <- out$baseline[n]
        out$baseline[n] <- out$baseline[n] - base
      } else {
        # find nearest valley 
        v <- which(out$mz %in% valleys)
        # to the right of n
        
        base <- out$baseline[v[min(which(v > n))]]
        # subtract the intensity of n by valley intensity
        out$baseline[n] <- out$baseline[n] - base
        out$baseline[which(out$baseline < 0)] <- 0
      }
    }
    out$baseline[is.na(out$baseline)] <- 0
    return(out)
  }
}

# ----------------------------------------------------------------------------
