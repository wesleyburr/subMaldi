# --------------------------------------------------------------------------------------------
# Date: January 28, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: Normalization Method - Quantile
# --------------------------------------------------------------------------------------------


# Quantile normalization consists of two steps:
# 1. Create a mapping between ranks and values. For rank 1, find the n values,
# one per spectrum, that are the smallest value on the spectrum, and save their
# averages. Similarly for rank 2 and the second smallest values, and on up to
# the n largest values, one per spectrum.
# 2. For each spectrum, replace the actual values with these averages.

# Basically, index each intensity value in each spectrum then sort
# by intensity (value). Probably need 2 truncate. Average spectrum for the sorted spectra
# is calculated. The averages are then inserted into the sorted spectra.
# The spectra are then reverted to their unsorted order using the index.


norm_quantile <- function(dat, mass_dat, spectra_cols){
  
  # ---------------------
  # LOGICAL CHECKS
  # ---------------------
  
  stopifnot(
    is.character(mass_dat),
    is.character(spectra_cols),
    mass_dat %in% colnames(dat),
    all(spectra_cols %in% colnames(dat)),
    is.numeric(lower),
    is.numeric(upper)
  )    
  
  if(length(spectra_cols) < 2){
    stop("Only one spectrum input. Please enter two spectra.")
  }
  
  mz <- dat[[mass_dat]]
  
  spectra <- lapply(spectra_cols, function(x){dat[x]})
  i <- do.call(what = data.frame, args = c(spectra))
  
  i <- data.frame(mz, i)
  colnames(i) <- c("full_mz", spectra_cols)
  
  i_melt <- melt(i, id.vars = "full_mz")
  colnames(i_melt) <- c("full_mz","Spectrum","Intensity")
  
  i_melt <- dplyr::filter(i_melt, Intensity != 0)
  
  i_melt <- i_melt %>% group_by(Spectrum)  %>% mutate(id = row_number())
  ordr <- i_melt[order(i_melt$Intensity, decreasing = TRUE),]
  
  r <- rep(NA, length(spectra_cols))
  
  for(j in 1:length(spectra_cols)){
    r[j] <- max(as.numeric(i_melt$id[which(i_melt$Spectrum == spectra_cols[j])]))
  }
  
  r_min <- min(r)
  
  logic <-which(r == r_min)
  
  trunc <- ordr[(which(ordr$id <= r[logic])),]
  
  i_trunc <- data.frame(matrix(ncol = length(unique(trunc$Spectrum)), nrow = max(trunc$id)))
  
  for(j in 1:length(unique(trunc$Spectrum))){
    
    i_trunc[j] <- trunc$Intensity[which(trunc$Spectrum == spectra_cols[j])]
    colnames(i_trunc)[j] <- spectra_cols[j]
    
  }
  
  i_trunc <- data.frame(i_trunc, avg = apply(i_trunc, 1, FUN = mean, na.rm = TRUE))
  
  for(j in 1:length(unique(trunc$Spectrum))){
    trunc$Intensity[which(trunc$Spectrum == colnames(i_trunc)[j])] <- i_trunc$avg
  }
  
  dat <- trunc[order(trunc[mass_dat], decreasing = FALSE), ] %>% spread(Spectrum, Intensity)
  
  dat[is.na(dat)] <- 0
  dat <- dat[-2]
  return(as.data.frame(dat))
}


