<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 191e6b7... Rendered new documentation using package 'roxygen2'.
=======
>>>>>>> ae03a50... Rendered new documentation using package 'roxygen2'.
##' Average Spectral Replicates
##' 
##' Combines spectral replicates either by averaging (method = "mean") or
##' summing (method = "sum") the intensity values across each row representing a
##' mass-to-charge value in \code{full_mz}.
##' 
##' 
##' @param dat The mapped spectral data frame, containing \code{full_mz} in the
##' first column.
##' @param method A character string; the method used to combine the spectra.
##' Methods include "sum" and "mean". Default = "mean."
##' @param spectra_cols A character vector; the names of the column in
##' \code{dat} containing the intensity data for the spectra-of-interest.
##' @return Returns a new column in the input data frame containing the
##' averaged intensity data.
##' @author Kristen Yeh <kristenyeh@@trentu.ca> Wesley Burr <wburr@@trentu.ca>
##' Sophie Castel <sophie.castel@@ontariotechu.net>
##' @references https://github.com/wesleyburr/subMaldi
##' @keywords manip methods
##' @examples
##' 
##' ## Load sample dataset "Master.rda"
##' data("Master")
##' 
##' ## Average blank spectrum 1 and 2 using the method "mean"
##' ex <- avgSpectra(Master, method = "mean", spectra_cols = c("Blank1", "Blank2"))
##' 
##' ## Average blank spectrum 1 and 2 using the method "sum"
##' ex <- avgSpectra(Master, method = "sum", spectra_cols = c("Blank1", "Blank2"))
##' 


<<<<<<< HEAD
<<<<<<< HEAD
# --------------------------------------------------------------------------------------------
# Date: January 27, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Average Spectral Replicates
=======
# --------------------------------------------------------------------------------------------
# Date: January 27, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI Average Spectra
>>>>>>> 3f68890... Updated avgSpectra
=======
# --------------------------------------------------------------------------------------------
# Date: January 27, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Average Spectral Replicates
>>>>>>> 191e6b7... Rendered new documentation using package 'roxygen2'.
=======
# --------------------------------------------------------------------------------------------
# Date: January 27, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI Average Spectra
>>>>>>> 5800ee0... Updated avgSpectra
=======
# --------------------------------------------------------------------------------------------
# Date: January 27, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Average Spectral Replicates
>>>>>>> ae03a50... Rendered new documentation using package 'roxygen2'.
=======
# --------------------------------------------------------------------------------------------
# Date: June 23, 2020
# Author: Kristen Yeh
# Title: subMALDI Average Spectra
>>>>>>> 3f68890... Updated avgSpectra
# --------------------------------------------------------------------------------------------

# Average intensities across rows per sample
# First user should organize standardized data frame so all samples are together
# First column should always be full_mz, the rest should be spectra, each scan per column

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD

avgSpectra <- function(dat, method = "mean", spectra_cols){
  
  # --------------
  # LOGICAL CHECKS
  # --------------
  
  if(length(spectra_cols) < 2){
    stop("Only one spectrum input. Please enter two spectra for averaging.")
  }
  
  if(!all(spectra_cols %in% colnames(dat))){
    logic <- which(!(spectra_cols %in% colnames(dat)))
    stop(c("Columns '",paste0(as.character(spectra_cols[logic]), sep = "', "), " not found in specified dataframe."))
  }
  
  if(method == "sum"){
    .avg_sum(dat = dat, spectra_cols) } 
  else if(method == "mean"){
    .avg_mean(dat = dat, spectra_cols) }
=======
# UPDATE JUNE 2020:
# Editing the function so that you select which spectra are averaged
# New column is made in original df beside the selected spectra
=======
>>>>>>> 5cce370... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies

avgSpectra <- function(dat, method = "mean", spectra_cols){
  
  # --------------
  # LOGICAL CHECKS
  # --------------
  
  if(length(spectra_cols) < 2){
    stop("Only one spectrum input. Please enter two spectra for averaging.")
  }
  
  if(!all(spectra_cols %in% colnames(dat))){
    logic <- which(!(spectra_cols %in% colnames(dat)))
    stop(c("Columns '",paste0(as.character(spectra_cols[logic]), sep = "', "), " not found in specified dataframe."))
  }
  
  if(method == "sum"){
<<<<<<< HEAD
=======
# UPDATE JUNE 2020:
# Editing the function so that you select which spectra are averaged
# New column is made in original df beside the selected spectra
=======
>>>>>>> 29ad768... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies

avgSpectra <- function(dat, method = "mean", spectra_cols){
  
  # --------------
  # LOGICAL CHECKS
  # --------------
  
  if(length(spectra_cols) < 2){
    stop("Only one spectrum input. Please enter two spectra for averaging.")
  }
  
  if(!all(spectra_cols %in% colnames(dat))){
    logic <- which(!(spectra_cols %in% colnames(dat)))
    stop(c("Columns '",paste0(as.character(spectra_cols[logic]), sep = "', "), " not found in specified dataframe."))
  }
  
  if(method == "sum"){
<<<<<<< HEAD
>>>>>>> 5800ee0... Updated avgSpectra
=======
# UPDATE JUNE 2020:
# Editing the function so that you select which spectra are averaged
# New column is made in original df beside the selected spectra

avgSpectra <- function(dat, method = "mean", spec1, spec2, 
                       spec3 = NULL, spec4 = NULL, spec5 = NULL, spec6 = NULL){
  
  if(method == "sum"){
>>>>>>> 3f68890... Updated avgSpectra
    .avg_sum(dat = dat, spec1 = spec1, spec2 = spec2, spec3 = spec3, spec4 = spec4,
             spec5 = spec5, spec6 = spec6) } 
  else{
    .avg_mean(dat = dat, spec1 = spec1, spec2 = spec2, spec3 = spec3, spec4 = spec4,
             spec5 = spec5, spec6 = spec6) }
<<<<<<< HEAD
<<<<<<< HEAD
>>>>>>> 3f68890... Updated avgSpectra
=======
    .avg_sum(dat = dat, spectra_cols) } 
  else if(method == "mean"){
    .avg_mean(dat = dat, spectra_cols) }
>>>>>>> 5cce370... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies
=======
>>>>>>> 5800ee0... Updated avgSpectra
=======
    .avg_sum(dat = dat, spectra_cols) } 
  else if(method == "mean"){
    .avg_mean(dat = dat, spectra_cols) }
>>>>>>> 29ad768... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies
=======
>>>>>>> 3f68890... Updated avgSpectra
}

# --------------
# METHOD = SUM
# --------------

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
.avg_sum <- function(dat, spectra_cols){
  
  mz <- dat$full_mz
  
  spectra <- lapply(spectra_cols, function(x){dat[x]})
  i <- do.call(what = data.frame, args = c(spectra))
  
  dat <- data.frame(cbind(mz, dat, Sum = apply(i, 1, sum, na.rm = TRUE)))
<<<<<<< HEAD
  
  return(dat)
}

=======
=======
>>>>>>> 5800ee0... Updated avgSpectra
=======
>>>>>>> 3f68890... Updated avgSpectra

.avg_sum <- function(dat, spec1, spec2, spec3 = NULL, spec4 = NULL,
                     spec5 = NULL, spec6 = NULL){
  
  if(is.null(spec6)){
  
  if(is.null(spec5)){
<<<<<<< HEAD
<<<<<<< HEAD
=======
.avg_sum <- function(dat, spectra_cols){
>>>>>>> 5cce370... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies
  
  mz <- dat$full_mz
  
  spectra <- lapply(spectra_cols, function(x){dat[x]})
  i <- do.call(what = data.frame, args = c(spectra))
  
  dat <- cbind(mz, dat, Sum = apply(i, 1, sum, na.rm = TRUE))
=======
>>>>>>> bede9d4... JOSS 12 (Issue):  Generalized functions for any number of spectra; removed redundancies
  
  return(dat)
}

<<<<<<< HEAD

>>>>>>> 3f68890... Updated avgSpectra
=======
>>>>>>> 5cce370... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies
=======
=======
.avg_sum <- function(dat, spectra_cols){
>>>>>>> 29ad768... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies
  
  mz <- dat$full_mz
  
  spectra <- lapply(spectra_cols, function(x){dat[x]})
  i <- do.call(what = data.frame, args = c(spectra))
  
  dat <- data.frame(cbind(mz, dat, Sum = apply(i, 1, sum, na.rm = TRUE)))
  
  return(dat)
}

<<<<<<< HEAD

>>>>>>> 5800ee0... Updated avgSpectra
=======
>>>>>>> 29ad768... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies
=======
  
  if(is.null(spec4)){
  
  if(is.null(spec3)){
  
  if(is.null(spec2)){
  
  # One spectrum
  stop("Only one spectrum input. Please enter two spectra for averaging.")}

  # ---------------------------------------------------------------------
  # Two spectra
  else{
    i1 <- dat[[spec1]]
    i2 <- dat[[spec2]]
    i <- data.frame(i1,i2)
    i <- transform(i, avg = apply(i,1, sum, na.rm = TRUE))
    
    dat <- transform(dat, "Sum" = 0)
    dat["Sum"] <- i$avg
    return(dat) }}
  
  # ---------------------------------------------------------------------
  # Three spectra
  else{
    i1 <- dat[[spec1]]
    i2 <- dat[[spec2]]
    i3 <- dat[[spec3]]
    i <- data.frame(i1,i2,i3)
    i <- transform(i, avg = apply(i,1, sum, na.rm = TRUE))
    
    dat <- transform(dat, "Sum" = 0)
    dat["Sum"] <- i$avg
    return(dat) }}
  
  # ---------------------------------------------------------------------
  #Four spectra
  else{
    i1 <- dat[[spec1]]
    i2 <- dat[[spec2]]
    i3 <- dat[[spec3]]
    i4 <- dat[[spec4]]
    i <- data.frame(i1,i2,i3,i4)
    i <- transform(i, avg = apply(i,1, sum, na.rm = TRUE))
    
    dat <- transform(dat, "Sum" = 0)
    dat["Sum"] <- i$avg
    return(dat) }}

  # ---------------------------------------------------------------------
  #Five spectra
  else{
    i1 <- dat[[spec1]]
    i2 <- dat[[spec2]]
    i3 <- dat[[spec3]]
    i4 <- dat[[spec4]]
    i5 <- dat[[spec5]]
    i <- data.frame(i1,i2,i3,i4,i5)
    i <- transform(i, avg = apply(i,1, sum, na.rm = TRUE))
    
    dat <- transform(dat, "Sum" = 0)
    dat["Sum"] <- i$avg
    return(dat) }}

  # ---------------------------------------------------------------------
  # Six spectra
  else{
    i1 <- dat[[spec1]]
    i2 <- dat[[spec2]]
    i3 <- dat[[spec3]]
    i4 <- dat[[spec4]]
    i5 <- dat[[spec5]]
    i6 <- dat[[spec6]]
    i <- data.frame(i1,i2,i3,i4,i5,i6)
    i <- transform(i, avg = apply(i,1, sum, na.rm = TRUE))
    
    dat <- transform(dat, "Sum" = 0)
    dat["Sum"] <- i$avg
    return(dat)
  }
}


>>>>>>> 3f68890... Updated avgSpectra
# --------------
# METHOD = MEAN
# --------------

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
.avg_mean <- function(dat, spectra_cols){
  
  mz <- dat$full_mz
  
  spectra <- lapply(spectra_cols, function(x){dat[x]})
  i <- do.call(what = data.frame, args = c(spectra))
  
  dat <- data.frame(mz, dat, Average = apply(i, 1, mean, na.rm = TRUE))
<<<<<<< HEAD
<<<<<<< HEAD
  
  return(dat)
}
=======
.avg_mean <- function(dat, spec1, spec2, spec3 = NULL, spec4 = NULL,
                      spec5 = NULL, spec6 = NULL){
=======
.avg_mean <- function(dat, spectra_cols){
>>>>>>> 5cce370... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies
  
  mz <- dat$full_mz
  
<<<<<<< HEAD
=======
.avg_mean <- function(dat, spec1, spec2, spec3 = NULL, spec4 = NULL,
                      spec5 = NULL, spec6 = NULL){
=======
.avg_mean <- function(dat, spectra_cols){
>>>>>>> 29ad768... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies
  
  mz <- dat$full_mz
  
<<<<<<< HEAD
>>>>>>> 5800ee0... Updated avgSpectra
=======
.avg_mean <- function(dat, spec1, spec2, spec3 = NULL, spec4 = NULL,
                      spec5 = NULL, spec6 = NULL){
  
  if(is.null(spec6)){
    
    if(is.null(spec5)){
      
      if(is.null(spec4)){
        
        if(is.null(spec3)){
          
          if(is.null(spec2)){
            
            # One spectrum
            stop("Only one spectrum input. Please enter two spectra for averaging.")}
          
          # ---------------------------------------------------------------------
          # Two spectra
          else{
            i1 <- dat[[spec1]]
            i2 <- dat[[spec2]]
            i <- data.frame(i1,i2)
            i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
            
            dat <- transform(dat, "Average" = 0)
            dat["Average"] <- i$avg
            return(dat) }}
        
        # ---------------------------------------------------------------------
        # Three spectra
        else{
          i1 <- dat[[spec1]]
          i2 <- dat[[spec2]]
          i3 <- dat[[spec3]]
          i <- data.frame(i1,i2,i3)
          i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
          
          dat <- transform(dat, "Average" = 0)
          dat["Average"] <- i$avg
          return(dat) }}
      
      # ---------------------------------------------------------------------
      #Four spectra
      else{
        i1 <- dat[[spec1]]
        i2 <- dat[[spec2]]
        i3 <- dat[[spec3]]
        i4 <- dat[[spec4]]
        i <- data.frame(i1,i2,i3,i4)
        i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
        
        dat <- transform(dat, "Average" = 0)
        dat["Average"] <- i$avg
        return(dat) }}
    
    # ---------------------------------------------------------------------
    #Five spectra
    else{
      i1 <- dat[[spec1]]
      i2 <- dat[[spec2]]
      i3 <- dat[[spec3]]
      i4 <- dat[[spec4]]
      i5 <- dat[[spec5]]
      i <- data.frame(i1,i2,i3,i4,i5)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      dat <- transform(dat, "Average" = 0)
      dat["Average"] <- i$avg
      return(dat) }}
  
>>>>>>> 3f68890... Updated avgSpectra
  # ---------------------------------------------------------------------
  # Six spectra
  else{
    i1 <- dat[[spec1]]
    i2 <- dat[[spec2]]
    i3 <- dat[[spec3]]
    i4 <- dat[[spec4]]
    i5 <- dat[[spec5]]
    i6 <- dat[[spec6]]
    i <- data.frame(i1,i2,i3,i4,i5,i6)
    i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
    
    dat <- transform(dat, "Average" = 0)
    dat["Average"] <- i$avg
    return(dat)
  }
}


















<<<<<<< HEAD
<<<<<<< HEAD
>>>>>>> 3f68890... Updated avgSpectra
=======
=======
>>>>>>> 29ad768... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies
  spectra <- lapply(spectra_cols, function(x){dat[x]})
  i <- do.call(what = data.frame, args = c(spectra))
  
  dat <- cbind(mz, dat, Average = apply(i, 1, mean, na.rm = TRUE))
<<<<<<< HEAD
=======
>>>>>>> bede9d4... JOSS 12 (Issue):  Generalized functions for any number of spectra; removed redundancies
=======
>>>>>>> f78c2c2... JOSS 12 (Issue):  Generalized functions for any number of spectra; removed redundancies
  
  return(dat)
}
>>>>>>> 5cce370... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies
=======
>>>>>>> 5800ee0... Updated avgSpectra
=======
  
  return(dat)
}
>>>>>>> 29ad768... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies
=======
>>>>>>> 3f68890... Updated avgSpectra
