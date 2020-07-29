# -----------------------------------------------------------------------
# Last Updated: July 15, 2020
# Author: Kristen Yeh
# Title: subMALDI Plot Label Points
# -----------------------------------------------------------------------


# Check that labels have less than or equal to decimal places as data
.decimalplaces <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

# will go inside plot func
.test <- function(mass_dat, lbl.fmt){
  x <- mass_dat
  dp <- c()
  out <- c()
  
  lbl.dec <- strsplit(lbl.fmt, "[.]")[[1]][2]
  lbl.dec <- as.numeric(gsub("[a-zA-Z ]", "", lbl.dec))
  
  for(i in 1:length(x)){
  dp[i] <- .decimalplaces(x[i])
  }
  
  dp <- max(dp)
  out <- isTRUE(dp < lbl.dec) 
  return(out)
}

  
  
 # -----------------------------------------------------------------------