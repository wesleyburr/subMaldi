#############################################################################
#
# IMPORT DATA INTO PAIRWISE DATA FRAME
#


# Import .csv mass spectra files from a directory and export as .rda's for use in data frame
# Basically just a quick and easy way to turn .csv files into individual data frames per spectrum
readcsvSpec <- function(spec_file, massCol, intenseCol){
  temp <- read.csv(file = spec_file)
  mass <- select(temp, massCol)
  Intensity <- select(temp, intenseCol)
  mass <- unlist(cbind(mass))
  Intensity <- unlist(cbind(Intensity))
  spec <- data.frame(mass, Intensity)
  return(spec)
}

readcsvDir <- function(direct, massCol, intenseCol, output){
  all_files <- list.files(path = direct, pattern = ".csv")
  for(j in 1:length(all_files)) {
    temp <- read.csv(  paste0(direct, all_files[ j ]) )
    mass <- select(temp, massCol)
    Intensity <- select(temp, intenseCol)
    mass <- unlist(cbind(mass))
    Intensity <- unlist(cbind(Intensity))
    spec <- data.frame(mass, Intensity)

    new_name <- strsplit(all_files[ j ], "\\.")[[1]][1]
    assign(x = new_name, spec)
    save(file = paste0(output, new_name, ".rda"), list = new_name)
  }
}

#############################################################################
#
# MAKE EMPTY DATA FRAME
#

createSpecDF <- function(min_mz = 53.76, max_mz = 1100, res = 0.0001, dig = 4) {
  full_mz <- seq(min_mz, max_mz, by = res)
  spec_df <- data.frame(full_mz,
                        Sample = 0)
  spec_df$full_mz <- round(spec_df$full_mz, digits = dig)
  return(spec_df)
}


#############################################################################
#
# ADJUST DATA TO MATCH full_mz + GET RID OF DUPLICATES
#

# aBN_A11 <- rm(aBN_A11.mz, aBN_A11.intense)
# Load your pairwise dataframe .rda's from import
# load("./Data/Objects/R Objects/Raw/August 1/aBN_A11.rda")

# Get rid of duplicated values and pick max. intensity; round to 4 digits
# Map to full_mz in spec_df

mapSpectrum <- function(dat, massCol, intenseCol, dig = 4, thresh = 1e-4, spec_df, colName) {
  # Sanity checks
  stopifnot( is.character(massCol), is.character(intenseCol),
             massCol %in% names(dat), intenseCol %in% names(dat) )
  stopifnot(colName %in% names(spec_df))

  # Round m/z data from dat to match full_mz
  dat <- round(dat, digits = dig)

  # Clean code by subsetting to vectors
  mass <- dat[[massCol]]
  intense <- dat[[intenseCol]]

  # Quantify the spaces between m/z values, make vector of all differences greater than threshold
  diffs <- mass[-1] - mass[-length(mass)]
  too_close <- which(diffs < thresh)
  throw_away <- c()

  if(length(too_close) > 0) { # not only isolated peaks
    # fix the 'too close together' problem: identify local maximums
    dupes <- which(duplicated(mass))  # identifies the duplicates, but not
    # the points they're duplicates _of_
    if(length(dupes) > 0) { # found some ...
      for(j in 1:length(dupes)) { # cycle through
        mz.val <- mass[dupes[j]]  # grab the mz that's duplicated
        all_dupes_tmp <- which(mass == mz.val)  # find all mz's that match
        which_max <- which(intense[all_dupes_tmp] ==
                             max(intense[all_dupes_tmp]))  # find the peak with the biggest intensity
        throw_away <- c(throw_away, all_dupes_tmp[-which_max]) # put the rest in throw-away
      }
    }
  }

  throw_away <- unique(throw_away) # could get duplicates in throw away

  # I think this is the part that needs to be fixed - original data frame isn't getting trimmed
  if(length(throw_away) > 0) {
    dat <- dat[-throw_away, ]
  } 

  # last minute sanity check
  if(length(which(duplicated(dat[[massCol]])) > 0)) {
    dat <- dat[-which(duplicated(dat[[massCol]])), ]
  }

  # Fill in the data frame with the rounded, unique data
  spec_df[spec_df$full_mz %in% dat[[massCol]], colName] <- dat[[intenseCol]]
  return(spec_df)

}

#############################################################################
#
# SUBTRACT BLANKS FROM SAMPLES
#

# Make sure you transform dat to have a Sub_Sample column for the subtracted spec.
# spec.df <- transform(spec.df, Sub.C15 = 0)

# Fill in the subtraction columns/Subtract your blank
subSpectra <- function(dat, Blank_Var, Sample, Sub_Sample, showNeg = FALSE){
  # Sanity checks
  stopifnot( is.character(Blank_Var), is.character(Sample), is.character(Sub_Sample),
             Blank_Var %in% names(dat), Sample %in% names(dat), Sub_Sample %in% names(dat) )

  samp <- dat[[Sample]]
  blank <- dat[[Blank_Var]]
  sub_samp <- dat[[Sub_Sample]]

  # Fill in subtraction vector
  sub_samp[blank == 0] <- samp[blank == 0]
  # Otherwise, subtract
  sub_samp[blank != 0 ] <- samp[blank != 0] - blank[blank != 0]

  if(showNeg == TRUE){
  } else {
    # Find negatives that make no sense, and wipe them out
    sub_samp[sub_samp < 0] <- 0
  }

  # Replace the data into the data frame and return
  dat[[Sub_Sample]] <- sub_samp
  return(dat)
}

#############################################################################
#
# AVERAGE SPECTRA
#

# Average intensities across rows per sample
# First user should organize standardized data frame so all samples are together
# First column should always be full_mz, the rest should be spectra, each scan per column

# Then average the spectra
avgSpectra <- function(dat, method = "mean"){
  if(method == "sum") {
    dat <- transform(dat, Sum = apply(dat[, -1], 1, sum, na.rm = TRUE))
  } else {
    dat <- transform(dat, Average = apply(dat[, -1],1, mean, na.rm = TRUE))
  }

  # Want to make this like for every value of full_mz,average the values in all but the first column (full_mz)
  return(dat)

}

#############################################################################
#
# REMOVE EMPTY ROWS TO REDUCE LOAD
#

# Throw out every all-zero row to lessen the computational load and get rid of empty values
# Wouldn't recommend using this unless you've already made a master frame with all of the samples you
# want to compare as your data will, once again, become irregularly spaced!
rmveEmpty <- function(dat){
  dat <- dat[rowSums(dat[, -1]) > 0, ]
  return(dat)
}

