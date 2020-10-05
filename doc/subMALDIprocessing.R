## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library("subMALDI")


data("bsline")

## ----warning = FALSE, fig.width = 6, fig.height = 3, fig.align = "center"-----
plotSpectrum(dat = bsline, mass_dat = bsline$mass, intensity_dat = bsline$raw,
             colour = "black", min_mz = 200, max_mz = 5000, x_ticks = 500)

## -----------------------------------------------------------------------------
sgolay <- smoothSpectrum(dat = bsline, mass_dat = "mass", intensity_dat = "raw",
                       method = "sgolay", p = 4, n = 7, m = 0, ts = 1)

## ----warning = FALSE, fig.width = 6, fig.height = 4, fig.align = "center"-----
test <- data.frame(bsline$mass, bsline$raw, sgolay$sg)
names(test) <- c("mass", "Raw Spectrum", "Savitzky-Golay Smooth")

plotSpectra(dat = test, mass_dat = "mass", spec1 = "Raw Spectrum", 
            spec2 = "Savitzky-Golay Smooth", min_mz = 200, max_mz = 5000,
            x_ticks = 500)

## -----------------------------------------------------------------------------
mov_avg <- smoothSpectrum(dat = bsline, mass_dat = bsline$mass, intensity_dat = bsline$raw,
                       method = "mov_avg", n = 7)

## -----------------------------------------------------------------------------
test <- createSpecDF(min_mz = 200, max_mz = 5000)
test <- select(test, full_mz)
test <- transform(test, "RawSpectrum" = 0, "MovingAverage" = 0)
test <- mapSpectrum(bsline, massCol = "mass", intenseCol = "raw", 
                 colName = "RawSpectrum", spec_df = test)
test <- mapSpectrum(mov_avg, massCol = "mass", intenseCol = "mov_avg", 
                 colName = "MovingAverage", spec_df = test)
test <- rmveEmpty(test)

## ----warning = FALSE, fig.width = 6, fig.height = 4, fig.align = "center"-----
plotSpectra(dat = test, mass_dat = "full_mz", spec1 = "RawSpectrum", 
            spec2 = "MovingAverage", min_mz = 200, max_mz = 5000,
            x_ticks = 500)

## -----------------------------------------------------------------------------
monotone_min <- baselineCorr(dat = sgolay, mass_dat = "mass", 
                             intensity_dat = "sg", method = "monotone_min")

## ----warning = FALSE, fig.width = 6, fig.height = 4, fig.align = "center"-----
test <- data.frame(sgolay$mass, sgolay$sg, monotone_min$baseline)
names(test) <- c("mz", "Smoothed", "BaselineCorrected")

plotSpectra(dat = test, mass_dat = "mz", spec1 = "Smoothed", 
            spec2 = "BaselineCorrected", min_mz = 200, max_mz = 5000,
            x_ticks = 500)

## -----------------------------------------------------------------------------
linear_int <- baselineCorr(dat = mov_avg, mass_dat = "mass", 
                           intensity_dat = "mov_avg", method = "linear", n = 7)

## ----warning = FALSE, fig.width = 6, fig.height = 4, fig.align = "center"-----
test <- data.frame(mov_avg$mass, mov_avg$mov_avg, linear_int$baseline)
names(test) <- c("mz", "Smoothed", "BaselineCorrected")

plotSpectra(dat = test, mass_dat = "mz", spec1 = "Smoothed", 
            spec2 = "BaselineCorrected", min_mz = 200, max_mz = 5000,
            x_ticks = 500)

## -----------------------------------------------------------------------------
loess <- baselineCorr(dat = bsline, mass_dat = "mass", 
                      intensity_dat = "raw", method = "loess")

## ----warning = FALSE, fig.width = 6, fig.height = 4, fig.align = "center"-----
test <- data.frame(bsline$mass, bsline$raw, loess$baseline)
names(test) <- c("mz", "RawSpectrum", "BaselineCorrected")

plotSpectra(dat = test, mass_dat = "mz", spec1 = "RawSpectrum", 
            spec2 = "BaselineCorrected", min_mz = 200, max_mz = 5000,
            x_ticks = 500)

## -----------------------------------------------------------------------------
snr <- peakDet(monotone_min, "mz", "baseline", method = "snr", 
                  n = 7, SNR_thresh = 3)

## ----warning = FALSE, fig.width = 6, fig.height = 4, fig.align = "center"-----
test <- data.frame(monotone_min$mz, monotone_min$baseline, snr$peaks)
names(test) <- c("mz", "Baseline", "Peaks")

plotSpectra(dat = test, mass_dat = "mz", spec1 = "Baseline", 
            spec2 = "Peaks", min_mz = 200, max_mz = 5000,
            x_ticks = 500)

## -----------------------------------------------------------------------------
slopes <- peakDet(linear_int, "mz", "baseline", method = "slopes",
                    n = 7)

## ----warning = FALSE, fig.width = 6, fig.height = 4, fig.align = "center"-----
test <- data.frame(linear_int$mz, linear_int$baseline, slopes$peaks)
names(test) <- c("mz", "Baseline", "Peaks")

plotSpectra(dat = test, mass_dat = "mz", spec1 = "Baseline", 
            spec2 = "Peaks", min_mz = 200, max_mz = 5000,
            x_ticks = 500)

## -----------------------------------------------------------------------------
library(MassSpecWavelet)

## -----------------------------------------------------------------------------
scales <- seq(1,40, by = 1)

## -----------------------------------------------------------------------------
y <- bsline$raw
x <- bsline$mass

## -----------------------------------------------------------------------------
coeff <- cwt(y, scales = scales, wavelet = "mexh")
coeff <- cbind(y,coeff)
colnames(coeff) <- c(0,scales)

## -----------------------------------------------------------------------------
localMax <- getLocalMaximumCWT(coeff) 

## -----------------------------------------------------------------------------
ridgeList <- getRidge(localMax)

## -----------------------------------------------------------------------------
majorPeakInfo <- identifyMajorPeaks(y, ridgeList, coeff, 
                                    scales = as.numeric(colnames(coeff)),
                                    SNR.Th = 3)
peakIndex <- majorPeakInfo$peakIndex

## -----------------------------------------------------------------------------
peaks <- data.frame(x,y,"Peaks" = FALSE)

## -----------------------------------------------------------------------------
for(i in peakIndex){
  peaks[i, "Peaks"] <- TRUE
}

## -----------------------------------------------------------------------------
peaks[which(peaks$Peaks == FALSE), "y"] <- 0

## -----------------------------------------------------------------------------
cwt <- select(peaks, x, y)

## ----warning = FALSE, fig.width = 6, fig.height = 4, fig.align = "center"-----
test <- data.frame(bsline$mass, bsline$raw, cwt$y)
names(test) <- c("mz", "RawSpectrum", "CWT")

plotSpectra(dat = test, mass_dat = "mz", spec1 = "RawSpectrum", 
            spec2 = "CWT", min_mz = 200, max_mz = 5000,
            x_ticks = 500)

