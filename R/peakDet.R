<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 191e6b7... Rendered new documentation using package 'roxygen2'.
##' Peak Detection
##' 
##' Two methods for peak detection in baseline corrected spectral data. Methods
##' include signal-to-noise ratio and slopes of peaks.
##' 
##' 
##' @param dat The name of the spectral data frame, containing \code{m/z} data
##' in the first column and spectral intensity data in subsequent columns.
##' @param mass_dat Character string. The name of the column in \code{dat}
##' containing the \emph{m/z} data for the spectrum.
##' @param intensity_dat Character string. The name of the column in \code{dat}
##' containing the intensity data for the spectrum.
##' @param method Character string. The method of peak detection. Either
##' \code{"snr"} for signal-to-noise ratio, or \code{"slopes"} for slopes of
##' peaks.
##' @param n Single numeric value. For both \code{method = "snr"} and
##' \code{method = "slopes"}, the window size used to calculate noise. Noise is
##' defined as the median of the absolute deviation (MAD) of points within a
##' window [1].
##' @param SNR_thresh Single numeric value. When \code{method = "snr"}, the
##' signal-to-noise ratio (SNR) threshold for discarding peaks. If the SNR of a
##' peak falls below this threshold, it will be discarded.
##' @return Returns a new data frame containing only the peaks which have
##' passed the detection criteria.
##' @section Methods: \describe{ \item{snr}{ Each spectrum is divided into
##' segments of size \code{n}. Noise is calculated as the median absolute
##' deviation of points within each segment [1]. If the intensity of a peak
##' divided by the noise in that segment is less than the indicated
##' \code{SNR_thresh}, the peak is discarded. } \item{slopes}{ Uses the shapes
##' of peaks to remove false peak candidates [1]. First, the left and right
##' endpoints of each peak are identified on the baseline. Next, the slopes of
##' each endpoint are evaluated. If the either the left or right slope are less
##' than a defined threshold, the peak candidate is discarded [1]. The
##' threshold is defined as half of the local noise level, or half of the
##' median absolute deviation in a window of size \code{n}.} }
##' @author Kristen Yeh <kristenyeh@@trentu.ca> Wesley Burr <wburr@@trentu.ca>
##' @seealso \code{\link{smoothSpectrum}}, \code{\link{baselineCorr}}
##' @references https://github.com/wesleyburr/subMaldi (1) Yang, C., He, Z. &
##' Yu, W. Comparison of public peak detection algorithms for MALDI mass
##' spectrometry data analysis. BMC Bioinformatics 10, 4 (2009).
##' https://doi.org/10.1186/1471-2105-10-4
##' @keywords methods manip
##' @examples
##' 
##' ## Load sample dataset "bsline"
##' data("bsline")
##' 
##' ## Baseline correct using method "linear"
##' linear <- baselineCorr(bsline, "mass", "raw", 
##'                         method = "linear", n = 7)
##' 
##' ## Detect peaks using method "snr"
##' snr <- peakDet(linear, "mz", "baseline", method = "snr", 
##'                   n = 7, SNR_thresh = 3)
##' 
##' ## Detect peaks using method "slopes"
##' slopes <- peakDet(linear, "mz", "baseline", method = "slopes",
##'                     n = 7)
##'


<<<<<<< HEAD
# ----------------------------------------------------------------------------
# Last Updated: August 25, 2020
# Author: Kristen Yeh
# Title: subMALDI: Peak Detection 
=======
=======
>>>>>>> 2931ed6... Added pre-processing vignette
# ----------------------------------------------------------------------------
# Last Updated: August 25, 2020
# Author: Kristen Yeh
# Title: subMALDI - Peak Detection Parent Function
<<<<<<< HEAD
>>>>>>> 5831098... Added pre-processing vignette
=======
# ----------------------------------------------------------------------------
# Last Updated: August 25, 2020
# Author: Kristen Yeh
# Title: subMALDI: Peak Detection 
>>>>>>> 191e6b7... Rendered new documentation using package 'roxygen2'.
=======
>>>>>>> 2931ed6... Added pre-processing vignette
# ----------------------------------------------------------------------------


peakDet <- function(dat, mass_dat, intensity_dat, method = NULL, n = NULL, 
                    SNR_thresh = NULL){
  
  if(is.null(method)){
    stop("Please select a valid method. See ?peakDet for list of methods.")
  } else if(method == "snr"){ 
    if(is.null(n) | is.null(SNR_thresh)){
      stop("Missing argument n or SNR_thresh.")
    }else{ .peak_snr(dat, mass_dat, intensity_dat, n = n, 
                     SNR_thresh = SNR_thresh) }
  } else if(method == "slopes"){ 
    if(is.null(n)){
      stop("Missing argument n. Please select a window size.")
    }else{.peak_slope(dat = dat, mass_dat = mass_dat, 
                intensity_dat = intensity_dat, n = n) }
  }
}


# ----------------------------------------------------------------------------