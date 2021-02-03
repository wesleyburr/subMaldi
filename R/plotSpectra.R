##' Plot and Compare Spectra
##' 
##' Plot single or multiple spectra. For multiple spectra, a grid layout can be called using 'nrows'.
##' 
##' 
##' @param dat The name of the spectral data frame, containing the \emph{m/z}
##' data in the first column.
##' @param mass_dat A character string; the name of the column in \code{dat}
##' containing the \emph{m/z} data.
##' @param spectra_cols A character string; the name(s) of the column(s) in \code{dat}
##' containing the intensity data for the spectra-of-interest.
##' @param palette A character element; the RColorBrewer palette to use. See below for available palettes. 
##' @param colours A character string indicating the desired colour(s)
##' @param span Single numeric value; the span of peak maxima in between each
##' label. Default = 5 (ignores two peak maxima on either side of each label).
##' @param thresh Single numeric value (0-100); the threshold of peak
##' intensities which should be labeled.
##' @param lbls Logical. If \code{lbls = TRUE}, labels indicating the
##' \emph{m/z} value of each peak maxima within the indicated \code{span} will
##' be included in the output plot. If \code{lbls = FALSE}, labels are not
##' shown.
##' @param lbl.fmt Character string in the format "\%a.bf", where \emph{a} is
##' the number of figures displayed to the left of decimal places in labels,
##' and \emph{b} is the number of figures displayed to the right of decimal
##' places in labels.  Default = "\%3.4f".
##' @param min_mz Single numeric value; minimum \emph{m/z} value of the
##' observed range.
##' @param max_mz Single numeric value; upper end of \emph{m/z} range observed
##' in spectra.
##' @param min_I Single numeric value; minimum intensity value of the observed
##' range.
##' @param max_I Single numeric value; upper end of the intensity range
##' observed in spectra.
##' @param x_ticks Single numeric value; the number of ticks on the x-axis.
##' @param nrows Single integer value; the number of rows in plot arrangement.
##' @param intensity_scale A character string; the method that should be used
##' for determining the y-axis scales for each spectrum. If \code{method =
##' "free_y"}, each spectrum will be plotted with its own intensity scale. If
##' \code{method = "fixed"}, each spectrum will be plotted with the y-axis of
##' the most intense spectrum in the set.
##' 
##' @return Returns a line plot of the input spectra.
##' 
##' @section RColorBrewer Palettes: 
##' \describe{\itemize{\item{\code{"Accent"}}
##'                    \item{\code{"Dark2"}} 
##'                    \item{\code{"Paired"}}  
##'                    \item{\code{"Pastel1"}}  
##'                    \item{\code{"Pastel2"}}  
##'                    \item{\code{"Set1"}}  
##'                    \item{\code{"Set2"} (default)} 
##'                    \item{\code{"Set3"}}
##'                    } 
##'           }
##' 
##' 
##' @author Kristen Yeh <kristenyeh@@trentu.ca> Wesley Burr <wburr@@trentu.ca> Sophie Castel <sophie.castel@@ontariotechu.net>
##' @references https://github.com/wesleyburr/subMaldi
##' @keywords methods aplot
##' @examples
##' 
##' ## Plotting using the sample dataset "Master.rda"
##' data("Master")
##' plotSpectra(dat = Master, mass_dat = "full_mz",
##'             spectra_cols = c("Blank1", "Blank2"),
##'             intensity_scale = "free_y", lbls = TRUE, nrows = 2, x_ticks = 10)

# -----------------------------------------------------------------------
# Last Updated: February 3, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Plot and Compare Multiple Spectra
# -----------------------------------------------------------------------

plotSpectra <- function(dat, mass_dat,
                        spectra_cols,
                        palette = "Set2",
                        colours = brewer.pal(length(spectra_cols), palette),
                        span = 5,
                        thresh = 0.1,
                        lbls = FALSE, 
                        lbl.fmt = "%3.4f",
                        min_mz = 0,
                        max_mz = max(dat[[mass_dat]]),
                        min_I = 0,
                        max_I = max(dat[spectra_cols]),
                        x_ticks = 100,
                        nrows = round_any(length(spectra_cols), 2, f = ceiling)/2,
                        intensity_scale = "free_y"){   
  
  # ----------------------------------
  # LOGICAL CHECKS
  # ----------------------------------
  
  colours <- colours[1:length(spectra_cols)]
  
  
  stopifnot(spectra_cols %in% colnames(dat),
            mass_dat %in% colnames(dat),
            is.character(colours),
            length(colours) == length(spectra_cols),
            intensity_scale == "free_x" | intensity_scale == "free_y" | intensity_scale == "free",
            all(is.numeric(nrows), 
                is.numeric(min_mz), 
                is.numeric(max_mz), 
                is.numeric(min_I), 
                is.numeric(max_I), 
                is.numeric(x_ticks),
                is.numeric(span),
                is.numeric(thresh)),
            palette %in% c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3")
            
            )
  
  
  mass <- dat[[mass_dat]]
  
  if(min_mz < min(mass)){
    warning("Specified value of 'min_mz' is beyond the mass range in 'dat'. Defaulting to the minimum mass.")
    min_mz <- floor(min(mass)) 
    }
  
  if(max_mz > max(mass)){
    warning("Specified value of 'max_mz' is beyond the mass range in 'dat'. Defaulting to the maximum mass.")
    max_mz <- ceiling(max(mass)) 
  }
  

  
  
  spectra <- lapply(X = spectra_cols, FUN = function(x){ dat[x] })
  
  i <- do.call(what = data.frame, args = c(spectra))
  
  dat <- data.frame(full_mz = mass, i)
  colnames(dat) <- c("full_mz", spectra_cols)
  
  dat_melt <- melt(dat, id.vars = "full_mz")
  colnames(dat_melt) <- c("full_mz","Spectrum","Intensity")
  
  p <- ggplot(data = dat_melt) + geom_line(aes(x = full_mz, y = Intensity, colour = Spectrum)) + 

        labs(x = expression(italic("m/z")), y = "Intensity") +

        facet_wrap(~Spectrum, nrow = nrows, scales = intensity_scale) +

        scale_x_continuous(limits = c(min_mz, max_mz), 
                           breaks = seq(min_mz, max_mz, 
                                    length.out = x_ticks)) +
    
        scale_y_continuous(limits = c(min_I, max_I)) +

        theme_bw() + theme(panel.border = element_blank(),
                           strip.background = element_blank(),
                           strip.text.x = element_blank(),
                           legend.position = "bottom",
                           axis.line = element_line(colour = "grey85",
                                                    size = 0.5)) +

        scale_color_manual(values = colours)
  
  if(lbls){
    
    test_lbl <- function(mass_dat, lbl.fmt){
      x <- mass_dat
      dp <- c()
      out <- c()
      
      lbl.dec <- strsplit(lbl.fmt, "[.]")[[1]][2]
      lbl.dec <- as.numeric(gsub("[a-zA-Z ]", "", lbl.dec))
      
      deci <- function(x) {
        if ((x %% 1) != 0) {
          nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
        } else {
          return(0)
        }
      }
      
      for(i in 1:length(x)){
        dp[i] <- .deci(x[i])
      }
      
      dp <- max(dp)
      out <- isTRUE(dp < lbl.dec) 
      return(out)
    }
    
    
    # Check that labels have <= decimal places as data
    tst <- test_lbl(mass_dat = mass, lbl.fmt = lbl.fmt)
    
    if(tst){
      stop("Label format indicates more decimal places 
           than given in data.")
    }
    
    p <- p + stat_peaks(aes(x = full_mz, y = Intensity, group = 1),
                        ignore_threshold = thresh, span = span,
                        x.label.fmt = lbl.fmt, geom = "text",
                        check_overlap = TRUE, color = "black", cex = 3.0)
  }
  
  return(p)
  
}


# -----------------------------------------------------------------------

