#
# Peak Labeller
#

find_top_peaks <- function(x, y, n_peaks = n_peaks) {

  if(length(y) > 100000) {
    q <- quantile(y, 0.995)
  } else {
    q <- quantile(y, 0.99)
  }
  top_10 <- which(y >= q)

  # ARBITRARY PRECISION
  highest <- vector(length = length(top_10))
  done <- FALSE
  j <- k <- 1
  while(!done) {
    our_range <- max(top_10[j]-50, 1):min(top_10[j]+50, length(y))
    candidates <- y[our_range]
    highest[k] <- our_range[min(which(candidates == max(candidates)))]
    k <- k + 1
    top_10 <- top_10[-which(top_10 %in% our_range)]
    if(length(top_10) == 0) { done <- TRUE }
  }
  highest <- highest[-which(highest == 0)]
  highest <- unique(highest)

  final_idx <- (highest[order(y[highest], decreasing = TRUE)])[1:n_peaks]
  dat <- data.frame(x = x[final_idx],
                    y = y[final_idx],
                    l = as.character(x[final_idx]),
                    stringsAsFactors = FALSE)
  return(dat)
}
