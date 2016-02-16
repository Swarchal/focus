#' Hampel test for outliers
#'
#' Applies a Hampel filter to a vector to detect outliers. An value is
#' defined as an outlier if it is beyond \code{sigma} median absolute
#' deviations (MAD) from the vector median.
#'
#' @param v numerical vector
#' @param sigma +/- MAD values for the normal range
#' @param na.rm Boolean, whether to remove NA values from median and mad calculations
#'
#' @return A vector of numerical values. 0 denotes a normal value, -1 is an
#'  outlier below the threshold, 1 is an outlier above the threshold.
#'
#' @export
#' @examples
#' vals <- sample(c(rnorm(1000), rnorm(10, 10), rnorm(10, -5)))
#' is_outlier <- hampel(vals)
#' plot(vals, col = as.factor(is_outlier), pch = 20)


hampel <- function(v, sigma = 4, na.rm = TRUE){
  
  # find median and mad value
  med_val <- median(v, na.rm)
  mad_val <- mad(v, na.rm)
  
  # calculate the upper and lower bounds
  h_pos <- med_val + sigma * mad_val
  h_neg <- med_val - sigma * mad_val
  
  # initialise empty vector for loop
  out <- rep(0, length(v))
  
  # loop through values returning either -1, 0 or +1
  for (i in 1:length(v)){
    if (v[i] > h_pos){
      out[i] <- 1
    } else if (v[i] < h_neg){
      out[i] <- -1
    }
  }
  
  return(out)
}
