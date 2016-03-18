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
  
  if (na.rm) v <- na.omit(v)
  
  # find median and mad value
  med_val <- median(v)
  mad_val <- mad(v)
  
  # calculate the upper and lower bounds
  h_pos <- med_val + sigma * mad_val
  h_neg <- med_val - sigma * mad_val
  
  # assign either, 1, -1 or 0
  out <- ifelse(v > h_pos, 1,
	    ifelse(v < h_neg, -1, 0))
  
  return(out)
}
