#' Find all image quality outliers
#'
#' \code{quality_matrix} calls the hampel outlier test for ImageQuality
#' columns in a dataframe.
#'
#' @param df dataframe containing ImageQuality data
#' @param ... additional arguments to be passed to \code{hampel()}
#'
#' @export

quality_matrix <- function(df, ...){
  
  # create dataframe of just ImageQuality data
  df_qc <- df[, get_QC_cols(df)]
  
  # calculate hampel outlier test for each image quality metric
  qc_out <- sapply(df_qc, function(x) hampel(x, ...))
  return(qc_out)
  
}


#' The number of QC metrics the image fails
#'
#' Given a dataframe or matrix produced from \code{quality_matrix}
#' \code{total outliers} will calculate the number of ImageQuality
#' columns the image failed.
#'
#' @param x dataframe or matrix of outlier values. An outlier is a non-zero number.
#'   \code{quality_matrix()} returns a useable dataframe for this.
#'
#' @export 

total_outliers <- function(x) apply(as.matrix(x), 1, function(x) sum(abs(x)))