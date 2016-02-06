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