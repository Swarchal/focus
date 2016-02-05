#' Get quality control columns
#'
#' Returns the columns in the dataframe produced from MeasureImageQuality
#'
#' @param df dataframe
#' @return Column names from the MeasureImageQuality module
#' @export

get_QC_cols <- function(df){
	if (!is.data.frame(df)){
		stop(paste(df, "needs to be a dataframe"))
	}
	names(df)[grep('ImageQuality_', names(df))]
}