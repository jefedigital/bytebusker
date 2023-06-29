#' bb_corrplot takes a dataframe and makes a nice correlation plot
#' of the numeric columns using corrr
#'
#' @param df a dataframe
#' @return a corrr rplot
#' @export
bb_corrplot <- function(df){

  corr_tbl <- df[,sapply(df, is.numeric)] %>%
    correlate() %>%
    rearrange() %>%
    shave(upper=TRUE)

  return(rplot(corr_tbl))
}
