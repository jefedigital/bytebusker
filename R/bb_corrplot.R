#' bb_corrplot
#'
#' Takes a dataframe and makes a nice correlation plot
#' of the numeric columns using corrr
#'
#' @param df a dataframe
#' @param title a title
#' @return a corrr rplot
#' @export
bb_corrplot <- function(df,title=NULL){

  corr_tbl <- df[,sapply(df, is.numeric)] %>%
    correlate() %>%
    rearrange() %>%
    shave(upper=TRUE)

  corr_plot <- rplot(corr_tbl) +
    ggtitle({{title}}) +
    theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

  return(corr_plot)
}
