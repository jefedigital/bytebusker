#' bb_corrplot
#'
#' Takes a dataframe and makes a nice correlation plot of the numeric columns using corrr
#'
#' @param df a dataframe
#' @param threshold a threshold 0-1 to filter absolute results
#' @param title a title
#' @return a corrr rplot
#' @export
bb_corrplot <- function(df, threshold=0, title=NULL){

  title <- str_glue(title, ' - threshold:', threshold)

  corr_tbl <- df[,sapply(df, is.numeric)] %>%
    correlate() %>%
    mutate_if(is.numeric, ~ ifelse(abs(.x) < threshold, 0, .x)) %>%
    rearrange() %>%
    shave(upper=TRUE)

  corr_plot <- rplot(corr_tbl) +
    ggtitle({{title}}) +
    theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

  return(corr_plot)
}
