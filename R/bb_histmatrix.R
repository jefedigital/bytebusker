#' bb_histmatrix
#'
#' This function takes a dataframe and returns a matrix of ggplot
#' geom_histograms of all the numeric columns.
#'
#' @param df a dataframe
#' @param title a title
#' @return a ggplot matrix of histograms
#' @export
bb_histmatrix <- function(df, title=NULL){

  hist_matrix <- df[,sapply(df, is.numeric)] %>%
    pivot_longer(cols = everything(),
                 names_to='variables',
                 values_to='values') %>%
    ggplot(aes(x=values)) +
    geom_histogram() +
    facet_wrap(~ variables, scales='free') +
    ggtitle({{title}})

  return(hist_matrix)
}
