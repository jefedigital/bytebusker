#' bb_histgrid
#'
#' Takes a dataframe and returns a grid of histograms of all the numeric columns.
#'
#' @param df a dataframe
#' @param title a title
#' @return a ggplot grid of histograms
#' @export
bb_histgrid<- function(df, title=NULL){

  histgrid <- df[,sapply(df, is.numeric)] %>%
    pivot_longer(cols = everything(),
                 names_to='variables',
                 values_to='values') %>%
    ggplot(aes(x=values)) +
    geom_histogram() +
    facet_wrap(~ variables, scales='free') +
    ggtitle({{title}})

  return(histgrid)
}
