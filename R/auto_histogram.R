#' Auto Histogram
#'
#' This function takes a dataframe and returns a matrix of ggplot
#' geom_histograms of all the numeric columns.
#'
#' @param df a dataframe
#' @return a ggplot matrix of histograms
#' @export
auto_histogram <- function(df){
  h <- df[,sapply(df, is.numeric)] %>%
    pivot_longer(cols = everything(),
                 names_to='variables',
                 values_to='values') %>%
    ggplot(aes(x=values)) +
    geom_histogram() +
    facet_wrap(~ variables, scales='free')
  return(h)
}
