#' bb_boxgrid
#'
#' Takes a dataframe and returns a matrix of ggplot geom_boxplots of all numeric columns.
#'
#' @param df a dataframe
#' @param title a title
#' @return a ggplot grid of boxplots
#' @export
bb_boxgrid <- function(df, title=NULL){

  boxgrid <- df[,sapply(df, is.numeric)] %>%
    pivot_longer(cols = everything(),
                 names_to='variables',
                 values_to='values') %>%
    ggplot(aes(x=values)) +
    geom_boxplot() +
    facet_wrap(~ variables, scales='free') +
    ggtitle({{title}})

  return(boxgrid)
}
