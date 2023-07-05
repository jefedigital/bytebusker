#' bb_boxmatrix
#'
#' Takes a dataframe and returns a matrix of ggplot geom_boxplots of all numeric columns.
#'
#' @param df a dataframe
#' @param title a title
#' @return a ggplot matrix of boxplots
#' @export
bb_boxmatrix <- function(df, title=NULL){

  box_matrix <- df[,sapply(df, is.numeric)] %>%
    pivot_longer(cols = everything(),
                 names_to='variables',
                 values_to='values') %>%
    ggplot(aes(x=values)) +
    geom_boxplot() +
    facet_wrap(~ variables, scales='free') +
    ggtitle({{title}})

  return(box_matrix)
}
