#' bb_scattergrid_y
#'
#' Takes a dataframe with a response variable in the first column, returns a grid of scatterplots of each predictor's relationship to the response, plus lm line of best fit.
#'
#' @param df a dataframe
#' @param title a title
#' @return a ggplot grid of scatterplots, with a line of best fit (lm)
#' @export
bb_scattergrid_y <- function(df, title=NULL){

  # only numerics
  df <- df %>% dplyr::select(where(is.numeric))

  # first column is the response
  include_cols <- 2:ncol(df)
  bb_response_label <- names(df[1])

  scattergrid <- df %>%
    pivot_longer(cols = all_of(include_cols),
                 names_to='variables',
                 values_to='values') %>%
    ggplot(aes(x=values, y= .data[[bb_response_label]] )) +
    geom_point(alpha=0.25) +
    geom_smooth(method=lm) +
    facet_wrap(~ variables, scales='free') +
    ggtitle({{title}})

  return(scattergrid)
}
