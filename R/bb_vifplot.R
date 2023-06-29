#' bb_vifplot
#'
#' Takes an lm object and makes a nice Variance Inflation Factor (VIF) plot using car::vif()
#'
#' @param lm a linear regression lm object
#' @param title a title
#' @return a VIF plot
#' @export
bb_vifplot <- function(lm,title=NULL){

  vif <- vif(lm)

  df_vif <- data.frame(vars=names(vif), vif=vif, row.names=NULL)

  vif_plot <- df_vif %>%
    ggplot(aes(x=vif, y=vars, label=round(vif,1))) +
    geom_col() +
    geom_text(hjust=-.4) +
    geom_vline(aes(xintercept=5), colour='red') +
    ggtitle({{title}})

  return(vif_plot)
}
