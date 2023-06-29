#' Scree Plot
#' Principal Component Analysis - PCA
#'
#' This function takes a prcomp object and returns a simple scree plot.
#'
#' @param prcomp a stats::prcomp object
#' @return a ggplot
#' @export
scree_plot <- function(prcomp){
  variance <- prcomp$sdev^2 / sum(prcomp$sdev^2)

  df <- as.data.frame(variance) %>%
    mutate(principal_component=row_number())

  plot <- df %>%
    ggplot(aes(x=principal_component, y=variance)) +
    geom_point() +
    geom_line() +
    ggtitle('PCA Scree Plot') +
    ylim(0,1) +
    scale_x_continuous(breaks=df$principal_component)

  return(plot)
}
