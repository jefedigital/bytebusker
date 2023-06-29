#' bb_screeplot
#' Principal Component Analysis - PCA
#'
#' This function takes a prcomp object and returns a simple scree plot
#' using ggplot. Specify 'bar' or 'line' as second parameter.
#'
#'
#' @param prcomp a stats::prcomp object
#' @param plot_type 'bar' or 'line', bar is default
#' @return a ggplot barplot or lineplot
#' @export
bb_screeplot <- function(prcomp, plot_type='bar'){

  variance <- prcomp$sdev^2 / sum(prcomp$sdev^2)

  df <- as.data.frame(variance) %>%
    mutate(principal_component=row_number())

  barplot <- df %>%
    ggplot(aes(x=principal_component, y=variance, label=round(variance,2))) +
    geom_col() +
    geom_text(vjust=-0.8) +
    ggtitle('PCA Scree Plot') +
    ylim(0,1) +
    scale_x_continuous(breaks=df$principal_component)

  lineplot <- df %>%
    ggplot(aes(x=principal_component, y=variance, label=round(variance,2))) +
    geom_point() +
    geom_line() +
    geom_text(vjust=-0.8) +
    ggtitle('PCA Scree Plot') +
    ylim(0,1) +
    scale_x_continuous(breaks=df$principal_component)

  if (plot_type == 'bar'){
    return(barplot)
    } else {
      return(lineplot)
      }

  }
