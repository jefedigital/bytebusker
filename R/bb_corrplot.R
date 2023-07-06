#' bb_corrplot
#'
#' Takes a dataframe and makes a nice correlation plot of the numeric columns using corrr
#'
#' @param df a dataframe
#' @param threshold a threshold 0-1 to filter absolute results
#' @param title a title
#' @return a corrr rplot
#' @export
bb_corrplot <- function(df, threshold=0, title='Corrplot'){

  title <- str_glue(title, ' - threshold:', threshold)

  remove_cols_rows <- function(tbl) {
    cols <- which(apply(tbl == 0 | is.na(tbl), 2, all))
    rows <- which(apply(tbl[,-1] == 0 | is.na(tbl)[,-1], 1, all))
    return(list(cols=cols,rows=rows))
  }

  corr_tbl <- df %>%
    correlate() %>%
    mutate_if(is.numeric, ~ ifelse(abs(.x) < {{threshold}}, 0, .x)) %>%
    rearrange()

  remove <- remove_cols_rows(corr_tbl)
  reduced_corr_tbl <- corr_tbl[-remove$rows, -remove$cols]

  corr_plot <- reduced_corr_tbl %>%
    shave(upper=TRUE) %>%
    rplot() +
    ggtitle({{title}}) +
    theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

  return(corr_plot)
}
