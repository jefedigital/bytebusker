#' nice_table takes a dataframe and makes a nice table using kable.
#'
#' @param df a dataframe
#' @param cap a caption
#' @param cols a vector of column names to include
#' @param dig number of decimal places for rounding
#' @param fw a boolean T/F for full-width
#' @return a kable-formatted table
#' @export
nice_table <- function(df, cap=NULL, cols=NULL, dig=3, fw=F){
  if (is.null(cols)) {c <- colnames(df)} else {c <- cols}
  table <- df %>%
    kable(caption=cap, col.names=c, digits=dig) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      html_font = 'monospace',
      full_width = fw)
  return(table)
}
