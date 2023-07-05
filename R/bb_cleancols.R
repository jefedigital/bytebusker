#' bb_cleancols
#'
#' Takes a dataframe and cleans up column labels
#'
#' @param df a dataframe
#' @param title a title
#' @return a df with nice column labels
#' @export
bb_cleancols <- function(df){

  names(df) <- str_to_lower(
    str_replace_all(names(df),
                    c(
                      " " = "_",
                      "," = "",
                      "\\*" = "",
                      "\\(" = "",
                      "\\)" = "",
                      "`" = "",
                      "\\/" = "_"
                      )))

  return(df)
}
