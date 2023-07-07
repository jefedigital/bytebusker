#' bb_regression_metrics
#'
#' Takes vectors of predictions and observations from regression models, returns common error metrics in a table using yardstick.
#'
#' @param pred a vector of predicted values
#' @param obs a vector of observed values
#' @param cap an optional caption
#' @param colname an optional column label
#' @param dig optional number of decimal places for rounding - default 3
#' @param fw optional boolean T/F for full-width - default F
#' @return a kable-formatted table
#' @export
bb_regression_metrics <- function(pred, obs, dig=3, cap=NA, colname='metrics'){

  obs <- as.vector(obs)
  pred <- as.vector(pred)

  rmse <- rmse_vec(obs, pred, na_rm = TRUE)
  mae <- mae_vec(obs, pred, na_rm = TRUE)
  rsq <- rsq_vec(obs, pred, na_rm = TRUE)
  mape <- mape_vec(obs, pred, na_rm = TRUE)

  df <- as.data.frame(c(rsq=rsq, rmse=rmse, mae=mae, mape=mape))
  names(df) <- colname

  return(bb_table(df, dig={{dig}}, cap={{cap}}))
}
