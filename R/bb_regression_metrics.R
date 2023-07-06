#' bb_regression_metrics
#'
#' Takes vectors of predictions and observations from regression models, returns common error metrics in a table using yardstick.
#'
#' @param pred a vector of predicted values
#' @param obs a vector of observed values
#' @param title an optional title
#' @param dig optional number of decimal places for rounding - default 3
#' @param fw optional boolean T/F for full-width - default F
#' @return a kable-formatted table
#' @export
bb_regression_metrics <- function(pred,obs,dig=3,title=NA){

  obs <- as.vector(obs)
  pred <- as.vector(pred)

  rmse <- rmse_vec(obs, pred, na_rm = TRUE)
  mae <- mae_vec(obs, pred, na_rm = TRUE)
  rsq <- rsq_vec(obs, pred, na_rm = TRUE)
  mape <- mape_vec(obs, pred, na_rm = TRUE)

  df <- as.data.frame(c(rmse=rmse, mae=mae, rsq=rsq, mape=mape))
  names(df) <- 'metrics'

  return(bb_table(df,dig={{dig}},title={{title}}))
}
