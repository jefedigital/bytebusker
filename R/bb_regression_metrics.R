#' bb_regression_metrics
#'
#' Takes vectors of predictions and observations from regression models, returns common error metrics in a df using yardstick.
#'
#' @param pred a vector of predicted values
#' @param obs a vector of observed values
#' @param colname an optional column label
#' @return a df
#' @export
bb_regression_metrics <- function(pred, resp, colname='metrics'){

  obs <- as.vector(resp)
  pred <- as.vector(pred)

  rmse <- rmse_vec(resp, pred, na_rm = TRUE)
  mae <- mae_vec(resp, pred, na_rm = TRUE)
  rsq <- rsq_vec(resp, pred, na_rm = TRUE)
  mape <- mape_vec(resp, pred, na_rm = TRUE)

  df <- as.data.frame(c(rsq=rsq, rmse=rmse, mae=mae, mape=mape))
  names(df) <- colname

  return(df)
}
