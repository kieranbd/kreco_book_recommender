#' Find Optimum L2 Regularisation Parameter Across Specified Range
#'
#' @param ratings_matrix The sparse ratings matrix, containing NAs where no
#'  rating exists, which is to be decomposed.
#' @param range_min Lower end of range of values over which to optimise. Default is 0. Must be positive numeric.
#' @param range_max Upper end of range of values over which to optimise. Default is 1. Must be positive numeric.
#' @param rank Matrix decomposition rank, k, which should be found prior to running aboptim by using
#' koptim.
#'
#' @return A single parameter for L2 regularisation which the user will then set alpha and beta to
#' when running nnfm.
#' @export
#'
#' @examples
#'
#' Find the optimum L2 regularisation parameter from a range between 0.2 and 1.2
#'
#' k <- koptim(ratings_matrix = wide_format, 0.2, 1.2)
aboptim <- function (ratings_matrix, range_min = 0, range_max = 1, rank = 1) {

  range <- seq(range_min, range_max, 0.1)
  mses <- sapply(X = range, FUN = function(x) {

    c(mse = tail(nnmf(A = wide_format,
                      k = rank,
                      alpha = x,
                      beta = x,
                      loss = "mse",
                      check.k = FALSE)$mse, n = 1), param = x)

  })
  temp <- as.data.frame(t(mses))
  param_min <- temp$param[which(temp$mse == min(temp$mse))]
}
