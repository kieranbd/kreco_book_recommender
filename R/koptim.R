#' Find Optimum Matrix Decomposition Rank Across Specified Range
#'
#' @param ratings_matrix The sparse ratings matrix, containing NAs where no
#'  rating exists, which is to be decomposed.
  #' @param k_min Lower end of range of values over which to optimise the rank.
  #' Type is positive integer. Default value is 1.
#' @param k_max Upper end of range of values over which to optimise the rank.
#' Type is positive integer. Default value is 5.
#'
#' @return The rank (ie value of k), for which the given sparse ratings matrix has the lowest MSE
#' over the given range.
#' @export
#'
#' @examples
#' # Find the optimum rank (k value) for a range of k between 1 and 5
#'
#' k <- koptim(wide_format, 1, 5)
#'
koptim <- function (ratings_matrix, k_min = 1, k_max = 5) {

  if(k_min < k_max & !(k_min %% 1) & !(k_max %% 1) & k_min > 0 & k_max > 0) {

    x_range <- seq(k_min, k_max, 1)
    mses <- sapply(X = x_range, FUN = function(x) {

      c(mse = tail(nnmf(A = ratings_matrix,
                        loss = "mse",
                        k = x,
                        check.k = FALSE)$mse, n = 1), k = x)

    })
    temp <- as.data.frame(t(mses))
    min_k <- temp$k[which(temp$mse == min(temp$mse))]

  } else {

    print("Please check your k_min and k_max values.")
  }
}
