#' Generate Ratings Matrix for Non-Zero Book Ratings in Ratings Data Frame
#'
#' @param ratings A table of numeric ratings of type data.frame, with columns: User.ID, ISBN, Book.Rating.
#' @param zeroes Boolean - default is TRUE - designating whether missing ratings should be zeroes (TRUE)
#' or NAs (FALSE). For matrix multiplication, missing values must be NAs.
#'
#' @return An m x n matrix with users as rows and books as columns.
#' @export
#'
#' @examples
#' # Create a matrix to be used for user-based collaborative filtering
#'
#' book_ratings <- data.frame(User.ID = c(277042, 276925),
#' ISBN = c(0316666343, 0385504209), Book.Rating = c(5, 7))
#'
#' wide_matrix <- matrixify (ratings = book_ratings, zeroes = TRUE)
#'
matrixify <- function (ratings,
                       zeroes = TRUE) {

  # LOAD PACKAGES
  library(tidyverse)
  library(NNLM)

  # PREPARE DATA
  # create the book_ratings table
  # drop the rows with zero as the rating
  ratings <- ratings[ratings$Book.Rating > 0,]

  # create the output table
  # spread the data across multiple columns
  output <- ratings %>%
    select(User.ID, ISBN, Book.Rating) %>%
    spread(key = ISBN, value = Book.Rating)

  # convert to matrix, make User.IDs the rownames
  readers <- as.character(unlist(output[,1]))

  # set row names of matrix equal to the titles of each reader
  row.names(output) <- readers

  # keep as matrix
  output <- as.matrix(output[,-1])

  if(zeroes) {

    # replace all NAs with 0
    output <- output %>% replace(., is.na(.), 0)

  } # else it comes out with NAs

  output
}
