#' Generate Recommendations Using Matrix Factorisation
#'
#' @param new_user Boolean (default value is FALSE), designating whether recommendations
#'  for a new or existing user will be made. If set to TRUE, a data frame containing information
#'  about the new user should be passed to the new_user_df argument.
#' @param L2 Boolean - default is FALSE - which sets whether or not to use L2 regularisation (LASSO)
#' on the rows and columns of the matrix decomposition.
#' @param reader_id The numeric ID of the user you wish to generate recommendations for.
#' @param books A table of books of type data.frame, with columns: ISBN, Book.Title, Book.Author.
#' @param ratings_matrix An m x n matrix containing users on the rows and books on the columns.
#' @param new_user_df A data frame containing the information of a new user's reading history.
#' See examples for usage.
#'
#' @return A data frame containing the top three books (title and author) recommended
#'  for the user specified.
#' @export
#'
#' @examples
#' # Generate recommendations for an existing user:
#'
#' mfrec (reader_id = "277042", books = book_info, ratings_matrix = matrixify_ratings)
#'
#' # Generate recommendations for a new user:
#'
#' new_reader <- data.frame(ISBN = c('0440234743', '0971880107', '0345417623'),
#' Book.Rating = c(2, 5, 3))
#'
#' mfrec (new_user = TRUE, books = book_info, ratings_matrix = matrixify_ratings,
#' new_user_df = new_reader)
#'
mfrec <- function (new_user = FALSE, # F if you want recs for existing user, T if supplying new user
                   L2 = FALSE, # flag for enabling L2 regularisation
                   reader_id, # the existing user for which you want recs
                   books, # information about the books being recommended
                   ratings_matrix, # ratings matrix for all users/books
                   new_user_df) {

  # LOAD PACKAGES
  library(tidyverse)
  library(NNLM)

  # local matrix variable
  wide_format_na <- ratings_matrix

  # NEW USER
  if(missing(reader_id) & new_user) {

    # find the last user ID
    # assign the last user ID + 1 to the new user
    new_reader_id = as.character(as.numeric(row.names(wide_format_na)[nrow(wide_format_na)]) + 1)

    # set current reader_id to be the new reader added
    reader_id <- new_reader_id

    # add new blank row to book ratings
    wide_format_temp <- rbind(wide_format_na, NA)

    # add row name to new user's row
    row.names(wide_format_temp)[nrow(wide_format_temp)] = new_reader_id

    # work through rows of our new user df
    user <- new_user_df

    # this loop adds the reader's ratings to the ratings matrix in the correct place
    for (i in 1:nrow(user)) {
      ISBN = as.character(user[i, 'ISBN'])
      rating = user[i, 'Book.Rating']
      wide_format_temp[new_reader_id, ISBN] <- rating
    }

    # change book_ratings globally
    wide_format_na <- wide_format_temp
  }

  # convert to matrix, make User.IDs the rownames
  readers <- as.character(row.names(wide_format_na))

  # set row names of matrix equal to the titles of each reader
  row.names(wide_format_na) <- readers

  # turn into character if not already
  reader = ifelse(is.character(reader_id), reader_id, as.character(reader_id))

  # MAKE RECOMMENDATION
  # define random seed
  set.seed(1)

  # create factorisation object
  if (L2) {
    decomp <- nnmf(A = wide_format_na,
                   loss = "mse",
                   k = 1,
                   alpha = c(1,0,0),
                   beta = c(1,0,0),
                   check.k = FALSE,
                   rel.tol = 1e-04,
                   max.iter = 5000)

  } else {
    decomp <- nnmf(A = wide_format_na,
                   loss = "mse",
                   k = 2,
                   check.k = FALSE)
  }

  # get predicted ratings
  predicted_ratings <-  decomp$W %*% decomp$H

  # transpose wide_format_na matrix
  ratings_transposed <- t(wide_format_na)

  # find which books the reader has already read
  reader_seen <- row.names(ratings_transposed)[!is.na(ratings_transposed[, reader])]

  # get the scores
  reader_scores <- data.frame(ISBN = colnames(predicted_ratings),
                              score = as.vector(predicted_ratings[reader,]),
                              seen = ratings_transposed[,reader])

  # join the scores to book info dataframe to get the title of each book
  reader_scores <- left_join(reader_scores, book_info)

  # sort unseen movies by score and remove the seen, ISBN and score columns
  reader_scores %>%
    filter(is.na(seen)) %>%
    arrange(desc(score)) %>%
    select(-ISBN, -seen, -score) %>%
    head(3)
}
