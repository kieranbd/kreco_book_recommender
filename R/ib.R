#' Generate Book Recommendations Using Item-Based Collaborative Filtering
#'
#' @param new_user Boolean (default value is FALSE), designating whether recommendations
#'  for a new or existing user will be made. If set to TRUE, a data frame containing information
#'  about the new user should be passed to the new_user_df argument.
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
#' matrixify_ratings <- matrixify (book_ratings)
#'
#' ib (reader_id = "277042", books = book_info, ratings_matrix = matrixify_ratings)
#'
#' # Generate recommendations for a new user:
#'
#' new_reader <- data.frame(ISBN = c('0440234743', '0971880107', '0345417623'),
#' Book.Rating = c(2, 5, 3))
#'
#' ib (new_user = TRUE, books = book_info, ratings_matrix = matrixify_ratings,
#' new_user_df = new_reader)
#'
ib <- function (new_user = FALSE, # F if you want recs for existing user, T if supplying new user
                                  reader_id, # the existing user for which you want recs
                                  books, # information about the books being recommended
                                  ratings_matrix, # ratings matrix for users/books
                                  new_user_df) { # data frame containing new user's ratings

  # local matrix variable
  wide_format <- ratings_matrix

  # NEW USER
  if(missing(reader_id) & new_user) {

    # find the last user ID
    # assign the last user ID + 1 to the new user
    new_reader_id = as.character(as.numeric(row.names(wide_format)[nrow(wide_format)]) + 1)

    # set current reader_id to be the new reader added
    reader_id <- new_reader_id

    # add new blank row to book ratings
    wide_format_temp <- rbind(wide_format, 0)

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
    wide_format <- wide_format_temp

  }

  # convert to matrix, make User.IDs the rownames
  readers <- as.character(row.names(wide_format))

  # set row names of matrix equal to the titles of each reader
  row.names(wide_format) <- readers

  # FUNCTIONS REQUIRED
  # cosine similarity matrix
  sim_matrix = function(ma, mb){
    mat = tcrossprod(ma, mb)
    t1 = sqrt(apply(ma, 1, crossprod))
    t2 = sqrt(apply(mb, 1, crossprod))
    mat / outer(t1,t2)
  }

  # MAKE RECOMMENDATION
  # book similarity matrix (150 x 150)
  # get all similarities between books
  book_similarities <- sim_matrix(t(wide_format), t(wide_format))

  # turn into character if not already
  reader = ifelse(is.character(reader_id), reader_id, as.character(reader_id))

  # transpose the ratings matrix
  trans_ratings = t(wide_format)

  # the books read by the reader
  reader_seen = row.names(trans_ratings)[trans_ratings[, reader] > 0]

  # get the scores
  reader_scores <- data.frame(ISBN = row.names(trans_ratings),
                              score = apply(book_similarities[,reader_seen], 1, sum),
                              seen = trans_ratings[,reader])

  # join the scores to book info dataframe to get the title of each book
  reader_scores <- left_join(reader_scores, books)

  # sort unseen movies by score and remove the seen, ISBN and score columns
  reader_scores %>%
    filter(seen == 0) %>%
    arrange(desc(score)) %>%
    select(-ISBN, -score, -seen) %>%
    head(3)
}
