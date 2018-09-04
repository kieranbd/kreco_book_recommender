<!-- README.md is generated from README.Rmd. Please edit that file -->
kreco
=====

The `kreco` package contains a suite of functions to enable easy calculation of book recommendations for readers with previous book rating histories.

Example 1: Create Wide-Format Matrix Using matrixify Function
-------------------------------------------------------------

We expect that all book ratings datasets will be in the form of the one below:

Once we are certain that we have the ratings dataset in this form, we are ready to use `matrixify` to prepare the data such that it may be used to generate recommendations. That is, convert it into wide-matrix format from long-format.

``` r
# set zeroes = TRUE as we will use the matrix for user-based collaborative filtering
wide_format <- kreco::matrixify(ratings = book_ratings,
                                zeroes = TRUE)
```

Example 2: Generate Recommendations for Existing User Using ub function
-----------------------------------------------------------------------

We have converted the ratings data into wide-matrix format in the example above. We can now use the matrix to generate recommendations for a particular existing user. We will use `User.ID = 277042` for this example.

``` r
# user exists, so we set new_user = FALSE, and leave new_user_df out completely
recommendations_ub <- ub(new_user = FALSE,
                         reader_id = "277042",
                         books = book_info, 
                         ratings_matrix = wide_format)
```

Finally we can view the data frame that was returned and see the top three books recommended for user `277042`.

``` r
recommendations_ub
```

Example 3: Generate Recommendations for a New User Using ib function
--------------------------------------------------------------------

To generate recommendations for a new user we can use the same `wide_format` matrix created above, but we'll need to hand the `ib` function a few additional pieces of information. First, we set `new_user = TRUE`, and then we'll need to define a data frame for the new reader which contains their historical ratings data. Lastly, we won't pass the function any information at the `reader_id` argument.

``` r
# data frame with new reader info
new_reader <- data.frame(ISBN = c('0440234743', '0971880107', '0345417623'),
                         Book.Rating = c(2, 5, 3))

# call the ib function accordingly
recommendations_ib <- ib(new_user = TRUE,
                         books = book_info, 
                         ratings_matrix = wide_format,
                         new_user_df = new_reader)
```

Finally, view the recommendations for this new user.

``` r
recommendations_ib
```

Example 4: Generate Recommendations for Existing User Using mfrec function
--------------------------------------------------------------------------

Lastly, we will use matrix factorisation to generate recommendations for the same user that we used in example 2. The `mfrec` function which we use for matrix factorisation-based recommendations contains the same parameters as the user- and item-based functions, with one addition: `L2`. We use this boolean argument to select whether or not L2 regularisation should be used for the rows and the columns during the matrix factorisation process.

``` r
# user exists, so we set new_user = FALSE, and leave new_user_df out completely
# we also set L2 = TRUE to use L2 regularisation (ie ridge regression)
recommendations_mfrec <- mfrec(new_user = FALSE,
                               reader_id = "277042",
                               books = book_info, 
                               ratings_matrix = wide_format,
                               L2 = TRUE)
```

Finally, we show the matrix factorisation recommendations.

``` r
recommendations_mfrec
```
