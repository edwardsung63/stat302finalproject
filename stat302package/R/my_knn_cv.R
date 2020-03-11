#' K-nearest Neighbors Cross Validation Function
#'
#' @import class(magrittr)
#'
#' This function predicts output using covaritates.
#' @param train A data frame to input.
#' @param cl It is a true value of my training data.
#' @param k_nn An integer that represents the number of neighbors.
#' @param k_cv An integer that represents the number of folds.
#' @keywords K-nearest Cross Validations
#'
#' @return The function will output a list of objects.
#'
#' @examples
#' my_knn_cv(train = iris[, -5], cl = iris$Species, 1, 5)
#' my_knn_cv(train = iris[, -5], cl = iris$Species, 5, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  n <- length(cl)
  fold <- sample(rep(1:k_cv, length = n))
  cross_val <- rep(NA, k_cv)
  # loop thru the folds
  for (i in 1:k_cv) {
    data_train <- train[fold != i, ] # Xi
    data_test <-  train[fold == i, ]  # Xi star
    # Train our models
    cl_train <- cl[fold != i] # Yi
    cl_test <- cl[fold == i]  # Yi star
    knn_output <- knn(train = data_train, cl = cl_train,
                      test = data_test, k = k_nn) #Yi star hat
    cross_val[i] <- sum(knn_output == cl_test) / length(cl_test)
  }
  #------------------------------
  yhat_star <- knn(train = train, cl = cl,
                   test = train, k = k_nn)

  # -----------------------------
  output <- list("class" = yhat_star, "cv_error" = mean(cross_val))
}
