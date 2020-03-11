#my_knn_cv:

  #within test-my_knn_cv.R
  t1 <- my_knn_cv(train = iris[, -5], cl = iris$Species, 1, 5)
  test_that("cv_error function works", {
    expect_equal(t1$cv_error, .96)
  })
  test_that("my_knn_cv outputs a list", {
    expect_is(my_knn_cv(train = iris[, -5], cl = iris$Species, 1, 5), "list")
  })
  test_that("non-list input throws error", {
    expect_error(my_knn_cv("a string"))
  })
