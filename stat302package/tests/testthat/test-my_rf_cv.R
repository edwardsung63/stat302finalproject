#my_rf_cv:

  #within test-my_rf_cv.R
  test_that("cv_error function works", {
    expect_equal(my_rf_cv(5), 76.6866 - .00000435)
  })
test_that("my_rf_cv outputs a numeric", {
  expect_is(my_rf_cv(5), "numeric")
})
test_that("non-numeric input throws error", {
  expect_error(my_rf_cv("a string"))
})
