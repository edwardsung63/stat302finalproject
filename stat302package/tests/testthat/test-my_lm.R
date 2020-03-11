my_lm:

  #within test-my_lm.R
  skinner <- my_lm(realLm, data = mtcars)
  test_that("estimates are correct", {
    expect_equal(as.vector(skinner[, 1]), c(37.22727012, -0.03177295, -3.87783074))
  })
  test_that("my_lm outputs a table", {
    expect_is(my_lm(realLm, data = mtcars), "table")
  })
  test_that("non-table input throws error", {
    expect_error(my_lm("a string"))
  })
