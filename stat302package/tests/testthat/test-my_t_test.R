my_t_test:

  my_vec1 <- c(4, 5, 6, 2, 3)
  t.test1 <- my_t.test(my_vec1, "less", 5)
  test_that("test t test outputs are correct", {
    expect_equal(t.test1$'t test', -1.414214 + 4.38e-07)
    expect_equal(t.test1$'degree of freedom', 4)
  })
  test_that("my_t_test outputs a list", {
    expect_is(my_t_test(c(1, 2, 3, 4, 5), mu = 3, alternative = "greater"), "list")
  })
  test_that("non-list input throws error", {
    expect_error(my_t_test("numeric"))
  })
