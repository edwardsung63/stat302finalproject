#' Sample t-test Function
#'
#' This function does the same as the built-in function t.test().
#' @param x A numeric vector of data.
#' @param alternative A string that specify the alternative hypothesis.
#' @param mu A number that indicates the null hypothesis value of the mean.
#' @keywords t-test
#'
#' @return A list with the following elements: test statistics, degree of freedom
#'        parameter of alternative, numeric p-value.
#'
#' @examples
#' my_t_test(c(2, 5, 3), "less", 2)
#'
#' @export
my_t_test <- function(x, alternative, mu) {
  # to throw an informative error
  if (!(alternative %in% c("two.sided", "less", "greater"))) {
    return("alternative should be two.sided, less or greater only.")
  } else {
    # calculate standard error
    se <- (sd(x) / sqrt(length(x)))
    # degree of freedom
    df <- length(x) - 1
    test_stat <- ((mean(x) - mu) / se)

    # choosing the under-curved area
    if (alternative == "less") {
      p_val <- pt(test_stat, df)
    } else if (alternative == "greater") {
      p_val <- pt(test_stat, df, lower.tail = FALSE)
    } else {
      p_val <- 2 * pt(-abs(test_stat), df)
    }
    # distinguishing whether the p value is valid
    if (p_val > 1 | p_val < 0) {
      return("p-value must be less than 1 or greater than 0 ")
    }
    # storing the list info to a variable
    output <- list(
      "test_statistic" = test_stat, "df" = df,
      "alternative" = alternative, "p-value" = p_val
    )
    return(output)
  }
}
