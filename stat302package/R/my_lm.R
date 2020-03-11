#' Linear Model Function
#'
#' This function fits a linear model to R.
#' @param formula A formula class object, similar to lm().
#' @param data An input data frame.
#' @keywords Linear Model
#'
#' @return A fitted linear model.
#'
#' @examples
#' data(gapminder)
#' gapminderLm <- lm(lifeExp ~ pop + gdpPercap, data = gapminder)
#' summary(gapminderLm)
#' my_lm(realLm, data = gapminder)
#'
#' @export
my_lm <- function(formula, data_df) {
  # get the independent value
  X <- model.matrix(formula, data_df)
  # to extract model from the data frame
  my_frame <- model.frame(formula, data_df)
  # get the dependent value
  Y <- model.response(my_frame)

  beta <- solve(t(X) %*% X) %*% t(X) %*% Y

  # degree of freedom
  df <- nrow(X) - ncol(X)

  # calculating test statistics
  sigma_sq <- sum((Y - X %*% beta)^2 / df)
  se <- ((sigma_sq) * solve((t(X) %*% X))) %>% diag()
  stats <- sqrt(se)
  test_stat <- ((beta - 0) / stats)

  # get the p value
  area_under <- 2 * pt(-abs(test_stat), df)

  # storing the result and printing out the list
  result <- cbind(beta, stats, test_stat, area_under)
  colnames(result) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  result <- as.table(result)
  return(result)
}
