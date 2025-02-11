# confidence ratio of two means (Fieller 1954)
# Fieller, EC. (1954). "Some problems in interval estimation". Journal of the Royal Statistical Society, Series B. 16 (2): 175–185. JSTOR 2984043.
# variance matrix elements as separate arguments for vectorisation of function

FiellerRatioCI <- function(a, b, varA, varB, covar, alpha = 0.05) {
  
  theta <- a/b
  z <- qnorm(1-alpha/2)
  g <- (z^2)*varB/b^2
  C <- sqrt(varA - 2*theta*covar + theta^2 * varB - g*(varA-covar^2/varB))
  min <- (1/(1-g))*(theta- g*covar/varB - z/b * C)
  max <- (1/(1-g))*(theta- g*covar/varB + z/b * C)
  return(tibble(mean = theta, lower = min, upper = max))

}
