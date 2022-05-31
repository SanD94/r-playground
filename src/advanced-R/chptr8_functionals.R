library(purrr)


x <- list(1:5, c(1:10, NA))
map_dbl(x, ~ mean(.x, na.rm = TRUE))
map_dbl(x, mean, na.rm = TRUE)


plus <- function(x, y) x + y

x <- rep(0, 4)
map_dbl(x, plus, runif(1))
map_dbl(x, ~ plus(.x, runif(1)))
