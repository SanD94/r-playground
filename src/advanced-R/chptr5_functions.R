library(magrittr)

double <- function(x) {
  message("Calculating...")
  x * 2
}

y <- 10
h02 <- function(x) {
  y <- 100
  x + 1
}
h02(y)
y

h03 <- function(x) {
  c(x, x)
}

20 %>% double %>% h03


`%piper%` <- function(x, y) {
  y(x)
}

5 %piper% sqrt
