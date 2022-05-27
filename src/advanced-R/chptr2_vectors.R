library(lobstr)

l0 <- list()
obj_size(l0)

l1 <- list(
  1:3,
  "a",
  c(TRUE, FALSE, TRUE),
  c(2.3, 5.0)
)


typeof(l1)
str(l1)

obj_size(l1)

obj_size(mtcars)
l2 <- list(mtcars, mtcars, mtcars, mtcars)
obj_size(l2)

# behavior of list and vector
l4 <- list(list(1, 2), c(3, 4))
l5 <- c(list(1, 2), c(3, 4))
str(l4)
str(l5)

l6 <- list(1:3)
str(l6)
l7 <- as.list(1:3)
str(l7)
