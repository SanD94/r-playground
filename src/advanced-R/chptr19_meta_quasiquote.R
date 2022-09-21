# quotation is the act of capturing an unevaluated expression
# unquotation is the ability to selectively evaluate parts of
# an otherwise quoted expression. Together, this is called
# quasiquotation.


library(rlang)
library(purrr)


paste("Good", "morning", "Hadley")
paste("Good", "afternoon", "Alice")

cement <- function(...) {
  args <- rlang::ensyms(...)
  paste(purrr::map(args, as_string), collapse = " ")
}

cement(Good, morning, Hadley)
cement(Good, afternoon, Alice)

# explicitly unquote the input
name <- "Hadley"
time <- "morning"
cement(Good, !!time, !!name)


# comparison
paste("Good", time, name)
cement(Good, !!time, !!name)

# evaluated argument obeys R's usual evaluation rules
# quoted argument is captured by the function, and is processed
# in some custom way.
# paste() evaluates all its arguments; cement() quotes all its arguments


# quoting
# first part of quasiquotation is quotation.
rlang::expr(x + y)
rlang::expr(1 / 2 / 3)
f1 <- function(x) expr(x); f1(a + b + c)
f2 <- function(x) enexpr(x); f2(a + b + c)
f <- function(...) enexprs(...); f(x = 1, y = 10 * z)
rlang::exprs(x = x ^ 2, y = y ^ 3, z = z ^ 4)

f <- function(...) ensyms(...); f(x); f("x")
# unquoting
x <- expr(-1)
rlang::expr(f(!!x, y))
a <- rlang::sym("y"); b <- 1; expr(f(!!a, !!b))

mean_rm <- function(var) {
  var <- rlang::ensym(var)
  rlang::expr(mean(!!var, na.rm = TRUE))
}
rlang::expr(!!mean_rm(x) + !!mean_rm(y))
x1 <- rlang::expr(x + 1)
x2 <- rlang::expr(x + 2)
rlang::expr(!!x1 / !!x2)

f <- rlang::expr(foo)
rlang::expr((!!f)(x, y))

f <- rlang::expr(pkg::foo)
rlang::expr((!!f)(x, y))
rlang::call2(f, rlang::expr(x), rlang::expr(y))

arg <- rlang::missing_arg()
rlang::expr(foo(!!rlang::maybe_missing(arg), !!rlang::maybe_missing(arg)))

# unquoting many arguments
# !! is a one-to-one replacement. !!! (unquote-s)
