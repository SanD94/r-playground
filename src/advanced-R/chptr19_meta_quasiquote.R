# quotation is the act of capturing an unevaluated expression
# unquotation is the ability to selectively evaluate parts of
# an otherwise quoted expression. Together, this is called
# quasiquotation.


library(rlang)
library(lobstr)
library(purrr)
library(dplyr)


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
# !! is a one-to-one replacement. !!! (unquote-splice) is one-to-many replacement.
xs <- rlang::exprs(1, a, -b)
rlang::expr(f(!!!xs, y))

ys <- rlang::set_names(xs, c("a", "b", "c"))
rlang::expr(f(!!!ys, d = 4))
rlang::call2("f", !!!xs, rlang::expr(y))

df <- data.frame(x = 1:5)
y <- 100
with(df, x + !!y)

# non-standard AST
x1 <- rlang::expr(class(!!data.frame(x = 10)))
x1; eval(x1)
rlang::expr_print(x1); lobstr::ast(!!x1)

x2 <- rlang::expr(f(!!c(1L, 2L, 3L, 4L, 5L))); x2
rlang::expr_print(x2); lobstr::ast(!!x2)

x3 <- rlang::expr(1 + !!rlang::expr(2 + 3)); x3
lobstr::ast(!!x3)

xy <- rlang::expr(x + y)
xz <- rlang::expr(x + z)
yz <- rlang::expr(y + z)
abc <- rlang::exprs(a, b, c)

rlang::expr(!!xy / !!yz)
rlang::expr(- (!!xz) ^ (!!yz))
rlang::expr(((!!xy)) + !!yz - !!xy)

rlang::expr(atan2(!!xy, !!yz))
rlang::call2("atan2", xy, yz)

rlang::expr(sum(!!xy, !!xy, !!yz))
rlang::call2("sum", xy, xy, yz)

rlang::expr(sum(!!!abc))
rlang::call2("sum", !!!abc)

rlang::expr(mean(c(!!!abc), na.rm = TRUE))
mean_param <- rlang::set_names(TRUE, "na.rm")
rlang::call2("mean", rlang::expr(c(!!!abc)), !!!mean_param)

foo_params <- rlang::set_names(c(xy, yz), c("a", "b"))
rlang::expr(foo(!!!foo_params))
rlang::call2("foo", !!!foo_params)

# tidy dots (...)

set_attr <- function(.x, ...) {
  attr <- rlang::list2(...)
  attributes(.x) <- attr
  .x
}

attrs <- list(x = 1, y = 2)
attr_name <- "z"

1:10 %>%
  set_attr(w = 0, !!!attrs, !!attr_name := 3) %>%
  str()

# direct exec
rlang::exec("mean", x = 1:10, na.rm = TRUE, trim = 0.1)

# undirect or mixed
args <- list(x = 1:10, na.rm = TRUE, trim = 0.1)
rlang::exec("mean", !!!args)
params <- list(na.rm = TRUE, trim = 0.1)
rlang::exec("mean", x = 1:10, !!!params)

# argument indirect
arg_name <- "na.rm"; arg_val <- TRUE
rlang::exec("mean", 1:10, !!arg_name := arg_val)

x <- c(runif(10), NA)
funs <- c("mean", "median", "sd")
purrr::map_dbl(funs, exec, x, na.rm = TRUE)

# exec() is closely related to call2(); where call2() returns an expression,
# exec() evaluates it.

str(rlang::dots_list(x = 1, x = 2))
str(rlang::dots_list(x = 1, x = 2, .homonyms = "first"))
str(rlang::dots_list(x = 1, x = 2, .homonyms = "last"))
str(rlang::dots_list(x = 1, x = 2, .homonyms = "error"))

# case studies

# case 1 -- ast
z <- rlang::expr(foo(x, y))
lobstr::ast(z)
lobstr::ast(!!z)
lobstr::ast(foo(x, y))

# case 2 -- linear model generation
intercept <- 10
coefs <- c(x1 = 5, x2 = -4)
coef_sym <- rlang::syms(names(coefs))
summands <- purrr::map2(coef_sym, coefs, ~ rlang::expr((!!.x * !!.y)))
summands <- c(intercept, summands)
eq <- purrr::reduce(summands, ~ rlang::expr(!!.x + !!.y))

var <- rlang::expr(y)
coef_sym <- purrr::map(seq_along(coefs), ~ rlang::expr((!!var)[[!!.x]]))
coef_sym


linear <- function(var, val) {
  var <- rlang::ensym(var)
  coef_name <- purrr::map(seq_along(val[-1]), ~ rlang::expr((!!var)[!!.x]))

  summands <- purrr::map2(val[-1], coef_name, ~ rlang::expr((!!.x * !!.y)))
  summands <- c(val[[1]], summands)

  purrr::reduce(summands, ~ rlang::expr(!!.x + !!.y))
}

linear(x, c(10, 5, -4))

# case 3 -- array slice
indices <- rep(list(missing_arg()), 3)
rlang::expr(x[!!!indices])
indices[[2]] <- 1
rlang::expr(x[!!!indices])


slice <- function(x, along, index) {
  stopifnot(length(along) == 1)
  stopifnot(length(index) == 1)

  nd <- length(dim(x))
  indices <- rep(list(missing_arg()), nd) 
  indices[[along]] <- index

  rlang::expr(x[!!!indices])
}

x <- array(sample(30), c(5, 2, 3))
slice(x, 1, 3)
slice(x, 2, 2)
slice(x, 3, 1)

# case 4 -- function creation
rlang::new_function(
  rlang::exprs(x =, y =),
  rlang::expr({x + y})
)

power <- function(exponent) {
  rlang::new_function(
    rlang::exprs(x = ),
    rlang::expr({ x ^ !!exponent }),
    caller_env()
  )
}

power(0.5)
curve(sin(exp(4 * x)), n = 1000)

curve2 <- function(expr, xlim = c(0, 1), n = 100) {
  expr <- rlang::enexpr(expr)
  f <- rlang::new_function(rlang::exprs(x = ), expr)
  
  x <- seq(xlim[1], xlim[2], length = n)
  y <- f(x)

  plot(x, y, type = "l", ylab = rlang::expr_text(expr))
}

curve2(sin(exp(4 * x)), n = 1000)
