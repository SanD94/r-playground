library(purrr)
library(rlang)
library(lobstr)
library(dplyr)

# basics
x <- 10
eval(rlang::expr(x))
y <- 2
eval(rlang::expr(x + y))
eval(rlang::expr(x + y), env(x = 1000))

eval(print(x + 1), env(x = 1000))
eval(rlang::expr(print(x + 1)), env(x = 1000))

# application -- local()
rm(x, y)
foo <- local({
  x <- 10
  y <- 200
  x + y
})
foo; x; y

local2 <- function(expr) {
  env <- env(caller_env())
  eval(rlang::enexpr(expr), env)
}

foo <- local2({
  x <- 10
  y <- 200
  x + y
})
foo; x; y

# application -- source()
source2 <- function(path, env = caller_env()) {
  file <- paste(readLines(path, warn = FALSE), collapse = "\n")
  exprs <- rlang::parse_exprs(file)

  res <- purrr::map(exprs, eval, env)

  invisible(res)
}

source3 <- function(file, env = parent.frame()) {
  lines <- parse(file)
  res <- eval(lines, envir = env)
  invisible(res)
}


# application -- function()
x <- 10; y <- 20
f <- eval(rlang::expr(function(x, y) !!x + !!y)); f
f <- rlang::new_function(
  rlang::exprs(x = , y = ),
  rlang::expr(!!x + !!y)
)
attr(f, "srcref") <- NULL; f

x <- 1
get2 <- function(name, env = caller_env()) {
  name_sym <- rlang::sym(name)
  eval(name_sym, env)
}
get2("x")

assign2 <- function(name, value, env = caller_env()) {
  name_sym <- rlang::sym(name)
  assign_expr <- rlang::expr(!!name_sym <- !!value)
  eval(assign_expr, env)
}
assign2("x", 4); x


# quosures

## creation
foo <- function(x) rlang::enquo(x)
foo(a + b)

rlang::quo(x + y + z)
rlang::new_quosure(rlang::expr(x + y), env(x = 1, y = 10))

## evaluation
q1 <- rlang::new_quosure(rlang::expr(x + y), env(x = 1, y = 10))
rlang::eval_tidy(q1)
rlang::get_expr(q1)
rlang::get_env(q1)

## dots
f <- function(...) {
  x <- 1
  g(..., f = x)
}
g <- function(...) {
  rlang::enquos(...)
}

x <- 0
qs <- f(global = x); qs
purrr::map_dbl(qs, eval_tidy)

## nesting
q2 <- rlang::new_quosure(rlang::expr(x), env(x = 1))
q3 <- rlang::new_quosure(rlang::expr(x), env(x = 10))
x <- rlang::expr(!!q2 + !!q3)
rlang::eval_tidy(x)
rlang::expr_print(x)

# data masks
q1 <- rlang::new_quosure(rlang::expr(x * y), env(x = 100))
df <- data.frame(y = 1:10)

rlang::eval_tidy(q1, df)

with2 <- function(data, expr) {
  expr <- rlang::enquo(expr)
  rlang::eval_tidy(expr, data)
}

x <- 100
with2(df, x * y)

with3 <- function(data, expr) {
  expr <- substitute(expr)
  eval(expr, data, caller_env())
}


# pronouns
with2(df, x) # which x, env or data?
x <- 1
df <- data.frame(x = 2)
with2(df, .data$x)
with2(df, .env$x)


subset2 <- function(data, rows) {
  rows <- rlang::enquo(rows)
  rows_val <- rlang::eval_tidy(rows, data)
  stopifnot(is.logical(rows_val))
  
  data[rows_val, ,drop = FALSE]
}


transform2 <- function(.data, ...) {
  dots <- rlang::enquos(...)

  for (i in seq_along(dots)) {
    name <- names(dots)[[i]]
    dot <- dots[[i]]
    
    .data[[name]] <- rlang::eval_tidy(dot, .data)
  }
  
  .data
}

select2 <- function(data, ...) {
  dots <- rlang::enquos(...)
  
  vars <- as.list(set_names(seq_along(data), names(data)))
  cols <- unlist(map(dots, rlang::eval_tidy, vars))
  
  data[, cols, drop=FALSE]
}

subset3 <- function(data, rows) {
  rows <- rlang::enquo(rows)
  rlang::eval_tidy(rlang::expr(data[!!rows, , drop = FALSE]), data = data)
}

arrange2 <- function(.df, ..., .na.last = TRUE) {
  args <- rlang::enquos(...)
  order_call <- rlang::expr(order(!!!args, na.last = !!.na.last))
  ord <- rlang::eval_tidy(order_call, .df)
  stopifnot(length(ord) == nrow(.df))
  .df[ord, , drop = FALSE]
}

# tidy eval
resample <- function(df, n) {
  idx <- sample(nrow(df), n, replace = TRUE)
  df[idx, , drop = FALSE]
}

subsample <- function(df, cond, n = nrow(df)) {
  cond <- rlang::enquo(cond)

  df <- subset2(df, !!cond)
  resample(df, n)
}

df <- data.frame(x = c(1, 1, 1, 2, 2), y = 1:5)
subsample(df, x == 1)

threshold_x <- function(df, val) {
  subset2(df, .data$x >= !!val)
}

x <- 10
no_x <- data.frame(y = 1:3)
threshold_x(no_x, 2)
has_val <- data.frame(x = 1:3, val = 9:11)
threshold_x(has_val, 2)

threshold_var <- function(df, var, val) {
  var <- rlang::as_string(rlang::ensym(var))
  subset2(df, .data[[var]] >= !!val)
}

threshold_expr <- function(df, expr, val) {
  expr <- rlang::enquo(expr)
  subset2(df, !!expr >= !!val )
}

df <- data.frame(x = 1:10)
threshold_var(df, x, 8)
