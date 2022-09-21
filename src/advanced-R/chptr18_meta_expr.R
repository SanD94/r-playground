library(rlang)
library(lobstr)
library(purrr)

# operation and its result are different two types.
y <- x * 10

# capturing the intent of the code without executing it.
# seperation of the description of the action from the action
z <- rlang::expr(y <- x * 10)
x <- 4; eval(z); y

lobstr::ast(f(x, "y", 1))
lobstr::ast(f(g(1, 2), h(3, 4, i())))
lobstr::ast(y <- x)
lobstr::ast(y < -x)
lobstr::ast(y <- x * 10)
lobstr::ast(`<-`(y, `*`(x, 10)))
expr(`<-`(y, `*`(x, 10)))

lobstr::ast(function(x = 1, y = 2) {})


# constants are self-quoting in the sense that the expression
# used to represent a constant is the same constant.
identical(rlang::expr(TRUE), TRUE)
identical(rlang::expr(1), 1)
identical(rlang::expr(2L), 2L)
identical(rlang::expr("x"), "x")


# a symbol is created in two ways: by capturing code that references
# an object with expr(), or turning a string into a symbol with
# rlang::sym()
rlang::expr(x); rlang::sym("x")
rlang::as_string(expr(x))
str(expr(x))
is.symbol(expr(x))

# a call object represents a captured function call.
# they a special type of list where the first component specifies
# the function to call, and the remaining elements are
# the arguments for that call.

lobstr::ast(read.table("important.csv", row.names = FALSE))
x <- rlang::expr(read.table("important.csv", row.names = FALSE))
typeof(x); is.call(x)

x[[1]]; is.symbol(x[[1]]); as.list(x[-1])
x[[2]]; x$row.names; length(x) - 1

rlang::call_standardise(x)
x$header <- TRUE; x

lobstr::ast(foo()); lobstr::ast("foo"())
lobstr::ast(pkg::foo(1))
lobstr::ast(obj$foo(1))
lobstr::ast(foo(1)(2))

rlang::call2("mean", x = expr(x), na.rm = TRUE)
rlang::call2(expr(base::mean), x = expr(x), na.rm = TRUE)
rlang::call2("<-", expr(x), 10)
rlang::call2("<-", x, 10)

# parsing and deparsing
x1 <- "y <- x + 10"
is.call(x1)

x2 <- rlang::parse_expr(x1); x2
is.call(x2)

x3 <- "a <- 1; a + 1"
rlang::parse_exprs(x3)

z <- rlang::expr(y <- x + 10)
rlang::expr_text(z)

# parsing and deparsing are not symmetric
cat(rlang::expr_text(
  rlang::expr({
    # This is a comment
    x <- `x` + 1
  })
))

# walking ast with recursive functions
expr_type <- function(x) {
  res <- typeof(x)
  if (rlang::is_syntactic_literal(x))
    res <- "constant"
  else if (is.symbol(x))
    res <- "symbol"
  else if (is.call(x))
    res <- "call"
  else if (is.pairlist(x))
    res <- "pairlist"
  res
}

switch_expr <- function(x, ...) {
  switch(
    expr_type(x),
    ...,
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}

recurse_call <- function(x) {
  switch_expr(
    x,
    # Base cases
    symbol = ,
    constant = ,

    # Recursive cases
    call = ,
    pairlist =
  )
}

expr_type(rlang::expr("a"))
expr_type(rlang::expr(x))
expr_type(rlang::expr(f(1,2)))


expr_type(rlang::expr(TRUE))
expr_type(rlang::expr(T))


logical_abbr_rec <- function(x) {
  switch_expr(
    x,
    # Base cases
    constant = FALSE,
    symbol = as_string(x) %in% c("F", "T"),

    # Recursive cases
    call = ,
    pairlist = purrr::some(x, logical_abbr_rec)
  )
}


logical_abbr <- function(x) {
  logical_abbr_rec(rlang::enexpr(x))
}

logical_abbr(mean(x, na.rm = T))
logical_abbr(function(x, na.rm = T) FALSE)

flat_map_chr <- function(.x, .f, ...) {
  purrr::flatten_chr(purrr::map(.x, .f, ...))
}

find_assign_call <- function(x) {
  if (rlang::is_call(x, "<-") && rlang::is_symbol(x[[2]])) {
    lhs <- rlang::as_string(x[[2]])
    children <- as.list(x)[-1]
  } else {
    lhs <- character()
    children <- as.list(x)
  }
  c(lhs, flat_map_chr(children, find_assign_rec))
}

find_assign_rec <- function(x) {
  switch_expr(
    x,
    # Base cases
    constant =,
    symbol = character(),

    # Recursive cases
    pairlist = flat_map_chr(x, find_assign_rec),
    call = find_assign_call(x)
  )
}

find_assign <- function(x) unique(find_assign_rec(enexpr(x)))
find_assign("x")
find_assign(x)


flat_map_chr(letters[1:3], ~ rep(., sample(3, 1)))

find_assign(a <- 1)
find_assign({
  a <- 1
  { b <- 2}
})

find_assign({
  a <- 1
  a <- 2
})

find_assign(a <- b <- c <- 1)
find_assign(system.time(x <- print(y <- 5)))

# specialised data structures
# pairlists

f <- expr(function(x, y = 10) x + y)
args <- f[[2]]; args; typeof(args)
pl <- pairlist(x = 1, y = 2); length(pl); pl$x

# missing arguments
rlang::is_missing(missing_arg())
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]; rlang::is_missing(args[[1]])

f <- expr(function(...) list(...))
args <- f[[2]]; rlang::is_missing(args[[1]])

# if you bind it to a variable, accessing gives you an error.
m <- missing_arg(); m
ms <- list(missing_arg(), missing_arg()); ms[[1]]


# expression vectors
exp1 <- parse(text = c("x <- 4; x"))
exp2 <- expression(x <- 4, x)

typeof(exp1); exp1; length(exp1); exp1[[1]]
typeof(exp2); exp2