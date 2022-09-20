library(lobstr)
library(rlang)

# link : https://adv-r.hadley.nz/meta-big-picture.html 

# the first big idea is that code is data.
# captured code is called an expression.
expr(mean(x, na.rm = TRUE))
expr(10 + 100 + 1000)

# only prints x, does not consider the expression from the user
capture_it_old <- function(x) {
  expr(x)
}

# prints the expression passed from the user
# automatically quotes the argument
capture_it <- function(x) {
  enexpr(x)
}

capture_it_old(a + b + c) # prints x
capture_it(a + b + c) # prints a + b + c

# after capturing, modification and inspection are possible.
f <- expr(f(x = 1, y = 2))
# argument add
f$z <- 3; f
# argument remove
f[[2]] <- NULL; f
# first element of the call is the function to be called,
# which means the first argument is in the second position.

# almost every programming language represents code as a tree
# called abstract syntax tree, or AST.
# in R, the tree can be inspected and manipulated.
lobstr::ast(f(a, "b"))
lobstr::ast(f1(f2(a,b), f3(1, f4(2))))
lobstr::ast(1 + 2 * 3)

# code can generate code
# rlang::call2() constructs a function call from its components
# but clanky
rlang::call2("f", 1, 2, 3)
rlang::call2("+", 1, call2("*", 2, 3))

# alternative is to build complex trees with a template
# unquote operator (!!) - BANG BANG!

xx <- expr(x + x)
yy <- expr(y + y)

expr(!!xx / !!yy) # prints the operator (x + x) / (y + y)
# !!x inserts the code tree stored in x into the expression.
# output preserves the operator precedence.

# unqouting is useful when wrapping it up into a function.
# gets user's expression with enexpr()
# use expr() and !! to create a new expression using the template.
cv <- function(var) {
  var <- rlang::enexpr(var)
  rlang::expr(sd(!!var) / mean(!!var))
}

cv(x)
cv(x + y)
cv(`)`)

# after inspection and modification, you also evaluate the expression.
# tool for evaluation is eval()
eval(rlang::expr(x + y), env(x = 1, y = 10))
eval(rlang::expr(x + y), env(x = 2, y = 100))
x <- 10; y <- 100
eval(rlang::expr(x + y))
# override functions to implement a domain specific language
# add a data mask so you can refer to variables in a data frame
# as if they are variables in an environment


# custom eval with functions
# binding names to functions allows you to override
# the behavior of existing functions.
string_math <- function(x) {
  e <- env(
    caller_env(),
    `+` = function(x, y) paste0(x, y),
    `*` = function(x, y) strrep(x, y)
  )
  eval(rlang::enexpr(x), e)
}

name <- "Hadley"
string_math("Hello " + name)
string_math(("x" * 2 + "-y") * 3)


library(dplyr)
library(RSQLite)
con <- DBI::dbConnect(RSQLite::SQLite(), filename = ":memory:")
mtcars_db <- copy_to(con, mtcars)

mtcars_db %>%
  filter(cyl > 2) %>%
  select(mpg:hp) %>%
  head(10) %>%
  show_query()


DBI::dbDisconnect(con)


# custom eval with data
# rebinding functions is powerful but exhausting.
# modify eval to look for variables in a data frame instead of an environment.
# subset() and transform() functions are important in this case.
# ggplot2::aes and dplyr::mutate() are other important functions as well.
# instead of using base::eval() stick to rlang::eval_tidy()

# as well as expression and env, eval_tidy() also takes a data mask,
# which is typically a data frame.
df <- data.frame(x = 1:5, y = sample(5))
rlang::eval_tidy(expr(x + y), df)

# useful technique for interactive analysis but that conveience comes
# at a cost: ambiguity.
# TODO: learn to use .data and .env pronouns.

# this function has a bug! this small bug is the conflict between
# the environment variable and passing argument. Same variable name in
# the passing argument is ignored if there is a binding internal to
# the function with the same name.
with2_old <- function(df, expr) {
  a <- 1000
  rlang::eval_tidy(rlang::enexpr(expr), df)
}

# instead of using enexpr(), use enquo()
# whenever you use a data mask, you must always use enquo()!
with2 <- function(df, expr) {
  a <- 1000
  rlang::eval_tidy(rlang::enquo(expr), df)
}

a <- 10
with2_old(df, x + a)
with2(df, x + a)

