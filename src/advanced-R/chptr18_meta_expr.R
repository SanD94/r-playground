library(rlang)
library(lobstr)

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
