library("microbenchmark")

df = data.frame(v = 1:4, name = letters[1:4])
microbenchmark(df[3,2], df[3, "name"], df$name[3])


x = 1:50000
cs_for <- function(x) {
  for (i in x) {
    if (i == 1)
      xc = x[i]
    else
      xc = c(xc, sum(x[1:i]))
  }
  xc
}

cs_apply <- function(x) {
  sapply(x, function(x) sum(1:x))
}


microbenchmark(cs_for(x), cs_apply(x), cumsum(x), times = 1)
