library(lobstr)

x <- c(1, 2, 3)
tracemem(x)
y <- x
y[[3]] <- 4L
y[[3]] <- 5L
untracemem(x)

obj_addr(x)
obj_addr(y)


means_list <- c(
  mean,
  base::mean,
  get("mean"),
  evalq(mean),
  match.fun("mean")
)
unique(obj_addrs(means_list))


l1 <- list(1, 2, 3)
l2 <- l1
ref(l1, l2)
l2[[3]] <- 4
ref(l1, l2)

# say hello to global string pool
x <- c("a", "a", "abc", "d")
ref(x, character = TRUE)

# copy-on-modify
a <- runif(1e6)
b <- list(a, a)
obj_size(b)
b[[1]][[1]] <- 10
obj_size(b)
obj_size(list())


# tracemem
x <- data.frame(matrix(runif(5 * 1e4), ncol = 5))
medians <- vapply(x, median, numeric(1))
tracemem(x)
for (i in 1:5) {
  x[[i]] <- x[[i]] - medians[[i]]
}
untracemem(x)

y <- as.list(x)
tracemem(y)
for (i in 1:5) {
  y[[i]] <- y[[i]] - medians[[i]]
}
untracemem(y)

for (i in seq_along(medians)) {
  x[[i]] <- x[[i]] - medians[[i]]
}

