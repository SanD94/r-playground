library(lobstr)
library(pryr)

dummy_func <- function(obj, ls = list(hello = 10, world = 20)) {
  print(obj_addr(ls))
  obj$ls <- ls
  obj
}


nope <- list()
hi <- nope

print(tracemem(nope))
print(obj_addr(hi))
print(obj_addr(nope))

nope <- dummy_func(nope)
hi <- dummy_func(hi)

print(ref(nope))
print(ref(hi))

untracemem(nope)

t <- "crysis"
l <- "crysis"
ref(t)
ref(l)
