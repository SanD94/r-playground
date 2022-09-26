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

## env and data referencing
library(tidyverse)

data <- tibble(a = 1:3, b = 6:8, n = c("nope", "hello", "world"))

dummy_func <- function(data, ivs, dv) {
  print(rlang::enquo(ivs)); print(rlang::enquo(dv))
  print("hello world!")
  print(data %>% select({{ ivs }}, {{ dv }}))
  names <- tidyselect::vars_select(names(data), {{ ivs }})
  print(names[1])
  dummy_func_next(data, names[1], {{ dv }})
}

dummy_func_next <- function(data, iv, dv) {
  print(rlang::expr(data %>% select({{ iv }}, {{ dv }})))
  print(data %>% select({{ iv }}, {{ dv }}))
  print("next hello world")
}

data %>% dummy_func(c(a, n), b)
