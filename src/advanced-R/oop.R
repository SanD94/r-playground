library(sloop)

f <- factor(c("a", "b", "c"))
typeof(f)
attributes(f)
s3_dispatch(print(f))

ftype(print)
ftype(str)
ftype(unclass)

ftype(t.test)
ftype(t.data.frame)

s3_get_method(weighted.mean.Date)