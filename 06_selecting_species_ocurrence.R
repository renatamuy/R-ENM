# selecting species by names

da <- data.frame(sp = rep(c("a", "b", "c"), 20), lat = rnorm(20), long = rnorm(20))
head(da)
da

subset(da, sp %in% c("a", "b", "c"))

sp.c <- c("a", "b", "c")

da[da$sp %in% d, ]


