### script select points frequency ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 17/06/2017

###--------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table)

# verify packages
search()

###--------------------------------------------------------------------###

# import data

# diretory
setwd("")

# data
# da <- read.table(".txt", h = T, sep = "\t")
# da

da <- data.table(sp = rep(c("sp1", "sp2", "sp3"), c(20, 5, 50)), 
		     lat = -rnorm(75, 22, 1), long = -rnorm(75, 45, 1))
da

head(da)
dim(da)

###--------------------------------------------------------------------###

# frequency
ocorr <- data.table(table(da$sp))
head(ocorr)
colnames(ocorr) <- c("sp", "f")
head(ocorr)

hist(ocorr$f, col = "gray")

#occurrencies
ocorr.10 <- ocorr[ocorr$f >= 10, ]
head(ocorr.10)
dim(ocorr.10)

###--------------------------------------------------------------------###

# merge
m <- merge(da, ocorr)
head(m)
dim(m)

# select
da.10 <- m[m$f >= 10,]
da.10

# check
dim(da.10)
length(unique(da.10$sp))

###--------------------------------------------------------------------###

# export
write.table(da.10, "occur_10.xls", sep = "\t", quote = F, row.names = F)

write.table(da.10[, -4], "occur_10.txt", sep = "\t", quote = F, row.names = F)

###--------------------------------------------------------------------###


















