### script ensemble average ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 05/06/2017

###-----------------------------------------------------------------------------------------###

# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
memory.limit(size = 1.75e13) 

# install and load packages
# install packages
# install.packages(c("raster", "rgdal", "data.table"), dep = T)

# load packages
library(raster) # sig 
library(rgdal) # sig
library(data.table) # tables

# verify packages
search()


###-----------------------------------------------------------------------------------------###

# import data
# directory
setwd("D:/_github/enmR/ouput")

# enms
# list files
tif <- list.files(patt = ".tif$")
tif

enm <- raster(tif[[1]])
enm
plot(enm)

# evaluate
txt <- list.files(patt = ".txt")
txt

eva <- lapply(txt, read.table)
eva
names(eva) <- txt
eva[[1]]

## average ensemble 

# lists
# species
sp <- sub("zEval_CCSM_svm_", "", sub(".txt", "", grep("svm", txt, value = T)))
sp

# gcms
gc <- c("CCSM")
gc

# periods
pe <- c("0k", "6k", "21k")
pe

# algorithms
al <- c("bioclim", "gower", "mahalanobis", "maxent", "svm")
al

# replicates
re <- 1:5
re

# variables
da <- data.table()
da

ens.al <- enm[[1]]
ens.al[] <- NA
names(ens.al) <- "ens.al"
ens.al

# ensembles
for(i in sp){		
  tif.sp <- grep(i, tif, value = T)
  
  for(j in gc){		
    tif.gc <- grep(j, tif.sp, value = T)
    
    for(k in pe){		
      tif.pe <- grep(k, tif.gc, value = T)
      
      for(l in al){		
        tif.al <- grep(l, tif.pe, value = T)
        
        for(m in re){		
          enm.al <- stack(tif.al)
          da[m] <- values(enm.al[[m]])}
        
        da <- data.table(v1 = rnorm(100), v2 = rnorm(100))
        da[, 1]
        
        ens.al[] <- apply(da, 1, mean)
        
        dir.create("ensemble_aver")
        setwd("ensemble_aver")
        writeRaster(ens.al, paste0("ensemble_aver_", i, "_", j, "_", k, "_", l, ".tif"), 
                    format = "GTiff")
        setwd("..")
        
        da <- data.table()
        ens.al[] <- NA}}}}

###-----------------------------------------------------------------------------------------###

