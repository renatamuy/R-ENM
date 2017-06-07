### script ensemble weighted average ###

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
library(data.table) # long tables

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

enm <- raster(tif[1])
enm
plot(enm)

# evaluate
txt <- list.files(patt = ".txt$")
txt

eva <- lapply(txt, read.table)
eva
names(eva) <- txt
eva

###-----------------------------------------------------------------------------------------###

## weighted average ensemble 
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

# ensembles
for(i in sp){
  tif.sp <- grep(i, tif, value = T)
  eva.sp <- eva[grep(i, names(eva))]

tss <- do.call("rbind", eva)$TSS
tss




va <- matrix(NA, nrow = ncell(enm), ncol = length(al))
va

ens.al <- enm[[1]]
ens.al[] <- NA
names(ens.al) <- "ens.al"
ens.al

for(i in sp){		
  asc.sp <- grep(i, asc, value = T)
  
  for(j in gc){		
    asc.gc <- grep(j, asc.sp, value = T)
    
    for(k in pe){		
      asc.pe <- grep(k, asc.gc, value = T)
      
      for(l in al){		
        asc.al <- grep(l, asc.pe, value = T)
        
        for(m in re){		
          enm.al <- stack(asc.al)
          va[, m] <- values(enm.al[[m]])}
        
        ens.al[] <- apply(va, 1, mean)
        
        writeRaster(ens.al, paste0("ensemble_aver_", i, "_", j, "_", k, "_", l, ".tif"), 
                    format = "GTiff")
        
        va <- matrix(NA, nrow = ncell(enm), ncol = length(al))
        ens.al[] <- NA}}}}

###-----------------------------------------------------------------------------------------###
