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
library(data.table) # tables

# verify packages
search()


###-----------------------------------------------------------------------------------------###

# import data
# directory
setwd("D:/ensemble")

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

## frequency ensemble 
# lists
# species
sp <- sub("zEval_svm_", "", sub(".txt", "", grep("svm", list.files(patt = ".txt$"), value = T)))
sp

# algorithms
al <- c("bioclim", "gower", "maxent", "svm")
al

# replicates
re <- 1:10
re

# ensembles
ens.re <- enm
ens.re[] <- 0
names(ens.re) <- "ens.re"
ens.re

ens.al <- enm
ens.al[] <- 0
names(ens.al) <- "ens.al"
ens.al

# for
for(i in sp){		
  tif.sp <- grep(i, tif, value = T)
  eva.sp <- eva[grep(i, names(eva))]
  
  for(j in al){		
    tif.al <- grep(j, tif.sp, value = T)
    eva.al <- eva.sp[grep(j, names(eva.sp))]
    
    for(k in re){		
      enm.al <- stack(tif.al)
      ens.re <- sum(ens.re, enm.al[[k]] >= eva.al[[1]][k, 1])}
    
    writeRaster(ens.re, paste0("ensemble_freq_", i, "_", j, ".tif"), 
                format = "GTiff")
    
    ens.al <- sum(ens.al, ens.re)
    
    ens.re[] <- 0}
  
  writeRaster(ens.al, paste0("ensemble_freq_", i, ".tif"), format = "GTiff")
  
  writeRaster(ens.al / (length(al) * length(re)), paste0("ensemble_freq_", i, 
                                                         "_bin.tif"), 
              format = "GTiff")
  
  ens.al[] <- 0}

###-----------------------------------------------------------------------------------------###
