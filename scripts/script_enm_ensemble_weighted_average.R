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

# periods
pe <- c("0k", "6k", "21k")
pe

# data.table
da <- data.table()
da

# raster
ens <- enm[[1]]
ens[] <- NA
names(ens) <- "ens"
ens

# ensemble
for(i in sp){
  tif.sp <- grep(i, tif, value = T)
  eva.sp <- eva[grep(i, names(eva))]

  tss <- do.call("rbind", eva.sp)$TSS
  id.tss <- which(tss > 0.5)

    for(j in pe){
      tif.pe <- grep(j, tif.sp, value = T)
      te <- data.table(pe = rep(j, ncell = enm), stack(tif.pe[id.tss])[])
      da <- rbind(da, te)}}
      
  da.s <- scale(da)
      
  ens[] <- apply(da.s, 1, mean)

  writeRaster(ens.al, paste0("ensemble_aver_", i, "_", j, ".tif"), 
              format = "GTiff")

  da <- data.table()
  ens[] <- NA}

###-----------------------------------------------------------------------------------------###
