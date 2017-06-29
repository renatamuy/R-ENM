### script ensemble weighted average ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 05/06/2017

###----------------------------------------------------------------------------###

# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, data.table, vegan)

# verify packages
search()

###----------------------------------------------------------------------------###

# import data
# directory
setwd("D:/amazonia/02_ouput")

# enms
# list files
tif <- list.files(patt = ".tif$")
tif

enm <- raster(tif[3])
enm
plot(enm)

# evaluate
txt <- list.files(patt = ".txt$")
txt

eva <- lapply(txt, read.table)
eva
names(eva) <- txt
eva

###-----------------------------------------------------------------------------###

## weighted average ensemble 
# lists
# species
sp <- sub("zEval_bio_svm_", "", sub(".txt", "", grep("svm", txt, value = T)))
sp

# data.table
da <- data.table()
da

# raster
ens <- enm[[1]]
ens[] <- NA
names(ens) <- "ens"
ens

# ensemble
dir.create("ensemble_wei")

for(i in sp){
  tif.sp <- grep(i, tif, value = T)
  eva.sp <- eva[grep(i, names(eva))]
  
  tss <- do.call("rbind", eva.sp)$TSS
  id.tss <- which(tss > .4)
  tss.05 <- tss[tss > .4]
  
  if(length(id.tss) == 0){
    
    print(paste0("Ops! The ensemble for ", i, " don't have models with TSS above 0.4!"))
    
  } else{
    da <- rbind(da, stack(tif.sp[id.tss])[], use.names = F)
    da.r <- data.table(decostand(da, "range", na.rm = T)) 
  
    ens[] <- apply(da.r, 1, function (x) sum(x * tss.05) / sum(tss.05))
    
    setwd("ensemble_wei")
    writeRaster(ens, paste0("ensemble_wei_aver_", i, ".tif"), format = "GTiff")
    setwd("..")
    
    print(paste0("Nice! The ensemble for ", i, " it's done!"))
  
    da <- data.table()
    ens[] <- NA}}

###----------------------------------------------------------------------------###
