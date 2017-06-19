### script ensemble weighted average ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 05/06/2017

###----------------------------------------------------------------------------###

# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, data.table)

# verify packages
search()

###----------------------------------------------------------------------------###

# import data
# directory
setwd("D:/_github/enmR/ouput")

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
sp <- sub("zEval_CCSM_svm_", "", sub(".txt", "", grep("svm", txt, value = T)))
sp

# periods
pe <- c("00k", "06k", "21k")
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
  id.tss <- which(tss > .5)
  tss.05 <- tss[tss > .5]

  for(j in pe){
    tif.pe <- grep(j, tif.sp, value = T)
    da <- rbind(da, stack(tif.pe[id.tss])[], use.names = F)}

  da.s <- data.table(decostand(da, "stand"))
  da.s.pe <- data.table(pe = rep(pe, each = ncell(enm)), da.s)

  for(k in pe){
    da.pe <- da.s.pe[pe == k, -1]
    ens[] <- apply(da.pe, 1, function (x) sum(x * tss.05) / sum(tss.05))

    dir.create("ensemble_wei")
    setwd("ensemble_wei")
    writeRaster(ens, paste0("ensemble_wei_aver_", i, "_", k, ".tif"), 
                format = "GTiff")
    setwd("..")}
      
  da <- data.table()
  ens[] <- NA}

###----------------------------------------------------------------------------###
