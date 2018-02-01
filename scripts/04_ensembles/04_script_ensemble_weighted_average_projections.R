### script ensemble weighted average ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 05/06/2017

###----------------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, data.table, vegan, viridis)

# verify packages
search()

###----------------------------------------------------------------------------###

# import data
# directory
setwd("E:/github_mauriciovancine/R-ENM/ouput_future")

# enms
# list files
tif <- list.files(patt = ".tif$")
tif

enm <- raster(tif[sample(1:length(tif), 1)])
enm
plot(enm, col = viridis(100), main = names(enm))

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
sp <- sub("zEval_ACCESS_svm_", "", sub(".txt", "", grep("svm", txt, value = T)))
sp

# periods
pe <- c("pres", "rcp45_2050", "rcp45_2070", "rcp85_2050", "rcp85_2070")
pe

# data.table
da <- data.table()
da

# ensemble
dir.create("ensemble_wei")

for(i in sp){
  
  # verify
  print(paste0("Nice! The ensemble ", i, " start!"))
  
  
  tif.sp <- grep(i, tif, value = T)
  eva.sp <- eva[grep(i, names(eva))]
  
  tss <- do.call("rbind", eva.sp)$TSS
  id.tss <- which(tss > .5)
  va.tss <- tss[tss > .5]
  
  if(length(id.tss) == 0){
    print(paste0("Ops! The ensemble for ", i, " don't have models with TSS above 0.5!"))
    
  } else{
  
  for(j in pe){
    tif.pe <- grep(j, tif.sp, value = T)
    da <- rbind(da, rasterToPoints(stack(tif.pe[id.tss])), use.names = F)
    }
  
  da.r <- data.table(decostand(da[, -c(1, 2)], "range", na.rm = T)) 
  
  da.r.pe <- data.table(da[, 1:2], pe = rep(pe, each = length(na.omit(enm[]))), da.r)
  
  
  for(k in pe){
    da.pe <- da.r.pe[pe == k, -3]
    da.pe.wa <- apply(da.pe[, -c(1, 2)], 1, function (x) sum(x * va.tss) / sum(va.tss))
    da.pe <- data.table(da.pe[, 1:2], da.pe.wa)
    
    gridded(da.pe) <- ~x + y 
    ens <- raster(da.pe)
    projection(ens) <- CRS("+proj=longlat +datum=WGS84")
    
    plot(ens, col = viridis(100))
    
    setwd("ensemble_wei")
    
    writeRaster(ens, paste0("ensemble_wei_aver_", i, "_", k, ".tif"), 
                format = "GTiff")
    
    setwd("..")
    
    print(paste0("Nice! The ensemble ", i, " for ", k, " it's done!"))
    
    }
  
  da <- data.table()
  
  }
  
  print("Yeh! It's over!!!")
  
  }

###----------------------------------------------------------------------------###
