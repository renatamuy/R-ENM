### script frequency ensemble ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 17/06/2017

###----------------------------------------------------------------------------###

# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, data.table, viridis)
search()

###----------------------------------------------------------------------------###

# import data
# directory
setwd("E:/github_mauriciovancine/R-ENM/output")

# enms
# list files
tif <- dir(patt = ".tif$")
tif

enm <- raster(tif[[1]])
enm
plot(enm, main = names(tif[[1]]), col = viridis(100))

# evaluate
txt <- list.files(patt = ".txt$")
txt

eva <- lapply(txt, read.table)
eva

names(eva) <- txt
eva

###----------------------------------------------------------------------------###

## frequency ensemble 
# lists
# species
sp <- sub("zEval_svm_", "", sub(".txt", "", grep("svm", txt, value = T)))
sp

# gcms
gc <- c("")
gc

# periods
pe <- c("")
pe

# algorithms
al <- c("bioclim", "gower", "mahalanobis", "maxent", "svm")
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

# directory
dir.create("ensemble_freq")

# for
for(i in sp){		
  
  tif.sp <- grep(i, tif, value = T)
  eva.sp <- eva[grep(i, names(eva))]
  
  print(paste0("The ensemble for ", i, " started, relax, take a coffee, it may take awhile..."))
  
  for(j in gc){
    
    tif.gc <- grep(j, tif.sp, value = T)
    eva.gc <- eva.sp[grep(j, names(eva.sp))]
    
    
    for(k in pe){	
      
      tif.pe <- grep(k, tif.gc, value = T)
      
      
      for(l in al){
        
        tif.al <- grep(l, tif.pe, value = T)
        eva.al <- eva.gc[grep(l, names(eva.gc))]
        
        
        for(m in re){	
          
          enm.al <- stack(tif.al)
          ens.re <- sum(ens.re, enm.al[[m]] >= eva.al[[1]][m, 1])
          
          }
        
        setwd("ensemble_freq")
        writeRaster(ens.re, paste0("ensemble_freq_", i, "_", j, "_", k, "_", l, ".tif"), 
                    format = "GTiff")
        
        setwd("..")
        
        ens.al <- sum(ens.al, ens.re)
        
        ens.re[] <- 0
        
        }
      
      
      setwd("ensemble_freq")
      writeRaster(ens.al, paste0("ensemble_freq_", i, "_", j, "_", k, "_freq.tif"), 
                  format = "GTiff")
      writeRaster(ens.al / (length(al) * length(re)), paste0("ensemble_freq_", i, 
                                                             "_", j, "_", k, "_bin.tif"), 
                  format = "GTiff")
      
      setwd("..")
      
      print(paste0("Nice! The ensemble of ", i, ", ", j, ", ", k, " it's done!"))
      
      ens.al[] <- 0
      
    }
    
    }
  
  print("Yeh! It's over!!!")
  
  }

###----------------------------------------------------------------------------###



