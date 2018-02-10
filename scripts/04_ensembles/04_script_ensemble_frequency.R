### script frequency ensemble ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 17/06/2017

###----------------------------------------------------------------------------###

# memory
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

names(eva) <- sub(".txt", "", sub("zEval_", "", txt))
eva <- do.call("rbind", eva)
eva

###----------------------------------------------------------------------------###

## frequency ensemble 
# lists
# species
sp <- sub("zEval_svm_", "", sub(".txt", "", grep("svm", txt, value = TRUE)))
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

# tss
tss <- .5

# ensembles
ens <- enm
ens[] <- 0
ens

# directory
dir.create("ensemble_freq")

# for
for(i in sp){
  
  tif.sp <- grep(i, tif, value = TRUE)
  eva.sp <- eva[grep(i, row.names(eva)), ]
  
  tss.id <- which(eva.sp$TSS > tss)
  
  tif.sp <- tif.sp[tss.id]
  eva.sp <- eva.sp[tss.id, ]
  
  print(paste0("The ensemble for ", i, " started, relax, take a coffee, it may take awhile..."))
  
  for(j in gc){
    
    tif.gc <- grep(j, tif.sp, value = TRUE)
    eva.gc <- eva.sp[grep(j, row.names(eva.sp)), ]
    
    
    for(k in pe){
      
      tif.pe <- grep(k, tif.gc, value = TRUE)
      
      
      for(l in al){
        
        tif.al <- grep(l, tif.pe, value = TRUE)
        eva.al <- eva.gc[grep(l, row.names(eva.gc)), ]
        
        enm.al <- stack(tif.al)
        
        for(m in 1:length(tif.al)){
          
          ens <- sum(ens, enm.al[[m]] >= eva.al[m, 1])
          
          }
        
        }
      
      setwd("ensemble_freq")

      writeRaster(ens / (length(tif.sp)), 
                  sub("__", "", paste0("ensemble_freq_", i, "_", j, "_", k, ".tif")), 
                  format = "GTiff", overwrite = TRUE)
      
      setwd("..")
      
      print(paste0("Nice! The ensemble of ", i, ", ", j, ", ", k, " it's done!"))
      
      ens[] <- 0
      
    }
    
    }
  
  print("Yeh! It's over!!!")
  
  }

###----------------------------------------------------------------------------###



