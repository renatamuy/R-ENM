### script ensemble de frequencia ###

# Thadeu Sobral de Souza - thadeusobral@gmail.com 
# Maurício Humberto Vancine - mauricio.vancine@gmail.com

###-----------------------------------------------------------------------------------------###

# 1. limpar a memoria e carregar os pacotes 
# limpar o workspace e aumentar a memoria para o r
rm(list = ls())
memory.limit(size = 1.75e13) 

# instalar e carregar pacotes
# install.packages(c("raster", "rgdal", "vegan"), dep = T)

# carregar pacotes
library(raster) # manejo de arquivos sig 
library(rgdal) # manejo de arquivos sig
library(vegan) # diversas analises multivariadas

# verificar pacotes carregados
search()


###-----------------------------------------------------------------------------------------###

# import data
# directory
setwd("D:/github/enm_r/data/03_saidas_enm")

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

###-----------------------------------------------------------------------------------------###

## frequency ensemble 
# lists
# species
sp <- sub("zEval_svm_", "", sub(".txt", "", grep("svm", list.files(patt = ".txt$"), value = T)))
sp

# gcms
gc <- "CCSM"
gc

# periods
pe <- c("0k", "6k", "21k")
pe

# algorithms
al <- c("Bioclim", "Gower", "Maha", "Maxent", "SVM")
al

# replicates
re <- 1:5
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
                  ens.re <- sum(ens.re, enm.al[[m]] >= eva.al[[1]][m, 1])}
                  
	      writeRaster(ens.re, paste0("ensemble_freq_", i, "_", j, "_", k, "_", l, ".tif"), 
			  format = "GTiff")

	      ens.al <- sum(ens.al, ens.re)
		  	
	      ens.re[] <- 0}

	   writeRaster(ens.al, paste0("ensemble_freq_", i, "_", j, "_", k, ".tif"), format = "GTiff")
	   writeRaster(ens.al / (length(al) * length(re)), paste0("ensemble_freq_", i, "_", j, "_", k, "_bin.tif"), 
		       format = "GTiff")
		
	   ens.al[] <- 0}}}

###-----------------------------------------------------------------------------------------###

## average ensemble 

# lists
# species
sp <- list("B.balansae")
sp

# gcms
gc <- list("CCSM")
gc

# periods
pe <- list("0k", "6k", "21k")
pe

# algorithms
al <- list("Bioclim", "Gower", "Maha", "Maxent", "SVM")
al

# replicates
re <- list(1:5)
re

# ensembles
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
		  
		  writeRaster(ens.al, paste0("ensemble_aver_", i, "_", j, "_", k, "_", l, ".asc"), 
			        format = "ascii")

	    va <- matrix(NA, nrow = ncell(enm), ncol = length(al))
            ens.al[] <- NA}}}}

###-----------------------------------------------------------------------------------------###

