### disciplina - modelagem de  nicho ecológico: teoria e pratica ###
### ppg ecologia e biodiversidade - unesp 2017 ###

# Thadeu Sobral de Souza - thadeusobral@gmail.com 
# Maurício Humberto Vancine - mauricio.vancine@gmail.com

###-----------------------------------------------------------------------------------------###
### 3. script ensemble ### 
###-----------------------------------------------------------------------------------------###


# 1. limpara a memoria e carregar os pacotes 
# limpar o workspace e aumentar a memoria para o r
rm(list = ls())
memory.limit(size = 10000000000000) 

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
setwd("D:/90_aulas_montadas/_disciplina_enm_R_unesp_2017/scripts_r/03_saidas_enm")

# enms
# list files
asc <- list.files(pattern = ".asc")
asc

enm <- stack(asc)
plot(enm[[1]])

# evaluate
txt <- list.files(pattern = ".txt")
txt

eva <- lapply(txt, read.table)
eva
eva[[1]]
names(eva) <- txt
eva

###-----------------------------------------------------------------------------------------###

# frequency ensemble 
# directory of output of ensemble
setwd("..")
getwd()
dir.create("04_ensembles")
setwd("./04_ensembles")
dir.create("frequency_ensemble")
setwd("./frequency_ensemble")
getwd()

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
ens.re <- enm[[1]]
ens.re[] <- 0
names(ens.re) <- "ens.re"
ens.re

ens.al <- enm[[1]]
ens.al[] <- 0
names(ens.al) <- "ens.al"
ens.al

for(i in sp){		
  enm.sp <- enm[[grep(i, names(enm))]]
  eva.sp <- eva[grep(i, names(eva))]

    for(j in gc){		
      enm.gc <- enm.sp[[grep(j, names(enm.sp))]]
      eva.gc <- eva.sp[grep(j, names(eva.sp))]
        
`	for(k in pe){		
          enm.pe <- enm.gc[[grep(k, names(enm.gc))]]

            for(l in al){		
	      enm.al <- enm.pe[[grep(l, names(enm.pe))]]
              eva.al <- eva.gc[grep(l, names(eva.gc))]
           
	        for(m in re){		
                  ens.re <- sum(ens.re, enm.al[[m]] >= eva.al[[1]][m, 1])}

          writeRaster(ens.re, paste0("ensemble_freq_", i, "_", j, "_", k, "_", l, ".asc"), 
		              format = "ascii")		  
		  
		  ens.al <- sum(ens.al, ens.re)
		  	
		  ens.re[] <- 0}

	   writeRaster(ens.al, 
		       paste0("ensemble_freq_", i, "_", j, "_", k, ".asc"), format = "ascii")
	   writeRaster(ens.al / (length(al) * max(re[[1]])), 
		       paste0("ensemble_freq_", i, "_", j, "_", k, "_bin.asc"), format = "ascii")
		
	   ens.al[] <- 0}}}


###-----------------------------------------------------------------------------------------###

# average ensemble 
# directory of output of ensemble
setwd("..")
getwd()
dir.create("average ensemble")
setwd("./average ensemble")
getwd()

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
  enm.sp <- enm[[grep(i, names(enm))]]
  
    for(j in gc){		
      enm.gc <- enm.sp[[grep(j, names(enm.sp))]]
              
	  for(k in pe){		
            enm.pe <- enm.gc[[grep(k, names(enm.gc))]]

              for(l in al){		
                enm.al <- enm.pe[[grep(l, names(enm.pe))]]
          	             
                  for(m in re){		
                    va[, m] <- values(enm.al[[m]])}
			
		    ens.al[] <- apply(va, 1, mean)
		  
		    writeRaster(ens.al, paste0("ensemble_aver_", i, "_", j, "_", k, "_", l, ".asc"), 
			        format = "ascii")

		va <- matrix(NA, nrow = ncell(enm), ncol = length(al))
		ens.al[] <- NA}}}}


###-----------------------------------------------------------------------------------------###


