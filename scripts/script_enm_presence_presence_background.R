### script enm ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 05/06/2017

###---------------------------------------------------------------------------###

# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
memory.limit(size = 1.75e13) 

# install and load packages
# install packages
# install.packages(c("raster", "rgdal", "dismo", "gam", "randomForest", "kernlab", 
#                    "rJava", "vegan"), dep = T)

# load packages
library(raster)
library(rgdal) 
library(dismo) 
library(gam) 
library(randomForest) 
library(kernlab) 
library(rJava) 
library(vegan) 

# verify packages
search()

###---------------------------------------------------------------------------###

# 2. import data
# directory
setwd("D:/_github/enmR/data")

# ocurrences
po <- read.table("Bromelia_balansae.txt", h = T)
head(po, 10)

plot(po$long, po$lat, pch = 20)

#  variables
tif <- list.files(patt = "tif")
tif

tif.0k <- grep("0k", tif, value = T)
tif.0k

tif.6k <- grep("6k", tif, value = T)
tif.6k

tif.21k <- grep("21k", tif, value = T)
tif.21k

env.stack.0k <- stack(tif.0k)
names(env.stack.0k) <- paste0("bio", c("02", "04", "10", "16", "17"))
env.stack.0k

env.stack.6k <- stack(tif.6k)
names(env.stack.6k) <- paste0("bio", c("02", "04", "10", "16", "17"))
env.stack.6k

env.stack.21k <- stack(tif.21k)
names(env.stack.21k) <- paste0("bio", c("02", "04", "10", "16", "17"))
env.stack.21k

plot(env.stack.0k)
plot(env.stack.6k)
plot(env.stack.21k)

plot(env.stack.0k[[1]])
points(po$long, po$lat, pch = 20)


## extract coordinates for background
# coordinates
id <- 1:ncell(env.stack.0k)
head(id, 50)
length(id)

co <- xyFromCell(env.stack.0k, id)
head(coord, 50)

plot(env.stack.0k[[1]])
points(co, pch = "o", cex = 1e-1)

# without NAs
va <- values(env.stack.0k)[, 1]
head(va, 50)
length(va)

co.va <- data.frame(co, va)
head(co.va, 20)

co.va.na <- na.omit(co.va)
head(co.va.na, 10)

cs <- co.va.na[, -3]
head(cs, 10)

colnames(cs) <- c("long", "lat")
head(cs, 10)

plot(env.stack.0k[[1]])
points(cs, pch = "o", cex = 1e-1)

###---------------------------------------------------------------------------###

# verify maxent
jar <- paste0(system.file(package = "dismo"), "/java/maxent.jar")
file.exists(jar)

###---------------------------------------------------------------------------###


### ENMs ###

# diretory
setwd("..")
getwd()
dir.create("ouput")
setwd("ouput")
getwd()

# aogcms
AOGCM <- "CCSM"
AOGCM

# enms
for(i in 1:length(levels(po[, 1]))){ # for to each specie
  # variables for evaluate
  eval.Bioclim <- NULL
  eval.Gower <- NULL
  eval.Maha <- NULL
  eval.Maxent <- NULL
  eval.SVM <- NULL
  eval.names <- NULL

  # selecting presence and absence
	id.specie <- levels(po[, 1])[i]
	pr.specie <- po[which(po[, 1] == id.specie), 2:3]
	id.background <- sample(nrow(coords), nrow(pr.specie))
	bc.specie <- coords[id.background, ]
	

  for(r in 1:5){	# number of replicas
    ## preparing the models
    # train and test data	
	  pr.sample.train <- sample(nrow(pr.specie), round(0.75 * nrow(pr.specie)))
	  bc.sample.train <- sample(nrow(bc.specie), round(0.75 * nrow(bc.specie)))
	  test <- na.omit(prepareData(x = env.stack.0k, p = pr.specie[-pr.sample.train, ], b = bc.specie[-bc.sample.train, ]))
  	train <- na.omit(prepareData(x = env.stack.0k, p = pr.specie[pr.sample.train, ], b = bc.specie[bc.sample.train, ]))

 
    ### algorithms
  	
    ## 1. bioclim
  	# 1.1 calibration
	  Bioclim <- bioclim(train[which(train[, 1] == 1), -1])	
	 
	  # 1.2 projection
    writeRaster(predict(env.stack.0k, Bioclim), paste0(AOGCM, "_bioclim_0k_", id.specie, r, ".tif"), format = "GTiff")	
    writeRaster(predict(env.stack.6k, Bioclim), paste0(AOGCM, "_bioclim_6k_", id.specie, r, ".tif"), format = "GTiff")
    writeRaster(predict(env.stack.21k, Bioclim), paste0(AOGCM, "_bioclim_21k_", id.specie, r, ".tif"), format = "GTiff") 
    
    # 1.3 evaluation
	  eBioclim <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Bioclim)
	  idBioclim <- which(eBioclim@t == as.numeric(threshold(eBioclim, "spec_sens")))
	  eval.Bioclim.sp <- c(eBioclim@t[idBioclim], eBioclim@auc, (eBioclim@TPR[idBioclim] + eBioclim@TNR[idBioclim] - 1))
	  eval.Bioclim <- rbind(eval.Bioclim, eval.Bioclim.sp)

	  
    ## 2. gower
	  # 2.1 calibration
	  Gower <- domain(train[which(train[, 1] == 1), -1])	

	  # 2.2 projection
    writeRaster(predict(env.stack.0k, Gower), paste0(AOGCM, "_gower_0k_", id.specie, r, ".tif"), format = "GTiff") 
    writeRaster(predict(env.stack.6k, Gower), paste0(AOGCM, "_gower_6k_", id.specie, r, ".tif"), format = "GTiff")
    writeRaster(predict(env.stack.21k, Gower), paste0(AOGCM, "_gower_21k_", id.specie, r, ".tif"), format = "GTiff") 
 
    # 2.3 evaluation
	  eGower <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Gower)
  	idGower <- which(eGower@t == as.numeric(threshold(eGower, "spec_sens")))
	  eval.Gower.sp <- c(eGower@t[idGower], eGower@auc, (eGower@TPR[idGower] + eGower@TNR[idGower] - 1))
	  eval.Gower <- rbind(eval.Gower, eval.Gower.sp)

	  
    ## 3. mahanobis	
	  # 3.1 calibration
	  Maha <- mahal(train[which(train[, 1] == 1), -1])	
	
	  # 3.2 projection
    writeRaster(predict(env.stack.0k, Maha), paste0(AOGCM, "_mahanobis_0k_", id.specie, r, ".tif"), format = "GTiff") 
    writeRaster(predict(env.stack.6k, Maha), paste0(AOGCM, "_mahanobis_6k_", id.specie, r, ".tif"), format = "GTiff")
    writeRaster(predict(env.stack.21k, Maha), paste0(AOGCM, "_mahanobis_21k_", id.specie, r, ".tif"), format = "GTiff") 
 
    # 3.3 evaluation
	  eMaha <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maha)
	  idMaha <- which(eMaha@t == as.numeric(threshold(eMaha, "spec_sens")))
	  eval.Maha.sp <- c(eMaha@t[idMaha], eMaha@auc, (eMaha@TPR[idMaha] + eMaha@TNR[idMaha] - 1))
	  eval.Maha <- rbind(eval.Maha, eval.Maha.sp)
	

    ## 4. maxent	
	  # 4.1 calibration
	  Maxent <- maxent(train[, -1], train[, 1])	

	  # 4.2 projection
    writeRaster(predict(env.stack.0k, Maxent), paste0(AOGCM, "_maxent_0k_", id.specie, r, ".tif"), format = "GTiff") 
    writeRaster(predict(env.stack.6k, Maxent), paste0(AOGCM, "_maxent_6k_", id.specie, r, ".tif"), format = "GTiff")
    writeRaster(predict(env.stack.21k, Maxent), paste0(AOGCM, "_maxent_21k_", id.specie, r, ".tif"), format = "GTiff") 
 
    # 4.3 evaluation
	  eMaxent <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maxent)
	  idMaxent <- which(eMaxent@t == as.numeric(threshold(eMaxent, "spec_sens")))
	  eval.Maxent.sp <- c(eMaxent@t[idMaxent], eMaxent@auc, (eMaxent@TPR[idMaxent] + eMaxent@TNR[idMaxent] - 1))
	  eval.Maxent <- rbind(eval.Maxent, eval.Maxent.sp)


    ## 5. svm	
	  # 5.1 calibration
	  SVM <- ksvm(pb ~ bio02 + bio04 + bio10 + bio16 + bio17, data = train)	

	  # 5.2 projection
    writeRaster(predict(env.stack.0k, SVM), paste0(AOGCM, "_svm_0k_", id.specie, r, ".tif"), format = "GTiff") 
    writeRaster(predict(env.stack.6k, SVM), paste0(AOGCM, "_svm_6k_", id.specie, r, ".tif"), format = "GTiff")
    writeRaster(predict(env.stack.21k, SVM), paste0(AOGCM, "_svm_21k_", id.specie, r, ".tif"), format = "GTiff") 
 
    # 5.3 evaluation
	  eSVM <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = SVM)
	  idSVM <- which(eSVM@t == as.numeric(threshold(eSVM, "spec_sens")))
	  eval.SVM.sp <- c(eSVM@t[idSVM], eSVM@auc, (eSVM@TPR[idSVM] + eSVM@TNR[idSVM] - 1))
	  eval.SVM <- rbind(eval.SVM, eval.SVM.sp)

	  eval.names <- c(eval.names, paste0(id.specie, r))	
	  
  } # ends for "r"

  dimnames(eval.Bioclim) <- list(eval.names, c("thrs", "AUC", "TSS"))
  dimnames(eval.Gower) <- list(eval.names, c("thrs", "AUC", "TSS"))  
  dimnames(eval.Maha) <- list(eval.names, c("thrs", "AUC", "TSS"))  
  dimnames(eval.Maxent) <- list(eval.names, c("thrs", "AUC", "TSS"))
  dimnames(eval.SVM) <- list(eval.names, c("thrs", "AUC", "TSS"))

  write.table(eval.Bioclim, paste0("zEval_", AOGCM, "_bioclim_", id.specie, ".txt"))
  write.table(eval.Gower, paste0("zEval_", AOGCM, "_gower_", id.specie, ".txt"))
  write.table(eval.Maha, paste0("zEval_", AOGCM, "_maha_", id.specie, ".txt"))
  write.table(eval.Maxent, paste0("zEval_", AOGCM, "_maxent_", id.specie, ".txt"))
  write.table(eval.SVM, paste0("zEval_", AOGCM, "_svm_", id.specie, ".txt"))

} # ends for"i"

###----------------------------------------------------------------------------###

