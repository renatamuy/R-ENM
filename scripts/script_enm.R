### script enm ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 05/06/2017

###---------------------------------------------------------------------------###

# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, dismo, gam, randomForest, kernlab, rJava, vegan)

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
ti <- list.files(patt = "tif")
ti

ti <- grep("0k", ti, value = T)
ti

en <- stack(ti)
names(en) <- paste0("bio", c("02", "04", "10", "16", "17"))
en

plot(en)

plot(en[[1]])
points(po$long, po$lat, pch = 20)


## extract coordinates for background
# coordinates
id <- 1:ncell(en)
head(id, 50)
length(id)

co <- xyFromCell(en, id)
head(co, 50)

plot(en[[1]])
points(co, pch = "o", cex = 1e-1)

# without NAs
va <- values(en)[, 1]
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

plot(en[[1]])
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

# variables
bio <- "bio"

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
	id.background <- sample(nrow(cs), nrow(pr.specie))
	bc.specie <- cs[id.background, ]
	

  for(r in 1:10){	# number of replicas
    ## preparing the models
    # train and test data	
	  pr.sample.train <- sample(nrow(pr.specie), round(0.7 * nrow(pr.specie)))
	  bc.sample.train <- sample(nrow(bc.specie), round(0.7 * nrow(bc.specie)))
	  test <- na.omit(prepareData(x = en, p = pr.specie[-pr.sample.train, ], b = bc.specie[-bc.sample.train, ]))
  	train <- na.omit(prepareData(x = en, p = pr.specie[pr.sample.train, ], b = bc.specie[bc.sample.train, ]))

    # verify 
  	print(paste0(id.specie, "_", ifelse(r < 10, paste0("0", r), r)))
  	
  	
    ### algorithms
  	
    ## 1. bioclim
  	# 1.1 calibration
	  Bioclim <- bioclim(train[which(train[, 1] == 1), -1])	
	 
	  # 1.2 projection
    writeRaster(predict(en, Bioclim), paste0(bio, "_bioclim_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")	
   
    # 1.3 evaluation
	  eBioclim <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Bioclim)
	  idBioclim <- which(eBioclim@t == as.numeric(threshold(eBioclim, "spec_sens")))
	  eval.Bioclim.sp <- c(eBioclim@t[idBioclim], eBioclim@auc, (eBioclim@TPR[idBioclim] + eBioclim@TNR[idBioclim] - 1))
	  eval.Bioclim <- rbind(eval.Bioclim, eval.Bioclim.sp)

	  
    ## 2. gower
	  # 2.1 calibration
	  Gower <- domain(train[which(train[, 1] == 1), -1])	

	  # 2.2 projection
    writeRaster(predict(en, Gower), paste0(bio, "_gower_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 

    # 2.3 evaluation
	  eGower <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Gower)
  	idGower <- which(eGower@t == as.numeric(threshold(eGower, "spec_sens")))
	  eval.Gower.sp <- c(eGower@t[idGower], eGower@auc, (eGower@TPR[idGower] + eGower@TNR[idGower] - 1))
	  eval.Gower <- rbind(eval.Gower, eval.Gower.sp)

	  
    ## 3. mahalanobis	
	  # 3.1 calibration
	  Maha <- mahal(train[which(train[, 1] == 1), -1])	
	
	  # 3.2 projection
    writeRaster(predict(en, Maha), paste0(bio, "_mahalanobis_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 

    # 3.3 evaluation
	  eMaha <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maha)
	  idMaha <- which(eMaha@t == as.numeric(threshold(eMaha, "spec_sens")))
	  eval.Maha.sp <- c(eMaha@t[idMaha], eMaha@auc, (eMaha@TPR[idMaha] + eMaha@TNR[idMaha] - 1))
	  eval.Maha <- rbind(eval.Maha, eval.Maha.sp)
	

    ## 4. maxent	
	  # 4.1 calibration
	  Maxent <- maxent(train[, -1], train[, 1])	

	  # 4.2 projection
    writeRaster(predict(en, Maxent), paste0(bio, "_maxent_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 

    # 4.3 evaluation
	  eMaxent <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maxent)
	  idMaxent <- which(eMaxent@t == as.numeric(threshold(eMaxent, "spec_sens")))
	  eval.Maxent.sp <- c(eMaxent@t[idMaxent], eMaxent@auc, (eMaxent@TPR[idMaxent] + eMaxent@TNR[idMaxent] - 1))
	  eval.Maxent <- rbind(eval.Maxent, eval.Maxent.sp)


    ## 5. svm	
	  # 5.1 calibration
	  SVM <- ksvm(pb ~ bio02 + bio04 + bio10 + bio16 + bio17, data = train)	

	  # 5.2 projection
    writeRaster(predict(en, SVM), paste0(bio, "_svm_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 

    # 5.3 evaluation
	  eSVM <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = SVM)
	  idSVM <- which(eSVM@t == as.numeric(threshold(eSVM, "spec_sens")))
	  eval.SVM.sp <- c(eSVM@t[idSVM], eSVM@auc, (eSVM@TPR[idSVM] + eSVM@TNR[idSVM] - 1))
	  eval.SVM <- rbind(eval.SVM, eval.SVM.sp)

	  eval.names <- c(eval.names, paste0(id.specie, ifelse(r < 10, paste0("0", r), r)))	
	  
  } # ends for "r"

  dimnames(eval.Bioclim) <- list(eval.names, c("thrs", "AUC", "TSS"))
  dimnames(eval.Gower) <- list(eval.names, c("thrs", "AUC", "TSS"))  
  dimnames(eval.Maha) <- list(eval.names, c("thrs", "AUC", "TSS"))  
  dimnames(eval.Maxent) <- list(eval.names, c("thrs", "AUC", "TSS"))
  dimnames(eval.SVM) <- list(eval.names, c("thrs", "AUC", "TSS"))

  write.table(eval.Bioclim, paste0("zEval_", bio, "_bioclim_", id.specie, ".txt"))
  write.table(eval.Gower, paste0("zEval_", bio, "_gower_", id.specie, ".txt"))
  write.table(eval.Maha, paste0("zEval_", bio, "_mahalanobis_", id.specie, ".txt"))
  write.table(eval.Maxent, paste0("zEval_", bio, "_maxent_", id.specie, ".txt"))
  write.table(eval.SVM, paste0("zEval_", bio, "_svm_", id.specie, ".txt"))

} # ends for"i"

###----------------------------------------------------------------------------###

