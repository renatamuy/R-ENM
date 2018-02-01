### script ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 04/01/2018

###----------------------------------------------------------------------------###

# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, data.table, vegan, ggplot2, stringr, viridis)
search()

###----------------------------------------------------------------------------###

# import data
# directory
setwd("F:/amphibians/01_persistence_af/02_output_st")

# enms
# list files
tif <- list.files(patt = ".tif$")
tif

enm <- raster(tif[1])
enm
plot(enm, col = viridis(100))

# evaluate
txt <- list.files(patt = ".txt$")
txt

eva <- lapply(txt, read.table)
eva
names(eva) <- txt
eva


## plot evaluete
# species
sp <- sub("zEval_bio_svm_", "", sub(".txt", "", grep("svm", txt, value = T)))
sp

# algorithms
al <- c("Bioclim", "Gower", "Maxent", "SVM")
al

dir.create("eval_boxplot")
setwd("eval_boxplot")

for(i in sp){
  ev.sp <- eva[grep(i, names(eva))]
  tss <- do.call("rbind", ev.sp)
  dat <- data.table(tss, alg = rep(al, each = 10), col = rep(c("forest green", "blue", "red", "orange"), each = 10))
  
  ggplot(data = dat, aes(x = alg, y = TSS)) + 
    geom_boxplot() + 
    geom_jitter(colour = dat$col, width = 0.2) +
    theme(legend.position = "none") +
    xlab("Algorithms") +
    ylab("TSS") + 
    
    theme_classic() +
    
    geom_hline(yintercept = .4, color = "red") + 
    
    ggtitle(bquote("" ~ italic(.(sub("_", " ", str_to_title(i)))))) + 
    
    theme(plot.title = element_text(lineheight = .8, face = "bold"), 
          axis.text = element_text(size = 12, colour = "black"), 
          axis.title = element_text(size = 15))
  
  ggsave(paste0("boxplot_jitter_", i, ".tiff"), he = 18, wi = 18, un = "cm", dpi = 300)}

setwd("..")


###-----------------------------------------------------------------------------###

## weighted average ensemble 

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
  tss.04 <- tss[tss > .4]
  
  if(length(id.tss) == 0){
    
    print(paste0("Ops! The ensemble for ", i, " don't have models with TSS above 0.4!"))
    
  } else{
    
    print(paste0("The ensemble for ", i, " started, relax, take a coffe, it may take a while...."))
    da <- rbind(da, stack(tif.sp[id.tss])[], use.names = F)
    da.r <- data.table(decostand(da, "range", na.rm = T)) 
    
    ens[] <- apply(da.r, 1, function (x) sum(x * tss.04) / sum(tss.04))
    
    setwd("ensemble_wei")
    writeRaster(ens, paste0("ens_wei_ave_", i, ".tif"), format = "GTiff")
    setwd("..")
    
    print(paste0("Nice! The ensemble for ", i, " it's done!"))
    
    da <- data.table()
    ens[] <- NA
  
      }
  
  print("Yeh! It's over!!!")
  
  }

###----------------------------------------------------------------------------###
