### script boxplot evaluate ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 04/01/2018

###----------------------------------------------------------------------------###

# memory
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
setwd("F:/amphibians/01_persistence_af/__enm_persistence/02_output_clim")

# evaluate
# list files
txt <- dir(patt = ".txt$")
txt

eva <- lapply(txt, read.table)
eva
names(eva) <- txt
eva

da <- do.call("rbind", eva)
da

###----------------------------------------------------------------------------###

## plot evaluete
# species
sp <- sub("zEval_svm_", "", sub(".txt", "", grep("svm", txt, value = T)))
sp

# algorithms
al <- c("Bioclim", "Gower", "Maxent", "SVM")
al

# directory
dir.create("boxplot")
setwd("boxplot")

for(i in sp){
  ev.sp <- eva[grep(i, names(eva))]
  da <- do.call("rbind", ev.sp)
  dat <- data.frame(da, alg = rep(al, each = 10), col = rep(c("forest green", "blue", "red", "orange"), each = 10))
  dat
  
  ggplot(data = dat, aes(x = alg, y = TSS)) + 
    geom_boxplot() + 
    geom_jitter(colour = dat$col, width = 0.2, size = 2) +
    theme(legend.position = "none") +
    xlab("Algorithms") +
    ylab("TSS") + 
    
    ylim(c(0, 1)) + 
    
    theme_classic() +
    
    geom_hline(yintercept = .5, color = "red") + 
    
    ggtitle(bquote("" ~ italic(.(sub("_", " ", str_to_title(i)))))) + 
    
    theme(plot.title = element_text(lineheight = 1.4, face = "bold"), 
          axis.text = element_text(size = 13, colour = "black"), 
          axis.title = element_text(size = 17))
  
  ggsave(paste0("boxplot_jitter_tss_", i, ".tiff"), he = 15, wi = 15, un = "cm", dpi = 300)



ggplot(data = dat, aes(x = alg, y = AUC)) + 
  geom_boxplot() + 
  geom_jitter(colour = dat$col, width = 0.2) +
  theme(legend.position = "none") +
  xlab("Algorithms") +
  ylab("AUC") + 
  
  ylim(c(0, 1)) + 
  
  theme_classic() +
  
  geom_hline(yintercept = .8, color = "red") + 
  
  ggtitle(bquote("" ~ italic(.(sub("_", " ", str_to_title(i)))))) + 
  
  
  theme(plot.title = element_text(lineheight = 1.2, face = "bold"), 
        axis.text = element_text(size = 15, colour = "black"), 
        axis.title = element_text(size = 20))

ggsave(paste0("boxplot_jitter_auc_", i, ".tiff"), he = 15, wi = 15, un = "cm", dpi = 300)

}

###-----------------------------------------------------------------------------###

