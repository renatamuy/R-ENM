### script mapas ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 27/05/2017

###------------------------------------------------------------------------------###

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
library(maps)

# verificar pacotes carregados
search()

###------------------------------------------------------------------------------###

### anfibios

# import data
# directory
setwd("D:/mineracao/03_analises/anfibios_enm_lpt")

# enms
# list files
tif <- list.files()
tif

enm <- stack(tif)
enm

# pontos
# diretorio da pasta de dados de entrada
setwd("D:/mineracao/01_dados")

# occurrence 
po <- read.table("ocorrencias_10.txt", h = T)
head(po, 10)

plot(po[, 2], po[, 3], col = po$sp, pch = 20, xlab = "long", ylab = "lat")

# especies
sp <- levels(po$sp)
sp

# diretorio
setwd("..")
dir.create("04_mapas")
setwd("04_mapas")
getwd()

# mapas
for(i in 1:length(sp)){
  tiff(paste0(sp[i], ".tif"), wi = 18, he = 18, un = "cm", res = 300, comp = "lzw")
  plot(enm[[i]], main = sp[i])
  points(po[po$sp == sp[i], 2:3], pch = 20, cex = 0.8) 
  map.scale(-35, -27, ratio = F, cex = 0.7)
  dev.off()}

  
  ###------------------------------------------------------------------------------###
