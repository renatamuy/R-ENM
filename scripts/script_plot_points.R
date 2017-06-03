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

# pontos
# diretorio da pasta de dados de entrada
setwd("D:/mineracao/01_dados")

# occurrence 
po <- read.table("ocorrencias_anfibios_10.txt", h = T)
head(po, 10)

plot(po[, 2], po[, 3], col = po$sp, pch = 20, xlab = "long", ylab = "lat")

# especies
sp <- levels(po$sp)
sp

# limite 
sh <- shapefile("limite_gcs_wgs.shp")
sh
plot(sh)

# diretorio
setwd("..")
dir.create("05_mapas_ocorrencias")
setwd("05_mapas_ocorrencias")
getwd()

# mapas
for(i in 1:length(sp)){
  tiff(paste0(sp[i], ".tif"), wi = 18, he = 18, un = "cm", res = 300, comp = "lzw")
  plot(sh, axes = T, col = "gray80", main = sp[i])
  points(po[po$sp == sp[i], 2:3], pch = 20, cex = 0.8) 
  map.scale(-35, -25, ratio = F, cex = 0.7)
  dev.off()}


###------------------------------------------------------------------------------###
