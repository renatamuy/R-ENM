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
setwd("D:/mineracao/02_saidas/lpt")

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
po <- read.table("ocorrencias.txt", h = T)
head(po, 10)

plot(po[, 2], po[, 3], col = po$sp, pch = 20, xlab = "long", ylab = "lat")

# especies
sp <- levels(po$sp)
sp

# estados
es <- shapefile("limite_gcs_wgs.shp")
es

plot(es)

# diretorio
setwd("..")
dir.create("04_mapas")
setwd("04_mapas")
dir.create("distribuicoes_finais")
setwd("distribuicoes_finais")
getwd()
dir()

# mapas
for(i in 1:length(sp)){
  tiff(paste0(sp[i], "_lpt.tif"), wi = 18, he = 18, un = "cm", res = 300, comp = "lzw")
  plot(enm[[i]], main = sp[i], legend = F, col = c("gray90", "black"))
  lines(es, col = "gray20")
  points(po[po$sp == sp[i], 2:3], pch = 20, cex = .9, col = "red") 
  map.scale(-51, -27, ratio = F, cex = 0.8)
  legend(-36, -25, c("Presence", "Absence"), fill = c("black", "gray90"), bty = "n")
  legend(-35.7, -24, "Occurrences", pch = 20, col = "red", bty = "n")
  dev.off()}

  
###------------------------------------------------------------------------------###

# directory
setwd("D:/mineracao/03_analises/cortes")

# enms direct
tif.d <- list.files(patt = "_direct")
tif.d

enm.d <- stack(tif.d)
enm.d

# enms indirect
tif.i <- list.files(patt = "indirect")
tif.i

enm.i <- stack(tif.i)
enm.i

# enms
tif.o <- list.files(patt = "out")
tif.o

enm.o <- stack(tif.o)
enm.o


# plots
setwd("D:/mineracao/04_mapas/limites")

for(i in 1:length(sp)){
  tiff(paste0(sp[i], "_direct.tif"), wi = 18, he = 18, un = "cm", res = 300, comp = "lzw")
  plot(enm.d[[i]], main = paste0(sp[i], "_direct"), legend = F, col = c("gray50", "black"))
  lines(es, col = "gray20")
  map.scale(-51, -27, ratio = F, cex = 0.8)
  legend(-36, -25, c("Presence", "Absence"), fill = c("black", "gray50"), bty = "n")
  dev.off()}


for(j in 1:length(sp)){
  tiff(paste0(sp[j], "_indirect.tif"), wi = 18, he = 18, un = "cm", res = 300, comp = "lzw")
  plot(enm.i[[j]], main = paste0(sp[j], "_indirect"), legend = F, col = c("gray50", "black"))
  lines(es, col = "gray20")
  map.scale(-51, -27, ratio = F, cex = 0.8)
  legend(-36, -25, c("Presence", "Absence"), fill = c("black", "gray50"), bty = "n")
  dev.off()}

for(k in 1:length(sp)){
  tiff(paste0(sp[k], "_output.tif"), wi = 18, he = 18, un = "cm", res = 300, comp = "lzw")
  plot(enm.o[[k]], main = paste0(sp[k], "_output"), legend = F, col = c("gray50", "black"))
  lines(es, col = "gray20")
  map.scale(-51, -27, ratio = F, cex = 0.8)
  legend(-36, -25, c("Presence", "Absence"), fill = c("black", "gray50"), bty = "n")
  dev.off()}


# numero de pixels
ta <- data.table()

for(i in 1:40){
  da.d <- data.table(sp = names(enm.d[[i]]), table(values(enm.d[[i]])))
  da.i <- data.table(sp = names(enm.i[[i]]), table(values(enm.i[[i]])))
  da.o <- data.table(sp = names(enm.o[[i]]), table(values(enm.o[[i]])))
  da <- rbind(da.d, da.i, da.o)
  ta <- rbind(ta, da)}
ta

write.table(ta, "_pixels.xls", quote = F, row.names = F, sep = "\t")
write.table(ta, "_pixels.txt", quote = F, row.names = F, sep = "\t")


