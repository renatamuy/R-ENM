### script exportar pontos para shape ###

# Maurício Humberto Vancine
# 30/05/2017

###---------------------------------------------------------------------###

# memoria
rm(list = ls())
memory.limit(size = 1.75e13)

# pacotes
#install.packages(c("raster", "rgdal"), dep = T)

library(raster)
library(rgdal)

###---------------------------------------------------------------------###

## pontos
# diretorio
setwd("")

# occurrence 
po <- read.table("", h = T)
head(po, 10)

po <- data.frame(sp = rep(paste0("sp0", 1:9), each = 5), long = rnorm(45, -40, 5),
                 lat = rnorm(45, -40, 5))
po

plot(po[, 2], po[, 3], col = po$sp, pch = 20, xlab = "long", ylab = "lat")

# especies
sp <- levels(po$sp)
sp

# shapes dos pontos
for(i in 1:length(sp)){
  po.s <- po[po$sp == sp[i], ]
  coordinates(po.s) <- ~long + lat
  crs(po.s) <- CRS("+proj=longlat +datum=WGS84")
  writeOGR(po.s, "D:/mineracao/01_dados/aves", paste0(sp[i]), 
           driver = "ESRI Shapefile", overwrite = T)}

###---------------------------------------------------------------------###