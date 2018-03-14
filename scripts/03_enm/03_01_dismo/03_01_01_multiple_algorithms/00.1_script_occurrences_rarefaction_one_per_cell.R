### script points to shapefile ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 06/11/2017

###---------------------------------------------------------------------###

# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# temp
tempdir <- function() "D:\\temps"
unlockBinding("tempdir", baseenv())
assignInNamespace("tempdir", tempdir, ns = "base", envir = baseenv())
assign("tempdir", tempdir, baseenv())
lockBinding("tempdir", baseenv())
tempdir()

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, data.table, vegan)

# verify packages
search()

###---------------------------------------------------------------------###

# variables
va <- raster("C:/Dados/GitHub/environmental_data/cur/2_5m/bio/bio01.tif")
va

# direstory
setwd("C:/dados/Github/Modelagem")
getwd()

# import
da <- fread("data/plantascalcariomodelo_unico_todas.txt")
da

da.sp.ra <- data.table()

# shapefile
setwd('data')
dir.create('points'); setwd('points')
for(i in unique(da$sp)){
  da.sp <- da[sp == i]
  
  da.sp <- data.table(da.sp, longitude = da.sp$long, latitude = da.sp$lat,
                      va = 1)
  
  coordinates(da.sp) <- ~ long + lat
  crs(da.sp) <- CRS("+init=epsg:4326")
  
  writeOGR(da.sp, "PointsAll", sub(" ", "_", paste0(i)), 
           driver = "ESRI Shapefile", overwrite_layer = T)
  
  ra <- rasterize(da.sp@data[, 2:3], va, da.sp@data$va)
  ra
  
  ra.sh <- rasterToPoints(ra)
  
  ra.sh.ex <- na.omit(data.table(ra.sh, extract(va, ra.sh[, 1:2])))[, 1:2]
  
  da.sp.ra <- rbind(da.sp.ra, 
                    data.table(sp = i, long = ra.sh.ex[, 1], lat = ra.sh.ex[, 2]))
  
  }
setwd('..')
da.sp.ra

getwd()

fwrite(da.sp.ra, "OnePointPerCell_2_5m.csv")





