### script rarefaction ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# Pablo Hendrigo Alves de Melo - pablopains@yahoo.com.br 
# 07/11/2017

###---------------------------------------------------------------------###

# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, data.table, vegan)

###---------------------------------------------------------------------###

# directory
setwd("E:/github/enmR/data")

# points
po <- fread("Bromelia_balansae.txt")
po

# variable
va <- raster("CCSM_0k_am_bio02.tif")
projection(va) <- "+init=epsg:4326"


# copy lat and long, more 1 value
po <- data.table(po, longitude = po$long, latitude = po$lat, va = 1)
po

coordinates(po) <- ~ long + lat
crs(po) <- crs(va)

ra <- rasterize(po@data[, 2:3], va, po@data$va)
ra

po.r <- data.table(sp = unique(po$sp), 
                   long = rasterToPoints(ra)[, 1], 
                   lat = rasterToPoints(ra)[, 2])
po.r

plot(ra)
points(po$longitude, po$latitude, pch = 20)
points(po.r$long, po.r$lat, pch = 20, col = "red")


fwrite(po.r, "Bromelia_balansae_rarefaction.csv")



# one point per cell
ra <- data.table(rasterToPoints(en[[1]])[, 1:2])
ra <- data.table(id = 1:nrow(ra), ra)
gridded(ra) <- ~ x + y
ra.r <- raster(ra) 
crs(ra.r) <- crs(va)  
plot(ra.r, col = viridis(100))
points(po$lon, po$lat, pch = 20)

po$oppc <- extract(ra.r, po[, c(2:3)])
table(po$oppc)
write.csv(po, "po_check_oppc.csv")

po <- na.omit(distinct(po, oppc, .keep_all = TRUE))
po

plot(en[[1]], col = viridis(100))
points(bc, pch = 20, cex = .5, col = "blue")
points(po$lon, po$lat, pch = 20, cex = .5, col = "red")


###---------------------------------------------------------------------###


# For multiple species

temp = list.files(pattern="*.csv")

da = do.call(rbind, lapply(temp, function(x) fread(x, stringsAsFactors = FALSE)))
                           
da.sp.ra <- data.table()

# shapefile
for(i in unique(da$sp)){
  da.sp <- da[sp == i]
  
  da.sp <- data.table(da.sp, longitude = da.sp$long, latitude = da.sp$lat,
                      va = 1)
  
  coordinates(da.sp) <- ~ long + lat
  projection(da.sp) <- "+init=epsg:4326"
  
  writeOGR(da.sp, "pontos", sub(" ", "_", paste0(i)), 
           driver = "ESRI Shapefile", overwrite_layer = T)
  
  ra <- rasterize(da.sp@data[, 2:3], va, da.sp@data$va)
  ra
  
  ra.sh <- rasterToPoints(ra)
  
  da.sp.ra <- rbind(da.sp.ra, 
                    data.table(sp = i, long = ra.sh[, 1], lat = ra.sh[, 2]))
  
  }

da.sp.ra

fwrite(da.sp.ra, "points.csv")


###---------------------------------------------------------------------###


