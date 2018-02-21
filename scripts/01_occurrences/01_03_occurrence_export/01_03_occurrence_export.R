### script export occurrences ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 20/02/2018

###---------------------------------------------------------------------------###

## memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

## packages
pacman::p_load(raster, rgdal, data.table, sf)
search()

###---------------------------------------------------------------------------###

# points
po <- data.table(id = rep(1:10, each = 10), 
                 longitude = rnorm(100, -40:-45), 
                 latitude = rnorm(100, -20:-25))
po

co <- "+proj=longlat +datum=WGS84"
co

setwd("E:/github_mauriciovancine/R-ENM/scripts/01_occurrences/01_03_occurrence_export")
dir.create("geopackage")

for(i in unique(po$id)){
  
  # gpkg
  po.i <- subset(po, id == i)
  po.sp <- data.table(po.i, lon = po.i$longitude, lat = po.i$latitude)
  po.sp <- st_as_sf(po.sp, coords = c("lon", "lat"), crs = co)
  setwd("geopackage")
  st_write(po.sp, dsn = paste0("po_", i, ".gpkg"), delete_layer = TRUE)
  setwd("..")
  
  # shp
  po.i <- subset(po, id == i)
  po.sp <- data.table(po.i, lon = po.i$longitude, lat = po.i$latitude)
  coordinates(po.sp) <- ~lon + lat
  crs(po.sp) <- crs(co)
  writeOGR(po.sp, dsn = "shapefile", layer = paste0("po_", i), driver = "ESRI Shapefile", 
           overwrite_layer = TRUE)
  
}

