### script amphibians raster map ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com

###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages 
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, ggplot2, data.table, dismo, maps, viridis, colorRamps, RCurl)

## functions
eval(parse(text = getURL("https://gist.githubusercontent.com/mauriciovancine/840428ae5511e78b5681af6f995e6348/raw/12228ca55408ba1cb06357a28ed86be6933a4d25/script_function_scalebar_north_arrow.R", 
                         ssl.verifypeer = F)))

## font
# https://nrelscience.org/2013/05/30/this-is-how-i-did-it-mapping-in-r-with-ggplot2/

# https://github.com/atredennick/Plot_geographical_data

###---------------------------------------------------------------------###

## data
# directory
setwd("E:/github/enmR/data")

# points
po <- fread("Bromelia_balansae.txt")
po

# raster
en <- raster("CCSM_0k_am_bio02.tif")
en

# convert the raster to points for plotting
en.p <- rasterToPoints(en)
en.p

# make the points a datatable for ggplot
dt <- data.table(en.p)
dt

# Make appropriate column headings
colnames(dt) <- c("long", "lat", "suitability")
dt

# vector
am <- shapefile("south_america_gcs_wgs84.shp")
am


# Now make the map
ggplot(data = dt, aes(y = lat, x = long)) +
  
  geom_raster(aes(fill = suitability)) +
  
  geom_point(data = po, aes(x = long, y = lat), shape = 21, 
             size = 2, fill = adjustcolor("black", .9)) +
  
  scale_fill_gradientn("Bio 02", colours = matlab.like(100)) + 
  # scale_fill_gradientn("Bio 02", colours = viridis(100)) + 
  
  coord_equal() +
  
  theme_minimal() +
  
  scaleBar(lon = -95, lat = -55, distanceLon = 500, distanceLat = 150,
           distanceLegend = -200, dist.unit = "km", legend.size = 3,
           arrow.length = 500, arrow.distance = 300, arrow.North.size = 5) +
  
  theme(legend.position = c(.1, .35),
        legend.background = element_blank(),
        legend.text = element_text(size = 7),
        legend.title = element_blank()) +
  
  annotate("text", -92, -22, label= "Bio 02", size = 4) +
  
  labs(title = bquote("" ~ italic(.(sub("_", " ", unique(po$sp))))))


ggsave("map.tiff", wi = 15, he = 15, un = "cm", dpi = 100)



###---------------------------------------------------------------------###
