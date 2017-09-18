### script amphibians raster map ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com

###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages 
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, ggplot2, data.table, dismo, maps, colorRamps, ncdf4)

###---------------------------------------------------------------------###

# directory
setwd("E:/github/enmR/data")

# read netdf
map <- raster("africa_cru_3.0.1961.2006.meanAnnPrec.nc")
map

# convert the raster to points for plotting
map.p <- rasterToPoints(map)
map.p

# make the points a dataframe for ggplot
dt <- data.table(map.p)
dt$precipitation[dt$precipitation < 0] <- NA
dt

# make appropriate column headings
colnames(dt) <- c("long", "lat", "precipitation")
dt

###---------------------------------------------------------------------###

# now make the map
ggplot(data = dt, aes(y = lat, x = long)) +
  geom_raster(aes(fill = precipitation)) +
  scale_fill_gradientn("Prep (mm)", colours = matlab.like2(100)) + 
  theme_bw() +
  coord_equal() +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16, angle = 90),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key = element_blank()) 

ggsave("map.tiff", dpi = 300)

###---------------------------------------------------------------------###
