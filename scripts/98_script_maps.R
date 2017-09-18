### script amphibians raster map ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com

###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages 
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, ggplot2, data.table, dismo, maps, colorRamps)

###---------------------------------------------------------------------###

## font
# https://github.com/atredennick/Plot_geographical_data

###---------------------------------------------------------------------###

# raster
map <- getData("worldclim", var = "bio", res = 10)
map

# convert the raster to points for plotting
map.p <- rasterToPoints(map[[1]])
map.p

# Make the points a datatable for ggplot
dt <- data.table(map.p)
dt$bio01_10 <- dt$bio01 / 10
  
# Make appropriate column headings
colnames(dt) <- c("long", "lat", "bio01", "bio01_10")
dt

# Call in point data, in this case a fake transect (csv file with lat and lon coordinates)
data(world.cities)

sites <- data.table(x = world.cities$long, y = world.cities$lat) 
sites


# Now make the map
ggplot(data = dt, aes(y = lat, x = long)) +
  geom_raster(aes(fill = bio01_10)) +
  scale_fill_gradientn("Temp (º C)", colours = matlab.like2(100)) + 
  geom_point(data = sites, aes(x = x, y = y), color = "black", size = .1, shape = 20) +
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