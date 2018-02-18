### script amphibians raster map ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com

###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages 
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, ggplot2, data.table, dismo, maps, viridis, colorRamps, RCurl,
               stringr, lettercase)

## functions
eval(parse(text = getURL("https://gist.githubusercontent.com/mauriciovancine/840428ae5511e78b5681af6f995e6348/raw/12228ca55408ba1cb06357a28ed86be6933a4d25/script_function_scalebar_north_arrow.R", 
                         ssl.verifypeer = F)))

## font
# https://nrelscience.org/2013/05/30/this-is-how-i-did-it-mapping-in-r-with-ggplot2/

# https://github.com/atredennick/Plot_geographical_data

# http://rmhogervorst.nl/cleancode/blog/2017/01/06/plotting-a-map-with-ggplot2.html

###---------------------------------------------------------------------###

## data
# points
po <- fread("E:/github_mauriciovancine/R-ENM/output/_output/_po.csv")
po

# raster
en <- raster("E:/github_mauriciovancine/R-ENM/output/_output/ensemble_wei/ensemble_wei_aver_haddadus_binotatus.tif")
en

# convert the raster to points for plotting
en.p <- rasterToPoints(en)
en.p

# make the points a datatable for ggplot
dt <- data.table(en.p)
dt

# Make appropriate column headings
colnames(dt) <- c("lon", "lat", "suitability")
dt

# vector
br <- getData("GADM", country = "BRA", level = 0)
br

# Now make the map
ggplot(data = dt, aes(y = lat, x = lon)) +
  
  geom_polygon(data = br, aes(x = long, y = lat, group = group),
               color = "black", fill = adjustcolor("white", 1), size = .01) + 
  
  geom_raster(aes(fill = suitability), alpha = .7) +
  
  geom_point(data = po, aes(x = lon, y = lat), shape = 21, 
             size = 1, fill = adjustcolor("black", .9)) +
  
  scale_fill_gradientn("Suitability", colours = matlab.like(100)) + 
  #scale_fill_gradientn("Suitability", colours = viridis(100)) + 
  
  coord_equal() +
  
  theme_minimal() +
  
  scaleBar(lon = -48, lat = -33, distanceLon = 500, distanceLat = 100,
           distanceLegend = -100, dist.unit = "km", legend.size = 3,
           arrow.length = 350, arrow.distance = 200, arrow.North.size = 5) +
  
  theme(legend.position = c(.2, .3),
        legend.background = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_blank()) +
  
  annotate("text", -67, -15, label= "Suitability", size = 5) +
  
  labs(title = bquote("" ~ italic(.(sub("_", " ", str_to_title(unique(po$sp)))))))


ggsave("map.tiff", wi = 15, he = 15, un = "cm", dpi = 100)



###---------------------------------------------------------------------###
