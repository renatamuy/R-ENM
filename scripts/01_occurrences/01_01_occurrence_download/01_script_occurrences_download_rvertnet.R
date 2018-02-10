### script download occurrences - data bases ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 25/07/2017

### rgbif ###

###---------------------------------------------------------------------------###

## memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

## packages
pacman::p_load(rvertnet, ggmap, data.table)

# verify
search()

###---------------------------------------------------------------------------###

## download
# search
se <- vertsearch(taxon = "Vitreorana uranoscopa")
se$data

colnames(se$data)

# export
fwrite(se$data, "data_vertnet.txt", sep = "\t")

###---------------------------------------------------------------------------###

# map
af <- get_map(location = c(min(na.omit(as.numeric(se$data$decimallongitude))), 
                           max(na.omit(as.numeric(se$data$decimallatitude))), 
                           max(na.omit(as.numeric(se$data$decimallongitude))), 
                           min(na.omit(as.numeric(se$data$decimallatitude)))), zoom = 5)

ggmap(af, extent = "panel") +
  geom_point(data = se$data, aes(x = as.numeric(decimallongitude), y = as.numeric(decimallatitude)), size = 2.7, col = "red") +
  geom_point(data = se$data, aes(x = as.numeric(decimallongitude), y = as.numeric(decimallatitude)), size = 3, pch = 1) +
  theme(legend.position = c(0.9, 0.2),
        legend.background = element_rect(color = "black", fill = "grey90", 
                                         size = 1, linetype = "solid")) +
  ggtitle(expression("Occurrences of" ~ italic("Vitreorana uranoscopa")))

ggsave("map_vertnet_vitreorana_uranoscopa.tiff", dpi = 300)
